/// Produces Swift code for parsing a grammar.
public class SwiftCodeGen {
    // TODO: Refactor to use MetaPropertyManager

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the raw contents to print atop the generated parser code.
    ///
    /// `parserHeader`
    public static let parserHeader: String = "parserHeader"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the raw contents to print atop the generated token type code.
    ///
    /// `tokenTypeHeader`
    public static let tokenTypeHeader: String = "tokenTypeHeader"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the name of the token type to generated.
    ///
    /// If no explicit token type is specified, the token's typename is set to
    /// `<@parserName>Token` by default.
    ///
    /// `tokenTypeName`
    public static let tokenTypeName: String = "tokenTypeName"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the name of the parser class to extend with the parsing methods.
    /// Assumes that the type exists already.
    ///
    /// `parserName`
    public static let parserName: String = "parserName"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the strategy of token call to emit; by default token checks
    /// are emitted with `PEGParser.expect(_:)`, and specifying a value of
    /// 'expectKind' for this meta-property indicates that the code generator
    /// should emit `PEGParser.expect(kind:)` calls for the string literals,
    /// instead.
    ///
    /// `tokenCall`
    public static let tokenCall: String = "tokenCall"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates whether to generate `return` statements for generated actions
    /// implicitly or not.
    ///
    /// Defaults to `true`, can be specified `true` or `false`, as either strings
    /// or identifiers.
    ///
    /// `implicitReturns`
    public static let implicitReturns: String = "implicitReturns"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates whether to generate named pattern binds for generated if-let
    /// patterns for alts.
    ///
    /// Defaults to `true`, can be specified `true` or `false`, as either strings
    /// or identifiers.
    ///
    /// `implicitBindings`
    public static let implicitBindings: String = "implicitBindings"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates whether to generate named patterns for token literals. Only in
    /// effect is `implicitBindings` is also `true`.
    ///
    /// Defaults to `true`, can be specified `true` or `false`, as either strings
    /// or identifiers.
    ///
    /// `bindTokenLiterals`
    public static let bindTokenLiterals: String = "bindTokenLiterals"

    /// Set of identifiers that cannot be used as bare identifiers in Swift, and
    /// must be escaped with backticks (`) to allow usage in declarations.
    public static var invalidBareIdentifiers: Set<String> {
        SwiftKeywords.keywords
    }

    /// Set of identifiers that cannot be used as bare labels in labeled expressions
    /// in Swift, and must be escaped with backticks (`) to allow usage in labeled
    /// expressions.
    public static var invalidBareLabels: Set<String> {
        ["inout"]
    }

    /// Used to keep track of reentrance in `typeForRule()` calls.
    var _typeForRuleOngoing: Set<String> = []

    let parserName: String
    let processedGrammar: ProcessedGrammar
    var grammar: InternalGrammar.Grammar {
        processedGrammar.grammar
    }
    var tokenDefinitions: [InternalGrammar.TokenDefinition] {
        processedGrammar.tokens
    }
    let buffer: CodeStringBuffer
    var declContext: DeclarationsContext

    var latestSettings: ParserGenSettings = .default
    var tokenCallKind: TokenCallKind = .expect

    /// List of productions generated so far.
    var generated: [RemainingProduction] = []
    /// Queue of productions that still remain to be generated.
    var remaining: [RemainingProduction] = []

    /// Aliasing for rules that may be used to circumvent name clashes.
    /// Maps grammar rule name -> generate code name.
    var ruleAliases: [String: String] = [:]

    var implicitReturns: Bool = true
    var implicitBindings: Bool = true
    var bindTokenLiterals: Bool = false
    var producerProtocol: ProducerProtocolInfo?

    var bindingEngine: BindingEngine

    /// Initializes a new `SwiftCodeGen`, preparing to generate the grammar and
    /// token definitions from a given grammar processor result.
    public init(from processedGrammar: ProcessedGrammar) {
        self.processedGrammar = processedGrammar
        self.tokenCallKind = processedGrammar.grammar.tokenCall().flatMap(TokenCallKind.init) ?? .expect

        parserName = processedGrammar.grammar.parserName() ?? "Parser"
        buffer = CodeStringBuffer()
        declContext = DeclarationsContext()
        bindingEngine = BindingEngine()
    }

    /// Initializes a new `SwiftCodeGen`, preparing to generate a given grammar.
    ///
    /// - Parameters:
    ///   - grammar: The grammar to generate.
    ///   - tokenDefinitions: A list of token definitions to use when examining string literals.
    public convenience init(
        grammar: InternalGrammar.Grammar,
        tokenDefinitions: [InternalGrammar.TokenDefinition] = [],
        ruleDependencyGraph: RuleDependencyGraph = .empty,
        tokenOcclusionGraph: TokenOcclusionGraph = .empty
    ) {
        self.init(
            from: .init(
                grammar: grammar,
                tokens: tokenDefinitions,
                ruleDependencyGraph: ruleDependencyGraph,
                tokenOcclusionGraph: tokenOcclusionGraph
            )
        )
    }

    /// Generates Swift parser code.
    public func generateParser(
        settings: ParserGenSettings = .default
    ) throws -> String {

        let decls = try _generateParser(settings: settings)

        let emitter = SwiftASTEmitter()
        let conditional = emitter.buffer.startConditionalEmitter()
        for decl in decls {
            conditional.emit("\n")
            emitter.emit(decl)
        }

        return emitter.finishBuffer()
    }

    private func memoizationMode(for rule: InternalGrammar.Rule) -> MemoizationMode {
        return
            if rule.isLeftRecursiveLeader {
                .memoizedLeftRecursive
            } else if !rule.isLeftRecursive {
                .memoized
            } else {
                .none
            }
    }

    private func memoizationMode(for production: RemainingProduction) -> MemoizationMode {
        switch production {
        case .rule(let rule):
            return memoizationMode(for: rule)
        case .auxiliary(_, let info), .nonStandardRepetition(let info, _):
            return info.memoizationMode
        }
    }

    // MARK: - Auxiliary structures

    enum TokenCallKind: String {
        /// "self.expect(<token value>)"
        case expect

        /// "self.expect(kind: <token value>)"
        case expectKind
    }

    /// Describes an error that can be raised during Swift parser code generation.
    public enum Error: Swift.Error, CustomStringConvertible {
        /// Issued during token type code generation, indicates that a token
        /// definition was found that has no syntax defined, so a unified token
        /// type cannot be generated.
        case tokenDefinitionMissingSyntax(InternalGrammar.TokenDefinition)

        /// Issued during token type code generation, indicates the associated
        /// `ProcessedGrammar.tokenOcclusionGraph` contains dependants of dynamic
        /// tokens that are not static.
        case tokenDependantIsNotStatic(
            InternalGrammar.TokenDefinition,
            dependant: InternalGrammar.TokenDefinition
        )

        public var description: String {
            switch self {
            case .tokenDefinitionMissingSyntax(let def):
                return """
                Cannot generate token type: All tokens must have a syntax defined; \
                found token '\(def.name)' that has no syntax.
                """
            case .tokenDependantIsNotStatic(let token, let dependant):
                return """
                Found token '\(token.name)' that has a dependant '\(dependant.name)' \
                that has a non-static syntax '\(dependant.tokenSyntax?.description ?? "<nil>")'.
                """
            }
        }
    }

    /// Settings that can be specified during parser code generation.
    public struct ParserGenSettings {
        /// Gets the static default settings configuration.
        public static let `default`: Self = Self(
            omitUnreachable: false,
            emitTypesInBindings: false,
            omitRedundantMarkRestores: false,
            emitProducerProtocol: false,
            emitVoidProducer: false
        )

        /// Whether to omit unreachable rules, as detected by a `GrammarProcessor`
        /// and flagged within each `InternalGrammar.Rule` node.
        public var omitUnreachable: Bool

        /// Whether to emit type annotations on all if-let bindings emitted.
        /// Type annotating only occurs when the type of the production being
        /// bound is resolvable by the code generator.
        public var emitTypesInBindings: Bool

        /// Whether to make attempts to omit redundant `self.mark()`/`self.restore(mark)`
        /// calls in parsing methods.
        ///
        /// Mark/restore calls can be omitted if all methods in the parser (including
        /// built-in PEGParser methods like `expect` and `maybe`) follow the rule
        /// that on failure, they restore the parser to the same point as when
        /// they where called. In such cases, if a parsing function's alt only
        /// calls one method in the alternative, a failure results in the parser
        /// backtracking from that method call, which is the same as backtracking
        /// to the start of the alternative.
        ///
        /// Emitting mark/restore calls around all parsing methods may reduce the
        /// chances of a rough parsing method breaking callers, and the overhead
        /// of calling a restore on a mark that points to the current parser state
        /// is very low, so the calls should only be removed if they become a
        /// measurable bottleneck.
        public var omitRedundantMarkRestores: Bool

        /// Whether to emit a `<Parser>Producer` protocol alongside the main parser
        /// extension which implements the actual production of alternatives, and
        /// can be swapped out for different implementations.
        ///
        /// This makes the main parser extension require a second generic type
        /// argument which takes in the producer that is used to produce the
        /// alternative results in its place.
        public var emitProducerProtocol: Bool

        /// If `emitProducerProtocol` is `true`, whether to emit another implementation
        /// of the protocol that produces all `Void` return values.
        ///
        /// If `emitProducerProtocol` is not `true`, this setting has no effect.
        public var emitVoidProducer: Bool

        public init(
            omitUnreachable: Bool,
            emitTypesInBindings: Bool,
            omitRedundantMarkRestores: Bool,
            emitProducerProtocol: Bool,
            emitVoidProducer: Bool
        ) {
            self.omitUnreachable = omitUnreachable
            self.emitTypesInBindings = emitTypesInBindings
            self.omitRedundantMarkRestores = omitRedundantMarkRestores
            self.emitProducerProtocol = emitProducerProtocol
            self.emitVoidProducer = emitVoidProducer
        }

        /// Returns a copy of `self` with a given keypath modified to be `value`.
        public func with<T>(_ keyPath: WritableKeyPath<Self, T>, value: T) -> Self {
            var copy = self
            copy[keyPath: keyPath] = value
            return copy
        }
    }

    /// Settings that can be specified during token type code generation.
    public struct TokenTypeGenSettings {
        /// Gets the static default settings configuration.
        public static let `default`: Self = Self(
            emitInlinable: false,
            accessLevel: nil,
            emitLengthSwitchPhaseInTokenOcclusionSwitch: false
        )

        /// Whether to emit tokenization methods as @inlinable declarations.
        public var emitInlinable: Bool

        /// The access level to emit the declarations as. If `nil`, declarations
        /// have no access level specified.
        ///
        /// Providing an access level different than `"internal"` or `nil` also
        /// generates an initializer for struct declarations.
        public var accessLevel: String?

        /// In the main token parsing body, a token that occludes another (such
        /// as a dynamic identifier token occluding a fixed keyword token), a
        /// switch is emitted that favors the fixed token constructions occluded
        /// by dynamic tokens. If this setting is `true`, an extra switch is
        /// emitted that further divides the fixed tokens along length before
        /// the final switch over each fixed token's string literal.
        ///
        /// The switch is only emitted if more than three fixed tokens are occluded
        /// by the same dynamic token, and at least two of the fixed tokens share
        /// the same literal string length.
        public var emitLengthSwitchPhaseInTokenOcclusionSwitch: Bool

        public init(
            emitInlinable: Bool,
            accessLevel: String?,
            emitLengthSwitchPhaseInTokenOcclusionSwitch: Bool
        ) {
            self.emitInlinable = emitInlinable
            self.accessLevel = accessLevel
            self.emitLengthSwitchPhaseInTokenOcclusionSwitch = emitLengthSwitchPhaseInTokenOcclusionSwitch
        }

        /// Returns a copy of `self` with a given keypath modified to be `value`.
        public func with<T>(_ keyPath: WritableKeyPath<Self, T>, value: T) -> Self {
            var copy = self
            copy[keyPath: keyPath] = value
            return copy
        }
    }

    /// Contains metadata about an auxiliary rule/method.
    struct AuxiliaryRuleInformation {
        typealias ReturnElement = (label: String?, type: CommonAbstract.SwiftType)

        /// The auxiliary rule's name.
        var name: String

        /// Holds the contents of common named items exposed by the rule's return
        /// type, as a tuple of elements.
        var bindings: [BindingEngine.Binding]

        /// The memoization mode for the rule.
        var memoizationMode: MemoizationMode

        /// Returns `true` if `self` and `other` are equal except for their names.
        func isEquivalent(to other: Self) -> Bool {
            self.bindings.elementsEqual(other.bindings, by: ==) && self.memoizationMode == other.memoizationMode
        }
    }

    /// The mode of memoization for a generated method.
    enum MemoizationMode: Equatable {
        /// No memoization.
        case none

        /// Use standard memoization.
        case memoized

        /// Use memoization that is specialize for left-recursive rule leaders.
        case memoizedLeftRecursive
    }

    /// Contains information about a non-standard repetition construct.
    struct RepetitionInfo: Equatable {
        /// The actual repetition item.
        var repetition: InternalGrammar.NamedItem

        /// A list of trailing items that must be parsed to succeed the repetition
        /// production.
        var trail: [InternalGrammar.NamedItem]
    }

    /// Encapsulates a remaining rule/auxiliary rule production.
    enum RemainingProduction {
        /// A grammar rule to generate.
        case rule(InternalGrammar.Rule)

        /// An auxiliary rule.
        case auxiliary(InternalGrammar.Rule, AuxiliaryRuleInformation)

        /// An auxiliary method that refers to a non-standard repetition within
        /// a rule's alternative.
        case nonStandardRepetition(
            AuxiliaryRuleInformation,
            RepetitionInfo
        )

        /// Returns a non-deduplicated identifier that can be used to refer to
        /// this production's method.
        ///
        /// For rule productions and auxiliary rules, it is the rule's name, and
        /// for other constructs it is the String value that will be used to generate
        /// the production's method's name.
        ///
        /// Can be set to alter the underlying production's name.
        var name: String {
            get {
                switch self {
                case .rule(let rule), .auxiliary(let rule, _):
                    return rule.name
                case .nonStandardRepetition(let ruleInfo, _):
                    return ruleInfo.name
                }
            }
            set {
                switch self {
                case .rule(var rule):
                    rule.name = newValue
                    self = .rule(rule)
                case .auxiliary(var rule, var info):
                    rule.name = newValue
                    info.name = newValue
                    self = .auxiliary(rule, info)
                case .nonStandardRepetition(var ruleInfo, let repInfo):
                    ruleInfo.name = newValue
                    self = .nonStandardRepetition(ruleInfo, repInfo)
                }
            }
        }

        /// Returns the production type associated with this production's method.
        ///
        /// For rule productions and auxiliary rules, it is the rule's
        /// ``InternalGrammar.Rule.type``, and for other constructs it is the
        /// computed value associated with the production's result.
        var productionType: CommonAbstract.SwiftType? {
            switch self {
            case .rule(let rule), .auxiliary(let rule, _):
                return rule.type
            case .nonStandardRepetition(let ruleInfo, _):
                return ruleInfo.bindings.be_asTupleType()
            }
        }

        /// Returns `true` if this production refers to a rule from the grammar.
        var isGrammarRule: Bool {
            switch self {
            case .rule: true
            default: false
            }
        }

        /// Returns `true` if `self` and `other` reference productions that are
        /// equivalent in terms of generated code, apart from function names.
        func isEquivalent(to other: Self) -> Bool {
            switch (self, other) {
            case (.rule(let lhs), .rule(let rhs)):
                return lhs.isEquivalent(to: rhs)

            case (.auxiliary(let lhsRule, let lhsInfo), .auxiliary(let rhsRule, let rhsInfo)):
                return lhsRule.isEquivalent(to: rhsRule) && lhsInfo.isEquivalent(to: rhsInfo)

            case (.nonStandardRepetition(let lhsRule, let lhsInfo), .nonStandardRepetition(let rhsRule, let rhsInfo)):
                return lhsRule.isEquivalent(to: rhsRule) && lhsInfo == rhsInfo

            default:
                return false
            }
        }
    }
}

// MARK: - Auxiliary method management

extension SwiftCodeGen {
    /// Generates a default expression representing the return of an alternative,
    /// ignoring its associated action.
    func defaultReturnExpression(
        for alt: InternalGrammar.Alt,
        productionType: CommonAbstract.SwiftType?
    ) -> String {
        // If no action is specified, attempt to return instead the named
        // item within the alt, if it's the only named item in the alt.
        if alt.namedItems.count == 1 {
            let bindings = bindingEngine.computeBindings(alt)
            if bindings.count == 1, let label = bindings[0].label {
                return escapeIdentifier(label)
            }
        }

        // Fallback: Return an initialization of the associated node type, assuming
        // it is not `nil` and is not a known existential type.
        if let type = productionType?.description, type != "Any" {
            if type == "Void" || type == "()" {
                return "()"
            } else {
                return "\(type)()"
            }
        }

        // If no other option was found, default to 'Node()'
        return "Node()"
    }

    /// Attempts to compute a default return action for a given set of return
    /// elements of a rule or auxiliary rule.
    func defaultReturnAction(
        for elements: [AuxiliaryRuleInformation.ReturnElement]
    ) -> InternalGrammar.Action {

        return defaultReturnAction(for: elements.map { element in
            (label: element.label ?? "_", identifier: element.label ?? "_")
        })
    }

    /// Attempts to compute a default return action for a given set of
    /// `<label>: <identifier>` pair expressions.
    func defaultReturnAction(
        for labeledExpressions: [(label: String, identifier: String)]
    ) -> InternalGrammar.Action {

        return .init(
            string: " \(defaultReturnExpression(for: labeledExpressions)) "
        )
    }

    /// Attempts to compute a default return expression for a given set of
    /// `<label>: <identifier>` pair expressions.
    func defaultReturnExpression(
        for labeledExpressions: [(label: String?, identifier: String)]
    ) -> String {

        // Avoid emitting single-tuple constructions
        if labeledExpressions.count == 1 {
            return escapeIdentifier(labeledExpressions[0].identifier)
        } else {
            let tupleElements = labeledExpressions.map { expr in
                if let label = expr.label {
                    "\(label): \(escapeIdentifier(expr.identifier))"
                } else {
                    escapeIdentifier(expr.identifier)
                }
            }.joined(separator: ", ")

            return "(\(tupleElements))"
        }
    }

    /// Enqueues an auxiliary rule to be generated based on a given rule as context.
    /// Returns the deduplicated, unique method name to use as a reference for
    /// further code generation.
    ///
    /// The type of the rule will be computed by inspecting the return elements
    /// of the alts provided, and will fallback to `Any` if no suitable result
    /// type could be computed.
    ///
    /// - note: Actions of all alts are replaced with the return expression for
    /// the auxiliary rule.
    func enqueueAuxiliaryRule(
        for production: RemainingProduction,
        suffix: String,
        unwrapped: Bool,
        alts: [InternalGrammar.Alt]
    ) -> String {

        var alts = alts

        var elements = bindingEngine.computeBindings(alts)
        if unwrapped {
            elements = elements.be_unwrapped()
        }

        let type: CommonAbstract.SwiftType =
            if elements.isEmpty {
                "Void"
            } else if elements.count == 1 {
                elements[0].type
            } else {
                .tuple(elements.map { element in
                    .init(label: element.label, element.type)
                })
            }

        // Produce a common action that passes the bound elements back to the
        // caller
        var action: InternalGrammar.Action = .init(string: " () ")
        if type != "Void" {
            action = defaultReturnAction(for: elements)
        }
        alts = alts.map { alt in
            var alt = alt
            alt.action = action
            return alt
        }

        return enqueueAuxiliaryRule(
            for: production,
            suffix: suffix,
            type: type,
            alts: alts
        )
    }

    /// Enqueues an auxiliary rule to be generated based on a given rule as context.
    /// Returns the deduplicated, unique method name to use as a reference for
    /// further code generation.
    ///
    /// The type of the rule will be computed by inspecting the return elements
    /// of the alts provided, and will fallback to `Any` if no suitable result
    /// type could be computed.
    ///
    /// - note: Action for the generated alt wrapping the item is replaced with
    /// the return expression for the auxiliary rule.
    func enqueueAuxiliaryRule(
        for production: RemainingProduction,
        suffix: String,
        namedItem: InternalGrammar.NamedItem
    ) -> String {

        enqueueAuxiliaryRule(for: production, suffix: suffix, unwrapped: true, alts: [
            .init(namedItems: [namedItem])
        ])
    }

    /// Enqueues an auxiliary rule to be generated based on a given production as
    /// context.
    /// Returns the deduplicated, unique method name to use as a reference for
    /// further code generation.
    ///
    /// - note: Return type will be optionally-wrapped by this method.
    func enqueueAuxiliaryRule(
        for production: RemainingProduction,
        suffix: String,
        type: CommonAbstract.SwiftType,
        alts: [InternalGrammar.Alt]
    ) -> String {

        let name = "_\(production.name)_\(suffix)"

        let memoization = memoizationMode(for: production)
        let info = AuxiliaryRuleInformation(
            name: name,
            bindings: bindingEngine.computeBindings(alts),
            memoizationMode: memoization == .memoizedLeftRecursive ? .none : memoization
        )

        let ruleName = enqueueAuxiliaryRule(
            .init(name: name, type: type, alts: alts),
            info
        )

        return ruleName
    }

    /// Enqueues an auxiliary production that encapsulates a minimal/maximal (`<`/`>`)
    /// repetition segment of an alternative's list of items.
    ///
    /// - note: Expects the first element of the sequence of named items to be
    /// the non-standard sequence, and the list of items to have more than one
    /// item.
    /// - precondition: `namedItems.count > 1`
    func enqueueNonStandardRepetition(
        for production: RemainingProduction,
        _ namedItems: some Collection<InternalGrammar.NamedItem>
    ) -> AuxiliaryRuleInformation {

        guard let lead = namedItems.first, namedItems.count > 1 else {
            preconditionFailure("namedItems.count > 1")
        }

        precondition(lead.hasNonStandardRepetition, "namedItems[0].hasNonStandardRepetition")

        let name = "_\(production.name)_nsr"

        var bindings: [BindingEngine.Binding] = []
        for (i, binding) in bindingEngine.computeBindings(namedItems).enumerated() {
            // Always include the first binding, as it's always generated and
            // returned by the generated repetition methods
            guard i > 0 else {
                bindings.append(binding)
                continue
            }

            // Bindings that have no label cannot be bound to an identifier and
            // thus returned from the generated repetition method; trim it from
            // the get-go here
            if binding.label != nil {
                bindings.append(binding)
            }
        }

        let memoization = memoizationMode(for: production)
        var information = AuxiliaryRuleInformation(
            name: name,
            bindings: bindings,
            memoizationMode: memoization == .memoizedLeftRecursive ? .none : memoization
        )
        let tail = Array(namedItems.dropFirst())

        let deduplicated = enqueueProduction(
            .nonStandardRepetition(
                information,
                RepetitionInfo(
                    repetition: lead,
                    trail: tail
                )
            )
        ).name

        information.name = deduplicated
        return information
    }

    /// Enqueues a given auxiliary rule, returning its deduplicated name for
    /// further referencing.
    private func enqueueAuxiliaryRule(
        _ rule: InternalGrammar.Rule,
        _ info: AuxiliaryRuleInformation
    ) -> String {

        var rule = rule
        rule.name = enqueueProduction(.auxiliary(rule, info)).name

        bindingEngine.registerRule(rule)

        return rule.name
    }

    /// Enqueues a given production into the queue of remaining productions.
    ///
    /// Returns a copy of the production with a deduplicated name for further
    /// referencing.
    ///
    /// - Parameters:
    ///   - production: The production to enqueue.
    ///   - reuseExisting: If `true`, and `production` is not a grammar rule,
    /// attempts to fetch an existing production of the same parameters, but
    /// potentially different names, and returns it instead of queueing the
    /// provided production.
    /// - Returns: A copy of the input production, with deduplicated names, or,
    /// if `reuseExisting` is `true` and an existing production matches the
    /// parameters of the provided production, that production is returned.
    private func enqueueProduction(
        _ production: RemainingProduction,
        reuseExisting: Bool = true
    ) -> RemainingProduction {

        if reuseExisting && !production.isGrammarRule {
            for previous in generated {
                if previous.isEquivalent(to: production) {
                    return previous
                }
            }
            for upcoming in remaining {
                if upcoming.isEquivalent(to: production) {
                    return upcoming
                }
            }
        }

        var production = production
        let decl = declContext.defineMethod(suggestedName: production.name)
        production.name = decl.name

        remaining.append(production)

        bindingEngine.registerCodeGenProduction(production)

        return production
    }
}

// MARK: - Rule return type management

extension SwiftCodeGen {
    func returnType(for rule: InternalGrammar.Rule) -> CommonAbstract.SwiftType {
        bindingEngine.returnTypeForRule(rule)
    }
}

// MARK: - Identifier/token management

extension SwiftCodeGen {
    /// Escapes the given identifier to something that can be declared as a local
    /// or member name in Swift.
    func escapeIdentifier(_ ident: String, isLabel: Bool = false) -> String {
        // Wildcard; return unchanged
        if ident == "_" {
            return ident
        }

        // Identifier already escaped; return unchanged
        if ident.hasPrefix("`") && ident.hasSuffix("`") {
            return ident
        }

        var requiresEscaping = false

        if isLabel {
            requiresEscaping = Self.invalidBareLabels.contains(ident)
        } else {
            requiresEscaping = Self.invalidBareIdentifiers.contains(ident)
        }

        if requiresEscaping {
            return "`\(ident)`"
        }

        return ident
    }

    /// Returns the appropriate handling of an identifier that may be a token
    /// identifier
    ///
    /// If the identifier matches a known token definition with explicit
    /// 'tokenCodeReference', returns `self.expect(<tokenCodeReference>)`, otherwise returns
    /// `self.<ident>()`, as a fallback.
    func expandTokenName(_ ident: String) -> String {
        if
            let token = bindingEngine.tokenDefinition(named: ident),
            let tokenCodeReference = tokenCodeReference(for: token)
        {
            return "self.expect(\(expectArguments(forResolvedToken: tokenCodeReference)))"
        }

        return "self.\(escapeIdentifier(ident))()"
    }

    /// Returns the arguments to invoke a `PEGParser.expect()` call, as a
    /// non-parenthesized labeled expression list separated by commas, in order
    /// to probe the parser about a specific token identifier.
    ///
    /// If no associated token identifier has been defined in a .tokens file,
    /// the result is a default `kind: <identifier>` or `<identifier>`,
    /// depending on the value of '@tokenCall' meta-property, if present.
    func expectArguments(forIdentifier identifier: String) -> String {
        // Check for explicit token aliases
        if
            let token = bindingEngine.tokenDefinition(named: identifier),
            let tokenCodeReference = tokenCodeReference(for: token)
        {
            return expectArguments(forResolvedToken: tokenCodeReference)
        }

        return expectArguments(forResolvedToken: identifier)
    }

    /// Returns the arguments to invoke a `PEGParser.expect()` call, as a
    /// non-parenthesized labeled expression list separated by commas, in order
    /// to probe the parser about a specific token literal.
    ///
    /// If no associated token literal has been defined in a .tokens file, the
    /// result is a default `kind: "<literal>"` or `"<literal>"`, depending on
    /// the value of '@tokenCall' meta-property, if present.
    func expectArguments(forLiteral literal: String, raw: String) -> String {
        // Check for explicit token aliases
        if
            let token = bindingEngine.tokenDefinition(ofRawLiteral: raw),
            let tokenCodeReference = tokenCodeReference(for: token)
        {
            return expectArguments(forResolvedToken: tokenCodeReference)
        }

        return expectArguments(forResolvedToken: literal)
    }

    /// Computes the static token name for a given token definition.
    ///
    /// If a custom static token was provided (`['.tokenCodeReference']`), that value
    /// is returned; otherwise, an attempt is made to compute the potential case
    /// name for a generated token type.
    ///
    /// If the token is missing both the static token and token syntax, it is
    /// assumed to be implemented off-lexer and the return is `nil`.
    func tokenCodeReference(for token: InternalGrammar.TokenDefinition) -> String? {
        if let tokenCodeReference = token.tokenCodeReference {
            return tokenCodeReference
        }
        if token.tokenSyntax == nil {
            return nil
        }

        return ".\(caseName(for: token))"
    }

    /// Does final expansion of token `self.expect` call arguments based on the
    /// current configuration of `tokenCallKind`.
    func expectArguments(forResolvedToken resolvedToken: String) -> String {
        if tokenCallKind == .expectKind {
            return "kind: \(resolvedToken)"
        } else {
            return "\(resolvedToken)"
        }
    }
}

// MARK: - Minimal/maximal repetition detection

extension SwiftCodeGen {
    /// Returns the first index at which the last valid minimal/maximal (`<`/`>`)
    /// repetition occurs within the given collection of named items.
    ///
    /// Return is `nil`, if no valid non-standard repetition is found.
    ///
    /// - note: a non-standard repetition can only occur if the repetition it is
    /// attached to is not the last item of an alternative. In this method's case,
    /// the collection of named items is considered to be an entire alternative's
    /// list of items.
    static func nonStandardRepetitionIndex<C: BidirectionalCollection<InternalGrammar.NamedItem>>(
        in namedItems: C
    ) -> C.Index? {
        namedItems.dropLast().firstIndex(where: \.hasNonStandardRepetition)
    }
}

// MARK: - Cut detection

extension SwiftCodeGen {
    /// Returns `true` if the rule makes use of cut (`~`) in one of its primary
    /// alts.
    func requiresCutFlag(_ node: InternalGrammar.Rule) -> Bool {
        requiresCutFlag(node.alts)
    }

    /// Returns `true` if one of the given alts makes use of cut (`~`) in one of
    /// its primary items.
    func requiresCutFlag(_ node: [InternalGrammar.Alt]) -> Bool {
        node.contains(where: requiresCutFlag)
    }

    /// Returns `true` if the given alt makes use of cut (`~`) in one of its
    /// primary items.
    ///
    /// If a cut happens after a non-standard repetition (repetitions with `<`/`>`
    /// suffix), the cut is not considered part of the alternative.
    func requiresCutFlag(_ node: InternalGrammar.Alt) -> Bool {
        requiresCutFlag(node.namedItems)
    }

    /// Returns `true` if the given named items makes use of cut (`~`).
    ///
    /// If a cut happens after a non-standard repetition (repetitions with `<`/`>`
    /// suffix), the cut is not considered part of the items.
    func requiresCutFlag(_ nodes: some BidirectionalCollection<InternalGrammar.NamedItem>) -> Bool {
        if let nonStandardIndex = Self.nonStandardRepetitionIndex(in: nodes) {
            return nodes[..<nonStandardIndex].contains(where: requiresCutFlag)
        }

        return nodes.contains(where: requiresCutFlag)
    }

    /// Returns `true` if the given named item makes use of cut (`~`).
    func requiresCutFlag(_ node: InternalGrammar.NamedItem) -> Bool {
        switch node {
        case .lookahead(.cut):
            return true
        default:
            return false
        }
    }
}

// MARK: - Mark requirement detection

extension SwiftCodeGen {
    /// Returns `true` if the rule requires a marker to backtrack in between alts.
    ///
    /// Always returns `true` if `latestSettings.omitRedundantMarkRestores` is
    /// `false`.
    func requiresMarkers(_ node: InternalGrammar.Rule) -> Bool {
        guard latestSettings.omitRedundantMarkRestores else {
            return true
        }

        return node.alts.contains(where: requiresMarkers)
    }

    /// Returns `true` if the one of the given alts requires a marker to backtrack
    /// in case of failure.
    ///
    /// Always returns `true` if `latestSettings.omitRedundantMarkRestores` is
    /// `false`.
    func requiresMarkers(_ nodes: [InternalGrammar.Alt]) -> Bool {
        guard latestSettings.omitRedundantMarkRestores else {
            return true
        }

        return nodes.contains(where: requiresMarkers)
    }

    /// Returns `true` if the given alt requires a marker to backtrack in case
    /// of failure.
    ///
    /// Markers are required if an alternative has more than one fallible bind,
    /// requiring a backtrack that returns to before the first bind.
    ///
    /// Always returns `true` if `latestSettings.omitRedundantMarkRestores` is
    /// `false`.
    func requiresMarkers(_ node: InternalGrammar.Alt) -> Bool {
        guard latestSettings.omitRedundantMarkRestores else {
            return true
        }

        let bindCount = node.namedItems.filter {
            switch $0 {
            case .item, .lookahead(.forced):
                return true
            case .lookahead(.negative), .lookahead(.positive), .lookahead(.cut):
                return false
            }
        }.count

        return bindCount > 1
    }
}

// MARK: - Convenience extensions

extension InternalGrammar.Grammar {
    func parserHeader() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.parserHeader)
    }

    func tokenTypeHeader() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.tokenTypeHeader)
    }

    func tokenTypeName() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.tokenTypeName)
    }

    func parserName() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.parserName)
    }

    func tokenCall() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.tokenCall)
    }

    func implicitReturns() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.implicitReturns)
    }

    func implicitBindings() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.implicitBindings)
    }

    func bindTokenLiterals() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.bindTokenLiterals)
    }

    private func _stringOrIdentMeta(named name: String) -> String? {
        guard
            let meta = metas.first(where: { $0.name == name })
        else {
            return nil
        }

        switch meta.value {
        case .string(let value)?:
            return value

        case .identifier(let value)?:
            return value

        case nil:
            return nil
        }
    }
}

extension InternalGrammar.Rule {
    /// Returns `true` if this rule makes use of minimal/maximal (`<`/`>`) repetition
    /// in one of its primary alts.
    ///
    /// - note: a non-standard repetition can only occur if the repetition it is
    /// attached to is not the last item of an alternative.
    var hasNonStandardRepetition: Bool {
        alts.contains(where: \.hasNonStandardRepetition)
    }
}

extension InternalGrammar.Alt {
    /// Returns `true` if this alt makes use of minimal/maximal (`<`/`>`)
    /// repetition in one of its primary items.
    ///
    /// - note: a non-standard repetition can only occur if the repetition it is
    /// attached to is not the last item of an alternative.
    var hasNonStandardRepetition: Bool {
        namedItems.dropLast().contains(where: \.hasNonStandardRepetition)
    }
}

extension InternalGrammar.NamedItem {
    /// Returns `true` if this named item makes use of minimal/maximal (`<`/`>`).
    var hasNonStandardRepetition: Bool {
        switch self {
        case .item(_, let item, _):
            return item.hasNonStandardRepetition
        default:
            return false
        }
    }
}

extension InternalGrammar.Item {
    /// Returns `true` if the this item makes use of minimal/maximal (`<`/`>`).
    var hasNonStandardRepetition: Bool {
        switch self {
        case .zeroOrMore(_, let repetitionMode),
            .oneOrMore(_, let repetitionMode),
            .gather(_, _, let repetitionMode):
            return repetitionMode != .standard

        default:
            return false
        }
    }
}

internal extension CommonAbstract.SwiftType {
    /// Returns a string representation of this Swift type that can be used as a
    /// valid Swift type in code.
    func scg_asValidSwiftType() -> String {
        self.description
    }

    /// Returns a string representation of this Swift type that can be used as a
    /// valid return type for production rules.
    func scg_asReturnType() -> String {
        scg_removingTupleLabels().description
    }

    /// Returns a copy of this type, deeply removing all tuple labels.
    func scg_removingTupleLabels() -> Self {
        switch self {
        case .tuple(let elements):
            return .tuple(
                elements.map({ .init(label: nil, $0.swiftType.scg_removingTupleLabels()) })
            )

        case .optional(let inner):
            return .optional(inner.scg_removingTupleLabels())

        case .array(let inner):
            return .array(inner.scg_removingTupleLabels())

        case .dictionary(let key, let value):
            return .dictionary(
                key: key.scg_removingTupleLabels(),
                value: value.scg_removingTupleLabels()
            )

        case .nominal(let identifier):
            return .nominal(identifier)

        case .nested(let base, let identifier):
            return .nested(base.scg_removingTupleLabels(), identifier)
        }
    }
}
