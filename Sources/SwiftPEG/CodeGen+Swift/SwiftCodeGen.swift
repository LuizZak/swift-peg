/// Produces Swift code for parsing a grammar.
public class SwiftCodeGen {
    // TODO: Refactor to use MetaPropertyManager

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the raw contents to print atop the generated parser code.
    public static let parserHeader: String = "parserHeader"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the raw contents to print atop the generated token type code.
    public static let tokenTypeHeader: String = "tokenTypeHeader"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the name of the parser class to extend with the parsing methods.
    /// Assumes that the type exists already.
    public static let parserName: String = "parserName"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the strategy of token call to emit; by default token checks
    /// are emitted with `PEGParser.expect(_:)`, and specifying a value of
    /// 'expectKind' for this meta-property indicates that the code generator
    /// should emit `PEGParser.expect(kind:)` calls for the string literals,
    /// instead.
    public static let tokenCall: String = "tokenCall"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates whether to generate `return` statements for generated actions
    /// implicitly or not.
    ///
    /// Defaults to `true`, can be specified `true` or `false`, as either strings
    /// or identifiers.
    public static let implicitReturns: String = "implicitReturns"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates whether to generate named pattern binds for generated if-let
    /// patterns for alts.
    ///
    /// Defaults to `true`, can be specified `true` or `false`, as either strings
    /// or identifiers.
    public static let implicitBindings: String = "implicitBindings"

    /// Set of identifiers that cannot be used as bare identifiers in Swift, and
    /// must be escaped with backticks (`) to allow usage in declarations.
    public static var invalidBareIdentifiers: Set<String> {
        SwiftKeywords.keywords
    }

    /// Used to keep track of reentrance in `typeForRule()` calls.
    var _typeForRuleOngoing: Set<String> = []

    let parserName: String
    let grammar: InternalGrammar.Grammar
    let tokenDefinitions: [InternalGrammar.TokenDefinition]
    let buffer: CodeStringBuffer
    var declContext: DeclarationsContext

    var latestSettings: ParserGenSettings = .default
    var tokenCallKind: TokenCallKind = .expect
    var remaining: [RemainingProduction] = []

    /// Aliasing for rules that may be used to circumvent name clashes.
    /// Maps grammar rule name -> generate code name.
    var ruleAliases: [String: String] = [:]

    var implicitReturns: Bool = true
    var implicitBindings: Bool = true

    var bindingEngine: BindingEngine

    /// Initializes a new `SwiftCodeGen`, preparing to generate the grammar and
    /// token definitions from a given grammar processor result.
    public convenience init(from processed: ProcessedGrammar) {
        self.init(
            grammar: processed.grammar,
            tokenDefinitions: processed.tokens
        )
    }

    /// Initializes a new `SwiftCodeGen`, preparing to generate a given grammar.
    ///
    /// - Parameters:
    ///   - grammar: The grammar to generate.
    ///   - tokenDefinitions: A list of token definitions to use when examining string literals.
    public init(
        grammar: InternalGrammar.Grammar,
        tokenDefinitions: [InternalGrammar.TokenDefinition] = []
    ) {
        self.grammar = grammar
        self.tokenDefinitions = tokenDefinitions
        self.tokenCallKind = grammar.tokenCall().flatMap(TokenCallKind.init) ?? .expect

        parserName = grammar.parserName() ?? "Parser"
        buffer = CodeStringBuffer()
        declContext = DeclarationsContext()
        bindingEngine = BindingEngine()
    }

    /// Generates Swift parser code.
    public func generateParser(
        settings: ParserGenSettings = .default
    ) throws -> String {

        self.latestSettings = settings

        buffer.resetState()

        // @parserHeader
        if let header = grammar.parserHeader() {
            buffer.emitLine(header)
        }
        // @implicitReturns
        if let value = grammar.implicitReturns() {
            switch value {
            case "true":
                implicitReturns = true
            case "false":
                implicitReturns = false
            default:
                // TODO: Issue diagnostic
                break
            }
        }
        // @implicitBindings
        if let value = grammar.implicitBindings() {
            switch value {
            case "true":
                implicitBindings = true
            case "false":
                implicitBindings = false
            default:
                // TODO: Issue diagnostic
                break
            }
        }

        bindingEngine.clearState()
        bindingEngine.implicitBindings = self.implicitBindings
        for rule in grammar.rules {
            bindingEngine.registerRule(rule)
        }
        for token in tokenDefinitions {
            bindingEngine.registerToken(token)
        }

        declContext = DeclarationsContext()
        declContext.push() // No need to pop as the context is meant to be replaced in new generate calls

        self.remaining = grammar.rules.map(RemainingProduction.rule)

        buffer.emit("extension \(parserName) ")
        try buffer.emitMembersBlock {
            try generateRemainingProductions()
        }

        return buffer.finishBuffer()
    }

    // MARK: - Main productions

    func generateRemainingProductions() throws {
        while !remaining.isEmpty {
            let next = remaining.removeFirst()

            try generateRemainingProduction(next)

            // Separate production members
            buffer.ensureDoubleNewline()
        }
    }

    func generateRemainingProduction(_ production: RemainingProduction) throws {
        switch production {
        case .rule(let rule):
            try generateRule(rule, for: production)

        case .auxiliary(let rule, _):
            try generateRule(rule, for: production)

        case .nonStandardRepetition(let auxInfo, let repetition):
            try generateNonStandardRepetition(
                auxInfo,
                repetition,
                for: production
            )
        }
    }

    // MARK: Production: Rule

    func generateRule(
        _ rule: InternalGrammar.Rule,
        for production: RemainingProduction
    ) throws {
        if latestSettings.omitUnreachable && !rule.isReachable {
            return
        }

        generateRuleDocComment(rule)

        // @memoized/@memoizedLeftRecursive
        // @inlinable
        let name = bindingEngine.alias(for: rule)
        let memoizationMode = memoizationMode(for: rule)
        generateRuleMethodAttributes(
            memoization: memoizationMode,
            memoizedName: name
        )

        // func <rule>() -> <return type>
        let fName = memoizationMode == .none ? name : "__\(name)"
        let returnType = bindingEngine.returnTypeForRule(rule)

        buffer.emit("public func \(fName)() throws -> \(returnType) ")
        try buffer.emitBlock {
            // let mark = self.mark()
            // var cut = CutFlag()
            //
            // ...if-let alt patterns...
            //
            // return nil
            let failReturnExpression = _failReturnExpression(for: rule.type)

            try generateRuleBody(
                hasCut: hasCut(rule),
                failReturnExpression: failReturnExpression
            ) {
                for alt in rule.alts {
                    try generateAlt(
                        alt,
                        in: production,
                        failReturnExpression: failReturnExpression
                    )
                }
            }
        }
    }

    // MARK: Production: Non-standard repetition with trailing cases

    func generateNonStandardRepetition(
        _ info: AuxiliaryRuleInformation,
        _ repetitionInfo: RepetitionInfo,
        for production: RemainingProduction
    ) throws {

        // Preparation

        let ruleName = info.name
        let ruleType = info.bindings.scg_asTupleType().scg_optionalWrapped()
        let returnType = ruleType.scg_asValidSwiftType()

        let trailName = ruleName + "_tail"
        let remainingElements = bindingEngine.computeBindings(repetitionInfo.trail)
        let trailInformation = AuxiliaryRuleInformation(
            name: trailName,
            bindings: remainingElements,
            memoizationMode: info.memoizationMode
        )

        let trailAction = defaultReturnAction(for: remainingElements)
        let trailProductionName = enqueueAuxiliaryRule(
            InternalGrammar.Rule(
                name: trailName,
                type: remainingElements.scg_asTupleType(),
                alts: [
                    .init(namedItems: repetitionInfo.trail, action: trailAction)
                ]
            ),
            trailInformation
        )

        let syntheticItem: InternalGrammar.Item =
            .atom(.ruleName(trailProductionName))

        // Synthesize method
        generateRuleDocComment(ruleName, info.bindings.scg_asTupleType(), [
            .init(namedItems: [repetitionInfo.repetition, .item(syntheticItem)])
        ])

        // @memoized/@memoizedLeftRecursive
        // @inlinable
        generateRuleMethodAttributes(
            memoization: info.memoizationMode,
            memoizedName: ruleName
        )

        let fName = info.memoizationMode == .none ? ruleName : "__\(ruleName)"

        buffer.emit("public func \(fName)() throws -> \(returnType) ")
        try buffer.emitBlock {
            func repetitionAtom() -> InternalGrammar.Atom {
                guard let atom = repetitionInfo.repetition.asItem?.atom else {
                    fatalError("Repetition request did not reference a repetition item?")
                }

                return atom
            }
            func repetitionType() -> CommonAbstract.SwiftType {
                return bindingEngine.typeForAtom(repetitionAtom())
            }
            func trailType() -> CommonAbstract.SwiftType {
                trailInformation.bindings.scg_asTupleType()
            }
            func fullType() -> CommonAbstract.SwiftType {
                info.bindings.scg_asTupleType()
            }

            let failReturnExpression = _failReturnExpression(for: ruleType)
            let info = RepetitionBodyGenInfo(
                production: production,
                ruleInfo: info,
                repetitionAtom: repetitionAtom(),
                repetitionInfo: repetitionInfo,
                trailName: trailProductionName,
                trailInfo: trailInformation,
                failReturnExpression: failReturnExpression,
                repetitionAtomType: repetitionType(),
                trailType: trailType(),
                fullType: fullType()
            )

            declContext.push()
            defer { declContext.pop() }

            switch repetitionInfo.repetition {
            case .item(_, .zeroOrMore(_, repetitionMode: .minimal), _):
                try generateZeroOrMoreMinimalBody(info)

            case .item(_, .zeroOrMore(_, repetitionMode: .maximal), _):
                try generateZeroOrMoreMaximalBody(info)

            case .item(_, .oneOrMore(_, repetitionMode: .minimal), _):
                try generateOneOrMoreMinimalBody(info)

            case .item(_, .oneOrMore(_, repetitionMode: .maximal), _):
                try generateOneOrMoreMaximalBody(info)

            default:
                fatalError("Repetition request's Item is not a non-standard repetition item?")
            }
        }
    }

    // MARK: - Rule-related productions

    /// ```
    /// /// ```
    /// /// ruleName[ruleType]:
    /// ///     | alt1
    /// ///     | alt2
    /// ///     ...
    /// ///     ;
    /// /// ```
    /// ```
    fileprivate func generateRuleDocComment(_ rule: InternalGrammar.Rule) {
        let ruleName = rule.name
        let ruleType = rule.type
        let alts = rule.alts

        generateRuleDocComment(ruleName, ruleType, alts)
    }

    /// ```
    /// /// ```
    /// /// ruleName[ruleType]:
    /// ///     | alt1
    /// ///     | alt2
    /// ///     ...
    /// ///     ;
    /// /// ```
    /// ```
    fileprivate func generateRuleDocComment(
        _ ruleName: String,
        _ ruleType: CommonAbstract.SwiftType?,
        _ alts: [InternalGrammar.Alt]
    ) {
        // Derive a doc comment for the generated rule
        let linePrefix = "///"

        buffer.emitLine("\(linePrefix) ```")
        buffer.emit("\(linePrefix) \(ruleName)")
        if let ruleType {
            buffer.emit("[\(ruleType.scg_asValidSwiftType())]")
        }
        buffer.emitLine(":")
        for alt in alts {
            buffer.emitLine("\(linePrefix)     | \(alt)")
        }
        buffer.emitLine("\(linePrefix)     ;")
        buffer.emitLine("\(linePrefix) ```")
    }

    /// ```
    /// <none> / @memoized("<name>") / @memoizedLeftRecursive("<name>")
    /// @inlinable
    /// ```
    func generateRuleMethodAttributes(
        memoization: MemoizationMode,
        memoizedName: String
    ) {
        switch memoization {
        case .none:
            break
        case .memoized:
            buffer.emitLine("@memoized(\(memoizedName.debugDescription))")
        case .memoizedLeftRecursive:
            buffer.emitLine("@memoizedLeftRecursive(\(memoizedName.debugDescription))")
        }

        buffer.emitLine("@inlinable")
    }

    /// Generates a preamble and default tail-end of a rule-like method body that
    /// has an optional return type and backtracks the parser, and optionally
    /// has access to a cut flag.
    ///
    /// Pushes a new declaration context for the duration of the method.
    ///
    /// ```
    /// let mark = self.mark()
    /// var cut = CutFlag()   (only if hasCut == true)
    /// <generator()>
    /// return <failReturnExpression>
    /// ```
    func generateRuleBody(
        hasCut: Bool,
        failReturnExpression: String,
        _ generator: () throws -> Void
    ) throws {
        declContext.push()
        defer { declContext.pop() }
        declContext.defineLocal(suggestedName: "mark")
        declContext.defineLocal(suggestedName: "cut")

        buffer.emitLine("let mark = self.mark()")
        if hasCut {
            buffer.emitLine("var cut = CutFlag()")
        }

        try generator()

        buffer.emitLine("return \(failReturnExpression)")
    }

    func generateAlt(
        _ alt: InternalGrammar.Alt,
        in production: RemainingProduction,
        failReturnExpression: String
    ) throws {
        if alt.namedItems.isEmpty { return }

        declContext.push()
        defer { declContext.pop() }

        buffer.emitNewline()

        // if block
        buffer.emitLine("if")
        try buffer.indented {
            // if bindings/conditions
            try generateNamedItems(alt.namedItems, in: production)
        }
        buffer.ensureNewline()

        // Successful alt match
        buffer.emitBlock {
            generateOnAltMatchBlock(alt, in: production)
        }

        // Alt failure results in a restore to a previous mark
        buffer.emitNewline()
        buffer.emitLine("self.restore(mark)")

        // Generate fail action, if present
        if let failAction = alt.failAction {
            buffer.emitLine(failAction.string.trimmingWhitespace())
        }

        if hasCut(alt) {
            buffer.emitNewline()
            buffer.emit("if cut.isOn ")
            buffer.emitBlock {
                buffer.emitLine("return \(failReturnExpression)")
            }
        }
    }

    /// Generates the block of code that is run when a given alt is matched
    /// successfully.
    ///
    /// If `self.implicitReturns` is `true`, always appends `return` to the start
    /// of the action's resolved string.
    func generateOnAltMatchBlock(
        _ alt: InternalGrammar.Alt,
        in production: RemainingProduction
    ) {
        if implicitReturns {
            buffer.emit("return ")
        }

        if let action = alt.action {
            buffer.emitLine(action.string.trimmingWhitespace())
            return
        }

        // If no action is specified, attempt to return instead the named
        // item within the alt, if it's the only named item in the alt.
        if alt.namedItems.count == 1 {
            let bindings = bindingEngine.computeBindings(alt)
            if bindings.count == 1, let label = bindings[0].label {
                buffer.emitLine(label)
                return
            }
        }

        // Fallback: Return an initialization of the associated node type, assuming
        // it is not `nil` and is not a known existential type, otherwise return
        // `Node()`.
        if let type = production.productionType?.description, type != "Any" {
            buffer.emitLine("\(type)()")
        } else {
            buffer.emitLine("Node()")
        }
    }

    /// Generates items as a sequence of if-let segments from a given sequence of
    /// named items, separated by commas.
    func generateNamedItems(
        _ namedItems: [InternalGrammar.NamedItem],
        in production: RemainingProduction
    ) throws {

        let nonStandardIndex = Self.nonStandardRepetitionIndex(in: namedItems)

        let commaEmitter = buffer.startConditionalEmitter()
        for (i, namedItem) in namedItems.enumerated() {
            commaEmitter.conditional { buffer in
                buffer.emitLine(",")
            }

            // Switch over to non-standard repetition, if present
            if i == nonStandardIndex {
                let auxInfo = enqueueNonStandardRepetition(
                    for: production,
                    suffix: "_nsr",
                    namedItems[i...]
                )

                // Emit a dummy item that binds the repetition's results
                let item: InternalGrammar.Item = .atom(.ruleName(auxInfo.name))
                let bindings = auxInfo.bindings

                try generateBindingsToItem(item, bindings, in: production)

                // Stop emitting items after the repetition binding is emitted
                break
            } else {
                try generateNamedItem(namedItem, in: production)
            }
        }
    }

    /// `let <bind>[: <BindingType>] = <production>`
    /// Or:
    /// `case let (<bind1>?, <bind2?>, ...) = <production>`
    /// Depending on how many bindings exist in the named item.
    ///
    /// Returns an array containing the deduplicated names of all named bindings
    /// created. Return is empty if no named bindings where created.
    @discardableResult
    func generateNamedItem(
        _ namedItem: InternalGrammar.NamedItem,
        in production: RemainingProduction
    ) throws -> [String] {

        switch namedItem {
        case .item(_, let item, _):
            var bindings = bindingEngine.bindings(for: namedItem)
            if bindings.isEmpty {
                bindings = [
                    ("_", bindingEngine.typeForNamedItem(namedItem))
                ]
            }

            return try generateBindingsToItem(item, bindings, in: production)

        case .lookahead(let lookahead):
            try generateLookahead(lookahead, in: production)
            return []
        }
    }

    /// Generates a binding pattern that binds the production described by `item`
    /// with a given set of bindings.
    ///
    /// Returns an array containing the deduplicated names of all named bindings
    /// created. Return is empty if no named bindings where created.
    @discardableResult
    func generateBindingsToItem(
        _ item: InternalGrammar.Item,
        _ bindings: [BindingEngine.Binding],
        in production: RemainingProduction
    ) throws -> [String] {

        // Emit bindings
        let bindingNames = try generatePatternBinds(bindings, in: production)

        buffer.emit(" = ")

        try generateItem(item, in: production)

        return bindingNames
    }

    func generateItem(
        _ item: InternalGrammar.Item,
        in production: RemainingProduction
    ) throws {
        switch item {
        case .optional(let atom):
            buffer.emit("try self.optional(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: production)
            }
            buffer.emit(")")

        case .optionalItems(let alts):
            buffer.emit("try self.optional(")
            buffer.emitInlinedBlock {
                let aux = enqueueAuxiliaryRule(
                    for: production,
                    suffix: "_opt",
                    alts
                )
                buffer.emit("try self.\(aux)()")
            }
            buffer.emit(")")

        case .gather(let sep, let item):
            buffer.emit("try self.gather(separator: ")
                try buffer.emitInlinedBlock {
                    try generateAtom(sep, in: production)
                }
            buffer.emit(", item: ")
                try buffer.emitInlinedBlock {
                    try generateAtom(item, in: production)
                }
            buffer.emit(")")

        case .zeroOrMore(let atom, _):
            buffer.emit("try self.repeatZeroOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: production)
            }
            buffer.emit(")")

        case .oneOrMore(let atom, _):
            buffer.emit("try self.repeatOneOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: production)
            }
            buffer.emit(")")

        case .atom(let atom):
            try generateAtom(atom, in: production)
        }
    }

    func generateLookahead(
        _ lookahead: InternalGrammar.Lookahead,
        in production: RemainingProduction
    ) throws {
        switch lookahead {
        case .forced(let atom):
            buffer.emit("try self.expectForced(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: production)
            }
            buffer.emit(#", \#(atom.description.debugDescription))"#)

        case .positive(let atom):
            buffer.emit("try self.positiveLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: production)
            }
            buffer.emit(")")

        case .negative(let atom):
            buffer.emit("try self.negativeLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: production)
            }
            buffer.emit(")")

        case .cut:
            buffer.emit("cut.toggleOn()")
        }
    }

    func generateAtom(
        _ atom: InternalGrammar.Atom,
        in production: RemainingProduction
    ) throws {
        switch atom {
        case .group(let group):
            let aux = enqueueAuxiliaryRule(
                for: production,
                suffix: "_group_",
                group
            )

            buffer.emit("try self.\(aux)()")

        case .ruleName(let ident):
            buffer.emit("try self.\(escapeIdentifier(ident))()")

        case .token(let ident):
            buffer.emit("try \(expandTokenName(ident))")

        case .anyToken:
            buffer.emit("try self.nextToken()")

        // Token literal
        case .string(let string, let raw):
            var literal = string

            // Avoid emitting single-quoted string literals
            if string.hasPrefix("'") {
                literal = #""\#(raw)""#
            } else {
                literal = string
            }

            // Escape backslashes contents
            literal = literal.replacing("\\", with: #"\\"#)

            let callArgs = self.expectArguments(forLiteral: literal, raw: raw)
            buffer.emit("try self.expect(\(callArgs))")
        }
    }

    /// `let <bind>[: <BindingType>]`
    /// Or:
    /// `case let (<bind1>?, <bind2?>, ...)`
    /// Depending on how many bindings exist in `bindings`.
    ///
    /// Returns an array containing the deduplicated names of all named bindings
    /// created. Return is empty if no named bindings where created.
    @discardableResult
    func generatePatternBinds(
        _ bindings: [BindingEngine.Binding],
        in production: RemainingProduction
    ) throws -> [String] {

        // When emitting more than one optional unwrapping on tuple types,
        // emit a case-let- binding so we can unwrap each identifier within
        // the tuple individually with a '<ident>?' pattern.
        var isCasePatternBind = false
        if bindings.count > 1 {
            isCasePatternBind = true
        }

        if isCasePatternBind {
            buffer.emit("case ")
        }

        buffer.emit("let ")

        var deduplicatedBindings: [String] = []

        var bindingNames = ""
        var bindingTypes = ""
        if bindings.count > 1 {
            bindingNames += "("
            bindingTypes += "("
        }

        for (i, binding) in bindings.enumerated() {
            var bindingName = binding.label ?? "_"
            let bindingType = binding.type

            if bindingName != "_" {
                bindingName = declContext.defineLocal(suggestedName: bindingName, type: nil).name
                deduplicatedBindings.append(bindingName)
            }

            if i > 0 {
                bindingNames += ", "
                bindingTypes += ", "
            }

            bindingNames += escapeIdentifier(bindingName)
            // Emit unwrap pattern
            if isCasePatternBind {
                bindingNames += "?"
            }

            bindingTypes += bindingType.description
        }

        if bindings.count > 1 {
            bindingNames += ")"
            bindingTypes += ")"
        }

        buffer.emit(bindingNames)

        if latestSettings.emitTypesInBindings && !isCasePatternBind {
            buffer.emit(": ")
            buffer.emit(bindingTypes)
        }

        return deduplicatedBindings
    }

    private func _failReturnExpression(for type: CommonAbstract.SwiftType?) -> String {
        switch type {
        case .tuple(let elements) where elements.count != 1:
            "(\(elements.map({ _ in "nil" }).joined(separator: ", ")))"
        default:
            "nil"
        }
    }

    private func memoizationMode(for rule: InternalGrammar.Rule) -> MemoizationMode {
        return
            if rule.isRecursiveLeader {
                .memoizedLeftRecursive
            } else if !rule.isRecursive {
                .memoized
            } else {
                .none
            }
    }

    private func memoizationMode(for production: RemainingProduction) -> MemoizationMode {
        switch production {
        case .rule(let rule), .auxiliary(let rule, _):
            return memoizationMode(for: rule)
        case .nonStandardRepetition(let info, _):
            return info.memoizationMode
        }
    }

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

        public var description: String {
            switch self {
            case .tokenDefinitionMissingSyntax(let def):
                return "Cannot generate token type: All tokens must have a syntax defined; found token '\(def.name)' that has no syntax."
            }
        }
    }

    /// Settings that can be specified during parser code generation.
    public struct ParserGenSettings {
        /// Gets the static default settings configuration.
        public static let `default`: Self = Self(
            omitUnreachable: false,
            emitTypesInBindings: false
        )

        /// Whether to omit unreachable rules, as detected by a `GrammarProcessor`
        /// and flagged within each `InternalGrammar.Rule` node.
        public var omitUnreachable: Bool

        /// Whether to emit type annotations on all if-let bindings emitted.
        /// Type annotating only occurs when the type of the production being
        /// bound is resolvable by the code generator.
        public var emitTypesInBindings: Bool

        public init(
            omitUnreachable: Bool,
            emitTypesInBindings: Bool
        ) {
            self.omitUnreachable = omitUnreachable
            self.emitTypesInBindings = emitTypesInBindings
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
            accessLevel: nil
        )

        /// Whether to emit tokenization methods as @inlinable declarations.
        public var emitInlinable: Bool

        /// The access level to emit the declarations as. If `nil`, declarations
        /// have no access level specified.
        ///
        /// Providing an access level different than `"internal"` or `nil` also
        /// generates an initializer for struct declarations.
        public var accessLevel: String?

        public init(
            emitInlinable: Bool,
            accessLevel: String?
        ) {
            self.emitInlinable = emitInlinable
            self.accessLevel = accessLevel
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
    struct RepetitionInfo {
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
        var name: String {
            switch self {
            case .rule(let rule), .auxiliary(let rule, _):
                return rule.name
            case .nonStandardRepetition(let ruleInfo, _):
                return ruleInfo.name
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
                return ruleInfo.bindings.scg_asTupleType()
            }
        }
    }
}

// MARK: Auxiliary method management

extension SwiftCodeGen {
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
        _ alts: [InternalGrammar.Alt]
    ) -> String {

        var alts = alts

        let elements = bindingEngine.computeBindings(alts)
        let type: CommonAbstract.SwiftType =
            if elements.isEmpty {
                "Any"
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
        if type != "Any" {
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
            alts
        )
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
        _ alts: [InternalGrammar.Alt]
    ) -> String {

        let name = "_\(production.name)_\(suffix)"

        let info = AuxiliaryRuleInformation(
            name: name,
            bindings: bindingEngine.computeBindings(alts),
            memoizationMode: memoizationMode(for: production)
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
        suffix: String,
        _ namedItems: some Collection<InternalGrammar.NamedItem>
    ) -> AuxiliaryRuleInformation {

        guard let lead = namedItems.first, namedItems.count > 1 else {
            preconditionFailure("namedItems.count > 1")
        }

        precondition(lead.hasNonStandardRepetition, "namedItems[0].hasNonStandardRepetition")

        let name = "_\(production.name)_\(suffix)"

        let bindings = bindingEngine.computeBindings(namedItems)

        let information = AuxiliaryRuleInformation(
            name: name,
            bindings: bindings,
            memoizationMode: memoizationMode(for: production)
        )
        let tail = Array(namedItems.dropFirst())

        enqueueProduction(
            .nonStandardRepetition(
                information,
                RepetitionInfo(
                    repetition: lead,
                    trail: tail
                )
            )
        )

        return information
    }

    /// Enqueues a given auxiliary rule, returning its deduplicated name for
    /// further referencing.
    private func enqueueAuxiliaryRule(
        _ rule: InternalGrammar.Rule,
        _ info: AuxiliaryRuleInformation
    ) -> String {

        let decl = declContext.defineMethod(suggestedName: rule.name)
        var rule = rule
        rule.name = decl.name

        enqueueProduction(.auxiliary(rule, info))

        bindingEngine.registerRule(rule)

        return decl.name
    }

    /// Enqueues a given production into the queue of remaining productions.
    private func enqueueProduction(_ production: RemainingProduction) {
        remaining.append(production)
    }
}

// MARK: Identifier/token management

extension SwiftCodeGen {
    /// Escapes the given identifier to something that can be declared as a local
    /// or member name in Swift.
    func escapeIdentifier(_ ident: String) -> String {
        // Wildcard; return unchanged
        if ident == "_" {
            return ident
        }

        // Identifier already escaped; return unchanged
        if ident.hasPrefix("`") && ident.hasSuffix("`") {
            return ident
        }

        if Self.invalidBareIdentifiers.contains(ident) {
            return "`\(ident)`"
        }

        return ident
    }

    /// Returns the appropriate handling of an identifier that may be a token
    /// identifier
    ///
    /// If the identifier matches a known token definition with explicit
    /// 'staticToken', returns `self.expect(<staticToken>)`, otherwise returns
    /// `self.<ident>()`, as a fallback.
    func expandTokenName(_ ident: String) -> String {
        if
            let token = bindingEngine.tokenDefinition(named: ident),
            let staticToken = staticToken(for: token)
        {
            return "self.expect(\(expectArguments(forResolvedToken: staticToken)))"
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
            let staticToken = staticToken(for: token)
        {
            return expectArguments(forResolvedToken: staticToken)
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
            let staticToken = staticToken(for: token)
        {
            return expectArguments(forResolvedToken: staticToken)
        }

        return expectArguments(forResolvedToken: literal)
    }

    /// Computes the static token name for a given token definition.
    ///
    /// If a custom static token was provided (`['.staticToken']`), that value
    /// is returned; otherwise, an attempt is made to compute the potential case
    /// name for a generated token type.
    ///
    /// If the token is missing both the static token and token syntax, it is
    /// assumed to be implemented off-lexer and the return is `nil`.
    func staticToken(for token: InternalGrammar.TokenDefinition) -> String? {
        if let staticToken = token.staticToken {
            return staticToken
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
    func hasCut(_ node: InternalGrammar.Rule) -> Bool {
        hasCut(node.alts)
    }

    /// Returns `true` if one of the given alts makes use of cut (`~`) in one of
    /// its primary items.
    func hasCut(_ node: [InternalGrammar.Alt]) -> Bool {
        node.contains(where: hasCut)
    }

    /// Returns `true` if the given alt makes use of cut (`~`) in one of its
    /// primary items.
    func hasCut(_ node: InternalGrammar.Alt) -> Bool {
        node.namedItems.contains(where: hasCut)
    }

    /// Returns `true` if the given named item makes use of cut (`~`).
    func hasCut(_ node: InternalGrammar.NamedItem) -> Bool {
        switch node {
        case .lookahead(.cut):
            return true
        default:
            return false
        }
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
            .oneOrMore(_, let repetitionMode):
            return repetitionMode != .standard

        default:
            return false
        }
    }
}

internal extension Sequence where Element == BindingEngine.Binding {
    /// Applies an optional type layer to bindings on this sequence.
    ///
    /// If the binding represents a tuple, with multiple bindings, each type
    /// binding within the tuple is optional-wrapped.
    func scg_optionalWrapped() -> [Element] {
        map {
            ($0.label, .optional($0.type))
        }
    }

    /// Applies optional unwrapping to each element within this binding.
    ///
    /// If the binding represents a tuple, with multiple bindings, each type
    /// binding within the tuple is unwrapped.
    func scg_unwrapped() -> [Element] {
        map {
            ($0.label, $0.type.unwrapped)
        }
    }

    /// Converts the bindings within this sequence of bindings into a Swift tuple
    /// type, with labels as required.
    func scg_asTupleType() -> CommonAbstract.SwiftType {
        .tuple(map { element in
            if let label = element.label {
                .labeled(label: label, element.type)
            } else {
                .unlabeled(element.type)
            }
        })
    }
}

internal extension CommonAbstract.SwiftType {
    /// Returns `self` unwrapped of an optional layer, unless `self` is a tuple
    /// type, in which case returns a tuple of each element unwrapped by one
    /// optional layer.
    func scg_unwrapped() -> Self {
        switch self {
        case .tuple(let types):
            return .tuple(types.map(\.unwrapped))
        default:
            return self.unwrapped
        }
    }

    /// Returns `self` wrapped in an optional layer, unless `self` is a tuple type,
    /// in which case returns a tuple of optional elements of the tuple type.
    func scg_optionalWrapped() -> Self {
        switch self {
        case .tuple(let types):
            return .tuple(types.map(\.optionalWrapped))
        default:
            return .optional(self)
        }
    }

    /// Returns a string representation of this Swift type that can be used as a
    /// valid Swift type in code.
    func scg_asValidSwiftType() -> String {
        // Ensure we don't emit labeled tuples with one element
        switch self {
        case .tuple(let elements) where elements.count == 1:
            return elements[0].swiftType.scg_asValidSwiftType()
        default:
            return self.description
        }
    }
}
