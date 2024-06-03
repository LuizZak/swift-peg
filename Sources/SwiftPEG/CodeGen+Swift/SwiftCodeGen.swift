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

        declContext = DeclarationsContext()
        declContext.push() // No need to pop as the context is meant to be replaced in new generate calls

        self.remaining = grammar.rules.map(RemainingProduction.rule)

        buffer.emit("extension \(parserName) ")
        try buffer.emitMembersBlock {
            try generateRemainingProductions()
        }

        return buffer.finishBuffer()
    }

    func generateRemainingProductions() throws {
        while !remaining.isEmpty {
            let next = remaining.removeFirst()

            try generateRemainingProduction(next)
        }
    }

    func generateRemainingProduction(_ production: RemainingProduction) throws {
        switch production {
        case .rule(let rule):
            try generateRule(rule)
        }
    }

    func generateRule(_ rule: InternalGrammar.Rule) throws {
        if latestSettings.omitUnreachable && !rule.isReachable {
            return
        }

        let name = alias(for: rule)

        // Derive a doc comment for the generated rule
        let linePrefix = "///"

        buffer.emitLine("\(linePrefix) ```")
        buffer.emit("\(linePrefix) \(rule.name)")
        if let type = rule.type {
            buffer.emit("[\(type.description)]")
        }
        buffer.emitLine(":")
        for alt in rule.alts {
            buffer.emitLine("\(linePrefix)     | \(alt)")
        }
        buffer.emitLine("\(linePrefix)     ;")
        buffer.emitLine("\(linePrefix) ```")

        // @memoized/@memoizedLeftRecursive
        var isMemoized = false
        if rule.isRecursiveLeader {
            isMemoized = true
            buffer.emitLine(#"@memoizedLeftRecursive("\#(name)")"#)
        } else if !rule.isRecursive {
            isMemoized = true
            buffer.emitLine(#"@memoized("\#(name)")"#)
        }

        // @inlinable
        buffer.emitLine("@inlinable")

        // func <rule>() -> <node>
        let fName = isMemoized ? "__\(name)" : name
        var returnType = typeForRule(rule) ?? defaultRuleType()
        // Ensure we don't emit labeled tuples with one element
        switch returnType {
        case .tuple(let elements) where elements.count == 1:
            returnType = elements[0].swiftType
        default:
            break
        }

        let failReturnExpression =
            switch rule.type {
            case .tuple(let elements) where elements.count != 1:
                "(\(elements.map({ _ in "nil" }).joined(separator: ", ")))"
            default:
                "nil"
            }

        buffer.emit("public func \(fName)() throws -> \(returnType) ")
        try buffer.emitBlock {
            declContext.push()
            defer { declContext.pop() }
            declContext.defineLocal(suggestedName: "mark")
            declContext.defineLocal(suggestedName: "cut")

            buffer.emitLine("let mark = self.mark()")
            if hasCut(rule) {
                buffer.emitLine("var cut = CutFlag()")
            }

            for alt in rule.alts {
                try generateAlt(alt, in: rule, failReturnExpression: failReturnExpression)
            }

            buffer.emitLine("return \(failReturnExpression)")
        }

        // Separate rule methods
        buffer.ensureDoubleNewline()
    }

    func generateAlt(
        _ alt: InternalGrammar.Alt,
        in rule: InternalGrammar.Rule,
        failReturnExpression: String
    ) throws {
        if alt.items.isEmpty { return }

        declContext.push()
        defer { declContext.pop() }

        // TODO: Handle minimal/maximal repetitions
        buffer.emitNewline()

        // if block
        buffer.emitLine("if")
        try buffer.indented {
            try generateNamedItems(alt.items, in: rule)
        }
        buffer.ensureNewline()

        // Successful alt match
        buffer.emitBlock {
            generateOnAltMatchBlock(alt, in: rule)
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
        in rule: InternalGrammar.Rule
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
        if alt.items.count == 1 {
            let bindings = computeBindings(alt)
            if bindings.count == 1, let label = bindings[0].label {
                buffer.emitLine(label)
                return
            }
        }

        // Fallback: Return an initialization of the associated node type, assuming
        // it is not `nil` and is not a known existential type, otherwise return
        // `Node()`.
        if let type = rule.type?.description, type != "Any" {
            buffer.emitLine("\(type)()")
        } else {
            buffer.emitLine("Node()")
        }
    }

    /// Generates items as a sequence of optional bindings.
    func generateNamedItems(
        _ namedItems: [InternalGrammar.NamedItem],
        in rule: InternalGrammar.Rule
    ) throws {
        let commaEmitter = buffer.startConditionalEmitter()
        for namedItem in namedItems {
            try generateNamedItem(namedItem, commaEmitter, in: rule)
        }
    }

    func generateNamedItem(
        _ namedItem: InternalGrammar.NamedItem,
        _ commaEmitter: CodeStringBuffer.ConditionalEmitter,
        in rule: InternalGrammar.Rule
    ) throws {

        commaEmitter.conditional { buffer in
            buffer.emitLine(",")
        }

        switch namedItem {
        case .item(_, let item, _):
            var bindings = self.bindings(for: namedItem)
            if bindings.isEmpty {
                bindings = [
                    ("_", typeForNamedItem(namedItem))
                ]
            }

            // Emit bindings
            try generatePatternBinds(bindings, in: rule)
            buffer.emit(" = ")
            try generateItem(item, in: rule)

        case .lookahead(let lookahead):
            try generateLookahead(lookahead, in: rule)
        }
    }

    func generateItem(_ item: InternalGrammar.Item, in rule: InternalGrammar.Rule) throws {
        switch item {
        case .optional(let atom):
            buffer.emit("try self.optional(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .optionalItems(let alts):
            buffer.emit("try self.optional(")
            buffer.emitInlinedBlock {
                let aux = enqueueAuxiliaryRule(
                    for: rule,
                    suffix: "_opt",
                    alts
                )
                buffer.emit("try self.\(aux)()")
            }
            buffer.emit(")")

        case .gather(let sep, let item):
            buffer.emit("try self.gather(separator: ")
                try buffer.emitInlinedBlock {
                    try generateAtom(sep, in: rule)
                }
            buffer.emit(", item: ")
                try buffer.emitInlinedBlock {
                    try generateAtom(item, in: rule)
                }
            buffer.emit(")")

        case .zeroOrMore(let atom, _):
            buffer.emit("try self.repeatZeroOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .oneOrMore(let atom, _):
            buffer.emit("try self.repeatOneOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .atom(let atom):
            try generateAtom(atom, in: rule)
        }
    }

    func generateLookahead(
        _ lookahead: InternalGrammar.Lookahead,
        in rule: InternalGrammar.Rule
    ) throws {
        switch lookahead {
        case .forced(let atom):
            buffer.emit("try self.expectForced(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(#", \#(atom.description.debugDescription))"#)

        case .positive(let atom):
            buffer.emit("try self.positiveLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .negative(let atom):
            buffer.emit("try self.negativeLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .cut:
            buffer.emit("cut.toggleOn()")
        }
    }

    func generateAtom(
        _ atom: InternalGrammar.Atom,
        in rule: InternalGrammar.Rule
    ) throws {
        switch atom {
        case .group(let group):
            let aux = enqueueAuxiliaryRule(
                for: rule,
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
    func generatePatternBinds(
        _ bindings: [Binding],
        in rule: InternalGrammar.Rule
    ) throws {

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

    /// Contains metadata about an auxiliary rule.
    struct AuxiliaryRuleInformation {
        typealias ReturnElement = (label: String, type: CommonAbstract.SwiftType)

        /// The auxiliary rule's name.
        var name: String

        /// Holds the contents of common named items exposed by the rule's return
        /// type, as a tuple of elements.
        var returnElements: [ReturnElement]
    }

    /// Encapsulates a remaining rule/auxiliary rule production.
    enum RemainingProduction {
        case rule(InternalGrammar.Rule)
    }
}

// MARK: Auxiliary method management

extension SwiftCodeGen {
    /// Attempts to compute a default return action for a given set of return
    /// elements of a rule or auxiliary rule.
    func defaultReturnAction(
        for elements: [AuxiliaryRuleInformation.ReturnElement]
    ) -> InternalGrammar.Action {

        let action: InternalGrammar.Action
        let labels: [String] = elements.map(\.label)

        // Avoid emitting single-tuple constructions
        if labels.count == 1 {
            action = .init(string: " \(escapeIdentifier(labels[0])) ")
        } else {
            let tupleElements = labels.map { label in
                "\(label): \(escapeIdentifier(label))"
            }.joined(separator: ", ")

            action = .init(string: " (\(tupleElements)) ")
        }

        return action
    }

    /// Computes the implicit return elements for a given rule.
    ///
    /// If no suitable set of common elements could be extracted, an empty array
    /// is returned.
    ///
    /// - note: Result removes a layer of optional wrapping from all bindings,
    /// since the code generator assumes that the bindings are part of a
    /// successful if-let pattern.
    func computeImplicitReturnElements(
        _ rule: InternalGrammar.Rule
    ) -> [AuxiliaryRuleInformation.ReturnElement] {

        computeReturnElements(rule.alts)
    }

    /// Computes the return elements for a given set of alts.
    ///
    /// If no suitable set of common elements could be extracted, an empty array
    /// is returned.
    ///
    /// - note: Result removes a layer of optional wrapping from all bindings,
    /// since the code generator assumes that the bindings are part of a
    /// successful if-let pattern.
    func computeReturnElements(
        _ alts: [InternalGrammar.Alt]
    ) -> [AuxiliaryRuleInformation.ReturnElement] {

        // Fill bindings

        var firstBindingNames: [String] = [] // For ordering later
        var bindingsPerAlt: [[String: CommonAbstract.SwiftType]] = []
        for alt in alts {
            var bindings: [String: CommonAbstract.SwiftType] = [:]

            for case let (name?, type?) in computeBindings(alt) {
                firstBindingNames.append(name)
                bindings[name] = type
            }

            bindingsPerAlt.append(bindings)
        }

        // Only consider alts that occur in all alts, with the same type
        var commonBindings: [String: CommonAbstract.SwiftType] = [:]
        let allKeys = Set(bindingsPerAlt.flatMap(\.keys))

        for key in allKeys {
            guard bindingsPerAlt.allSatisfy({ $0[key] != nil }) else {
                continue
            }
            let bindingTypes = Set(bindingsPerAlt.compactMap({ $0[key] }))
            guard bindingTypes.count == 1, let type = bindingTypes.first else {
                continue
            }

            commonBindings[key] = type
        }

        var pairs = commonBindings.map {
            ($0.key, $0.value)
        }
        // Order bindings based on their appearance on the first alt
        pairs.sort { (b1, b2) in
            guard
                let index1 = firstBindingNames.firstIndex(of: b1.0),
                let index2 = firstBindingNames.firstIndex(of: b2.0)
            else {
                // Found bindings that are not in common with all/first alt?
                return false
            }

            return index1 < index2
        }

        return pairs.map {
            (label: $0.0, type: $0.1)
        }
    }

    /// Computes the bindings for a given of alt based on the elements that are
    /// contained within.
    ///
    /// - note: Result removes a layer of optional wrapping from all bindings,
    /// since the code generator assumes that the bindings are part of a
    /// successful if-let pattern.
    func computeBindings(
        _ alt: InternalGrammar.Alt
    ) -> [Binding] {

        var result: [Binding] = []
        for item in alt.items {
            for binding in bindings(for: item) {
                result.append(binding)
            }
        }
        return result
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
        for rule: InternalGrammar.Rule,
        suffix: String,
        _ alts: [InternalGrammar.Alt]
    ) -> String {

        var alts = alts

        let elements = computeReturnElements(alts)
        let type: CommonAbstract.SwiftType =
            if elements.isEmpty {
                "Any"
            } else if elements.count == 1 {
                elements[0].type
            } else {
                .tuple(elements.map { element in
                    .labeled(label: element.label, element.type)
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
            for: rule,
            suffix: suffix,
            type: type,
            alts
        )
    }

    /// Enqueues an auxiliary rule to be generated based on a given rule as context.
    /// Returns the deduplicated, unique method name to use as a reference for
    /// further code generation.
    ///
    /// - note: Return type will be optionally-wrapped by this method.
    func enqueueAuxiliaryRule(
        for rule: InternalGrammar.Rule,
        suffix: String,
        type: CommonAbstract.SwiftType,
        _ alts: [InternalGrammar.Alt]
    ) -> String {

        let name = "_\(rule.name)_\(suffix)"

        let ruleName = enqueueAuxiliaryRule(.init(name: name, type: type, alts: alts))

        return ruleName
    }

    /// Enqueues a given auxiliary rule, returning its deduplicated name for
    /// further referencing.
    private func enqueueAuxiliaryRule(_ rule: InternalGrammar.Rule) -> String {
        let decl = declContext.defineMethod(suggestedName: rule.name)
        var rule = rule
        rule.name = decl.name

        enqueueProduction(.rule(rule))

        return decl.name
    }

    /// Enqueues a given production into the queue of remaining productions.
    private func enqueueProduction(_ production: RemainingProduction) {
        remaining.append(production)
    }
}

// MARK: Type resolution

extension SwiftCodeGen {
    /// Returns the type to use when referring to tokens returned by a parser.
    ///
    /// - note: Only valid within `PEGParser` members.
    func swiftTokenType() -> CommonAbstract.SwiftType {
        "TokenResult"
    }

    /// Returns the default type to use when rules specify no return type.
    ///
    /// - note: Rules always have an optional layer added to their return types.
    func defaultRuleType() -> CommonAbstract.SwiftType {
        .optional("Node")
    }

    /// Attempts to statically infer the type of a given rule.
    ///
    /// - note: Rules always have an optional layer added to their return types.
    func typeForRule(_ rule: InternalGrammar.Rule) -> CommonAbstract.SwiftType? {
        if _typeForRuleOngoing.contains(rule.name) {
            return nil
        }

        return rule.type?.scg_optionalWrapped()
    }

    /// Attempts to statically infer the type of a given named item.
    ///
    /// Since named items are interpreted as if-let bindings by the code generator,
    /// the return type is optionally-unwrapped, if it is an optional type,
    /// except for items that explicitly provide a type (`name[<swiftType>]=someItem`),
    /// which returns the provided type as-is.
    func typeForNamedItem(_ namedItem: InternalGrammar.NamedItem) -> CommonAbstract.SwiftType {
        switch namedItem {
        case .item(_, _, let type?):
            return type

        case .item(_, let item, _):
            return typeForItem(item).unwrapped

        case .lookahead:
            return "Bool"
        }
    }

    /// Attempts to statically infer the type of an item.
    ///
    /// Always optionally-wrapped.
    func typeForItem(_ item: InternalGrammar.Item) -> CommonAbstract.SwiftType {
        switch item {
        case .oneOrMore(let atom, _), .zeroOrMore(let atom, _), .gather(_, let atom):
            // Remove one layer of optionality, as it's consumed by the repetition
            // process to gauge when the repetition should end
            let type = self.typeForAtom(atom).scg_unwrapped()
            return .array(type).scg_optionalWrapped()

        case .optionalItems(let alts):
            let elements = computeReturnElements(alts)
            return elements.scg_asTupleType().scg_optionalWrapped()

        case .optional(let atom):
            return typeForAtom(atom).scg_optionalWrapped()

        case .atom(let atom):
            return typeForAtom(atom)
        }
    }

    /// Attempts to statically infer the type of an atom.
    ///
    /// Can be either a token type, or a rule's production.
    /// Always optionally-wrapped.
    func typeForAtom(_ atom: InternalGrammar.Atom) -> CommonAbstract.SwiftType {
        switch atom {
        case .group(let alts):
            let elements = computeReturnElements(alts)
            return elements.scg_asTupleType().scg_optionalWrapped()

        case .anyToken, .token, .string:
            return .optional(swiftTokenType())

        case .ruleName(let ruleName):
            if let rule = findRule(named: ruleName) {
                return typeForRule(rule) ?? defaultRuleType()
            }

            return defaultRuleType()
        }
    }
}

// MARK: Alias management

extension SwiftCodeGen: TokenLiteralResolver {
    /// Type for if-let pattern binds.
    typealias Binding = (label: String?, type: CommonAbstract.SwiftType)

    /// Searches within grammar and remaining rules for one with a matching name.
    ///
    /// If none are found, returns `nil`, instead.
    func findRule(named name: String) -> InternalGrammar.Rule? {
        if let rule = grammar.rule(named: name) {
            return rule
        }
        for production in remaining {
            switch production {
            case .rule(let rule) where rule.name == name:
                return rule
            default:
                continue
            }
        }

        return nil
    }

    /// Returns the alias for referencing the given rule in code with `self.<rule alias>()`.
    func alias(for rule: InternalGrammar.Rule) -> String {
        if let alias = self.ruleAliases[rule.name] {
            return alias
        }

        return rule.name
    }

    /// Returns bindings for a given named item, to be used for if-let binding.
    ///
    /// The result might have more than one element, in case the item expands
    /// into auxiliary rules with multiple elements.
    ///
    /// Since named items are interpreted as if-let bindings by the code generator,
    /// the return type is optionally-unwrapped, if it is an optional type,
    /// except for items that explicitly provide a type (`name[<swiftType>]=someItem`),
    /// which returns the provided type as-is.
    func bindings(for namedItem: InternalGrammar.NamedItem) -> [Binding] {
        switch namedItem {
        case .item(let name?, let item, let type):
            return [(name, type ?? typeForItem(item).scg_unwrapped())]

        case .item(_, let item, _):
            let bindings = bindings(for: item)
            return bindings.scg_unwrapped()

        case .lookahead:
            return []
        }
    }

    /// Returns bindings for a given item, to be used for if-let binding.
    ///
    /// The result might have more than one element, in case the item expands
    /// into auxiliary rules with multiple elements.
    ///
    /// - note: Bindings from this layer have extra optionality still.
    func bindings(for item: InternalGrammar.Item) -> [Binding] {
        switch item {
        case .zeroOrMore(let atom, _),
            .oneOrMore(let atom, _),
            .gather(_, let atom):
            // Remove one layer of optionality, as it's consumed by the repetition
            // process to gauge when the repetition should end
            var bindings = bindings(for: atom)
            bindings = bindings.scg_unwrapped()
            // Propagate binding names if this is a single-binding construct
            if bindings.count == 1 {
                return [(bindings[0].label, .array(bindings[0].type))]
            }

            return [(nil, .array(bindings.scg_asTupleType()))]

        case .optional(let atom):
            // TODO: Validate an optional bind of multiple results
            return bindings(for: atom).scg_optionalWrapped()

        case .optionalItems(let alts):
            let elements = computeReturnElements(alts)
            return elements.scg_asBindings()

        case .atom(let atom):
            return bindings(for: atom)
        }
    }

    /// Returns bindings for a given atom, to be used for if-let binding.
    ///
    /// The result might have more than one element, in case the atom expands
    /// into auxiliary rules with multiple elements.
    ///
    /// - note: Bindings from this layer have extra optionality still.
    func bindings(for atom: InternalGrammar.Atom) -> [Binding] {
        switch atom {
        case .group(let alts):
            let elements = computeReturnElements(alts)
            return elements.scg_asBindings()

        case .token(let ident):
            return [(implicitBindings ? ident.lowercased() : nil, typeForAtom(atom))]

        case .anyToken(let ident):
            return [(implicitBindings ? ident.lowercased() : nil, typeForAtom(atom))]

        case .ruleName(let ident):
            return [(implicitBindings ? ident : nil, typeForAtom(atom))]

        case .string(_, let literal):
            let identifier = tokenName(ofRawLiteral: literal)
            return [(implicitBindings ? identifier : nil, typeForAtom(atom))]
        }
    }

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
            let token = tokenDefinition(named: ident),
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
            let token = tokenDefinition(named: identifier),
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
            let token = tokenDefinition(ofRawLiteral: raw),
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

    /// Returns a token definition from `self.tokenDefinitions` of a matching
    /// name, or `nil`, if none is found.
    func tokenDefinition(named name: String) -> InternalGrammar.TokenDefinition? {
        self.tokenDefinitions.first(where: { $0.name == name })
    }

    /// Returns a token definition from `self.tokenDefinitions` that has a literal
    /// value matching the given (non-quoted) value, or `nil`, if none is found.
    func tokenDefinition(ofRawLiteral literal: String) -> InternalGrammar.TokenDefinition? {
        self.tokenDefinitions.first(where: { $0.computedLiteral == literal })
    }

    /// Returns the name of a token that has a literal value matching the given
    /// (non-quoted) value, or `nil`, if none is known.
    func tokenName(ofRawLiteral literal: String) -> String? {
        tokenDefinition(ofRawLiteral: literal)?.name
    }
}

// MARK: - Minimal/maximal repetition detection

extension SwiftCodeGen {
    /// Returns `true` if the rule makes use of minimal/maximal (`<`/`>`) repetition
    /// in one of its primary alts.
    func hasNonStandardRepetition(_ node: InternalGrammar.Rule) -> Bool {
        node.alts.contains(where: hasNonStandardRepetition(_:))
    }

    /// Returns `true` if one of the given alts makes use of minimal/maximal
    /// (`<`/`>`) repetition in one of its primary items.
    func hasNonStandardRepetition(_ node: [InternalGrammar.Alt]) -> Bool {
        node.contains(where: hasNonStandardRepetition(_:))
    }

    /// Returns `true` if the given alt makes use of minimal/maximal (`<`/`>`)
    /// repetition in one of its primary items.
    func hasNonStandardRepetition(_ node: InternalGrammar.Alt) -> Bool {
        node.items.contains(where: hasNonStandardRepetition(_:))
    }

    /// Returns `true` if the given named item makes use of minimal/maximal (`<`/`>`).
    func hasNonStandardRepetition(_ node: InternalGrammar.NamedItem) -> Bool {
        switch node {
        case .item(_, let item, _):
            return hasNonStandardRepetition(item)
        default:
            return false
        }
    }

    /// Returns `true` if the given item makes use of minimal/maximal (`<`/`>`).
    func hasNonStandardRepetition(_ node: InternalGrammar.Item) -> Bool {
        switch node {
        case .zeroOrMore(_, let repetitionMode),
            .oneOrMore(_, let repetitionMode):
            return repetitionMode != .standard

        default:
            return false
        }
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
        node.items.contains(where: hasCut)
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

private extension Sequence where Element == SwiftCodeGen.Binding {
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

private extension Sequence where Element == SwiftCodeGen.AuxiliaryRuleInformation.ReturnElement {
    /// Converts the return elements within this sequence of return elements into
    /// a Swift tuple type, with labels as required.
    func scg_asTupleType() -> CommonAbstract.SwiftType {
        .tuple(map {
            .labeled(label: $0.label, $0.type)
        })
    }

    /// Converts this sequence of return elements into a list of bindings.
    func scg_asBindings() -> [SwiftCodeGen.Binding] {
        map { element in
            (element.label, element.type)
        }
    }
}

private extension CommonAbstract.SwiftType {
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
}
