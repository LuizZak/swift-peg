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

    /// Set of identifiers that cannot be used as bare identifiers in Swift, and
    /// must be escaped with backticks (`) to allow usage in declarations.
    public static var invalidBareIdentifiers: Set<String> {
        SwiftKeywords.keywords
    }

    let parserName: String
    let grammar: InternalGrammar.Grammar
    let tokenDefinitions: [InternalGrammar.TokenDefinition]
    let buffer: CodeStringBuffer
    var declContext: DeclarationsContext

    var latestSettings: ParserGenSettings = .default
    var tokenCallKind: TokenCallKind = .expect
    var remaining: [InternalGrammar.Rule] = []
    var ruleAliases: [String: String] = [:]
    var implicitReturns: Bool = true

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

        if let header = grammar.parserHeader() {
            buffer.emitLine(header)
        }
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

        declContext = DeclarationsContext()
        declContext.push() // No need to pop as the context is meant to be replaced in new generate calls

        self.remaining = grammar.rules

        buffer.emit("extension \(parserName) ")
        try buffer.emitMembersBlock {
            try generateRemainingRules()
        }

        return buffer.finishBuffer()
    }

    func generateRemainingRules() throws {
        while !remaining.isEmpty {
            let next = remaining.removeFirst()

            if !latestSettings.omitUnreachable || next.isReachable {
                try generateRule(next)
            }
        }
    }

    func generateRule(_ rule: InternalGrammar.Rule) throws {
        let type = rule.type?.description ?? "Node"
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
        buffer.emit("public func \(fName)() throws -> \(type)? ")
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
                try generateAlt(alt, in: rule)
            }

            buffer.emitLine("return nil")
        }

        // Separate rule methods
        buffer.ensureDoubleNewline()
    }

    func generateAlt(_ alt: InternalGrammar.Alt, in rule: InternalGrammar.Rule) throws {
        if alt.items.isEmpty { return }

        declContext.push()
        defer { declContext.pop() }

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
                buffer.emitLine("return nil")
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
        if alt.items.count == 1, let alias = alt.items[0].alias() {
            buffer.emitLine(escapeIdentifier(alias))
            return
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

        let alias = namedItem.alias() ?? "_"

        switch namedItem {
        case .item(_, let item, _):
            var resolvedName = alias
            if alias != "_" {
                resolvedName = declContext.defineLocal(suggestedName: alias, type: nil).name
            }

            // TODO: Allow hoisting named items of productions such as optional
            // TODO: groups and gathers so their auxiliary methods can return tuples
            // TODO: with the defined labels to be used by actions at the enclosed
            // TODO: alt
            buffer.emit("let \(escapeIdentifier(resolvedName)) = ")
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
                    type: "Any",
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

        case .zeroOrMore(let atom):
            buffer.emit("try self.repeatZeroOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .oneOrMore(let atom):
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
                type: "Any",
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
            omitUnreachable: false
        )

        /// Whether to omit unreachable rules, as detected by a `GrammarProcessor`
        /// and flagged within each `InternalGrammar.Rule` node.
        public var omitUnreachable: Bool

        public init(omitUnreachable: Bool) {
            self.omitUnreachable = omitUnreachable
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
}

// MARK: Auxiliary method management

extension SwiftCodeGen {
    /// Enqueues an auxiliary rule to be generated based on a given rule as context.
    /// Returns the deduplicated, unique method name to use as a reference for
    /// further code generation.
    func enqueueAuxiliaryRule(
        for rule: InternalGrammar.Rule,
        suffix: String,
        type: CommonAbstract.SwiftType? = nil,
        _ alts: [InternalGrammar.Alt]
    ) -> String {

        let name = "_\(rule.name)_\(suffix)"

        return enqueueAuxiliaryRule(.init(name: name, type: type, alts: alts))
    }

    /// Enqueues a given auxiliary rule, returning its deduplicated name for
    /// further referencing.
    func enqueueAuxiliaryRule(_ rule: InternalGrammar.Rule) -> String {
        let decl = declContext.defineMethod(suggestedName: rule.name)
        var rule = rule
        rule.name = decl.name

        remaining.append(rule)

        return decl.name
    }
}

// MARK: Alias management

extension SwiftCodeGen: TokenLiteralResolver {

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

    /// Returns the alias for referencing the given rule in code with `self.<rule alias>()`.
    func alias(for rule: InternalGrammar.Rule) -> String {
        if let alias = self.ruleAliases[rule.name] {
            return alias
        }

        return rule.name
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
