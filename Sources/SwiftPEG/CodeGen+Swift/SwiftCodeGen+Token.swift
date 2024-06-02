extension SwiftCodeGen {
    /// Generates Swift code defining the Token type of the grammar.
    public func generateTokenType(
        settings: TokenTypeGenSettings = .default
    ) throws -> String {
        buffer.resetState()

        if let missingSyntax = tokenDefinitions.first(where: { $0.tokenSyntax == nil }) {
            throw Error.tokenDefinitionMissingSyntax(missingSyntax)
        }

        if let header = grammar.tokenTypeHeader() {
            buffer.emitLine(header)
        }

        let tokenName = "\(parserName)Token"

        generateAccessLevel(settings: settings)
        try buffer.emitBlock("struct \(tokenName): TokenType, CustomStringConvertible") {
            try generateTokenTypeMembers(settings: settings)
        }

        return buffer.finishBuffer()
    }

    func generateTokenTypeMembers(settings: TokenTypeGenSettings) throws {
        generateAccessLevel(settings: settings)
        buffer.emitLine("var kind: TokenKind")

        generateAccessLevel(settings: settings)
        buffer.emitLine("var string: Substring")

        // var length: Int
        buffer.ensureDoubleNewline()
        try generateTokenTypeLength(settings: settings)

        // var description: String
        buffer.ensureDoubleNewline()
        try generateTokenTypeDescription(settings: settings)

        // init(kind: TokenKind, string: Substring)
        buffer.ensureDoubleNewline()
        try generateTokenTypeInitializer(settings: settings)

        // static func produceDummy(_ kind: TokenKind) -> Self
        buffer.ensureDoubleNewline()
        generateTokenTypeProduceDummy(settings: settings)

        // func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring
        buffer.ensureDoubleNewline()
        try generateTokenTypeParser(settings: settings, modifiers: ["static"])

        // enum TokenKind
        buffer.ensureDoubleNewline()
        try generateTokenKindEnum(settings: settings)

        // func consume_<TOKEN1>
        // func consume_<TOKEN2>
        //   ...
        buffer.ensureDoubleNewline()
        try generateTokenParsers(settings: settings)
    }

    /// `var length: Int`
    func generateTokenTypeLength(settings: TokenTypeGenSettings) throws {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitBlock("var length: Int") {
            buffer.emitLine("string.count")
        }
    }

    /// `var description: String`
    func generateTokenTypeDescription(settings: TokenTypeGenSettings) throws {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitBlock("var description: String") {
            buffer.emitLine("String(string)")
        }
    }

    /// `init(kind: TokenKind, string: Substring)`
    func generateTokenTypeInitializer(settings: TokenTypeGenSettings) throws {
        // If access level is not `nil` or "internal", produce an initializer
        // for the token type
        guard settings.accessLevel != nil && settings.accessLevel != "internal" else {
            return
        }

        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitBlock("init(kind: TokenKind, string: Substring)") {
            buffer.emitLine("self.kind = kind")
            buffer.emitLine("self.string = string")
        }
    }

    /// `static func produceDummy(_ kind: TokenKind) -> Self`
    func generateTokenTypeProduceDummy(settings: TokenTypeGenSettings) {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitBlock("static func produceDummy(_ kind: TokenKind) -> Self") {
            buffer.emitLine(#".init(kind: kind, string: "<dummy>")"#)
        }
    }

    /// `func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring`
    func generateTokenTypeParser(settings: TokenTypeGenSettings, modifiers: [String] = []) throws {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitWithSeparators(modifiers + ["func"], separator: " ")
        try buffer.emitBlock(" from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring") {
            try generateTokenTypeParserBody()
        }
    }

    func generateTokenTypeParserBody() throws {
        buffer.emitLine("guard !stream.isEof else { return nil }")
        buffer.emitLine("stream.markSubstringStart()")
        buffer.ensureDoubleNewline()

        // TODO: Attempt to generate a switch over the first peeked character
        // TODO: like in SwiftPEGGrammar's Token parser?

        let tokensToEmit = tokenDefinitions.filter(showEmitInTokenParser)
        let sorted = self.sortedTokens(tokensToEmit)

        for token in sorted {
            let tokenName = caseName(for: token)
            let method = parseMethodName(for: token)
            let parseInvocation = "\(method)(from: &stream)"

            buffer.emitBlock("if \(parseInvocation)") {
                buffer.emitLine("return .init(kind: .\(tokenName), string: stream.substring)")
            }
        }

        buffer.ensureDoubleNewline()
        buffer.emitLine("return nil")
    }

    // MARK: TokenKind generation

    func generateTokenKindEnum(settings: TokenTypeGenSettings) throws {
        generateAccessLevel(settings: settings)
        try buffer.emitBlock("enum TokenKind: TokenKindType") {
            let emitter = buffer.startConditionalEmitter()

            let tokensToEmit = self.tokenDefinitions.filter(self.showEmitInTokenType)
            let sorted = self.sortedTokens(tokensToEmit)
            for token in sorted {
                try generateTokenKindEnumCase(token, prevCaseSeparator: emitter)
            }

            buffer.ensureDoubleNewline()
            try generateTokenKindDescription(settings: settings)
        }
    }

    func generateTokenKindEnumCase(
        _ token: InternalGrammar.TokenDefinition,
        prevCaseSeparator: CodeStringBuffer.ConditionalEmitter
    ) throws {
        if let syntax = token.tokenSyntax {
            /// Emit doc-comment for case
            prevCaseSeparator.ensureEmptyLine()
            generateTokenDocComment(token, syntax, short: true)
        }

        let tokenName = caseName(for: token)
        buffer.emitLine("case \(escapeIdentifier(tokenName))")
    }

    func generateTokenKindDescription(
        settings: TokenTypeGenSettings
    ) throws {

        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitBlock("var description: String") {
            buffer.emitLine("switch self {")

            for token in tokenDefinitions where showEmitInTokenType(token) {
                guard let syntax = token.tokenSyntax else {
                    continue
                }
                let name = caseName(for: token)

                if let literal = syntax.staticTerminal() {
                    buffer.emitLine("case .\(name): \(tok_escapeLiteral(literal))")
                } else {
                    buffer.emitLine(#"case .\#(name): "\#(token.name)""#)
                }
            }

            buffer.emitLine("}")
        }
    }

    // MARK: - consume_ method generation

    func generateTokenParsers(settings: TokenTypeGenSettings) throws {
        for token in tokenDefinitions {
            buffer.ensureDoubleNewline()
            try generateTokenParser(token, settings: settings, modifiers: ["static"])
        }
    }

    func generateTokenParser(
        _ token: InternalGrammar.TokenDefinition,
        settings: TokenTypeGenSettings,
        modifiers: [String] = []
    ) throws {
        guard let tokenSyntax = token.tokenSyntax else {
            return
        }

        generateTokenDocComment(token, tokenSyntax)

        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitWithSeparators(modifiers + ["func"], separator: " ")
        buffer.emit(" \(parseMethodName(for: token))<StringType>(from stream: inout StringStream<StringType>) -> Bool ")
        try buffer.emitBlock {
            try generateTokenParserBody(tokenSyntax)
        }
    }

    func generateTokenDocComment(
        _ token: InternalGrammar.TokenDefinition,
        _ tokenSyntax: CommonAbstract.TokenSyntax,
        short: Bool = false
    ) {
        // Derive a doc comment for the generated syntax
        let linePrefix = "///"

        if short && tokenSyntax.alts.count == 1 {
            buffer.emitLine("\(linePrefix) `\(tok_describe(tokenSyntax.alts[0]))`")
            return
        }

        buffer.emitLine("\(linePrefix) ```")
        buffer.emit("\(linePrefix) \(token.name)")
        if let staticToken = token.staticToken {
            buffer.emit(#"["\#(staticToken)"]"#)
        }
        buffer.emitLine(":")
        for alt in tokenSyntax.alts {
            buffer.emitLine("\(linePrefix)     | \(tok_describe(alt))")
        }
        buffer.emitLine("\(linePrefix)     ;")
        buffer.emitLine("\(linePrefix) ```")
    }

    func generateTokenParserBody(_ tokenSyntax: CommonAbstract.TokenSyntax) throws {
        // Simplify token definitions that consist of a single literal
        if
            let literal = tokenSyntax.staticTerminal()
        {
            buffer.emitLine("stream.advanceIfNext(\(tok_escapeLiteral(literal)))")
            return
        }

        buffer.emitLine("guard !stream.isEof else { return false }")
        buffer.emitLine("let state = stream.save()")
        buffer.emitNewline()

        // TODO: Alternate do statement for an if statement depending on leading
        // TODO: item, and remove the nesting altogether if there is only one alt

        let emitter = buffer.startConditionalEmitter()
        for alt in tokenSyntax.alts {
            emitter.emit("\n")

            buffer.emitLine("alt:")
            buffer.emit("do ")
            try buffer.emitBlock {
                try generateTokenParserAlt(alt, bailStatement: .break(label: "alt"))
            }

            buffer.emitNewline()
            buffer.emitLine("stream.restore(state)")
        }

        emitter.emit("\n")
        buffer.emitLine("return false")
    }

    func generateTokenParserAlt(
        _ alt: CommonAbstract.TokenAlt,
        bailStatement: BailStatement
    ) throws {
        for item in alt.items {
            try generateTokenParserItem(item, bailStatement: bailStatement)
            buffer.ensureDoubleNewline()
        }

        buffer.emitLine("return true")
    }

    /// Generates token parser items as sequences of checks against the input
    /// stream that either succeed and proceed forward within the same level,
    /// or fail with a `break alt` statement.
    func generateTokenParserItem(
        _ item: CommonAbstract.TokenItem,
        bailStatement: BailStatement
    ) throws {

        switch item {
        case .zeroOrMore(let alts):
            // Generate loop
            try generateAtomLoop(alts)

        case .oneOrMore(let alts):
            // Generate a first check outside the loop
            try generateAtomAlts(alts, bailStatement: bailStatement)

            // Generate loop
            try generateAtomLoop(alts)

        case .optionalGroup(let alts):
            try generateAtomAlts(alts, bailStatement: .none)

        case .group(let alts):
            // Like a one-or-more, but without a loop
            try generateAtomAlts(alts, bailStatement: bailStatement)

        case .optionalAtom(let atom):
            try generateIfAtom(atom, bailStatement: .custom(tok_advanceExpr(for: atom)))
            buffer.emitNewline()

        case .atom(let atom):
            try generateGuardAtom(atom, bailStatement: bailStatement)
            buffer.emitLine(try tok_advanceExpr(for: atom))
            buffer.emitNewline()
        }
    }

    /// Generates a guard statement that checks that a given atom matches
    /// on the string stream before proceeding.
    ///
    /// Within the body of the guard, the provided a bail statement is issued.
    func generateGuardAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatement
    ) throws {

        buffer.emit("guard \(try tok_conditional(for: atom)) else ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates an if- statement that checks that a given atom matches
    /// on the string stream before bailing.
    ///
    /// Within the body of the if, the provided a bail statement is issued.
    func generateIfAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatement
    ) throws {

        buffer.emit("if \(try tok_conditional(for: atom)) ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates a `while` loop that continually consumes the first matched
    /// atom in the provided list, returning
    func generateAtomLoop(_ alts: [CommonAbstract.TokenAtom]) throws {
        buffer.ensureDoubleNewline()
        buffer.emitLine("loop:")
        buffer.emit("while !stream.isEof ")
        try buffer.emitBlock {
            try generateAtomAlts(alts, bailStatement: .break(label: "loop"))
        }
    }

    /// Generates a series of if-else conditions that attempt to match one of
    /// the provided terminals, falling back to a break statement if all terminals
    /// failed.
    ///
    /// Used to generate zero-or-more and one-or-more constructions.
    func generateAtomAlts(
        _ alts: [CommonAbstract.TokenAtom],
        bailStatement: BailStatement
    ) throws {
        // TODO: Perform switch statement emission

        guard !alts.isEmpty else { return }

        if try tryGenerateAsSwitch(alts, bailStatement: bailStatement) {
            return
        }

        try buffer.emitWithSeparators(alts, separator: " else ") { atom in
            buffer.emit("if ")
            buffer.emit(try tok_conditional(for: atom))
            buffer.emit(" ")
            try buffer.emitInlinedBlock {
                buffer.emit(try tok_advanceExpr(for: atom))
            }
        }

        // Final else block
        buffer.emit(" else ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates the given set of alternating terminals as a switch statement,
    /// modifying the buffer and returning `true` if successful.
    ///
    /// If the alts cannot be simplified to a single switch statement, the buffer
    /// is untouched and `false` is returned.
    private func tryGenerateAsSwitch(
        _ alts: [CommonAbstract.TokenAtom],
        bailStatement: BailStatement
    ) throws -> Bool {

        guard canSimplifyAsSwitch(alts) else {
            return false
        }

        // Whether the given terminal can be combine with others in the same
        // switch-case
        func canCombine(_ term: CommonAbstract.TokenTerminal) -> Bool {
            switch term {
            case .identifier:
                return false
            case .rangeLiteral, .literal:
                return true
            case .characterPredicate, .any:
                return false
            }
        }

        // Whether the given atom can be combine with others in the same
        // switch-case
        func canCombine(_ atom: CommonAbstract.TokenAtom) -> Bool {
            atom.excluded.isEmpty && canCombine(atom.terminal)
        }

        /// Returns the combination of two atoms, as a list of atoms that cover
        /// the same range of inputs as the two input atoms.
        func combination(of lhs: CommonAbstract.TokenAtom, _ rhs: CommonAbstract.TokenAtom) -> [CommonAbstract.TokenAtom] {
            guard lhs.excluded.isEmpty && rhs.excluded.isEmpty else {
                return [lhs, rhs]
            }
            if lhs == rhs {
                return [lhs]
            }

            switch (lhs.terminal, rhs.terminal) {
            // Merge range literals
            case (.rangeLiteral(let lhsLow, let lhsHigh), .rangeLiteral(let rhsLow, let rhsHigh))
                where lhsHigh.contents == rhsLow.contents:

                return [.init(terminal: .rangeLiteral(lhsLow, rhsHigh))]

            // Merge literal into ranged literals that it is contained within
            case (.literal(let lhsLiteral), .rangeLiteral(let rhsLow, let rhsHigh))
                where (rhsLow.contents...rhsHigh.contents).contains(lhsLiteral.contents):

                return [rhs]
            case (.rangeLiteral(let lhsLow, let lhsHigh), .literal(let rhsLiteral))
                where (lhsLow.contents...lhsHigh.contents).contains(rhsLiteral.contents):

                return [lhs]

            default:
                return [lhs, rhs]
            }
        }

        // Produces the pattern for a switch-case for a given terminal
        func casePattern(_ term: CommonAbstract.TokenTerminal) -> String {
            switch term {
            case .characterPredicate(let ident, let predicate):
                return "let \(ident) where \(predicate.trimmingWhitespace())"
            case .rangeLiteral(let start, let end):
                return "\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))"
            case .literal(let literal):
                return "\(tok_escapeLiteral(literal))"
            case .any:
                return "_"
            default:
                return ""
            }
        }

        // Produces the pattern for a switch-case for a given atom
        func casePattern(_ atom: CommonAbstract.TokenAtom) -> String {
            casePattern(atom.terminal)
        }

        buffer.emitLine("switch stream.peek() {")

        // Indicates that an 'any' atom was found; this invalidates any further
        // cases, and to avoid warnings about unreachable cases we skip past any
        // alt after the 'any' atom.
        var stopEarly = false

        var index = 0
        while index < alts.count && !stopEarly {
            defer { index += 1 }
            let alt = alts[index]

            switch alt.terminal {
            case .any:
                buffer.emitLine("default:")
                stopEarly = true
            default:
                if canCombine(alt) {
                    var combined: [CommonAbstract.TokenAtom] = [alt]

                    var nextIndex = index + 1
                    while nextIndex < alts.count && !stopEarly {
                        let nextAlt = alts[nextIndex]
                        guard canCombine(nextAlt) && tok_length(for: alt) == tok_length(for: nextAlt) else {
                            break
                        }
                        let lastCombined = combination(of: combined[combined.count - 1], nextAlt)
                        combined[(combined.count - 1)...] = lastCombined[...]
                        nextIndex += 1
                    }

                    switch alt {
                    default:
                        let patterns = combined
                            .map(casePattern)
                            .joined(separator: ", ")
                        buffer.emitLine("case \(patterns):")
                    }

                    index = nextIndex - 1
                } else {
                    switch alt {
                    default:
                        buffer.emitLine("case \(casePattern(alt)):")
                    }
                }
            }

            try buffer.indented {
                buffer.emitLine(try tok_advanceExpr(for: alt))
            }
        }
        // Emit default block
        if !stopEarly {
            buffer.emitLine("default:")
            buffer.indented {
                bailStatement.emit(into: buffer)
            }
        }

        buffer.emitLine("}")

        return true
    }

    /// Whether the given set of alts can be simplified to a single switch statement
    /// that inspects a single token from the stream.
    func canSimplifyAsSwitch(_ alts: [CommonAbstract.TokenAtom]) -> Bool {
        for alt in alts {
            if !alt.excluded.isEmpty {
                return false
            }

            switch alt.terminal {
            case .identifier:
                return false

            case .characterPredicate, .rangeLiteral, .literal, .any:
                guard tok_length(for: alt) == 1 else {
                    return false
                }
            }
        }

        return true
    }

    // MARK: - Conditional checkers

    private func showEmitInTokenType(_ token: InternalGrammar.TokenDefinition) -> Bool {
        !token.isFragment
    }

    private func showEmitInTokenParser(_ token: InternalGrammar.TokenDefinition) -> Bool {
        !token.isFragment
    }

    // MARK: - Conditional emissions

    private func generateInlinableAttribute(settings: TokenTypeGenSettings) {
        if settings.emitInlinable {
            buffer.emitLine("@inlinable")
        }
    }

    private func generateAccessLevel(settings: TokenTypeGenSettings) {
        if let accessLevel = settings.accessLevel {
            buffer.emit("\(accessLevel) ")
        }
    }

    // MARK: - Static token transformations

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given atom.
    private func tok_conditional(for atom: CommonAbstract.TokenAtom) throws -> String {
        let conditionals = try atom.excluded.map(tok_conditional) + [tok_conditional(for: atom.terminal)]

        return conditionals.joined(separator: ", ")
    }

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given terminal.
    private func tok_conditional(for term: CommonAbstract.TokenTerminal) throws -> String {
        switch term {
        case .characterPredicate(let ident, let action):
            return "let \(ident) = stream.safePeek(), \(action.trimmingWhitespace())"

        case .rangeLiteral(let start, let end):
            return "!stream.isEof, (\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))).contains(stream.peek())"

        case .literal(let literal):
            return "stream.isNext(\(tok_escapeLiteral(literal)))"

        case .identifier(let ident):
            return  "\(parseMethodName(for: ident))(from: &stream)"

        case .any:
            return "!stream.isEof"
        }
    }

    private func tok_conditional(for exclude: CommonAbstract.TokenExclusion) throws -> String {
        switch exclude {
        case .string(let literal):
            return "!stream.isNext(\(tok_escapeLiteral(literal)))"

        case .identifier(let ident):
            return "stream.negativeLookahead(\(parseMethodName(for: ident))(from:))"
        }
    }

    /// Returns the appropriate `StringStream.advance` call that advances the
    /// stream forward by the given atom's required length.
    private func tok_advanceExpr(for term: CommonAbstract.TokenAtom) throws -> String {
        let length = tok_length(for: term)
        if length == 0 {
            return ""
        }
        if length == 1 {
            return "stream.advance()"
        }
        return "stream.advance(\(length))"
    }

    /// Returns how many extended grapheme clusters should be skipped for a given
    /// atom to match.
    private func tok_length(for atom: CommonAbstract.TokenAtom) -> Int {
        tok_length(for: atom.terminal)
    }

    /// Returns how many extended grapheme clusters should be skipped for a given
    /// terminal to match.
    private func tok_length(for term: CommonAbstract.TokenTerminal) -> Int {
        switch term {
        case .characterPredicate:
            return 1

        case .rangeLiteral:
            return 1

        case .literal(let literal):
            return literal.contents.count

        case .identifier:
            // Sub-syntaxes automatically advance the stream forward
            return 0

        case .any:
            return 1
        }
    }

    /// Escapes a given `DualString` into a Swift string literal expression.
    private func tok_escapeLiteral(_ string: CommonAbstract.DualString) -> String {
        switch string {
        case .fromSource(_, let original):
            // Convert single-quote into double-quote
            guard original.hasPrefix("'") else {
                return original
            }

            let terminator = "\""

            return terminator + StringEscaping.escapeTerminators(
                original.dropFirst().dropLast(),
                terminator: terminator
            ) + terminator

        case .fromCode(let contents):
            return tok_escapeLiteral(contents)
        }
    }

    /// Escapes a given string into a Swift string literal expression.
    private func tok_escapeLiteral(_ literal: some StringProtocol) -> String {
        StringEscaping.escapeAsStringLiteral(literal)
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ alt: CommonAbstract.TokenAlt) -> String {
        alt.items.map({ tok_describe($0) }).joined(separator: " ")
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ item: CommonAbstract.TokenItem) -> String {
        switch item {
        case .zeroOrMore(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))*"

        case .oneOrMore(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))+"

        case .optionalGroup(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))?"

        case .group(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))"

        case .optionalAtom(let term):
            return "\(tok_describe(term))?"

        case .atom(let term):
            return tok_describe(term)
        }
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ atom: CommonAbstract.TokenAtom) -> String {
        let comp = atom.excluded.map({ tok_describe($0) }) + [tok_describe(atom.terminal)]
        return comp.joined(separator: " ")
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ exclude: CommonAbstract.TokenExclusion) -> String {
        switch exclude {
        case .identifier(let identifier):
            return "!\(identifier)"

        case .string(let literal):
            return "!\(tok_escapeLiteral(literal))"
        }
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ term: CommonAbstract.TokenTerminal) -> String {
        switch term {
        case .characterPredicate(let c, let pred):
            return "\(c) {\(pred)}"

        case .rangeLiteral(let start, let end):
            return "\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))"

        case .identifier(let ident):
            return ident

        case .literal(let literal):
            return tok_escapeLiteral(literal)

        case .any:
            return "."
        }
    }

    /// Derives an identifier to use as an enumeration case label for a given
    /// token.
    ///
    /// If the token has a static token string that matches `<someIdentifier>`
    /// or `.<someIdentifier>`, returns `<someIdentifier>`, otherwise returns
    /// the token's name.
    func caseName(for token: InternalGrammar.TokenDefinition) -> String {
        guard let staticToken = token.staticToken else {
            return token.name
        }
        if staticToken.hasPrefix(".") {
            let suffix = staticToken.dropFirst()
            guard SwiftSyntaxExt.isIdentifier(suffix) else {
                return token.name
            }

            return String(suffix)
        } else {
            guard SwiftSyntaxExt.isIdentifier(staticToken) else {
                return token.name
            }

            return String(staticToken)
        }
    }

    private func parseMethodName(for token: InternalGrammar.TokenDefinition) -> String {
        return parseMethodName(for: token.name)
    }

    private func parseMethodName(for identifier: String) -> String {
        return "consume_\(identifier)"
    }

    /// Returns the input set of tokens, sorted so that tokens that compute as
    /// prefix of other tokens come later in parsing attempts.
    private func sortedTokens(_ tokens: [InternalGrammar.TokenDefinition]) -> [InternalGrammar.TokenDefinition] {
        // Generate a graph of prefix-dependencies
        let graph = GenericDirectedGraph<InternalGrammar.TokenDefinition>()
        graph.addNodes(tokens)

        for token in tokens {
            guard let tokenSyntax = token.tokenSyntax else {
                continue
            }
            guard let tokenNode = graph.nodes.first(where: { $0.value.name == token.name }) else {
                continue
            }

            for other in tokens where token.name != other.name {
                guard let otherSyntax = other.tokenSyntax else {
                    continue
                }
                guard let otherNode = graph.nodes.first(where: { $0.value.name == other.name }) else {
                    continue
                }

                if tokenSyntax.isPrefix(of: otherSyntax) && !otherSyntax.isPrefix(of: tokenSyntax) {
                    graph.addEdge(from: otherNode, to: tokenNode)
                } else if
                    tokenSyntax.isStatic(),
                    !otherSyntax.isStatic(),
                    !graph.hasPath(from: otherNode, to: tokenNode)
                {
                    // Add a synthetic dependency that forces static tokens to be
                    // emitted before dynamic tokens
                    graph.addEdge(from: tokenNode, to: otherNode)
                }
            }
        }

        guard var sorted = graph.topologicalSorted() else {
            // TODO: Apply some fallback strategy
            return tokens
        }

        // Favor whitespace token to be first
        if let index = sorted.firstIndex(where: \.value.isWhitespace) {
            let token = sorted.remove(at: index)
            sorted.insert(token, at: 0)
        }

        return sorted.map(\.value)
    }

    enum BailStatement {
        /// Indicates that the bail statement should expand to a no-op, non
        /// control-flow-altering statement.
        case none

        /// Provides a custom expansion for the bail statement.
        case custom(String)

        /// Indicates that the bail statement should expand to:
        /// ```
        /// break [label]
        /// ```
        case `break`(label: String? = nil)

        /// Indicates that the bail statement should expand to:
        /// ```
        /// stream.restore(state)
        /// return false
        /// ```
        case restoreAndReturn(stateIdentifier: String = "state")

        func emit(into buffer: CodeStringBuffer) {
            switch self {
            case .none:
                buffer.emitLine("_=()")

            case .custom(let line):
                buffer.emitLine(line)

            case .break(nil):
                buffer.emitLine("break")

            case .break(let label?):
                buffer.emitLine("break \(label)")

            case .restoreAndReturn(let state):
                buffer.emitLine("stream.restore(\(state))")
                buffer.emitLine("return false")
            }
        }
    }
}

private extension InternalGrammar.TokenDefinition {
    var isWhitespace: Bool {
        self.staticToken == ".whitespace" || self.staticToken == "whitespace" || self.name == "whitespace"
    }
}
