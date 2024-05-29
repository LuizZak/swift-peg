extension SwiftCodeGen {
    func generateTokenType() throws -> String {
        buffer.resetState()



        return buffer.finishBuffer()
    }

    func generateTokenParser(_ token: InternalGrammar.TokenDefinition) throws {
        guard let tokenSyntax = token.tokenSyntax else {
            return
        }

        let name = token.name

        // Derive a doc comment for the generated syntax
        let linePrefix = "///"

        buffer.emitLine("\(linePrefix) ```")
        buffer.emit("\(linePrefix) \(name)")
        if let staticToken = token.staticToken {
            buffer.emit(#"["\#(staticToken)"]"#)
        }
        buffer.emitLine(":")
        for alt in tokenSyntax.alts {
            buffer.emitLine("\(linePrefix)     | \(tok_describe(alt))")
        }
        buffer.emitLine("\(linePrefix)     ;")
        buffer.emitLine("\(linePrefix) ```")

        buffer.emit("func consume_\(name)<StringType>(from stream: inout StringStream<StringType>) -> Bool ")
        try buffer.emitBlock {
            try generateTokenParserBody(tokenSyntax)
        }
    }

    func generateTokenParserBody(_ tokenSyntax: CommonAbstract.TokenSyntax) throws {
        // Simplify token definitions that consist of a single literal
        if
            tokenSyntax.alts.count == 1,
            tokenSyntax.alts[0].atoms.count == 1,
            case .terminal(.literal(let lit)) = tokenSyntax.alts[0].atoms[0]
        {
            buffer.emitLine("stream.advanceIfNext(\(tok_escapeLiteral(lit)))")
            return
        }
        
        buffer.emitLine("guard !stream.isEof else { return false }")
        buffer.emitLine("let state = stream.save()")
        buffer.emitNewline()

        // TODO: Alternate do statement for an if statement depending on leading
        // TODO: atom, and remove the nesting altogether if there is only one alt

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
        for atom in alt.atoms {
            try generateTokenParserAtom(atom, bailStatement: bailStatement)
            buffer.ensureDoubleNewline()
        }

        buffer.emitLine("return true")
    }

    /// Generates token parser atoms as sequences of checks against the input
    /// stream that either succeed and proceed forward within the same level,
    /// or fail with a `break alt` statement.
    func generateTokenParserAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatement
    ) throws {

        switch atom {
        case .zeroOrMore(let alts):
            // Generate loop
            try generateTerminalLoop(alts)

        case .oneOrMore(let alts):
            // Generate a first check outside the loop
            try generateTerminalAlts(alts, bailStatement: bailStatement)

            // Generate loop
            try generateTerminalLoop(alts)
        
        case .group(let alts):
            // Like a one-or-more, but without a loop
            try generateTerminalAlts(alts, bailStatement: bailStatement)

        case .terminal(let terminal):
            try generateGuardTerminal(terminal, bailStatement: bailStatement)
            buffer.emitLine(try tok_advanceExpr(for: terminal))
            buffer.emitNewline()
        }
    }

    /// Generates a guard statement that checks that a given terminal matches
    /// on the string stream before proceeding.
    ///
    /// Within the body of the guard, a break statement, optionally labeled with
    /// a given label, is issued.
    func generateGuardTerminal(
        _ terminal: CommonAbstract.TokenTerminal,
        bailStatement: BailStatement
    ) throws {

        buffer.emit("guard \(try tok_conditional(for: terminal)) else ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates a `while` loop that continually consumes the first matched
    /// terminal in the provided list, returning 
    func generateTerminalLoop(_ alts: [CommonAbstract.TokenTerminal]) throws {
        buffer.emit("while !stream.isEof ")
        try buffer.emitBlock {
            try generateTerminalAlts(alts, bailStatement: .break())
        }
    }

    /// Generates a series of if-else conditions that attempt to match one of
    /// the provided terminals, falling back to a break statement if all terminals
    /// failed.
    ///
    /// Used to generate zero-or-more and one-or-more constructions.
    func generateTerminalAlts(
        _ alts: [CommonAbstract.TokenTerminal],
        bailStatement: BailStatement
    ) throws {
        // TODO: Perform switch statement emission

        guard !alts.isEmpty else { return }

        if try tryGenerateAsSwitch(alts, bailStatement: bailStatement) {
            return
        }

        try buffer.emitWithSeparators(alts, separator: " else ") { terminal in
            buffer.emit("if ")
            buffer.emit(try tok_conditional(for: terminal))
            buffer.emit(" ")
            try buffer.emitInlinedBlock {
                buffer.emit(try tok_advanceExpr(for: terminal))
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
        _ alts: [CommonAbstract.TokenTerminal],
        bailStatement: BailStatement
    ) throws -> Bool {

        guard canSimplifyAsSwitch(alts) else {
            return false
        }

        // Whether the given terminal can be combine with others in the same
        // switch-case
        func canCombine(_ term: CommonAbstract.TokenTerminal) -> Bool {
            switch term {
            case .excludingLiteral, .excludingIdentifier, .identifier:
                return false
            case .rangeLiteral, .literal:
                return true
            case .characterPredicate, .any:
                return false
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

        buffer.emitLine("switch stream.peek() {")

        // Indicates that an 'any' terminal was found; this invalidates any further
        // cases, and to avoid warnings about unreachable cases we skip past any
        // alt after the 'any' terminal.
        var stopEarly = false

        var index = 0
        while index < alts.count && !stopEarly {
            defer { index += 1 }
            let alt = alts[index]

            switch alt {
            case .any:
                buffer.emitLine("default:")
                stopEarly = true
            default:
                if canCombine(alt) {
                    var nextIndex = index + 1
                    while nextIndex < alts.count && !stopEarly {
                        let nextAlt = alts[nextIndex]
                        guard canCombine(nextAlt) && tok_length(for: alt) == tok_length(for: nextAlt) else {
                            break
                        }
                        nextIndex += 1
                    }

                    switch alt {
                    default:
                        let patterns = alts[index..<nextIndex]
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
    func canSimplifyAsSwitch(_ alts: [CommonAbstract.TokenTerminal]) -> Bool {
        for alt in alts {
            switch alt {
            case .excludingLiteral, .excludingIdentifier, .identifier:
                return false
            case .characterPredicate, .rangeLiteral, .literal, .any:
                if tok_length(for: alt) != 1 {
                    return false
                }
                continue
            }
        }

        return true
    }

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given terminal.
    private func tok_conditional(for term: CommonAbstract.TokenTerminal) throws -> String {
        switch term {
        case .characterPredicate(let ident, let action):
            return "let \(ident) = stream.safePeek(), \(action.trimmingWhitespace())"

        case .excludingLiteral(let literal, let next):
            return "!stream.isNext(\(tok_escapeLiteral(literal))), \(try tok_conditional(for: next))"

        case .excludingIdentifier(let ident, let next):
            // TODO: Handle literal exclusion
            return "!\(ident), \(try tok_conditional(for: next))"

        case .rangeLiteral(let start, let end):
            return "!stream.isEof, (\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))).contains(stream.peek())"

        case .literal(let literal):
            return "stream.isNext(\(tok_escapeLiteral(literal)))"

        case .identifier(let ident):
            return ident // TODO: Handle identifiers better

        case .any:
            return "!stream.isEof"
        }
    }

    /// Returns the appropriate `StringStream.advance` call that advances the
    /// stream forward by the given terminal's required length.
    private func tok_advanceExpr(for term: CommonAbstract.TokenTerminal) throws -> String {
        let length = tok_length(for: term)
        if length == 1 {
            return "stream.advance()"
        }
        return "stream.advance(\(length))"
    }

    /// Returns how many extended grapheme clusters should be skipped for a given
    /// terminal to match.
    private func tok_length(for term: CommonAbstract.TokenTerminal) -> Int {
        switch term {
        case .characterPredicate:
            return 1

        case .excludingLiteral(_, let next):
            return tok_length(for: next)

        case .excludingIdentifier(_, let next):
            return tok_length(for: next)

        case .rangeLiteral:
            return 1
        
        case .literal(let literal):
            return literal.count

        case .identifier:
            return 1 // TODO: Handle identifiers better

        case .any:
            return 1
        }
    }

    /// Escapes a given string into a Swift string literal expression.
    private func tok_escapeLiteral(_ literal: String) -> String {
        StringEscaping.escapeAsStringLiteral(literal)
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ alt: CommonAbstract.TokenAlt) -> String {
        alt.atoms.map(tok_describe).joined(separator: " ")
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ atom: CommonAbstract.TokenAtom) -> String {
        switch atom {
        case .zeroOrMore(let terms):
            return "(\(terms.map(tok_describe(_:)).joined(separator: " | ")))*"

        case .oneOrMore(let terms):
            return "(\(terms.map(tok_describe(_:)).joined(separator: " | ")))+"

        case .group(let terms):
            return "(\(terms.map(tok_describe(_:)).joined(separator: " | ")))"

        case .terminal(let term):
            return tok_describe(term)
        }
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ term: CommonAbstract.TokenTerminal) -> String {
        switch term {
        case .characterPredicate(let c, let pred):
            return "\(c) {\(pred)}"

        case .excludingIdentifier(let ident, let rem):
            return "!\(ident) \(tok_describe(rem))"

        case .excludingLiteral(let literal, let rem):
            return "!\(tok_escapeLiteral(literal)) \(tok_describe(rem))"

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

    enum BailStatement {
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
