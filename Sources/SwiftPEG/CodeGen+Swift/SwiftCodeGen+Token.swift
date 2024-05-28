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

        buffer.emit("func consume\(name)<StringType>(from stream: StringStream<StringType>) -> Bool")
        try buffer.emitBlock {
            try generateTokenParserBody(tokenSyntax)
        }
    }

    func generateTokenParserBody(_ tokenSyntax: CommonAbstract.TokenSyntax) throws {
        buffer.emitLine("guard !stream.isEof else { return false }")
        buffer.emitLine("let state = save()")
        buffer.emitLine()

        // TODO: Alternate do statement for an if statement depending on leading
        // TODO: atom, and remove the nesting altogether if there is only one alt

        for alt in tokenSyntax.alts {
            buffer.emitLine("alt:")
            buffer.emit("do ")
            try buffer.emitBlock {
                try generateTokenParserAlt(alt, bailStatement: "break alt")
            }

            buffer.emitLine("restore(state)")
        }

        buffer.emitLine("restore(state)")
        buffer.emitLine("return false")
    }

    func generateTokenParserAlt(
        _ alt: CommonAbstract.TokenAlt,
        bailStatement: String
    ) throws {
        for atom in alt.atoms {
            try generateTokenParserAtom(atom, bailStatement: "break alt")
        }
    
        buffer.emitLine("return true")
    }

    /// Generates token parser atoms as sequences of checks against the input
    /// stream that either succeed and proceed forward within the same level,
    /// or fail with a `break alt` statement.
    func generateTokenParserAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: String
    ) throws {

        switch atom {
        case .characterPredicate(let ident, let action):
            buffer.emit("guard !isEof, let \(ident) = peek(), \(action)")
            buffer.emitBlock {
                buffer.emitLine(bailStatement)
            }
        
        case .zeroOrMore(let alts):
            // Generate loop
            try generateTerminalLoop(alts)

        case .oneOrMore(let alts):
            // Generate a first check outside the loop
            try generateTerminalAlts(alts, bailStatement: bailStatement)

            // Generate loop
            try generateTerminalLoop(alts)

        case .terminal(let terminal):
            try generateGuardTerminal(terminal, bailStatement: bailStatement)
            buffer.emit(try tok_advanceExpr(for: terminal))
        }
    }

    /// Generates a guard statement that checks that a given terminal matches
    /// on the string stream before proceeding.
    ///
    /// Within the body of the guard, a break statement, optionally labeled with
    /// a given label, is issued.
    func generateGuardTerminal(
        _ terminal: CommonAbstract.TokenTerminal,
        bailStatement: String
    ) throws {

        buffer.emit("guard \(try tok_conditional(for: terminal)) else ")
        buffer.emitBlock {
            buffer.emitLine(bailStatement)
        }
    }

    /// Generates a `while` loop that continually consumes the first matched
    /// terminal in the provided list, returning 
    func generateTerminalLoop(_ alts: [CommonAbstract.TokenTerminal]) throws {
        buffer.emit("while !isEof ")
        try buffer.emitBlock {
            try generateTerminalAlts(alts, bailStatement: "break")
        }
    }

    /// Generates a series of if-else conditions that attempt to match one of
    /// the provided terminals, falling back to a break statement if all terminals
    /// failed.
    ///
    /// Used to generate zero-or-more and one-or-more constructions.
    func generateTerminalAlts(
        _ alts: [CommonAbstract.TokenTerminal],
        bailStatement: String
    ) throws {

        guard !alts.isEmpty else { return }

        try buffer.emitWithSeparators(alts, separator: " else ") { terminal in
            buffer.emit("if ")
            buffer.emit(try tok_conditional(for: terminal))
            buffer.emit(" ")
            try buffer.emitInlinedBlock {
                buffer.emit(try tok_advanceExpr(for: terminal))
            }
        }

        // Final else block
        buffer.emitBlock {
            buffer.emitLine(bailStatement)
        }
    }

    private func tok_conditional(for term: CommonAbstract.TokenTerminal) throws -> String {
        switch term {
        case .excludingLiteral(let literal, let next):
            return "!isNext(\(try tok_escapeLiteral(literal))) && \(try tok_conditional(for: next))"

        case .excludingIdentifier(let ident, let next):
            return "!\(ident) && \(try tok_conditional(for: next))"

        case .rangeLiteral(let start, let end):
            return "!isEof && (\(try tok_escapeLiteral(start))...\(try tok_escapeLiteral(end))).contains(peek())"
        
        case .literal(let literal):
            return "isNext(\(try tok_escapeLiteral(literal)))"

        case .identifier(let ident):
            return ident // TODO: Handle identifiers better

        case .any:
            return "!isEof"
        }
    }

    /// Returns the appropriate `StringStream.advance` call that advances the
    /// stream forward by the given terminal's required length.
    private func tok_advanceExpr(for term: CommonAbstract.TokenTerminal) throws -> String {
        let length = try tok_length(for: term)
        if length == 0 {
            return "advance()"
        }
        return "advance(\(length))"
    }

    /// Returns how many extended grapheme clusters should be skipped for a given
    /// terminal to match.
    private func tok_length(for term: CommonAbstract.TokenTerminal) throws -> Int {
        switch term {
        case .excludingLiteral(_, let next):
            return try tok_length(for: next)

        case .excludingIdentifier(_, let next):
            return try tok_length(for: next)

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

    private func tok_escapeLiteral(_ literal: String) throws -> String {
        literal
    }
}
