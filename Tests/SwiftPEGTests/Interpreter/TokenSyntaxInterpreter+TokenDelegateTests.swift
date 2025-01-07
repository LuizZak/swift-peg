import Testing

@testable import SwiftPEG

struct TokenSyntaxInterpreter_TokenDelegateTests {
    @Test
    func produceToken_parseByLength() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        let resultMinus = try assertUnwrap(try sut.produceToken(string: "-"))
        let resultArrow = try assertUnwrap(try sut.produceToken(string: "->"))

        assertEqual(resultMinus.token as? Substring, "-")
        assertEqual(resultMinus.length, 1)
        assertEqual(resultArrow.token as? Substring, "->")
        assertEqual(resultArrow.length, 2)
    }

    @Test
    func produceToken_ignoreFragments() throws {
        let tokens = try parseTokenDefinitions(#"""
        $arrow: minus '>' ;
        %minus: '-' ;
        """#)
        let sut = makeSut(tokens)

        let resultMinus = try sut.produceToken(string: "-")
        let resultArrow = try assertUnwrap(try sut.produceToken(string: "->"))

        assertNil(resultMinus)
        assertEqual(resultArrow.token as? Substring, "->")
        assertEqual(resultArrow.length, 2)
    }

    @Test
    func tokenResult_matchesTokenName_correctMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertTrue(sut.tokenResult(("-", 1), matchesTokenName: "minus"))
        assertTrue(sut.tokenResult(("->", 2), matchesTokenName: "arrow"))
    }

    @Test
    func tokenResult_matchesTokenName_incorrectMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertFalse(sut.tokenResult(("-", 2), matchesTokenName: "minus"))
        assertFalse(sut.tokenResult((">", 2), matchesTokenName: "minus"))
        assertFalse(sut.tokenResult(("->", 1), matchesTokenName: "arrow"))
    }

    @Test
    func tokenResult_matchesTokenLiteral_correctMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertTrue(sut.tokenResult(("-", 1), matchesTokenLiteral: "-"))
        assertTrue(sut.tokenResult(("->", 2), matchesTokenLiteral: "->"))
    }

    @Test
    func tokenResult_matchesTokenLiteral_incorrectMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertFalse(sut.tokenResult(("-", 2), matchesTokenLiteral: "-"))
        assertFalse(sut.tokenResult((">", 2), matchesTokenLiteral: "-"))
        assertFalse(sut.tokenResult(("->", 1), matchesTokenLiteral: "->"))
    }
}

// MARK: - Test internals

private typealias Sut = TokenSyntaxInterpreter

private func makeSut(
    _ tokens: [InternalGrammar.TokenDefinition]
) -> Sut {

    Sut(tokenDefinitions: tokens)
}

private func parseTokenDefinitions(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> [InternalGrammar.TokenDefinition] {
    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let tokens = try parser.tokensFile(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return tokens.compactMap { decl in
        guard let decl = decl as? SwiftPEGGrammar.TokenDefinition else {
            return nil
        }

        return InternalGrammar.TokenDefinition.from(decl)
    }
}
