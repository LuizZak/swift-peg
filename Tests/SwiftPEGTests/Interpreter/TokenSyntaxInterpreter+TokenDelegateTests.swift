import XCTest

@testable import SwiftPEG

class TokenSyntaxInterpreter_TokenDelegateTests: XCTestCase {
    func testProduceToken_parseByLength() throws {
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

    func testProduceToken_ignoreFragments() throws {
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

    func testTokenResult_matchesTokenName_correctMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertTrue(sut.tokenResult(("-", 1), matchesTokenName: "minus"))
        assertTrue(sut.tokenResult(("->", 2), matchesTokenName: "arrow"))
    }

    func testTokenResult_matchesTokenName_incorrectMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertFalse(sut.tokenResult(("-", 2), matchesTokenName: "minus"))
        assertFalse(sut.tokenResult((">", 2), matchesTokenName: "minus"))
        assertFalse(sut.tokenResult(("->", 1), matchesTokenName: "arrow"))
    }

    func testTokenResult_matchesTokenLiteral_correctMatches() throws {
        let tokens = try parseTokenDefinitions(#"""
        $minus: '-' ;
        $arrow: '->' ;
        """#)
        let sut = makeSut(tokens)

        assertTrue(sut.tokenResult(("-", 1), matchesTokenLiteral: "-"))
        assertTrue(sut.tokenResult(("->", 2), matchesTokenLiteral: "->"))
    }

    func testTokenResult_matchesTokenLiteral_incorrectMatches() throws {
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

    return tokens.map(InternalGrammar.TokenDefinition.from)
}
