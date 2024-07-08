import XCTest

@testable import SwiftPEG

class TokenDFA_PrefixTests: XCTestCase {
    func testIsPrefix_nonInlined_identity() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'abc' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[0])

        assertTrue(sut.isPrefix(of: token1))
    }

    func testIsPrefix_nonInlined_matchingNodesAndEdges() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'abc' 'c'+ 'd' ;
        $b: 'abc' 'c'+ 'd' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])

        assertTrue(sut.isPrefix(of: token1))
    }

    func testIsPrefix_nonInlined_terminalPrefix() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'ab' ;
        $b: 'abc' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])

        assertTrue(sut.isPrefix(of: token1))
    }

    func testIsPrefix_nonInlined_equivalentTerminals() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'ab' 'cd' ;
        $b: 'abc' 'de' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])

        assertTrue(sut.isPrefix(of: token1))
    }

    func testIsPrefix_nonInlined_branchingPaths() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'? 'b' ;
        $b: 'a' 'b' | 'b' 'c'* 'b' ;
        $c: 'c'? 'b' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])
        let token2 = try makeSut(tokens[2])

        assertTrue(sut.isPrefix(of: token1))
        assertFalse(token2.isPrefix(of: token1))
    }

    func testIsPrefix_nonInlined_zeroOrMoreCenter() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' 'c' ;
        $b: 'a' 'b'* 'c' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])

        assertTrue(sut.isPrefix(of: token1))
        assertTrue(token1.isPrefix(of: sut))
    }

    func testIsPrefix_nonInlined_oneOrMoreCenter() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' 'c' ;
        $b: 'a' 'b'+ 'c' ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])

        assertFalse(sut.isPrefix(of: token1))
        assertFalse(token1.isPrefix(of: sut))
    }

    func testIsPrefix_nonInlined_decimalsAndFloats() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: '0'...'9'+ ;
        $b: '0'...'9'+ '.' '0'...'9'+ ;
        """#)
        let sut = try makeSut(tokens[0])
        let token1 = try makeSut(tokens[1])

        assertTrue(sut.isPrefix(of: token1))
        assertFalse(token1.isPrefix(of: sut))
    }
}

// MARK: - Test internals

private func makeSut(
    _ tokenDefinition: InternalGrammar.TokenDefinition,
    file: StaticString = #file,
    line: UInt = #line
) throws -> TokenDFA {

    try assertUnwrap(TokenDFA.from(tokenDefinition), file: file, line: line)
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
