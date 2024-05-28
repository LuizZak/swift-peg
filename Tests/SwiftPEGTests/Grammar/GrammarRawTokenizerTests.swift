import XCTest

@testable import SwiftPEG

class GrammarRawTokenizerTests: XCTestCase {
    func testParseTokens() throws {
        let sut = makeSut(#"""
        "a"  'b' ; 
        $ [ ] abc 123 """
        abc
        """ a ... . .. ....
        """#)

        try assertTokensAndLocations(sut, [
            (.string(.doubleQuote(#""a""#)),             makeLocation(line: 1, column: 1)),
            (.whitespace("  "),                          makeLocation(line: 1, column: 4)),
            (.string(.singleQuote(#"'b'"#)),             makeLocation(line: 1, column: 6)),
            (.whitespace(" "),                           makeLocation(line: 1, column: 9)),
            (.semicolon,                                 makeLocation(line: 1, column: 10)),
            (.whitespace(" \n"),                         makeLocation(line: 1, column: 11)),
            (.dollarSign,                                makeLocation(line: 2, column: 1)),
            (.whitespace(" "),                           makeLocation(line: 2, column: 2)),
            (.leftSquare,                                makeLocation(line: 2, column: 3)),
            (.whitespace(" "),                           makeLocation(line: 2, column: 4)),
            (.rightSquare,                               makeLocation(line: 2, column: 5)),
            (.whitespace(" "),                           makeLocation(line: 2, column: 6)),
            (.identifier("abc"),                         makeLocation(line: 2, column: 7)),
            (.whitespace(" "),                           makeLocation(line: 2, column: 10)),
            (.digits("123"),                             makeLocation(line: 2, column: 11)),
            (.whitespace(" "),                           makeLocation(line: 2, column: 14)),
            (.string(.tripleQuote(#""""\#nabc\#n""""#)), makeLocation(line: 2, column: 15)),
            (.whitespace(" "),                           makeLocation(line: 4, column: 4)),
            (.identifier("a"),                           makeLocation(line: 4, column: 5)),
            (.whitespace(" "),                           makeLocation(line: 4, column: 6)),
            (.ellipsis,                                  makeLocation(line: 4, column: 7)),
            (.whitespace(" "),                           makeLocation(line: 4, column: 10)),
            (.period,                                    makeLocation(line: 4, column: 11)),
            (.whitespace(" "),                           makeLocation(line: 4, column: 12)),
            (.period,                                    makeLocation(line: 4, column: 13)),
            (.period,                                    makeLocation(line: 4, column: 14)),
            (.whitespace(" "),                           makeLocation(line: 4, column: 15)),
            (.ellipsis,                                  makeLocation(line: 4, column: 16)),
            (.period,                                    makeLocation(line: 4, column: 19)),
        ])
    }

    func testParseTokens_onNewlineLocations() throws {
        let sut = makeSut(#"""
        @token a ;
        @token b ;
        """#)

        try assertTokensAndLocations(sut, [
            (.at,                   makeLocation(line: 1, column: 1)),
            (.identifier("token"),  makeLocation(line: 1, column: 2)),
            (.whitespace(" "),      makeLocation(line: 1, column: 7)),
            (.identifier("a"),      makeLocation(line: 1, column: 8)),
            (.whitespace(" "),      makeLocation(line: 1, column: 9)),
            (.semicolon,            makeLocation(line: 1, column: 10)),
            (.whitespace("\n"),     makeLocation(line: 1, column: 11)),
            (.at,                   makeLocation(line: 2, column: 1)),
            (.identifier("token"),  makeLocation(line: 2, column: 2)),
            (.whitespace(" "),      makeLocation(line: 2, column: 7)),
            (.identifier("b"),      makeLocation(line: 2, column: 8)),
            (.whitespace(" "),      makeLocation(line: 2, column: 9)),
            (.semicolon,            makeLocation(line: 2, column: 10)),
        ])
    }

    func testSkipsComments() throws {
        let sut = makeSut(#"""
        # A line comment
        a b c
        d # Another line comment
        e
        """#)

        try assertTokensAndLocations(sut, [
            (.whitespace("\n"),   makeLocation(line: 1, column: 17)),
            (.identifier("a"),    makeLocation(line: 2, column: 1)),
            (.whitespace(" "),    makeLocation(line: 2, column: 2)),
            (.identifier("b"),    makeLocation(line: 2, column: 3)),
            (.whitespace(" "),    makeLocation(line: 2, column: 4)),
            (.identifier("c"),    makeLocation(line: 2, column: 5)),
            (.whitespace("\n"),   makeLocation(line: 2, column: 6)),
            (.identifier("d"),    makeLocation(line: 3, column: 1)),
            (.whitespace(" "),    makeLocation(line: 3, column: 2)),
            (.whitespace("\n"),   makeLocation(line: 3, column: 25)),
            (.identifier("e"),    makeLocation(line: 4, column: 1)),
        ])
    }
}

// MARK: - Test internals

private func makeSut(_ source: String) -> GrammarRawTokenizer {
    GrammarRawTokenizer(source: source)
}

private func makeLocation(line: Int, column: Int) -> FileSourceLocation {
    FileSourceLocation(line: line, column: column)
}

private func assertTokens(
    _ sut: GrammarRawTokenizer,
    _ tokens: [SwiftPEGGrammar.GrammarToken],
    file: StaticString = #file,
    line: UInt = #line
) throws {

    for (i, expected) in tokens.enumerated() {
        guard let next = try sut.next() else {
            fail(
                "Unexpected nil token at index \(i)",
                file: file,
                line: line
            )
            return
        }
        if expected != next.token {
            fail(
                "Mismatched token at index \(i): Expected \(expected) found \(next)",
                file: file,
                line: line
            )
            return
        }
    }
}

private func assertTokensAndLocations(
    _ sut: GrammarRawTokenizer,
    _ values: [(SwiftPEGGrammar.GrammarToken, FileSourceLocation)],
    file: StaticString = #file,
    line: UInt = #line
) throws {

    for (i, expected) in values.enumerated() {
        guard let next = try sut.next() else {
            fail(
                "Unexpected nil token at index \(i)",
                file: file,
                line: line
            )
            return
        }
        if expected != next {
            fail(
                "Mismatched token/location at index \(i): Expected \(expected) found \(next)",
                file: file,
                line: line
            )
            return
        }
    }
}
