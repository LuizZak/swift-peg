import XCTest

@testable import SwiftPEG

class AltReducerTests: XCTestCase {
    func testReduce_staticAlt() throws {
        let alt = try parseAlt("""
        a b 'c'
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(result, alt)
    }

    func testReduce_optionalItem() throws {
        let alt = try parseAlt("""
        a b? (c d)? e
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a e
            """)
        )
    }

    func testReduce_optionalItems() throws {
        let alt = try parseAlt("""
        a b ['c'] d [e [f] g.h+]
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a b d
            """)
        )
    }

    func testReduce_zeroOrMore() throws {
        let alt = try parseAlt("""
        a b* c
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a c
            """)
        )
    }

    func testReduce_gatherSeparator_returnsOneOrMoreNode() throws {
        let alt = try parseAlt("""
        a (b?).c+
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a c+
            """)
        )
    }

    func testReduce_gatherNode_elidesGather() throws {
        let alt = try parseAlt("""
        a b.(c?)+
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a
            """)
        )
    }
}

// MARK: - Test internals

private func makeSut(_ alt: InternalGrammar.Alt) -> AltReducer {
    AltReducer(alt)
}

private func parseAlt(
    _ alt: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> InternalGrammar.Alt {

    let tokenizer = GrammarRawTokenizer(source: alt)
    let parser = GrammarParser(raw: tokenizer)

    guard let alt = try parser.alt() else {
        throw parser.makeSyntaxError()
    }

    return InternalGrammar.Alt.from(alt)
}

private func parseGrammar(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> SwiftPEGGrammar.Grammar {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let grammar = try parser.start() else {
        throw parser.makeSyntaxError()
    }

    return grammar
}
