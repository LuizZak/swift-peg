import XCTest

@testable import SwiftPEG

class MetagrammarParserTests: XCTestCase {
    func testGrammar_emptyGrammar_returnsNil() throws {
        let stubTokenizer = stubTestTokenizer([
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.grammar()

        assertNil(result)
    }

    func testGrammar_metaPropertyOnly_returnsNil() throws {
        let stubTokenizer = stubTestTokenizer([
            "@", "ident",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.grammar()

        assertNil(result)
    }

    func testGrammar_singleRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "rule", ":", "'a'", ";"
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 0)
        assertEqual(result.rules.count, 1)
    }
}

// MARK: - Test internals

private func makeSut<Raw: RawTokenizerType>(_ tokenizer: Raw) -> MetagrammarParser<Raw> {
    return MetagrammarParser(raw: tokenizer)
}

private func stubTestTokenizer(_ tokens: [Metagrammar.Token]) -> TestRawTokenizer<Metagrammar.Token> {
    return TestRawTokenizer(tokens: tokens)
}
