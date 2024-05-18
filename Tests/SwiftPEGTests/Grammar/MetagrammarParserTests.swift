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
            "@", "ident", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.grammar()

        assertNil(result)
    }

    func testGrammar_singleRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "rule", ":", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 0)
        assertEqual(result.rules.count, 1)
    }

    func testGrammar_metaPropertyAndRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "@", "ident", ";",
            "rule", ":", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 1)
        assertEqual(result.rules.count, 1)
    }

    func testGrammar_twoRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "ruleA", ":", "'a'", ";",
            "ruleB", ":", "'b'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 0)
        assertEqual(result.rules.count, 2)
    }

    func testRule_barStart_returnsRule() throws {
        let stubTokenizer = stubTestTokenizer([
            "ruleA", ":", "|", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.rule())

        assertEqual(result.alts.count, 1)
    }

    func testRule_twoAlts_returnsRule() throws {
        let stubTokenizer = stubTestTokenizer([
            "ruleA", ":", "'b'", "|", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.rule())

        assertEqual(result.alts.count, 2)
    }
}

// MARK: - Test internals

private func makeSut<Raw: RawTokenizerType>(_ tokenizer: Raw) -> MetagrammarParser<Raw> {
    return MetagrammarParser(raw: tokenizer)
}

private func stubTestTokenizer(_ tokens: [Metagrammar.Token]) -> TestRawTokenizer<Metagrammar.Token> {
    return TestRawTokenizer(tokens: tokens)
}
