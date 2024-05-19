import XCTest

@testable import SwiftPEG

class MetagrammarParserTests: XCTestCase {
    typealias Sut = MetagrammarParser<TestRawTokenizer<Metagrammar.MetagrammarToken>>

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

        let error: Sut.SyntaxError = try assertCast(sut.makeSyntaxError())
        assertNil(result)
        assertEqual(
            error,
            Sut.SyntaxError.unexpectedEof(
                #"Syntax error @ 3: Unexpected end-of-stream"#,
                .init(owner: sut.tokenizer, index: 3)
            )
        )
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

    func testGrammar_largeGrammar_returnsGrammar() throws {
        var tokens: [Metagrammar.MetagrammarToken] = []
        let tokensToCopy: [Metagrammar.MetagrammarToken] = [
            "ruleA", ":",
                "|", "'a'",
                "|", "'b'",
                "|", "'c'", "'d'", "'e'",
                "|", "'f'", "'g'", "'h'",
                ";",
        ]
        for _ in 0..<500 {
            tokens.append(contentsOf: tokensToCopy)
        }
        let stubTokenizer = stubTestTokenizer(tokens)
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.rules.count, 500)
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

    func testAlt_withAction_emptyAction_returnsAlt() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "'b'", "{", "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNotNil(result.action)
        assertNil(result.action?.balancedTokens)
    }

    func testAlt_withAction_filledAction_returnsAlt() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "'b'", "{", ".", ",", "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNotNil(result.action)
        assertNotNil(result.action?.balancedTokens)
        assertEqual(result.action?.balancedTokens?.tokens, [
            ".", ",",
        ])
    }
}

// MARK: - Test internals

private func makeSut<Raw: RawTokenizerType>(_ tokenizer: Raw) -> MetagrammarParser<Raw> {
    return MetagrammarParser(raw: tokenizer)
}

private func stubTestTokenizer(_ tokens: [Metagrammar.MetagrammarToken]) -> TestRawTokenizer<Metagrammar.MetagrammarToken> {
    return TestRawTokenizer(tokens: tokens)
}
