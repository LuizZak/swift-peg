import XCTest
import Testing

@testable import SwiftPEG

struct GrammarParserTests {
    typealias Sut = GrammarParser<TestGrammarTokenizer>

    @Test
    func stringEscaping() throws {
        let tokenizer = rawTokenizer(#"""
        @meta "a\nb\"" ;
        start: a ;
        """#)
        let sut = makeSut(tokenizer)

        let result = try assertUnwrap(try sut.grammar())

        let meta = try assertUnwrap(result.metas.first)
        let value = try assertCast(meta.value, to: SwiftPEGGrammar.MetaStringValue.self)

        assertEqual(value.string.asStringLiteral(), #""a\nb\"""#)
        assertEqual(value.string.rawContents(), "a\nb\"")
    }

    @Test
    func grammar_emptyGrammar_returnsNil() throws {
        let stubTokenizer = stubTestTokenizer([
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.grammar()

        assertNil(result)
    }

    @Test
    func grammar_metaPropertyOnly_returnsNil() throws {
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
                #"Unexpected end-of-stream but expected: "@" or "IDENTIFIER""#,
                .init(owner: sut.tokenizer, index: 3),
                location: "3",
                expected: [.at, .identifier]
            )
        )
    }

    @Test
    func grammar_singleRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "rule", ":", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 0)
        assertEqual(result.rules.count, 1)
    }

    @Test
    func grammar_metaPropertyAndRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "@", "ident", ";",
            "rule", ":", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 1)
        assertEqual(result.rules.count, 1)
    }

    @Test
    func grammar_twoRule_returnsGrammar() throws {
        let stubTokenizer = stubTestTokenizer([
            "ruleA", ":", "'a'", ";",
            "ruleB", ":", "'b'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.grammar())

        assertEqual(result.metas.count, 0)
        assertEqual(result.rules.count, 2)
    }

    @Test
    func grammar_largeGrammar_returnsGrammar() throws {
        var tokens: [SwiftPEGGrammar.Token] = []
        let tokensToCopy: [SwiftPEGGrammar.Token] = [
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

    @Test
    func rule_barStart_returnsRule() throws {
        let stubTokenizer = stubTestTokenizer([
            "ruleA", ":", "|", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.rule())

        assertEqual(result.alts.count, 1)
    }

    @Test
    func rule_twoAlts_returnsRule() throws {
        let stubTokenizer = stubTestTokenizer([
            "ruleA", ":", "'b'", "|", "'a'", ";",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.rule())

        assertEqual(result.alts.count, 2)
    }

    @Test
    func alt_withAction_emptyAction_returnsNil() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "'b'", "{", "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNil(result.action)
    }

    @Test
    func alt_withAction_filledAction_returnsAlt() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "'b'", "{", ".", ",", "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNotNil(result.action)
        assertNotNil(result.action?.rawAction)
        assertEqual(result.action?.rawAction, ".,")
    }

    @Test
    func alt_withAction_whitespaceIsPreserved() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "{",
                " ", ".", "\t", "[", "\n", ",", "]", "   ",
            "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNotNil(result.action)
        assertNotNil(result.action?.rawAction)
        assertEqual(result.action?.rawAction, " .\t[\n,]   ")
    }

    @Test
    func alt_withFailAction_filledAction_returnsAlt() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "'b'", "!!", "{", ".", ",", "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNotNil(result.failAction)
        assertNotNil(result.failAction?.rawAction)
        assertEqual(result.failAction?.rawAction, ".,")
    }

    @Test
    func alt_withActionAndFailAction_filledAction_returnsAlt() throws {
        let stubTokenizer = stubTestTokenizer([
            "'a'", "'b'", "{", "+", "-", "}", "!!", "{", ".", ",", "}",
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(sut.alt())

        assertNotNil(result.action)
        assertNotNil(result.action?.rawAction)
        assertEqual(result.action?.rawAction, "+-")
        assertNotNil(result.failAction)
        assertNotNil(result.failAction?.rawAction)
        assertEqual(result.failAction?.rawAction, ".,")
    }

    @Test
    func alt_action_consumesWhitespace() throws {
        let tokenizer = rawTokenizer("""
        a { b.c() }
        """)
        let sut = makeSut(tokenizer)

        let result = try assertUnwrap(sut.alt())

        assertEqual(result.action?.rawAction, " b.c() ")
    }

    @Test
    func parseRepetitionMode() throws {
        let tokenizer = rawTokenizer("""
        a* a*< a*> a+ a+< a+>
        """)
        let sut = makeSut(tokenizer)

        let result = try assertUnwrap(sut.namedItems())

        assertEqual((result[0].item as? SwiftPEGGrammar.ZeroOrMoreItem)?.repetitionMode, .standard)
        assertEqual((result[1].item as? SwiftPEGGrammar.ZeroOrMoreItem)?.repetitionMode, .minimal)
        assertEqual((result[2].item as? SwiftPEGGrammar.ZeroOrMoreItem)?.repetitionMode, .maximal)
        assertEqual((result[3].item as? SwiftPEGGrammar.OneOrMoreItem)?.repetitionMode, .standard)
        assertEqual((result[4].item as? SwiftPEGGrammar.OneOrMoreItem)?.repetitionMode, .minimal)
        assertEqual((result[5].item as? SwiftPEGGrammar.OneOrMoreItem)?.repetitionMode, .maximal)
    }
}

// MARK: - Test internals

private func makeSut<Raw: RawTokenizerType>(_ tokenizer: Raw) -> GrammarParser<Raw> {
    return GrammarParser(raw: tokenizer)
}

private func stubTestTokenizer(_ tokens: [GrammarParserToken]) -> TestGrammarTokenizer {
    return TestGrammarTokenizer(tokens: tokens)
}

private func rawTokenizer(_ source: String) -> GrammarRawTokenizer {
    return GrammarRawTokenizer(source: source)
}
