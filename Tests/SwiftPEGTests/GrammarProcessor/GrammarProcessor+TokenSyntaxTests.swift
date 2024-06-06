import XCTest

@testable import SwiftPEG

class GrammarProcessor_TokenSyntaxTests: XCTestCase {
    func testDiagnoseRepeatedTokenName() throws {
        let delegate = stubDelegate(tokensFile: """
        $token1 ;
        $token2: 'a' ;
        $token1: 'b' ;
        """)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let error = try assertUnwrap(assertThrows(errorType: GrammarProcessor.GrammarProcessorError.self) {
            try sut.process(grammar)
        })

        assertEqual(error.description, """
        Token 'token1' re-declared @ line 3 column 1. Original declaration @ line 1 column 1.
        """)
    }

    func testDiagnoseRepeatedTokenName_fragmentsCountAsTokens() throws {
        let delegate = stubDelegate(tokensFile: """
        $token1 ;
        %token2: 'a' ;
        %token1: 'b' ;
        """)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let error = try assertUnwrap(assertThrows(errorType: GrammarProcessor.GrammarProcessorError.self) {
            try sut.process(grammar)
        })

        assertEqual(error.description, """
        Token 'token1' re-declared @ line 3 column 1. Original declaration @ line 1 column 1.
        """)
    }

    func testDiagnoseReentrantToken() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b 'c' d ;
        $b: 'b' ;
        $d: 'd' a ;
        """)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let error = try assertUnwrap(assertThrows(errorType: GrammarProcessor.GrammarProcessorError.self) {
            try sut.process(grammar)
        })

        assertEqualsOneOf(
            error.description,
            """
            Recursivity in token definitions is not supported; recursive cycle: d -> a -> d starting @ line 3 column 1
            """,
            """
            Recursivity in token definitions is not supported; recursive cycle: a -> d -> a starting @ line 1 column 1
            """
        )
    }

    func testMergesSequentialTokenTerminals() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: 'a' 'b' | 'c'+ 'd' 'e' | 'f' 'g'+ 'h';
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: 'ab' | 'c'+ 'de' | 'f' 'g'+ 'h';
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_literalTerminals() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b "c" !d e;
        $b: "b" ;
        %d: "d" ;
        %e: "e" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b "c" !"d" "e";
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_terminals() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b d e ;
        $b: "b" ;
        %d: d { d.isLetter } ;
        %e: "e"..."g" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b d { d.isLetter } "e"..."g" ;
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_groups() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b ("c" | d);
        $b: "b" ;
        %d: "d" | "e";
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b ("c" | "d" | "e");
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_alts() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b | "c" | d | f;
        $b: "b" ;
        %d: "d" | "e" ;
        %f: "f" "g" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b | "c" | "d" | "e" | "fg";
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_repeatedFragments() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $STRING[".string"]:
            | tripleQuote ('\\"""' | backslashEscape | !tripleQuote .)* tripleQuote
            | doubleQuote ('\\"' | backslashEscape | !doubleQuote !'\n' .)* doubleQuote
            | singleQuote ("\\'" | backslashEscape | !singleQuote !'\n' .)* singleQuote
            ;

        %tripleQuote: '"""' ;
        %doubleQuote: '"' ;
        %singleQuote: "'" ;
        %backslashEscape: '\\\\' | '\\' ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $STRING[".string"]:
            | '"""' ('\\"""' | '\\\\' | '\\' | !'"""' .)* '"""'
            | '"' ('\\"' | '\\\\' | '\\' | !'"' !'\n' .)* '"'
            | "'" ("\\'" | '\\\\' | '\\' | !"'" !'\n' .)* "'"
            ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_sequentialFragments() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: b c ;
        %b: 'b' ;
        %c: 'c' ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $a: 'bc' ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_nestedFragments() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: b ;
        %b: 'b' c ;
        %c: 'c' ;
        $d: b c b ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $a: 'bc' ;
        $d: 'bccbc' ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testInlineFragments_altTrails() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b d !e ;
        $b: "b" ;
        %d: d { d.isLetter } ;
        %e: "e"..."g" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b d { d.isLetter } !e ;
        $b: "b" ;
        %e: "e"..."g" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    func testSortTokens() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'a'+ ;
        $b: 'b' ;
        $c: 'c'+ ;
        $d: 'd' ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $b: 'b' ;
        $d: 'd' ;
        $a: 'a'+ ;
        $c: 'c'+ ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqual(processed.tokens, expected)
    }

    func testSortTokens_staticTerminals_preferPrefixesLast() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'abc'+ ;
        $b: 'a' ;
        $c: 'abcd'+ ;
        $d: 'ab' ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $d: 'ab' ;
        $b: 'a' ;
        $a: 'abc'+ ;
        $c: 'abcd'+ ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqual(processed.tokens, expected)
    }
}

// MARK: - Test internals

private func stubDelegate(tokensFile: String) -> TestGrammarProcessorDelegate {
    let delegate = TestGrammarProcessorDelegate()
    delegate.grammarProcessor_loadTokensFileNamed_stub = { (_, _, _) in
        return tokensFile
    }

    return delegate
}

private func makeSut(_ delegate: GrammarProcessor.Delegate) -> GrammarProcessor {
    return GrammarProcessor(delegate: delegate)
}

private func makeGrammar() -> SwiftPEGGrammar.Grammar {
    return SwiftPEGGrammar.Grammar(metas: [
        makeMeta(name: "tokensFile", string: "file.tokens")
    ], rules: [
        makeRule(name: "start", [])
    ])
}

private func makeMeta(name: String, string: String) -> SwiftPEGGrammar.Meta {
    SwiftPEGGrammar.Meta(
        name: makeIdent(name),
        value: SwiftPEGGrammar.MetaStringValue(string: makeString(string))
    )
}

private func makeRule(name: String, _ alts: [SwiftPEGGrammar.Alt]) -> SwiftPEGGrammar.Rule {
    SwiftPEGGrammar.Rule(
        name: .init(
            name: makeIdent(name),
            type: nil
        ),
        action: nil,
        failAction: nil,
        alts: alts
    )
}

private func makeIdent(_ ident: String) -> SwiftPEGGrammar.Token {
    .identifier(Substring(ident))
}

private func makeString(_ string: String) -> SwiftPEGGrammar.GrammarString {
    .init(pieces: [.literal(string)], quote: .doubleQuote)
}

private func parseTokens(
    _ tokensFile: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> [SwiftPEGGrammar.TokenDefinition] {

    let tokenizer = GrammarRawTokenizer(source: tokensFile)
    let parser = GrammarParser(raw: tokenizer)

    guard let result = try parser.tokensFile(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return result
}

private func parseTokenDefinitions(
    _ tokensFile: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> [InternalGrammar.TokenDefinition] {

    return
        try parseTokens(tokensFile, file: file, line: line)
        .map(InternalGrammar.TokenDefinition.from)
}
