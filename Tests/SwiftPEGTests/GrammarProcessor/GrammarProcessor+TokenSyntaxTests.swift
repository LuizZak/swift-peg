import Testing

@testable import SwiftPEG

struct GrammarProcessor_TokenSyntaxTests {
    @Test
    func diagnoseRepeatedTokenName() throws {
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

    @Test
    func diagnoseRepeatedTokenName_fragmentsCountAsTokens() throws {
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

    @Test
    func diagnoseReentrantToken() throws {
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

    @Test
    func mergesSequentialTokenTerminals() throws {
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

    @Test
    func inlineFragments_literalTerminals() throws {
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

    @Test
    func inlineFragments_terminals() throws {
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

    @Test
    func inlineFragments_groups() throws {
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

    @Test
    func inlineFragments_alts() throws {
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

    @Test
    func inlineFragments_alts_withSequentialItems() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $tok: a | b ;
        %a: | b | "c" | d | e;
        $b: | "b" ;
        %d: | "d" "e" ;
        %e: | "f" g? "h" ;
        %g: | h+ ;
        %h: | !"\u{000A}" !"\u{000D}" . ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $tok: | b | "c" | "de" | "f" g? "h" | b;
        $b: | "b" ;
        %g: | (!"\u{000A}" !"\u{000D}" .)+ ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    @Test
    func inlineFragments_alts_bug1() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $whitespace[".whitespace"]:
            | whitespace_item+
            ;

        %whitespace_item:
            | comment
            ;

        %line_break:
            | "\u{000A}"
            | "\u{000D}\u{000A}"
            | "\u{000D}"
            ;

        %comment:
            | '//' comment_text line_break
            ;

        %comment_text:
            | comment_text_item+
            ;

        %comment_text_item:
            | !"\u{000A}" !"\u{000D}" .
            ;
        """#)
        let expected = try parseTokenDefinitions(#"""
        $whitespace[".whitespace"]: (whitespace_item)+ ;
        %whitespace_item : '//' comment_text line_break ;
        %comment_text: (!"\u{a}" !"\u{d}" .)+ ;
        %line_break : "\u{a}" | "\u{d}\u{a}" | "\u{d}" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    @Test
    func inlineFragments_alts_copiesTrailExclusions() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b | "c" | d | f !"j" ;
        $b: "b" ;
        %d: "d" !"d" | "e" !"e" ;
        %f: "f" "g" !"h" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b | "c" | "d" !"d" | "e" !"e" | "fg" !"h" !"j";
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    @Test
    func inlineFragments_repeatedFragments() throws {
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

    @Test
    func inlineFragments_sequentialFragments() throws {
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

    @Test
    func inlineFragments_nestedFragments() throws {
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

    @Test
    func inlineFragments_exclusions() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b !d "c";
        $b: "b" ;
        %d: "d" | "e" | "f"..."h" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b !"d" !"e" !"f"..."h" "c";
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    @Test
    func inlineFragments_exclusions_expandsGroups() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b !d "c";
        $b: "b" ;
        %d: "d" | "e" | "f" ("g") "h";
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b !"d" !"e" !"fgh" "c";
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    @Test
    func inlineFragments_altTrailExclusions() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: b d !e ;
        $b: "b" ;
        %d: d { d.isLetter } ;
        %e: "e"..."g" ;
        """)
        let expected = try parseTokenDefinitions(#"""
        $a: b d { d.isLetter } !"e"..."g" ;
        $b: "b" ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqualUnordered(processed.tokens, expected)
    }

    @Test
    func sortTokens() throws {
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

    @Test
    func sortTokens_staticTerminals_preferPrefixesLast() throws {
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

    @Test
    func tokenOcclusionGraph_noOcclusions() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'abc' ;
        $b: 'b'+ ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEmpty(processed.tokenOcclusionGraph.nodes)
        assertEmpty(processed.tokenOcclusionGraph.edges)
    }

    @Test
    func tokenOcclusionGraph_ignoresFragments_fragmentOccludesToken() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'a' ;
        %b: 'a'+ | 'a'+ ;
        $c: !b 'b' ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertCount(processed.tokens, 3)
        assertEmpty(processed.tokenOcclusionGraph.nodes)
        assertEmpty(processed.tokenOcclusionGraph.edges)
    }

    @Test
    func tokenOcclusionGraph_ignoresFragments_tokenOccludesFragment() throws {
        let delegate = stubDelegate(tokensFile: #"""
        %a: 'a' ;
        $b: 'a'+ | 'a'+ ;
        $c: !b 'b' ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertCount(processed.tokens, 2)
        assertEmpty(processed.tokenOcclusionGraph.nodes)
        assertEmpty(processed.tokenOcclusionGraph.edges)
    }

    @Test
    func tokenOcclusionGraph_occlusion_doesNotCheckAlts() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'a' | 'aa' | 'aaa';
        $b: 'a'+ ;
        $c ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEmpty(processed.tokenOcclusionGraph.nodes)
        assertEmpty(processed.tokenOcclusionGraph.edges)
    }

    @Test
    func tokenOcclusionGraph_partialOcclusion() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'abc' ;
        $b: 'a'+ ;
        $c ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEmpty(processed.tokenOcclusionGraph.nodes)
        assertEmpty(processed.tokenOcclusionGraph.edges)
    }

    @Test
    func tokenOcclusionGraph_occlusion() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: 'a' ;
        $b: 'a'+ ;
        $c ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        assertEqual(processed.tokenOcclusionGraph.nodes, [
            "a", "b",
        ])
        assertEqual(processed.tokenOcclusionGraph.edges, [
            .init(start: "b", end: "a"),
        ])
    }

    @Test
    func diagnostics_unfulfillableAtom() throws {
        let delegate = stubDelegate(tokensFile: #"""
        $a: !'a' 'a' ;
        """#)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        _=try sut.process(grammar)

        assertEqual(sut.test_diagnosticMessages(), """
        Token $a @ line 1 column 1 contains atom '!'a' 'a'' which has a token \
        terminal + exclusion that cannot ever be fulfilled by any input, and \
        will never match.
        """)
    }

#if PERFORMANCE_TESTS

    @Test
    func tokenOcclusionGraph_performance_manyDynamicTokens_manyStaticTokens() throws {
        let dynamicTokenCount = 1000
        let staticTokenCount = 1000

        // Data setup

        var tokensFile = ""
        var expectedNodes: Set<TokenOcclusionGraph.Node> = []
        var expectedEdges: Set<TokenOcclusionGraph.Edge> = []

        var dynamicTokens: [String] = []
        var staticTokens: [String] = []

        for i in 0..<dynamicTokenCount {
            let name = "d\(i)"
            let literal = "a\(i)"

            dynamicTokens.append(name)

            tokensFile += """
            \n$\(name): "\(literal)"+ ;
            """
        }

        for i in 0..<staticTokenCount {
            let name = "s\(i)"
            let literal = "a\(i)"

            staticTokens.append(name)

            tokensFile += """
            \n$\(name): "\(literal)" ;
            """
        }

        for (dynamicToken, staticToken) in zip(dynamicTokens, staticTokens) {
            expectedNodes.insert(dynamicToken)
            expectedNodes.insert(staticToken)
            expectedEdges.insert(.init(start: dynamicToken, end: staticToken))
        }

        // Test execution

        let delegate = stubDelegate(tokensFile: tokensFile)
        let grammar = makeGrammar()
        let sut = makeSut(delegate)

        let processed = try sut.process(grammar)

        // Validations

        assertEqual(processed.tokenOcclusionGraph.nodes, expectedNodes)
        assertEqual(processed.tokenOcclusionGraph.edges, expectedEdges)
    }

#endif // #if PERFORMANCE_TESTS
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
    .init(pieces: [.literal(string)], quote: .doubleQuote, location: 0)
}

private func parseTokens(
    _ tokensFile: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> [SwiftPEGGrammar.TokenFileDeclaration] {

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
        .compactMap { decl in
            guard let decl = decl as? SwiftPEGGrammar.TokenDefinition else {
                return nil
            }

            return InternalGrammar.TokenDefinition.from(decl)
        }
}
