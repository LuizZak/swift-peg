import XCTest

@testable import SwiftPEG

class GrammarProcessorTests: XCTestCase {
    func testUnknownReferenceError() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [],
            [start]
        )
        let sut = makeSut()

        assertThrows({ try sut.process(grammar) })

        assertCount(sut.errors, 1)
        assertEqual(sut.test_errorMessages(), """
            Reference to unknown identifier 'a' @ 0 in rule 'start'. \
            Did you forget to forward-declare a token with '@token a;' or define \
            it in '@tokensFile "<file.tokens>"'?
            """)
    }

    func testFragmentReferenceInParserError() throws {
        let delegate = stubDelegate(tokensFile: """
        %a: 'a' ;
        """)
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                makeMeta(name: "tokensFile", string: "")
            ],
            [start]
        )
        let sut = makeSut(delegate)

        assertThrows({ try sut.process(grammar) })

        assertCount(sut.errors, 1)
        assertEqual(sut.test_errorMessages(), """
            Reference to token fragment 'a' @ 0 found in parser in rule 'start'. \
            Token fragments cannot be referred by the parser, and can only be used as part of definition of tokens.
            """)
    }

    func testFragmentSpecifiesStaticTokenDiagnostic() throws {
        let delegate = stubDelegate(tokensFile: """
        $a: 'a' ;
        %b[".b"]: 'b' ;
        """)
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                makeMeta(name: "tokensFile", string: "")
            ],
            [start]
        )
        let sut = makeSut(delegate)

        _=try assertNoThrow({ try sut.process(grammar) })

        assertCount(sut.diagnostics, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Token fragment %b @ line 2 column 1 specifies a static token value, which is not relevant for fragments and will be ignored.
            """)
    }

    func testAnyToken() throws {
        let atom = makeAtom(ident: "ANY", identity: .unresolved)
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem(atom: atom) ]),
        ])
        let grammar = makeGrammar(
            metas: [
                makeMeta(name: "anyToken", identifier: "ANY"),
            ],
            [start]
        )
        let sut = makeSut()

        let result = try sut.process(grammar)

        assertEmpty(sut.diagnostics)
        assertEqual(atom.identity, .anyToken)
        switch result.grammar.rules[0].alts[0].items[0] {
        case .item(_, .atom(.anyToken), _):
            success()
        default:
            fail("Expected atom to be identified as InternalGrammar.Atom.anyToken, found \(result.grammar.rules[0].alts[0].items[0])")
        }
    }

    func testAnyToken_noValue_diagnostics() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                makeMeta(name: "anyToken"),
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", identifier: "a"),
            ],
            [start]
        )
        let sut = makeSut()

        _=try sut.process(grammar)

        assertCount(sut.diagnostics, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Unexpected value '<empty>' for @anyToken: expected: An identifier: A unique identifier for the any token reference.
            """)
    }

    func testAnyToken_stringValue_diagnostics() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                makeMeta(name: "anyToken", string: "a"),
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", identifier: "a"),
            ],
            [start]
        )
        let sut = makeSut()

        _=try sut.process(grammar)

        assertCount(sut.diagnostics, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Unexpected value '"a"' for @anyToken: expected: An identifier: A unique identifier for the any token reference.
            """)
    }

    func testRedefinedTokenDiagnostics() throws {
        let grammarString = """
        @token a ;
        @token b ;

        @token a ;
        @token a ;
        @token b ;

        start: a ;
        """
        let grammar = try parseGrammar(grammarString)
        let sut = makeSut()

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .metaPropertyDiagnostic:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 3)
        assertEqual(sut.test_diagnosticMessages(), """
            @token with value 'a' @ line 4 column 1 has been declared at least once @ line 1 column 1.
            @token with value 'a' @ line 5 column 1 has been declared at least once @ line 1 column 1.
            @token with value 'b' @ line 6 column 1 has been declared at least once @ line 2 column 1.
            """)
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

private func makeSut(_ delegate: GrammarProcessor.Delegate? = nil) -> GrammarProcessor {
    return GrammarProcessor(delegate: delegate)
}

private func makeGrammar(
    metas: [SwiftPEGGrammar.Meta] = [],
    _ rules: [SwiftPEGGrammar.Rule]
) -> SwiftPEGGrammar.Grammar {
    return SwiftPEGGrammar.Grammar(metas: metas, rules: rules)
}

private func makeMeta(name: String, identifier: String) -> SwiftPEGGrammar.Meta {
    SwiftPEGGrammar.Meta(
        name: makeIdent(name),
        value: SwiftPEGGrammar.MetaIdentifierValue(identifier: makeIdent(identifier))
    )
}

private func makeMeta(name: String, string: String) -> SwiftPEGGrammar.Meta {
    SwiftPEGGrammar.Meta(
        name: makeIdent(name),
        value: SwiftPEGGrammar.MetaStringValue(string: makeString(string))
    )
}

private func makeMeta(name: String) -> SwiftPEGGrammar.Meta {
    SwiftPEGGrammar.Meta(name: makeIdent(name), value: nil)
}

private func makeRule(name: String, _ alts: [SwiftPEGGrammar.Alt]) -> SwiftPEGGrammar.Rule {
    SwiftPEGGrammar.Rule(
        name: .init(
            name: makeIdent(name),
            type: nil
        ),
        alts: alts
    )
}

private func makeAlt(_ items: [SwiftPEGGrammar.NamedItem]) -> SwiftPEGGrammar.Alt {
    SwiftPEGGrammar.Alt(
        namedItems: items,
        action: nil,
        failAction: nil
    )
}

private func makeNamedItem(_ ident: String, identity: SwiftPEGGrammar.IdentAtom.Identity = .ruleName) -> SwiftPEGGrammar.NamedItem {
    makeNamedItem(
        atom: SwiftPEGGrammar.IdentAtom(
            identifier: makeIdent(ident),
            identity: identity
        )
    )
}

private func makeNamedItem(atom: SwiftPEGGrammar.Atom) -> SwiftPEGGrammar.NamedItem {
    .init(
        name: nil,
        item: SwiftPEGGrammar.AtomItem(
            atom: atom
        ),
        type: nil,
        lookahead: nil
    )
}

private func makeAtom(ident: String, identity: SwiftPEGGrammar.IdentAtom.Identity = .unresolved) -> SwiftPEGGrammar.IdentAtom {
    SwiftPEGGrammar.IdentAtom(identifier: makeIdent(ident), identity: identity)
}

private func makeIdent(_ ident: String) -> SwiftPEGGrammar.Token {
    .identifier(Substring(ident))
}

private func makeString(_ string: String) -> SwiftPEGGrammar.GrammarString {
    .init(pieces: [.literal(string)], quote: .doubleQuote)
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
