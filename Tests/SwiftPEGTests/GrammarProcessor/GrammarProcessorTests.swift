import XCTest

@testable import SwiftPEG

class GrammarProcessorTests: XCTestCase {
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
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

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
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

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
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _=try sut.process(grammar)

        assertCount(sut.diagnostics, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Unexpected value '"a"' for @anyToken: expected: An identifier: A unique identifier for the any token reference.
            """)
    }

    func testUnreachableRuleDiagnostics() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeNamedItem("rule1") ]),
        ])
        let rule1 = makeRule(name: "rule1", [
            makeAlt([ makeNamedItem("rule2"), makeNamedItem("a") ]),
        ])
        let rule2 = makeRule(name: "rule2", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let rule3 = makeRule(name: "rule3", [
            makeAlt([ makeNamedItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", identifier: "a"),
                makeMeta(name: "token", identifier: "b"),
            ],
            [start, rule1, rule2, rule3]
        )
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        // Act 1

        _ = try sut.process(grammar)

        var diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .unreachableRule(let rule, "start")
                where rule === rule3:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Rule 'rule3' @ 0 is not reachable from the set start rule 'start'.
            """)

        // Act 2

        _ = try sut.process(grammar, entryRuleName: "rule3")

        diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .unreachableRule(let rule, "rule3")
                where rule === start || rule === rule1 || rule === rule2:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 3)
        assertEqual(sut.test_diagnosticMessages(), """
            Rule 'rule3' @ 0 is not reachable from the set start rule 'start'.
            Rule 'rule1' @ 0 is not reachable from the set start rule 'rule3'.
            Rule 'rule2' @ 0 is not reachable from the set start rule 'rule3'.
            Rule 'start' @ 0 is not reachable from the set start rule 'rule3'.
            """)
    }

    func testUnreachableRuleDiagnostics_searchesNestedIdentifiers() throws {
        var makeRef = _RuleGen()
        let grammarString = """
        start:
            | \(makeRef[0])
            | (\(makeRef[1]) | \(makeRef[2]))
            | \(makeRef[3]) ?
            | \(makeRef[4]) *
            | \(makeRef[5]) +
            | \(makeRef[6]).\(makeRef[7])+
            | ! \(makeRef[8])
            | & \(makeRef[9])
            | ~ \(makeRef[10])
            | named=\(makeRef[11])
            ;
        
        \(makeRef.dumpRules())
        """
        let grammar = try parseGrammar(grammarString)
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diagnostics = sut.test_diagnostics(where: { diag in
            switch diag {
            case .unreachableRule:
                return true
            default:
                return false
            }
        })
        assertEmpty(diagnostics, message: "in grammar \(grammarString)")
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
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

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

private func makeSut(_ delegate: GrammarProcessor.Delegate? = nil) -> GrammarProcessor {
    return GrammarProcessor(delegate: delegate)
}

private func makeGrammar(
    metas: [SwiftPEGGrammar.Meta] = [],
    _ rules: [SwiftPEGGrammar.Rule]
) -> SwiftPEGGrammar.Grammar {
    return SwiftPEGGrammar.Grammar(metas: metas, rules: rules)
}

private func stubDelegate() -> TestGrammarProcessorDelegate {
    return TestGrammarProcessorDelegate()
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

private func makeRule(name: String) -> SwiftPEGGrammar.Rule {
    makeRule(name: name, [
        makeAlt([makeNamedItem("-")])
    ])
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

private func makeAtom(group: [SwiftPEGGrammar.Alt]) -> SwiftPEGGrammar.Atom {
    SwiftPEGGrammar.GroupAtom(alts: group)
}

private func makeAtom(ident: String, identity: SwiftPEGGrammar.IdentAtom.Identity = .unresolved) -> SwiftPEGGrammar.IdentAtom {
    SwiftPEGGrammar.IdentAtom(identifier: makeIdent(ident), identity: identity)
}

private func makeIdent(_ ident: String) -> SwiftPEGGrammar.Token {
    .identifier(Substring(ident))
}

private func makeString(_ string: String) -> SwiftPEGGrammar.Token {
    .string(.doubleQuote(Substring(#""\#(string)""#)))
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

private struct _RuleGen {
    var rules: [String] = []

    subscript(index: Int) -> String {
        mutating get {
            let name = "rule\(index)"
            rules.append(name)
            return name
        }
    }

    func dumpRules() -> String {
        rules.map({ "\($0): '-' ;" }).joined(separator: " ")
    }
}
