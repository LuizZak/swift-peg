import Testing

@testable import SwiftPEG

struct GrammarProcessor_AltOrderingTests {
    @Test
    func altOrderDiagnostics() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeItem("rule1") ]),
        ])
        let alt1 = makeAlt([ makeItem("a") ])
        let alt2 = makeAlt([ makeItem("a"), makeItem("b") ])
        let rule1 = makeRule(name: "rule1", [
            alt1,
            alt2,
        ])
        let grammar = makeGrammar(
            metas: [
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", value: "a"),
                makeMeta(name: "token", value: "b"),
            ],
            [start, rule1]
        )
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue(let rule, let prior, let alt)
                where rule === rule1 && prior === alt1 && alt === alt2:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Alt 'a' @ 0 always succeeds before 'a b' @ 0 can be tried in rule rule1 @ 0.
            """)
    }

    @Test
    func altOrderDiagnostics_inspectsNestedAlts() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeItem("rule1") ]),
        ])
        let alt1 = makeAlt([ makeItem("a") ])
        let alt2 = makeAlt([ makeItem("a"), makeItem("b") ])
        let rule1 = makeRule(name: "rule1", [
            makeAlt([
                makeItem(atom: makeAtom(group: [alt1, alt2]))
            ])
        ])
        let grammar = makeGrammar(
            metas: [
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", value: "a"),
                makeMeta(name: "token", value: "b"),
            ],
            [start, rule1]
        )
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue(let rule, let prior, let alt)
                where rule === rule1 && prior === alt1 && alt === alt2:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Alt 'a' @ 0 always succeeds before 'a b' @ 0 can be tried in rule rule1 @ 0.
            """)
    }

    @Test
    func altOrderDiagnostics_inspectsAltsByReduction() throws {
        // Ensure that alts are checked against each other based on their reduced
        // form, i.e. removing any optional production
        let grammar = try parseGrammar("""
        @token a; @token b; @token c; @token d;

        start:
            | a b? c
            | a c d
            ;
        """)
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Alt 'a b? c' (when reduced as 'a c') @ line 4 column 7 always succeeds before 'a c d' @ line 5 column 7 can be tried in rule start @ line 3 column 1.
            """)
    }

    @Test
    func altOrderDiagnostics_detectsNullableEarlyAlt() throws {
        let grammar = try parseGrammar("""
        @token a; @token b; @token c;

        start:
            | a? b*
            | a b b c
            | a b c
            ;
        """)
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 2)
        assertEqual(sut.test_diagnosticMessages(), """
            Alt 'a? b*' (when reduced as '<empty>') @ line 4 column 7 always succeeds before 'a b b c' @ line 5 column 7 can be tried in rule start @ line 3 column 1.
            Alt 'a? b*' (when reduced as '<empty>') @ line 4 column 7 always succeeds before 'a b c' @ line 6 column 7 can be tried in rule start @ line 3 column 1.
            """)
    }

    @Test
    func altOrderDiagnostics_computesPermutations() throws {
        let grammar = try parseGrammar("""
        @token a; @token b; @token c; @token d; @token e;

        start:
            | a? b? c
            | a c d
            | b c e
            | e? c b
            ;
        """)
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 3)
        assertEqual(sut.test_diagnosticMessages(), """
            Alt 'a? b? c' (when reduced as 'c') @ line 4 column 7 always succeeds before 'a c d' @ line 5 column 7 can be tried in rule start @ line 3 column 1.
            Alt 'a? b? c' (when reduced as 'c') @ line 4 column 7 always succeeds before 'b c e' @ line 6 column 7 can be tried in rule start @ line 3 column 1.
            Alt 'a? b? c' (when reduced as 'c') @ line 4 column 7 always succeeds before 'e? c b' (when reduced as 'c b') @ line 7 column 7 can be tried in rule start @ line 3 column 1.
            """)
    }

    @Test
    func altOrderDiagnostics_computesPermutations_ignoresOptionalPrefixInLatterAlts() throws {
        let grammar = try parseGrammar("""
        @token a; @token b; @token c; @token d; @token e;

        start:
            | a
            | a? b
            ;
        """)
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 0)
        assertEqual(sut.test_diagnosticMessages(), "")
    }

    @Test
    func altOrderDiagnostics_stressMaxPermutations() throws {
        let grammar = try parseGrammar("""
        @token a; @token b; @token c; @token d; @token e;

        start:
            | a? a? a? a? a? a? a? a? a? a? a? c
            | b? b? b? b? b? b? b? b? b? b? b? d
            ;
        """)
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .altOrderIssue:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 0)
        assertEqual(sut.test_diagnosticMessages(), "")
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

private func makeMeta(name: String, value: String) -> SwiftPEGGrammar.Meta {
    SwiftPEGGrammar.Meta(
        name: makeIdent(name),
        value: SwiftPEGGrammar.MetaIdentifierValue(identifier: makeIdent(value))
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

private func makeAlt(_ items: [SwiftPEGGrammar.NamedItem]) -> SwiftPEGGrammar.Alt {
    SwiftPEGGrammar.Alt(
        namedItems: items,
        action: nil,
        failAction: nil
    )
}

private func makeItem(_ ident: String, identity: SwiftPEGGrammar.IdentAtom.Identity = .ruleName) -> SwiftPEGGrammar.NamedItem {
    makeItem(
        atom: SwiftPEGGrammar.IdentAtom(
            identifier: makeIdent(ident),
            identity: identity
        )
    )
}

private func makeItem(atom: SwiftPEGGrammar.Atom) -> SwiftPEGGrammar.NamedItem {
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

private func makeIdent(_ ident: String) -> SwiftPEGGrammar.Token {
    .identifier(Substring(ident))
}
