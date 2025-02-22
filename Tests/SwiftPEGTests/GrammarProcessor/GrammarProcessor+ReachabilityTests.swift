import Testing

@testable import SwiftPEG

struct GrammarProcessor_ReachabilityTests {
    @Test
    func unreachableRuleDiagnostics() throws {
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
        let sut = makeSut()

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
            case .unreachableRule(let rule, "start")
                where rule === rule3:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 2)
        assertEqual(sut.test_diagnosticMessages(), """
            Rule 'rule3' @ 0 is not reachable from the set start rule 'start'.
            Rule 'start' @ 0 is not reachable from the set start rule 'rule3'.
            """)
    }

    @Test
    func unreachableRuleDiagnostics_searchesNestedIdentifiers() throws {
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
            | && \(makeRef[10])
            | ~ \(makeRef[11])
            | named=\(makeRef[12])
            ;
        
        \(makeRef.dumpRules())
        """
        let grammar = try parseGrammar(grammarString)
        let sut = makeSut()

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

    @Test
    func unreachableRuleDiagnostics_recursiveRules() throws {
        let grammarString = """
        @token a; @token b; @token c; @token d;

        start: rule1 ;

        rule1: a b | rule2 d ;
        rule2: rule1 b | c d ;
        """
        let grammar = try parseGrammar(grammarString)
        let sut = makeSut()

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

    @Test
    func unreachableRuleDiagnostics_sharedDependency() throws {
        let grammarString = #"""
        @token a; @token b; @token c; @token d;

        start: rule1 ;

        rule1: rule2 ;
        rule2: a;

        rule3: rule4;
        rule4: rule2;
        """#
        let grammar = try parseGrammar(grammarString)
        let sut = makeSut()

        _ = try sut.process(grammar)

        let diags = sut.test_diagnosticsCount(where: { diag in
            switch diag {
            case .unreachableRule:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 1)
        assertEqual(sut.test_diagnosticMessages(), """
            Rule 'rule3' @ line 8 column 1 is not reachable from the set start rule 'start'.
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

private func makeMeta(name: String, identifier: String) -> SwiftPEGGrammar.Meta {
    SwiftPEGGrammar.Meta(
        name: makeIdent(name),
        value: SwiftPEGGrammar.MetaIdentifierValue(identifier: makeIdent(identifier))
    )
}

private func makeRule(name: String, _ alts: [SwiftPEGGrammar.Alt]) -> SwiftPEGGrammar.Rule {
    SwiftPEGGrammar.Rule(
        name: .init(
            name: makeIdent(name),
            type: nil
        ),
        parameters: nil,
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

private func makeNamedItem(_ ident: String, identity: SwiftPEGGrammar.IdentAtom.Identity = .ruleName) -> SwiftPEGGrammar.NamedItem {
    makeNamedItem(
        atom: SwiftPEGGrammar.IdentAtom(
            identifier: makeIdent(ident),
            parameters: nil,
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

private func makeIdent(_ ident: String) -> SwiftPEGGrammar.Token {
    .identifier(Substring(ident))
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
