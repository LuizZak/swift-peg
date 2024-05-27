import XCTest

@testable import SwiftPEG

class GrammarProcessorTests: XCTestCase {
    func testUnreachableRuleDiagnostics() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeItem("rule1") ]),
        ])
        let rule1 = makeRule(name: "rule1", [
            makeAlt([ makeItem("rule2"), makeItem("a") ]),
        ])
        let rule2 = makeRule(name: "rule2", [
            makeAlt([ makeItem("a") ]),
        ])
        let rule3 = makeRule(name: "rule3", [
            makeAlt([ makeItem("a") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", value: "a"),
                makeMeta(name: "token", value: "b"),
            ],
            [start, rule1, rule2, rule3]
        )
        let delegate = stubDelegate()
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        var diags = sut.diagnosticsCount(where: { diag in
            switch diag {
            case .unreachableRule(let rule, "start")
                where rule === rule3:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 1)

        _ = try sut.process(grammar, entryRuleName: "rule3")

        diags = sut.diagnosticsCount(where: { diag in
            switch diag {
            case .unreachableRule(let rule, "rule3")
                where rule === start || rule === rule1 || rule === rule2:
                return true
            default:
                return false
            }
        })
        assertEqual(diags, 3)
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

        let diagnostics = sut.diagnostics(where: { diag in
            switch diag {
            case .unreachableRule:
                return true
            default:
                return false
            }
        })
        assertEmpty(diagnostics, message: "in grammar \(grammarString)")
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

private func makeRule(name: String) -> SwiftPEGGrammar.Rule {
    makeRule(name: name, [
        makeAlt([makeItem("-")])
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
