import XCTest

@testable import SwiftPEG

class GrammarProcessor_NullabilityTests: XCTestCase {
    func testDetectLeftRecursiveRules_indirect() throws {
        let start = makeRule(name: "start", [
            makeAlt([ makeItem("rule1") ]),
        ])
        let rule1 = makeRule(name: "rule1", [
            makeAlt([ makeItem("a"), makeItem("b") ]),
            makeAlt([ makeItem("rule2"), makeItem("d") ]),
        ])
        let rule2 = makeRule(name: "rule2", [
            makeAlt([ makeItem("rule1"), makeItem("b") ]),
            makeAlt([ makeItem("c"), makeItem("d") ]),
        ])
        let grammar = makeGrammar(
            metas: [
                // Non-rule identifiers must be declared as tokens
                makeMeta(name: "token", value: "a"),
                makeMeta(name: "token", value: "b"),
                makeMeta(name: "token", value: "c"),
                makeMeta(name: "token", value: "d"),
            ],
            [start, rule1, rule2]
        )
        let delegate = stubDelegate()

        let sut = try GrammarProcessor(grammar, delegate: delegate)

        assertEmpty(sut.diagnostics)
    }
}

// MARK: - Test internals

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
        alts: alts
    )
}

private func makeAlt(_ items: [SwiftPEGGrammar.NamedItem]) -> SwiftPEGGrammar.Alt {
    SwiftPEGGrammar.Alt(
        namedItems: items,
        action: nil
    )
}

private func makeItem(_ ident: String, identity: SwiftPEGGrammar.IdentAtom.Identity = .ruleName) -> SwiftPEGGrammar.NamedItem {
    .init(
        name: nil,
        item: SwiftPEGGrammar.AtomItem(
            atom: SwiftPEGGrammar.IdentAtom(
                identifier: makeIdent(ident),
                identity: identity
            )
        ),
        type: nil,
        lookahead: nil
    )
}

private func makeIdent(_ ident: String) -> SwiftPEGGrammar.Token {
    .identifier(Substring(ident))
}
