import XCTest

@testable import SwiftPEG

class CodeGen_NullabilityTests: XCTestCase {
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
        let grammar = makeGrammar([start, rule1, rule2])

        let sut = try CodeGen(grammar)

        assertEmpty(sut.diagnostics)
    }
}

// MARK: - Test internals

private func makeGrammar(_ rules: [Metagrammar.Rule]) -> Metagrammar.Grammar {
    return Metagrammar.Grammar(metas: [], rules: rules)
}

private func makeRule(name: String, _ alts: [Metagrammar.Alt]) -> Metagrammar.Rule {
    Metagrammar.Rule(
        name: .init(
            name: .init(token: name, location: 0),
            type: nil
        ),
        alts: alts
    )
}

private func makeAlt(_ items: [Metagrammar.NamedItem]) -> Metagrammar.Alt {
    Metagrammar.Alt(
        namedItems: items,
        action: nil
    )
}

private func makeItem(_ ident: String) -> Metagrammar.NamedItem {
    .init(
        name: nil,
        item: Metagrammar.AtomItem(
            atom: Metagrammar.IdentAtom(identifier: makeIdent(ident))
        ),
        type: nil,
        lookahead: nil
    )
}

private func makeIdent(_ ident: String) -> Metagrammar.IdentifierToken {
    .init(token: ident, location: 0)
}
