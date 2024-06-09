import XCTest

@testable import SwiftPEG

class GrammarProcessor_RecursivityTests: XCTestCase {
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
        let sut = makeSut(delegate)

        _ = try sut.process(grammar)

        assertEmpty(sut.diagnostics)
    }

    func testLeftRecursive() throws {
        let grammarString = #"""
        start: rule1 ;

        rule1:
            | rule2
            ;
        rule2:
            | '(' rule1 ')'
            | rule3 '*'
            | rule4 '+'
            ;
        rule3:
            | rule2 '-'
            ; 
        rule4:
            | rule2 '/'
            ;
        """#
        let grammar = try parseGrammar(grammarString)
        let sut = makeSut()

        let result = try sut.process(grammar)

        assertEqual(sut.test_diagnosticMessages(), "")
        assertEqualUnordered(
            result.grammar.rules.map({ ($0.name, $0.isLeftRecursive) }), [
                ("start", false),
                ("rule1", false),
                ("rule2", true),
                ("rule3", true),
                ("rule4", true),
            ], compare: ==
        )
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
