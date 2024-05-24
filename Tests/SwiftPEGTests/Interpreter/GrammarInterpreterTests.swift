import XCTest

@testable import SwiftPEG

class GrammarInterpreterTests: XCTestCase {
    func testProduceRule_tokensAndString() throws {
        let source = """
        a , b
        """;
        let grammar = InternalGrammar.Grammar(rules: [
            .init(name: "ruleA", alts: [
                .init(items: ["A", "','", "B"], action: "ruleA_alt0")
            ])
        ])
        let delegate = mockDelegate()
        delegate.addToken("a", named: "A").addToken(",").addToken("b", named: "B")
        delegate.addAlt(action: "ruleA_alt0") { ctx -> Any in
            assertEqual(ctx.valueNames, ["a", nil, "b"])
            assertEqual(ctx.values.count, 3)

            return "Success!"
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce(ruleName: "ruleA"))

        assertEqual(result as? String, "Success!")
    }

    func testProduceRule_multiAlt() throws {
        let source = """
        a , b
        """;
        let grammar = InternalGrammar.Grammar(rules: [
            .init(name: "ruleA", alts: [
                .init(items: ["A", "','", "B", "','", "C"], action: "ruleA_alt0"),
                .init(items: ["A", "','", "B"], action: "ruleA_alt1"),
            ])
        ])
        let delegate = mockDelegate()
        delegate.addToken("a", named: "A").addToken(",").addToken("b", named: "B")
        delegate.addAlt(action: "ruleA_alt1") { ctx -> Any in
            assertEqual(ctx.valueNames, ["a", nil, "b"])
            assertEqual(ctx.values.count, 3)

            return "Success!"
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce(ruleName: "ruleA"))

        assertEqual(result as? String, "Success!")
    }

    func testProduceRule_nestedRules() throws {
        let source = """
        a , b + c
        """;
        let grammar = InternalGrammar.Grammar(rules: [
            .init(name: "ruleA", alts: [
                .init(items: ["A", "','", "ruleB"], action: "ruleA_alt0"),
            ]),
            .init(name: "ruleB", alts: [
                .init(items: ["B", "'+'", "C"], action: "ruleB_alt0"),
            ]),
        ])
        let delegate = mockDelegate()
        delegate
            .addToken("a", named: "A")
            .addToken("b", named: "B")
            .addToken("c", named: "C")
            .addTokens(",", "+")
        delegate.addAlt(action: "ruleA_alt0") { ctx -> Any in
            assertEqual(ctx.valueNames, ["a", nil, "ruleB"])
            assertEqual(ctx.values.count, 3)
            assertEqual(ctx.values[2] as? String, "rubeB result")

            return "Success!"
        }
        delegate.addAlt(action: "ruleB_alt0") { ctx -> Any in
            assertEqual(ctx.valueNames, ["b", nil, "c"])
            assertEqual(ctx.values.count, 3)

            return "rubeB result"
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce(ruleName: "ruleA"))

        assertEqual(result as? String, "Success!")
    }
}

// MARK: - Test internals

private func makeSut(
    _ grammar: InternalGrammar.Grammar,
    _ delegate: TestInterpreterDelegate,
    source: String
) -> GrammarInterpreter {

    return GrammarInterpreter(grammar: grammar, source: source, delegate: delegate)
}

private func mockDelegate() -> TestInterpreterDelegate {
    return TestInterpreterDelegate()
}
