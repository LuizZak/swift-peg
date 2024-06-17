import XCTest

@testable import SwiftPEG

class GrammarInterpreterTests: XCTestCase {
    func testProduceRule_tokensAndString() throws {
        let source = """
        a , b
        """;
        let grammar = InternalGrammar.Grammar(rules: [
            .init(name: "ruleA", alts: [
                .init(namedItems: ["A", "','", "B"], action: "ruleA_alt0"),
            ]),
        ])
        let delegate = mockDelegate()
        delegate
            .addToken("a", named: "A")
            .addToken("b", named: "B")
            .addToken(",")
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
                .init(namedItems: ["A", "','", "B", "','", "C"], action: "ruleA_alt0"),
                .init(namedItems: ["A", "','", "B"], action: "ruleA_alt1"),
            ]),
        ])
        let delegate = mockDelegate()
        delegate
            .addToken("a", named: "A")
            .addToken("b", named: "B")
            .addToken(",")
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
                .init(namedItems: ["A", "','", "ruleB"], action: "ruleA_alt0"),
            ]),
            .init(name: "ruleB", alts: [
                .init(namedItems: ["B", "'+'", "C"], action: "ruleB_alt0"),
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

    func testProduceRule_isCached() throws {
        let source = """
        b
        """;
        let ruleBActionName = "ruleBAction"
        let grammar = InternalGrammar.Grammar(rules: [
            .init(name: "ruleA", alts: [
                .init(namedItems: ["ruleB", "','", "b"], action: "ruleA_alt0"),
                .init(namedItems: ["ruleB"], action: "ruleA_alt1"),
            ]),
            .init(name: "ruleB", alts: [
                .init(namedItems: ["B"], action: .init(string: ruleBActionName)),
            ]),
        ])
        let delegate = mockDelegate()
        delegate
            .addToken("b", named: "B")
            .addToken(",")
        delegate.addAlt(action: "ruleA_alt0") { ctx -> Any in
            return fail("Did not expect alt to match")
        }
        delegate.addAlt(action: "ruleA_alt1") { ctx -> Any in
            assertEqual(ctx.valueNames, ["ruleB"])
            assertEqual(ctx.values.count, 1)
            assertEqual(ctx.values[0] as? String, "Rule B")

            return "Success!"
        }
        delegate.addAlt(action: .init(stringLiteral: ruleBActionName)) { ctx -> Any in
            return "Rule B"
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce(ruleName: "ruleA"))

        assertEqual(result as? String, "Success!")
        assertEqual(delegate.countOfAlts(actionName: ruleBActionName), 1)
    }

    func testProduceRule_recursiveRule() throws {
        let source = """
        a , a , a
        """;
        let grammar = InternalGrammar.Grammar(rules: [
            .init(name: "ruleA", alts: [
                .init(namedItems: ["ruleA", "','", "A"], action: "ruleA_alt0"),
                .init(namedItems: ["A"], action: "ruleA_alt1"),
            ]).with(\.isLeftRecursive, value: true).with(\.isLeftRecursiveLeader, value: true),
        ])
        let delegate = mockDelegate()
        delegate
            .addToken("a", named: "A")
            .addToken(",")
        delegate.addAlt(action: "ruleA_alt0") { ctx -> Any in
            return ctx.values.map({ "\($0)" }).joined(separator: " ")
        }
        delegate.addAlt(action: "ruleA_alt1") { ctx -> Any in
            return "a"
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce(ruleName: "ruleA"))

        assertEqual(sut.recursionCount, 4)
        assertEqual(result as? String, "a , a , a")
    }

    func testProduceRule_nonStandardRepetition() throws {
        let source = """
        a, b, c, and d
        """
        let grammar = try parseInternalGrammar("""
        @token IDENT ;

        start: oxford_list ;
        oxford_list:
            | item (',' item)*> ',' and item { result }
            ;
        item:
            | IDENT {item}
            ;
        and:
            | IDENT {and}
            ;
        """)
        let delegate = mockDelegate()
        delegate
            .addToken("and")
            .addToken(StringMatcher.prefix(.regex(pattern: #"\w+"#)), named: "IDENT")
            .addToken(",")
        delegate.addAlt(action: .exact("item")) { ctx -> Any? in
            return ctx.values.joinedAsString()
        }
        delegate.addAlt(action: .exact("and")) { ctx -> Any? in
            let string = ctx.values.joinedAsString()
            return string == "and" ? "and" : nil
        }
        delegate.addAlt(action: .contains("',' item")) { ctx -> Any? in
            return ctx.values.joinedAsString(separator: " ")
        }
        delegate.addAlt(action: .contains("oxford_list")) { ctx -> Any? in
            return ctx.values.joinedAsString()
        }
        delegate.addAlt(action: .contains("result")) { ctx -> Any? in
            guard let value0 = ctx.values[0] as? String else {
                return nil
            }
            guard let value1 = ctx.values[1] as? [String] else {
                return nil
            }

            return ([value0] + value1).joined() + ctx.values.dropFirst(2).map({ "\($0)" }).joined(separator: " ")
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce())

        assertEqual(result as? String, "a, b, c, and d")
    }

    func testProduceRule_nonStandardGather() throws {
        let source = """
        a, b, c, and d
        """
        let grammar = try parseInternalGrammar("""
        @token IDENT ;

        start: oxford_list ;
        oxford_list:
            | ','.item+> ',' and item { result }
            ;
        item:
            | IDENT {item}
            ;
        and:
            | IDENT {and}
            ;
        """)
        let delegate = mockDelegate()
        delegate
            .addToken("and")
            .addToken(StringMatcher.prefix(.regex(pattern: #"\w+"#)), named: "IDENT")
            .addToken(",")
        delegate.addAlt(action: .exact("item")) { ctx -> Any? in
            return ctx.values.joinedAsString()
        }
        delegate.addAlt(action: .exact("and")) { ctx -> Any? in
            let string = ctx.values.joinedAsString()
            return string == "and" ? "and" : nil
        }
        delegate.addAlt(action: .contains("oxford_list")) { ctx -> Any? in
            return ctx.values.joinedAsString()
        }
        delegate.addAlt(action: .contains("result")) { ctx -> Any? in
            guard let value0 = ctx.values[0] as? [String] else {
                return nil
            }

            return value0.joined(separator: ", ") + ctx.values.dropFirst().map({ "\($0)" }).joined(separator: " ")
        }
        let sut = makeSut(grammar, delegate, source: source)

        let result = try assertUnwrap(sut.produce())

        assertEqual(result as? String, "a, b, c, and d")
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

private func parseInternalGrammar(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> InternalGrammar.Grammar {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let grammar = try parser.start(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    let processor = GrammarProcessor(delegate: nil)
    return try processor.process(grammar).grammar
}

private extension Sequence where Element == Any {
    func joinedAsString(separator: String = "") -> String {
        map { "\($0)" }.joined(separator: separator)
    }
}
