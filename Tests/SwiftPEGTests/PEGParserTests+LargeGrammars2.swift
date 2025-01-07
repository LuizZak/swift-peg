import Testing

@testable import SwiftPEG

struct PEGParser_LargeGrammarsTests2 {
    @Test
    func grammar_1_rules() throws {
        try runTest(rules: 1)
    }

    @Test
    func grammar_10_rules() throws {
        try runTest(rules: 10)
    }

    @Test
    func grammar_100_rules() throws {
        try runTest(rules: 100)
    }

    @Test
    func grammar_1000_rules() throws {
        try runTest(rules: 1000)
    }

#if !DEBUG
    @Test
    func grammar_10_000_rules() throws {
        try runTest(rules: 10_000)
    }
#endif
}

// MARK: - Test internals

private func runTest(
    rules: Int,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let testSource = makeTestSource(rules: rules)
    let rawTokenizer = makeRawTokenizer(testSource)
    let parser = makeSut(rawTokenizer)

    do {
        _ = try parser.start()
    } catch {
        throw parser.makeSyntaxError()
    }

    if try !parser.isEOF() {
        throw parser.makeSyntaxError()
    }
}

private func makeSut<T>(_ raw: T) -> GrammarParser<T> where T: GrammarRawTokenizer {
    return GrammarParser(raw: raw)
}

private func makeRawTokenizer(_ source: String) -> GrammarRawTokenizer {
    return .init(source: source)
}

private func makeTestSource(rules: Int) -> String {
    guard rules >= 1 else { return "start: '_';" }

    var result = "start: rule0 ;\n"

    for ruleIndex in 0..<rules {
        result += "rule\(ruleIndex): 'a' | 'b' | c=('c')+ ;\n"
    }

    return result
}
