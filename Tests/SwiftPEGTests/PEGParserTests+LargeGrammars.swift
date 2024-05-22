import XCTest

@testable import SwiftPEG

class PEGParserLargeGrammarsTests: XCTestCase {
    static let grammarLines = """
        1 + 2 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + ((((((11 * 12 * 13 * 14 * 15 + 16 * 17 + 18 * 19 * 20))))))
        2*3 + 4*5*6
        12 + (2 * 3 * 4 * 5 + 6 + 7 * 8)
        """

    func testGrammar_1_copy() throws {
        try runTest(copies: 1)
    }

    func testGrammar_10_copies() throws {
        try runTest(copies: 10)
    }

    func testGrammar_100_copies() throws {
        try runTest(copies: 100)
    }
}

// MARK: - Test internals

private func runTest(
    copies: Int,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let source = PEGParserLargeGrammarsTests.grammarLines
    let linesPerCopy = source.split(separator: "\n").count
    let testSource = makeTestSource(source, copies: copies)
    let rawTokenizer = makeRawTokenizer(testSource)
    let parser = makeSut(rawTokenizer)

    for _ in 0..<(copies * linesPerCopy) {
        do {
            _ = try parser.start()
        } catch {
            throw parser.makeSyntaxError()
        }
    }

    if !parser.tokenizer.isEOF {
        throw parser.makeSyntaxError()
    }
}

private func makeSut<T>(_ raw: T) -> TestGrammarParser<T> where T: TestGrammarRawTokenizer {
    return TestGrammarParser(raw: raw)
}

private func makeRawTokenizer(_ source: String) -> TestGrammarRawTokenizer {
    return .init(source: source)
}

private func makeTestSource(_ source: String, copies: Int) -> String {
    guard copies >= 1 else { return source }

    return String(repeating: source, count: copies)
}
