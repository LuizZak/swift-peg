import SwiftPEG

class PerformanceTest {
    var useStringBuffer: Bool = false

    func run() throws {
        let tokensToCopy: [String] = [
            "ruleA", "[", "Some", ".", "SwiftType", "<", "Int", ">", "]", ":",
                "|", "'a'",
                "|", "'b'",
                "|", "'c'", "'d'", "'e'",
                "|", "'f'", "'g'", "'h'",
                ";",
        ]
        let copies = 10_000

        if useStringBuffer {
            let tokenString = tokensToCopy.joined(separator: " ")
            var buffer: String = ""
            for _ in 0..<copies {
                buffer.append(tokenString)
            }
            let tokenizer = stringRawTokenizer(buffer)
            let parser = makeParser(tokenizer)

            print("Parsing sample with \(buffer.count) characters...")
            try runParser(parser)
        } else {
            let tokenCount = tokensToCopy.count * copies

            print("Parsing sample with \(tokenCount) tokens...")

            var tokens: [String] = []
            for _ in 0..<copies {
                tokens.append(contentsOf: tokensToCopy)
            }
            let tokenizer = arrayRawTokenizer(tokens)
            let parser = makeParser(tokenizer)

            try runParser(parser)
        }
    }

    func runParser<R: RawTokenizerType>(_ parser: GrammarParser<R>) throws {
        let stopwatch = Stopwatch.start()

        guard let result = try parser.grammar(), try parser.isEOF() else {
            throw parser.makeSyntaxError()
        }

        assert(result.rules.count == 10_000, "result.rules.count == 10_000")

        let duration = stopwatch.stop()
        print("Success! Parsed in \(String(format: "%.2lf", duration))s")
    }

    private func makeParser<Raw: RawTokenizerType>(_ tokenizer: Raw) -> GrammarParser<Raw> {
        return GrammarParser(raw: tokenizer)
    }

    private func stringRawTokenizer(_ source: String) -> GrammarRawTokenizer {
        return GrammarRawTokenizer(source: source)
    }

    private func arrayRawTokenizer(_ tokens: [String]) -> ArrayRawTokenizer<GrammarParserToken> {
        return ArrayRawTokenizer(tokens: tokens.map({ string in
            var stream = StringStream(source: string)
            return .from(stream: &stream)!
        }))
    }

    private func arrayRawTokenizer(_ tokens: [GrammarParserToken]) -> ArrayRawTokenizer<GrammarParserToken> {
        return ArrayRawTokenizer(tokens: tokens)
    }
}
