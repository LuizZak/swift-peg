import SwiftPEG

class PerformanceTest {
    var useStringBuffer: Bool = false

    func run() throws {
        let tokensToCopy: [Metagrammar.MetagrammarToken] = [
            "ruleA", ":",
                "|", "'a'",
                "|", "'b'",
                "|", "'c'", "'d'", "'e'",
                "|", "'f'", "'g'", "'h'",
                ";",
        ]
        let copies = 10_000

        if useStringBuffer {
            let tokenString = tokensToCopy.map(\.string).joined(separator: " ")
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

            var tokens: [Metagrammar.MetagrammarToken] = []
            for _ in 0..<copies {
                tokens.append(contentsOf: tokensToCopy)
            }
            let tokenizer = arrayRawTokenizer(tokens)
            let parser = makeParser(tokenizer)

            try runParser(parser)
        }
    }

    func runParser<R: RawTokenizerType>(_ parser: MetagrammarParser<R>) throws {
        let stopwatch = Stopwatch.start()

        guard let result = try parser.grammar(), parser.tokenizer.isEOF else {
            throw parser.makeSyntaxError()
        }

        assert(result.rules.count == 10_000, "result.rules.count == 10_000")

        let duration = stopwatch.stop()
        print("Success! Parsed in \(String(format: "%.2lf", duration))s")
    }
    
    private func makeParser<Raw: RawTokenizerType>(_ tokenizer: Raw) -> MetagrammarParser<Raw> {
        return MetagrammarParser(raw: tokenizer)
    }

    private func stringRawTokenizer(_ source: String) -> MetagrammarRawTokenizer {
        return MetagrammarRawTokenizer(source: source)
    }

    private func arrayRawTokenizer(_ tokens: [Metagrammar.MetagrammarToken]) -> ArrayRawTokenizer<Metagrammar.MetagrammarToken> {
        return ArrayRawTokenizer(tokens: tokens)
    }
}
