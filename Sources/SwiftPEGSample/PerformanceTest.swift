import SwiftPEG

enum PerformanceTest {
    static func run() throws {
        var tokens: [Metagrammar.MetagrammarToken] = []
        let tokensToCopy: [Metagrammar.MetagrammarToken] = [
            "ruleA", ":",
                "|", "'a'",
                "|", "'b'",
                "|", "'c'", "'d'", "'e'",
                "|", "'f'", "'g'", "'h'",
                ";",
        ]

        for _ in 0..<10_000 {
            tokens.append(contentsOf: tokensToCopy)
        }
        let tokenizer = arrayRawTokenizer(tokens)
        let parser = makeParser(tokenizer)

        let stopwatch = Stopwatch.start()

        print("Parsing sample with \(tokens.count) tokens...")

        guard let result = try parser.grammar() else {
            throw parser.makeSyntaxError()
        }

        assert(result.rules.count == 10_000, "result.rules.count == 10_000")

        let duration = stopwatch.stop()
        print("Success! Parsed in \(String(format: "%.2lf", duration))s")
    }
    
    private static func makeParser<Raw: RawTokenizerType>(_ tokenizer: Raw) -> MetagrammarParser<Raw> {
        return MetagrammarParser(raw: tokenizer)
    }

    private static func arrayRawTokenizer(_ tokens: [Metagrammar.MetagrammarToken]) -> ArrayRawTokenizer<Metagrammar.MetagrammarToken> {
        return ArrayRawTokenizer(tokens: tokens)
    }
}
