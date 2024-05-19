import SwiftPEG

class GrammarParsingSample {
    func parse() throws {
        let file = SwiftPEG.Resources.metagrammarFile

        let fileString = try String(contentsOf: file, encoding: .utf8)

        let tokenizer = MetagrammarRawTokenizer(source: fileString)
        let parser = MetagrammarParser(raw: tokenizer)

        let stopwatch = Stopwatch.start()

        print("Parsing metagrammar.gram...")

        guard let grammar = try parser.grammar() else {
            throw parser.makeSyntaxError()
        }

        let duration = stopwatch.stop()
        print("Success! Parsed in \(String(format: "%.2lf", duration))s")

        if !tokenizer.isEOF {
            let visitor = PrintingNodeVisitor()
            let walker = NodeWalker(visitor: visitor)
            walker.walk(grammar)

            print("Warning: Tokenizer did not consume entire file!")

            print(parser.makeSyntaxError().description)

            print("Remaining tokens:")

            var tokens: [String] = []
            while let next = try tokenizer.next() {
                tokens.append(next.string)
            }

            print(tokens)
        } else {
            print("Number of meta-properties: \(grammar.metas.count)")

            for property in grammar.metas {
                if let value = property.value {
                    print("@\(property.name.identifier) = \(value.shortDebugDescription)")
                } else {
                    print("@\(property.name.identifier)")
                }
            }

            print("Number of rules: \(grammar.rules.count)")

            for rule in grammar.rules {
                if let type = rule.name.type {
                    print("\(rule.name.name.identifier) (of type \(type.name))")
                } else {
                    print("\(rule.name.name.identifier)")
                }
            }
        }
    }
}

extension GrammarParsingSample {
    static func run() throws {
        let sample = GrammarParsingSample()
        try sample.parse()
    }
}
