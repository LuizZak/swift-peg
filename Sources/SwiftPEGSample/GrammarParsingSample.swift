import SwiftPEG

class GrammarParsingSample {
    var verbose: Bool = false

    func run() throws {
        try parse()
    }

    func parse() throws {
        let file = SwiftPEG.Resources.metagrammarFile

        let fileString = try String(contentsOf: file, encoding: .utf8)

        let tokenizer = MetagrammarRawTokenizer(source: fileString)
        let parser = MetagrammarParser(raw: tokenizer)

        let stopwatch = Stopwatch.start()

        if verbose {
            print("Parsing metagrammar.gram...")
        }

        guard let grammar = try parser.grammar() else {
            throw parser.makeSyntaxError()
        }

        let duration = stopwatch.stop()
        if verbose {
            print("Success! Parsed in \(String(format: "%.2lf", duration))s")
        }

        if !tokenizer.isEOF {
            let visitor = PrintingNodeVisitor()
            let walker = NodeWalker(visitor: visitor)
            try walker.walk(grammar)

            print("Warning: Tokenizer did not consume entire file!")

            print(parser.makeSyntaxError().description)

            print("Remaining tokens:")

            var tokens: [Metagrammar.MetagrammarToken.TokenString] = []
            while let next = try tokenizer.next() {
                tokens.append(next.token.string)
            }

            print(tokens)
        } else {
            if verbose {
                print("Number of meta-properties: \(grammar.metas.count)")

                for property in grammar.metas {
                    if let value = property.value {
                        print("@\(property.name.string) = \(value.shortDebugDescription)")
                    } else {
                        print("@\(property.name.string)")
                    }
                }

                print("Number of rules: \(grammar.rules.count)")

                for rule in grammar.rules {
                    if let type = rule.name.type {
                        print("\(rule.name.name.string) (of type \(type.name))")
                    } else {
                        print("\(rule.name.name.string)")
                    }
                }
            }

            let processor = try GrammarProcessor(grammar, delegate: self, verbose: verbose)

            for diagnostic in processor.diagnostics {
                print(diagnostic.description)
            }

            let swiftCodeGen = SwiftCodeGen(from: processor)

            let parser = try swiftCodeGen.generateParser()

            if verbose {
                print("Generated parser code:")
                print("-------------------------------------------------")
            }
            print(parser)
        }
    }
}

extension GrammarParsingSample: GrammarProcessor.Delegate {

    func grammarProcessor(
        _ processor: GrammarProcessor,
        loadTokensFileNamed name: String
    ) throws -> String {

        guard let url = SwiftPEG.Resources.resources.url(forResource: name, withExtension: nil) else {
            throw Error.tokensFileNotFound(name)
        }

        return try String(contentsOf: url)
    }

    enum Error: Swift.Error {
        case tokensFileNotFound(String)
    }
}
