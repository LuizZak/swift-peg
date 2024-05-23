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

            let codeGen = try GrammarProcessor(grammar, verbose: verbose)

            for diagnostic in codeGen.diagnostics {
                print(diagnostic.description)
            }

            let swiftCodeGen = SwiftCodeGen(grammar: codeGen.generatedGrammar())

            let parser = try swiftCodeGen.generateParser()

            if verbose {
                print("Generated parser code:")
                print("-------------------------------------------------")
            }
            print(parser)
        }
    }
}
