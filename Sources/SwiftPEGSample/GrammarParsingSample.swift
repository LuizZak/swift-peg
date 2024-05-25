import Foundation
import SwiftPEG

private let _hardcodedPath = #file

class GrammarParsingSample {
    var verbose: Bool = false
    var useBuiltInFiles: Bool = false

    func run() throws {
        try parse()
    }

    func resolveFileName(_ name: String) -> URL {
        if useBuiltInFiles {
            guard let url = SwiftPEG.Resources.resources.url(forResource: name, withExtension: nil) else {
                fatalError("Could not find file \(name)!")
            }

            return url
        } else {
            let current = URL(fileURLWithPath: _hardcodedPath)

            return current
                .deletingLastPathComponent()
                .deletingLastPathComponent()
                .appendingPathComponent("SwiftPEG")
                .appendingPathComponent("Grammar")
                .appendingPathComponent(name)
        }
    }

    func parse() throws {
        let file = resolveFileName("metagrammar.gram")

        let fileString = try String(contentsOf: file, encoding: .utf8)

        let tokenizer = GrammarRawTokenizer(source: fileString)
        let parser = GrammarParser(raw: tokenizer)

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

            print("Warning: Tokenizer did not consume entire file!", to: &standardError)

            print(parser.makeSyntaxError().description, to: &standardError)

            print("Remaining tokens:", to: &standardError)

            var tokens: [SwiftPEGGrammar.GrammarToken.TokenString] = []
            while let next = try tokenizer.next() {
                tokens.append(next.token.string)
            }

            print(tokens, to: &standardError)
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

            let processor = GrammarProcessor(delegate: self, verbose: verbose)
            let result = try processor.process(grammar)

            for diagnostic in processor.diagnostics {
                print(diagnostic.description, to: &standardError)
            }

            let swiftCodeGen = SwiftCodeGen(from: result)

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
        loadTokensFileNamed name: String,
        ofGrammar grammar: SwiftPEGGrammar.Grammar
    ) throws -> String {

        let url = resolveFileName(name)

        return try String(contentsOf: url)
    }

    enum Error: Swift.Error {
        case tokensFileNotFound(String)
    }
}
