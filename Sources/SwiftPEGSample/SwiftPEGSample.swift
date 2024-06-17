import Foundation
import SwiftPEG

@main
struct SwiftPEGSample {
    static func main() throws {
        let args = ProcessInfo.processInfo.arguments.dropFirst()

        do {
            if args.first == "benchmark" {
                print("Running benchmark...")
                let sample = PerformanceTest()
                sample.useStringBuffer = args.contains("--use-strings") || args.contains("-s")
                try sample.run()
            } else {
                let verbose = args.contains("--verbose") || args.contains("-v")

                if verbose {
                    print("Parsing \(SwiftPEG.Resources.metagrammarFile.relativePath)...")
                }

                let sample = GrammarParsingSample()
                sample.useBuiltInFiles = args.contains("--use-builtin-grammar")
                sample.emitTokenType = args.contains("--emit-token") || args.contains("-t")
                sample.omitRedundantMarkRestores = args.contains("--omit-redundant-marks") || args.contains("-o")
                sample.emitSyntaxNodeLayout = args.contains("--emit-syntax-node-layout") || args.contains("-l")
                sample.verbose = verbose

                try sample.run()
            }
        } catch let error as ParserError {
            print(error.description)
            throw error
        }
    }
}
