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
                sample.useStringBuffer = args.contains("--use-strings")
                try sample.run()
            } else {
                let verbose = args.contains("--verbose")

                if verbose {
                    print("Parsing \(SwiftPEG.Resources.metagrammarFile.relativePath)...")
                }

                let sample = GrammarParsingSample()
                sample.verbose = verbose

                try sample.run()
            }
        } catch let error as ParserError {
            print(error.description)
            throw error
        }
    }
}
