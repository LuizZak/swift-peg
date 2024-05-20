import Foundation
import SwiftPEG

@main
struct SwiftPEGSample {
    static func main() throws {
        let args = ProcessInfo.processInfo.arguments.dropFirst()

        do {
            if args.first == "benchmark" {
                print("Running benchmark...")
                try PerformanceTest.run()
            } else {
                print("Parsing \(SwiftPEG.Resources.metagrammarFile.relativePath)...")
                try GrammarParsingSample.run()
            }
        } catch let error as ParserError {
            print(error.description)
            throw error
        }
    }
}
