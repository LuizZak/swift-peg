import SwiftPEG

@main
struct SwiftPEGSample {
    static func main() throws {
        do {
            try GrammarParsingSample.run()
        } catch let error as ParserError {
            print(error.description)
            throw error
        }
    }
}
