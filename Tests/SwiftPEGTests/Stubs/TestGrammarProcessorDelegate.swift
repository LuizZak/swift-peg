import SwiftPEG

class TestGrammarProcessorDelegate: GrammarProcessor.Delegate {
    var grammarProcessor_loadTokensFileNamed_stub: ((GrammarProcessor, String) throws -> String)?
    var grammarProcessor_loadTokensFileNamed_calls: [(String)] = []
    var grammarProcessor_loadTokensFileNamed_callCount: Int = 0
    func grammarProcessor(
        _ processor: GrammarProcessor,
        loadTokensFileNamed name: String
    ) throws -> String {

        grammarProcessor_loadTokensFileNamed_calls.append(name)
        grammarProcessor_loadTokensFileNamed_callCount += 1

        if let stub = grammarProcessor_loadTokensFileNamed_stub {
            return try stub(processor, name)
        }

        throw Error.unimplemented
    }

    enum Error: Swift.Error {
        case unimplemented
    }
}
