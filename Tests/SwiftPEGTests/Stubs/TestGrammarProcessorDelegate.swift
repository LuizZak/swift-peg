import SwiftPEG

class TestGrammarProcessorDelegate: GrammarProcessor.Delegate {
    var grammarProcessor_loadTokensFileNamed_stub: ((GrammarProcessor, String, SwiftPEGGrammar.Grammar) throws -> String)?
    var grammarProcessor_loadTokensFileNamed_calls: [(String, SwiftPEGGrammar.Grammar)] = []
    var grammarProcessor_loadTokensFileNamed_callCount: Int = 0
    func grammarProcessor(
        _ processor: GrammarProcessor,
        loadTokensFileNamed name: String,
        ofGrammar grammar: SwiftPEGGrammar.Grammar
    ) throws -> String {

        grammarProcessor_loadTokensFileNamed_calls.append((name, grammar))
        grammarProcessor_loadTokensFileNamed_callCount += 1

        if let stub = grammarProcessor_loadTokensFileNamed_stub {
            return try stub(processor, name, grammar)
        }

        throw Error.unimplemented
    }

    var grammarProcessor_importFileNamed_stub: ((GrammarProcessor, String, SwiftPEGGrammar.Grammar) throws -> String)?
    var grammarProcessor_importFileNamed_calls: [(String, SwiftPEGGrammar.Grammar)] = []
    var grammarProcessor_importFileNamed_callCount: Int = 0
    func grammarProcessor(
        _ processor: GrammarProcessor,
        importFileNamed name: String,
        ofGrammar grammar: SwiftPEGGrammar.Grammar
    ) throws -> String {

        grammarProcessor_importFileNamed_calls.append((name, grammar))
        grammarProcessor_importFileNamed_callCount += 1

        if let stub = grammarProcessor_importFileNamed_stub {
            return try stub(processor, name, grammar)
        }

        throw Error.unimplemented
    }

    enum Error: Swift.Error {
        case unimplemented
    }
}
