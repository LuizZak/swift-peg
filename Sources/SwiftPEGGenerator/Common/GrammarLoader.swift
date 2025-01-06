import Foundation
import SwiftPEG

/// Provides loading and caching support for SwiftPEG grammar files.
class GrammarLoader {
    /// Active base URL provided at initialization.
    let baseURL: URL

    /// URL to use for the purposes of searching .tokens files included in grammar
    /// files.
    ///
    /// Defaults to `baseURL` at initialization.
    var tokensFileBaseURL: URL

    /// URL to use for the purposes of searching .gram files included in grammar
    /// files.
    ///
    /// Defaults to `baseURL` at initialization.
    var importFileBaseURL: URL

    /// Initializes a new grammar loader, to be based on a given path for the
    /// purposes of loading auxiliary files like .tokens files.
    init(baseURL: URL) throws {
        self.baseURL = baseURL
        self.tokensFileBaseURL = baseURL
        self.importFileBaseURL = baseURL
    }

    /// Initializes a new grammar loader, to be based on the parent path of the
    /// given path for the purposes of loading auxiliary files like .tokens files.
    init(baseURLOfPath fileURL: URL) throws {
        self.baseURL = fileURL.deletingLastPathComponent()
        self.tokensFileBaseURL = baseURL
        self.importFileBaseURL = baseURL
    }

    /// Loads a raw grammar file at a given URL.
    func loadRawGrammarFile(_ url: URL) throws -> SwiftPEGGrammar.Grammar {
        let source = try String(contentsOf: url)
        let rawTokenizer = GrammarRawTokenizer(source: source)
        let parser = GrammarParser(raw: rawTokenizer)

        guard let grammar = try parser.start(), try parser.isEOF() else {
            throw parser.makeSyntaxError()
        }

        return grammar
    }

    /// Loads a grammar file at a given URL, and processes it using 'GrammarProcessor'.
    func loadProcessedGrammar(
        _ url: URL,
        verbose: Bool = false,
        entryRuleName: String? = nil
    ) throws -> ProcessedGrammar {
        let rawGrammar = try loadRawGrammarFile(url)

        return try processGrammar(
            rawGrammar,
            verbose: verbose,
            entryRuleName: entryRuleName
        )
    }

    /// Loads a grammar file at a given URL, and processes it using 'GrammarProcessor'.
    func processGrammar(
        _ rawGrammar: SwiftPEGGrammar.Grammar,
        verbose: Bool = false,
        entryRuleName: String? = nil
    ) throws -> ProcessedGrammar {
        let processor = GrammarProcessor(delegate: self, verbose: verbose)

        if let entryRuleName {
            return try processor.process(rawGrammar, entryRuleName: entryRuleName)
        }

        return try processor.process(rawGrammar)
    }

    enum Error: Swift.Error {
        case importFailed(name: String)
    }
}

extension GrammarLoader: GrammarProcessor.Delegate {
    func grammarProcessor(
        _ processor: GrammarProcessor,
        loadTokensFileNamed name: String,
        ofGrammar grammar: SwiftPEGGrammar.Grammar
    ) throws -> String {
        let source = try String(contentsOf: tokensFileBaseURL.appendingPathComponent(name))

        return source
    }

    func grammarProcessor(
        _ processor: GrammarProcessor,
        importFileNamed name: String,
        ofGrammar grammar: SwiftPEGGrammar.Grammar
    ) throws -> String {
        let source = try String(contentsOf: importFileBaseURL.appendingPathComponent(name))

        return source
    }
}
