import Foundation
import ArgumentParser
import SwiftPEG
import Console

/// Common options shared by all sub-commands.
struct CommonOptions: ParsableArguments {
    @Option(
        help: """
        Path to SwiftPEG grammar file, usually with '.gram' extension
        """
    )
    var grammarFile: String

    @Option(
        help: """
        Specifies a custom base path to use when attempting to load '.tokens' files \
        included by grammar files.

        If no value is specified, the path defaults to the parent path of the input \
        grammar file.
        """
    )
    var overrideTokensFilePath: String?

    @Flag(
        name: .shortAndLong,
        help: """
        Emits no messages to standard output and standard error except error messages \
        and parser/token type output.

        Overrides 'verbose'.
        """
    )
    var silent: Bool = false

    @Flag(
        name: .shortAndLong,
        help: """
        Whether to emit diagnostic messages from internal grammar processors
        """
    )
    var verbose: Bool = false

    @Option(
        help: """
        Entry rule for the grammar file. Defaults to 'start' if no specified.
        """
    )
    var entryRuleName: String = "start"

    /// Parses a SwiftPEG grammar file using the settings specified within this
    /// options group.
    func parseRawGrammar() throws -> SwiftPEGGrammar.Grammar {
        let url = URL(fileURLWithPath: grammarFile)

        let loader = try GrammarLoader(baseURLOfPath: url)

        if let overrideTokensFilePath {
            loader.tokensFileBaseURL = URL(fileURLWithPath: overrideTokensFilePath)
        }

        return try loader.loadRawGrammarFile(url)
    }

    /// Parses a SwiftPEG grammar file using the settings specified within this
    /// options group.
    func processGrammar(grammar: SwiftPEGGrammar.Grammar) throws -> ProcessedGrammar {
        let url = URL(fileURLWithPath: grammarFile)

        let loader = try GrammarLoader(baseURLOfPath: url)

        if let overrideTokensFilePath {
            loader.tokensFileBaseURL = URL(fileURLWithPath: overrideTokensFilePath)
        }

        return try loader.processGrammar(
            grammar,
            verbose: !silent && verbose,
            entryRuleName: entryRuleName
        )
    }

    func makeConsole() -> ConsoleType {
        if silent {
            return Console(output: NullOutputStream())
        } else {
            return Console(output: StandardErrorTextStream())
        }
    }
}
