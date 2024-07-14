import ArgumentParser
import SwiftPEG

struct Validate: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "validate",
        discussion: """
        Validates parser and tokens files from .gram SwiftPEG grammar files, producing
        diagnostics for syntax issues and internal warnings
        """
    )

    @OptionGroup(title: "Global")
    var commonOptions: CommonOptions

    func run() throws {
        let console = commonOptions.makeConsole()

        console.printStage(name: "Parsing grammar...")
        let rawGrammar = try commonOptions.parseRawGrammar()

        console.printStage(name: "Processing grammar...")
        _ = try commonOptions.processGrammar(grammar: rawGrammar)

        console.printSuccessMessage()
    }
}
