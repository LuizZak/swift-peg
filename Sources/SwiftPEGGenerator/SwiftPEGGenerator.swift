import ArgumentParser
import SwiftPEG

@main
struct SwiftPEGGenerator: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "SwiftPEGGenerator",
        discussion: """
        Generates parser and tokens files from .gram SwiftPEG grammar files.
        """,
        subcommands: [
            Generate.self,
            Validate.self,
            GenerateTokenType.self,
            GenerateTokenLayout.self,
        ],
        defaultSubcommand: Generate.self
    )

    func run() throws {

    }
}
