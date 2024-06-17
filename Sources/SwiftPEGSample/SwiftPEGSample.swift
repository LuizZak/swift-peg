import Foundation
import ArgumentParser
import SwiftPEG

@main
struct SwiftPEGSample: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "SwiftPEGSample",
        discussion: """
        Sample binary for inspecting SwiftPEG's internal grammar and other SwiftPEG
        grammar files.
        """,
        subcommands: [
            GrammarParsingSample.self,
            PerformanceTest.self
        ],
        defaultSubcommand: GrammarParsingSample.self
    )

    func run() throws {

    }
}
