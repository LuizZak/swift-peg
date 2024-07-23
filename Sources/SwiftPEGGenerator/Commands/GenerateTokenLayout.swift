import Foundation
import ArgumentParser
import Console
import SwiftPEG

struct GenerateTokenLayout: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "token-layout",
        discussion: """
        Generates token layout views for .gram SwiftPEG grammar files.
        """
    )

    @OptionGroup(title: "Global")
    var commonOptions: CommonOptions

    func run() throws {
        let console = commonOptions.makeConsole()

        console.printStage(name: "Parsing grammar...")
        let rawGrammar = try commonOptions.parseRawGrammar()

        console.printStage(name: "Processing grammar...")
        let processed = try commonOptions.processGrammar(grammar: rawGrammar)

        let layoutGen = SyntaxNodeLayoutGen(processedGrammar: processed)

        console.printStage(name: "Generating syntax node layouts...")
        let layoutNodes = try layoutGen.generateSyntaxNodes()

        for layoutNode in layoutNodes {
            print(layoutNode)
        }
    }
}
