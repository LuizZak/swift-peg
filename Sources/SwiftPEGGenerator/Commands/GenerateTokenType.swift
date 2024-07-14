import ArgumentParser
import SwiftPEG

struct GenerateTokenType: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "token-type",
        discussion: """
        Generates a Token type file from .gram/.tokens SwiftPEG grammar files.
        """
    )

    @OptionGroup(title: "Global")
    var commonOptions: CommonOptions

    @OptionGroup
    var options: TokenTypeGenSettings

    func run() throws {
        let console = commonOptions.makeConsole()

        console.printStage(name: "Parsing grammar...")
        let rawGrammar = try commonOptions.parseRawGrammar()

        console.printStage(name: "Processing grammar...")
        let processed = try commonOptions.processGrammar(grammar: rawGrammar)

        console.printStage(name: "Generating token type...")
        let codeGen = SwiftCodeGen(from: processed)
        let settings = try options.toSwiftCodeGenTokenTypeGenSettings()
        let output = try codeGen.generateTokenType(settings: settings)

        print(output)

        console.printSuccessMessage()
    }

    struct TokenTypeGenSettings: ParsableArguments {
        @Flag(
            help: """
            Emit '@inlinable' attributes in all generated methods.
            """
        )
        var emitInlinable: Bool = false

        @Option(
            help: """
            Emit all methods and types with a given access level.
            """
        )
        var accessLevel: String?

        @Flag(
            help: """
            Attempt to generate switch statements in top-level token parser method \
            to alternate between static and dynamic alternatives by inspecting the \
            length of the results before switching into the actual static token \
            strings.
            """
        )
        var emitLengthSwitchPhaseInTokenOcclusionSwitch: Bool = false

        func toSwiftCodeGenTokenTypeGenSettings() throws -> SwiftCodeGen.TokenTypeGenSettings {
            switch accessLevel {
            case nil:
                break
            case "open", "public", "package", "internal", "fileprivate", "private":
                break
            case let access?:
                throw Error.invalidAccessLevel(access)
            }

            return .init(
                emitInlinable: emitInlinable,
                accessLevel: accessLevel,
                emitLengthSwitchPhaseInTokenOcclusionSwitch: emitLengthSwitchPhaseInTokenOcclusionSwitch
            )
        }

        enum Error: Swift.Error {
            case invalidAccessLevel(String)
        }
    }
}
