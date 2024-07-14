import ArgumentParser
import SwiftPEG

struct GenerateTokenType: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "token-type",
        discussion: """
        Generates a Token type file from .gram/.tokens SwiftPEG grammar files.
        """
    )

    @OptionGroup
    var commonOptions: CommonOptions

    @OptionGroup
    var options: TokenTypeGenSettings

    @Option(
        help: """
        File path to save generated code to.
        """
    )
    var output: String

    func run() throws {
        let console = commonOptions.makeConsole()

        console.printStage(name: "Parsing grammar...")
        let rawGrammar = try commonOptions.parseRawGrammar()

        console.printStage(name: "Processing grammar...")
        let processed = try commonOptions.processGrammar(grammar: rawGrammar)

        console.printStage(name: "Generating token type...")
        let codeGen = SwiftCodeGen(from: processed)
        let settings = try options.toSwiftCodeGenTokenTypeGenSettings()
        let generated = try codeGen.generateTokenType(settings: settings)

        try generated.write(
            to: .init(fileURLWithPath: output),
            atomically: true,
            encoding: .utf8
        )

        console.printSuccessMessage()
    }

    struct TokenTypeGenSettings: ParsableArguments {
        @Flag(
            help: """
            Emit '@inlinable' attributes in all generated methods.
            """
        )
        var inlinable: Bool = false

        @Option(
            help: """
            Emit all methods and types with a given access level.
            """
        )
        var accessLevel: String?

        @Flag(
            help: """
            Attempt to generate switch statements in top-level token parser method \
            that alternate between dynamic and shadowed static tokens by inspecting the \
            length of the results before switching into the actual static token \
            strings.
            """
        )
        var emitLengthSwitch: Bool = false

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
                emitInlinable: inlinable,
                accessLevel: accessLevel,
                emitLengthSwitchPhaseInTokenOcclusionSwitch: emitLengthSwitch
            )
        }

        enum Error: Swift.Error {
            case invalidAccessLevel(String)
        }
    }
}
