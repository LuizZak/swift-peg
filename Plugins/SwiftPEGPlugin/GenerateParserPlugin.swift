import PackagePlugin
import Foundation

@main
struct GenerateParserPlugin: CommandPlugin {
    func performCommand(
        context: PluginContext,
        arguments: [String]
    ) async throws {
        let tool = try context.tool(named: "SwiftPEGGenerator")

        let exec = URL(fileURLWithPath: tool.path.string)

        let result = try runProcess(exec, arguments: arguments)

        // Check whether the subprocess invocation was successful.
        guard result.terminationReason == .exit && result.terminationStatus == 0 else {
            let problem = "\(result.terminationReason):\(result.terminationStatus)"
            Diagnostics.error("""
            SwiftPEGGenerator invocation failed: \(problem)
            """)
            return
        }
    }

    private func runProcess(
        _ exec: URL,
        arguments: [String]
    ) throws -> ProcessResult {
        let process = Process()

        process.executableURL = exec
        process.arguments = arguments

        try process.run()

        process.waitUntilExit()

        return .init(
            terminationReason: process.terminationReason,
            terminationStatus: process.terminationStatus
        )
    }
}

private struct ProcessResult {
    var terminationReason: Process.TerminationReason
    var terminationStatus: Int32
}
