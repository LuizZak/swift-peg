import PackagePlugin
import Foundation

@main
struct GenerateParserPlugin: CommandPlugin {
    func performCommand(
        context: PluginContext,
        arguments: [String]
    ) async throws {
        /*
        // We'll be invoking `swift-format`, so start by locating it.
        let swiftFormatTool = try context.tool(named: "swift-format")

        // By convention, use a configuration file in the package directory.
        let configFile = context.package.directory.appending(".swift-format.json")

        // Iterate over the targets in the package.
        for target in context.package.targets {
            // Skip any type of target that doesn't have source files.
            // Note: We could choose to instead emit a warning or error here.
            guard let target = target as? SourceModuleTarget else { continue }

            // Invoke `swift-format` on the target directory, passing a configuration
            // file from the package directory.
            let swiftFormatExec = URL(fileURLWithPath: swiftFormatTool.path.string)
            let swiftFormatArgs = [
                "--configuration", "\(configFile)",
                "--in-place",
                "--recursive",
                "\(target.directory)"
            ]
            let process = try Process.run(swiftFormatExec, arguments: swiftFormatArgs)
            process.waitUntilExit()

            // Check whether the subprocess invocation was successful.
            if process.terminationReason == .exit && process.terminationStatus == 0 {
                print("Formatted the source code in \(target.directory).")
            }
            else {
                let problem = "\(process.terminationReason):\(process.terminationStatus)"
                Diagnostics.error("swift-format invocation failed: \(problem)")
            }
        }
        */
    }
}
