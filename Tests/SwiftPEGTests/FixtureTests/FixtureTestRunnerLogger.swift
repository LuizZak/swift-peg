import Foundation
import Console
import SwiftPEG

/// Manages standard output messages produced by a fixture test runner.
class FixtureTestRunnerLogger {
    private var scopeStackDepth: Int = 0
    private var isOnNewline = true
    private var indentation: String = "  "
    private var indentationString: String {
        String(repeating: indentation, count: scopeStackDepth)
    }

    //

    func logMessage(_ string: ConsoleString, terminator: String = "\n") {
        logMessage(consoleString: string, terminator: terminator)
    }

    func beginLogMessageScope(depth: Int = 1, _ label: ConsoleString) {
        logMessage("\(">", color: .yellow) \(formatted: label)")
        beginLogMessageScope(depth: depth)
    }

    func beginLogMessageScope(depth: Int = 1) {
        scopeStackDepth += depth
    }

    func endLogMessageScope(depth: Int = 1) {
        scopeStackDepth = max(0, scopeStackDepth - depth)
    }

    //

    private func logMessage(consoleString message: ConsoleString, terminator: String = "\n") {
        var message = message
        if isOnNewline {
            message = indentationString + message
        }

        _log(message, terminator: terminator)

        isOnNewline = terminator.hasSuffix("\n")
    }

    private func _log(_ string: ConsoleString, terminator: String = "\n") {
        print(string.terminalFormatted(), terminator: terminator)
    }
}
