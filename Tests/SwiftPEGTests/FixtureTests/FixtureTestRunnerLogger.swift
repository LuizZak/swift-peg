import Foundation
import Console
import SwiftPEG

/// Manages standard output messages produced by a fixture test runner.
class FixtureTestRunnerLogger {
    private var scopeStack: [ConsoleString] = []
    private var isOnNewline = true
    private var indentation: String = "  "
    private var indentationString: String {
        String(repeating: indentation, count: scopeStack.count)
    }

    //

    func logMessage(_ string: ConsoleString, terminator: String = "\n") {
        logMessage(consoleString: string, terminator: terminator)
    }

    func beginLogMessageScope(_ label: ConsoleString) {
        logMessage("\(">", color: .yellow) \(formatted: label)")
        scopeStack.append(label)
    }

    func endLogMessageScope() {
        scopeStack.removeLast()
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

extension ConsoleString.Interpolation {
    /// Interpolates a given meta-property's name into this console string as
    /// `@<propertyName>`.
    mutating func appendInterpolation(name meta: SwiftPEGGrammar.Meta) {
        appendInterpolation("@\(meta.name.string)", color: .magenta)
    }

    /// Interpolates a URL by appending the URL's base URL as light text, before
    /// appending the relative path of the URL as normal text, with the last path
    /// component as a cyan color.
    mutating func appendInterpolation(url: URL, includeBase: Bool = false) {
        if includeBase, let baseUrl = url.baseURL {
            appendInterpolation(baseUrl.path, format: .light)
        }

        appendInterpolation(url.deletingLastPathComponent().relativePath)
        appendInterpolation("/")
        appendInterpolation(url.lastPathComponent, color: .cyan)
    }
}
