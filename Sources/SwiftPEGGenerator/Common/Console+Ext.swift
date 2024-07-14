import Console

extension ConsoleType {
    func printStage(name: ConsoleString, terminator: String = "\n") {
        printLine("\("•", color: .yellow) \(formatted: name)", terminator: terminator)
    }

    func printSuccessMessage() {
        printLine("\("✓", color: .green) Success!")
    }

    func printReplacingLineEnding(count: Int, _ replacement: ConsoleString, terminator: String = "\n") {
        command(.cursorBack(count))
        command(.eraseInLine(.toEnd))
        printLine(replacement, terminator: terminator)
    }
}
