import XCTest

@testable import SwiftPEG

public protocol DiffTestCaseFailureReporter {
    func _recordFailure(
        withDescription description: String,
        inFile filePath: StaticString,
        atLine lineNumber: UInt,
        expected: Bool
    )
}

public extension DiffTestCaseFailureReporter {

    func diffTest(
        expected input: String,
        highlightLineInEditor: Bool = true,
        diffOnly: Bool = false,
        file: StaticString = #filePath,
        line: UInt = #line
    ) -> DiffingTest {

        let location = DiffLocation(file: file, line: line)
        let diffable = DiffableString(string: input, location: location)

        return DiffingTest(
            expected: diffable,
            testCase: self,
            highlightLineInEditor: highlightLineInEditor,
            diffOnly: diffOnly
        )
    }
}

/// Represents a location for a diff'd string
public struct DiffLocation {
    var file: StaticString
    var line: UInt

    public init(file: StaticString, line: UInt) {
        self.file = file
        self.line = line
    }
}

public struct DiffableString {
    var string: String
    var location: DiffLocation

    public init(string: String, location: DiffLocation) {
        self.string = string
        self.location = location
    }
}

public class DiffingTest {
    var expectedDiff: DiffableString
    let testCase: DiffTestCaseFailureReporter
    let highlightLineInEditor: Bool
    let diffOnly: Bool

    public init(
        expected: DiffableString,
        testCase: DiffTestCaseFailureReporter,
        highlightLineInEditor: Bool,
        diffOnly: Bool
    ) {

        self.expectedDiff = expected
        self.testCase = testCase
        self.highlightLineInEditor = highlightLineInEditor
        self.diffOnly = diffOnly
    }

    public func diff(
        _ actual: String,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {

        if expectedDiff.string == actual {
            return
        }

        let diffStringSection = makeDiffStringSection(expected: expectedDiff.string, actual: actual)
        let message: String = makeErrorMessage(actual: actual, diffStringSection: diffStringSection)

        guard
            highlightLineInEditor,
            let (diffStartLine, diffStartColumn) = actual.firstDifferingLineColumn(against: expectedDiff.string) else
        {
            fail(
                message: """
                Strings don't match:

                \(message)
                """,
                line: expectedDiff.location.line
            )
            return
        }

        // Report inline in Xcode or other editor now
        let expectedLineRanges = expectedDiff.string.lineRanges()
        let actualLineRanges = actual.lineRanges()

        if diffStartLine - 1 < expectedLineRanges.count && actualLineRanges.count == expectedLineRanges.count {
            let actualLineContent = actual[actualLineRanges[max(0, diffStartLine - 1)]]

            fail(
                message: """
                \(prefixStringMismatchMessage("Difference starts here: Actual line reads '\(actualLineContent)'"))

                \(message)
                """,
                line: expectedDiff.location.line + UInt(diffStartLine)
            )
        } else if actualLineRanges.count < expectedLineRanges.count {
            let isAtLastColumn: Bool = {
                guard let last = expectedLineRanges.last else {
                    return false
                }

                let dist = expectedDiff.string.distance(from: last.lowerBound, to: last.upperBound)

                return diffStartColumn == dist + 1
            }()

            if diffStartLine == expectedLineRanges.count - 1 && isAtLastColumn {
                let resultLineContent = expectedDiff.string[expectedLineRanges[diffStartLine]]

                fail(
                    message: """
                    \(prefixStringMismatchMessage("Difference starts here: Expected matching line '\(resultLineContent)'"))

                    \(message)
                    """,
                    line: expectedDiff.location.line + UInt(diffStartLine + 1)
                )
            } else {
                let actualLineContent = actual[actualLineRanges[max(0, diffStartLine - 1)]]

                fail(
                    message: """
                    \(prefixStringMismatchMessage("Difference starts here: Actual line reads '\(actualLineContent)'"))

                    \(message)
                    """,
                    line: expectedDiff.location.line + UInt(diffStartLine)
                )
            }
        } else if diffStartLine - 1 < expectedLineRanges.count {
            let resultLineContent = expectedDiff.string[expectedLineRanges[diffStartLine - 1]]

            fail(
                message: """
                \(prefixStringMismatchMessage("Difference starts here: Expected matching line '\(resultLineContent)'"))

                \(message)
                """,
                line: expectedDiff.location.line + UInt(diffStartLine)
            )
        } else {
            fail(
                message: """
                \(prefixStringMismatchMessage("Difference starts here: Extraneous content after this line"))

                \(message)
                """,
                line: expectedDiff.location.line + UInt(expectedLineRanges.count)
            )
        }
    }

    func fail(message: String, line: UInt) {
        testCase._recordFailure(
            withDescription: message,
            inFile: expectedDiff.location.file,
            atLine: line,
            expected: true
        )
    }

    /// Prefixes messages that are displayed at the head of test failure messages.
    func prefixStringMismatchMessage(_ message: String) -> String {
        "Strings don't match: \(message.lowercasedFirstLetter)"
    }

    func makeErrorMessage(actual: String, diffStringSection: String) -> String {
        if diffOnly {
            """
            Diff (between ---):

            \(diffStringSection)
            """
        } else {
            """
            Actual result (between ---):

            ---
            \(actual)
            ---

            Expected (between ---):

            ---
            \(expectedDiff.string)
            ---

            Diff (between ---):

            \(diffStringSection)
            """
        }
    }

    func makeDiffStringSection(expected: String, actual: String) -> String {
        func formatOmittedLinesMessage(_ omittedLines: Int) -> String {
            switch omittedLines {
            case 0:
                return ""
            case 1:
                return " [1 line omitted]"
            default:
                return " [\(omittedLines) lines omitted]"
            }
        }

        guard let (diffLine, _) = actual.firstDifferingLineColumn(against: expected) else {
            return """
            ---
            \(actual.makeDifferenceMarkString(against: expected))
            ---
            """
        }

        let diffString = actual.makeDifferenceMarkString(against: expected)

        let (result, linesBefore, linesAfter) = omitLines(diffString, aroundLine: diffLine)

        return """
        ---\(formatOmittedLinesMessage(linesBefore))
        \(result)
        ---\(formatOmittedLinesMessage(linesAfter))
        """
    }

    func omitLines(
        _ string: String,
        aroundLine line: Int,
        contextLinesBefore: Int = 3,
        contextLinesAfter: Int = 3
    ) -> (result: String, linesBefore: Int, linesAfter: Int) {

        let lines = string.split(separator: "\n", omittingEmptySubsequences: false)
        let minLine = max(0, line - contextLinesBefore)
        let maxLine = min(lines.count, line + contextLinesAfter)

        var result: [Substring] = []

        for lineIndex in minLine..<line {
            result.append(lines[lineIndex])
        }

        if line < lines.count {
            result.append(lines[line])
        }

        if line + 1 < maxLine {
            for lineIndex in (line + 1)..<maxLine {
                guard lineIndex < lines.count else {
                    break
                }

                result.append(lines[lineIndex])
            }
        }

        return (result.joined(separator: "\n"), minLine, lines.count - maxLine)
    }
}

// MARK: - XCTestCase: TestCaseFailureReporter
extension XCTestCase: DiffTestCaseFailureReporter {
    public func _recordFailure(
        withDescription description: String,
        inFile filePath: StaticString,
        atLine lineNumber: UInt,
        expected: Bool
    ) {

        #if os(macOS)

        let location = XCTSourceCodeLocation(
            filePath: filePath.description,
            lineNumber: Int(lineNumber)
        )

        let issue = XCTIssueReference(
            type: .assertionFailure,
            compactDescription: description,
            detailedDescription: nil,
            sourceCodeContext: XCTSourceCodeContext(location: location),
            associatedError: nil,
            attachments: []
        )

        #if XCODE

        self.record(
            issue
        )

        #else // #if XCODE

        self.record(
            issue as XCTIssue
        )

        #endif // #if XCODE

        #else // #if os(macOS)

        XCTFail(description, file: filePath, line: lineNumber)

        #endif // #if os(macOS)
    }
}

struct StandardOutputDiffTestCaseFailureReporter: DiffTestCaseFailureReporter {

    func _recordFailure(
        withDescription description: String,
        inFile filePath: StaticString,
        atLine lineNumber: UInt,
        expected: Bool
    ) {
        print("\(filePath):\(lineNumber): \(description)")
    }
}
