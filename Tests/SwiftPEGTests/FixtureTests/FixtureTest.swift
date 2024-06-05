import Foundation
import Console
import SwiftPEG

/// A collection of test fixtures that came from a single file.
struct FixtureTestsFile {
    var file: URL
    var fixtures: [FixtureTest]

    /// Flags for a fixture test file
    var flags: Flags = []

    struct Flags: OptionSet {
        /// A flag for files that are meant to be tested, and all other tests in
        /// other files without the flag are to be skipped.
        ///
        /// If no file has a focus flag, then all fixture tests from all files
        /// are executed as normal.
        static let focus: Self = Self(rawValue: 0b0000_0001)

        var rawValue: Int

        init(rawValue: Int) {
            self.rawValue = rawValue
        }
    }
}

/// A fixture test definition.
struct FixtureTest {
    var title: ConsoleString
    var diagnosticTarget: any LineDiagnosticTarget
    var testFunction: (FixtureTestContext) throws -> Void

    var file: URL {
        diagnosticTarget.fileUrl
    }
}

struct FixtureTestResults {
    var title: ConsoleString
    var failures: [FixtureTestFailure]

    var failed: Bool {
        !failures.isEmpty
    }
}

struct FixtureTestFailure {
    var message: String
    var file: URL
    var line: Int
}

protocol FixtureTestContext {
    /// Requests the beginning of a diff test.
    func diffTest(
        expected input: String,
        lineOffset: Int,
        highlightLineInEditor: Bool,
        diffOnly: Bool
    ) -> DiffingTest

    /// Requests that a given grammar be processed.
    func processGrammar(
        _ grammar: SwiftPEGGrammar.Grammar
    ) throws -> ProcessedGrammar

    /// Requests the source line location of a given node.
    func sourceLine(of node: Node) -> Int
}

extension FixtureTestContext {
    func diffTest(
        expected input: String,
        lineOffset: Int
    ) -> DiffingTest {
        diffTest(
            expected: input,
            lineOffset: lineOffset,
            highlightLineInEditor: true,
            diffOnly: false
        )
    }
}
