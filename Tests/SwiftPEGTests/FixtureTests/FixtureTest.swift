import Console
import SwiftPEG

/// A fixture test execution context.
struct FixtureTest {
    var title: ConsoleString
    var diagnosticTarget: any LineDiagnosticTarget
    var testFunction: (FixtureTestContext) throws -> Void
    var failures: [FixtureTestFailure] = []

    var failed: Bool {
        !failures.isEmpty
    }
}

struct FixtureTestFailure {
    var message: String
    var file: String
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
