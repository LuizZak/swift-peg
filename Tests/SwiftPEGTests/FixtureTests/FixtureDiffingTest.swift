class FixtureDiffingTest: DiffingTest {
    let target: any LineDiagnosticTarget

    public init(
        expected: DiffableString,
        testCase: DiffTestCaseFailureReporter,
        highlightLineInEditor: Bool,
        diffOnly: Bool,
        target: any LineDiagnosticTarget
    ) {

        self.target = target

        super.init(
            expected: expected,
            testCase: testCase,
            highlightLineInEditor: highlightLineInEditor,
            diffOnly: diffOnly
        )
    }

    override func fail(message: String, line: UInt) {
        target.diagnoseError(message: message, line: Int(line))
    }

    override func prefixStringMismatchMessage(_ message: String) -> String {
        message
    }

    override func makeErrorMessage(actual: String, diffStringSection: String) -> String {
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

            Diff (between ---):

            \(diffStringSection)
            """
        }
    }
}
