import XCTest

class StringDiffTestingTests: XCTestCase {
    var testReporter: TestDiffReporter!

    override func setUp() {
        super.setUp()

        testReporter = TestDiffReporter()
    }

    func testDiffSimpleString() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    abc
                    def
                    """
            ).diff(
                """
                abc
                df
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:4: Strings don't match: difference starts here: Actual line reads 'df'

            Expected (between ---):

            ---
            abc
            def
            ---

            Actual result (between ---):

            ---
            abc
            df
            ---

            Diff (between ---):

            ---
            abc
            df
            ~^ Difference starts here
            ---
            """
        )
    }

    func testDiffEmptyStrings() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter.diffTest(expected: "").diff("")
        #sourceLocation()

        XCTAssertEqual(testReporter.messages.count, 0)
    }

    func testDiffEqualStrings() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    abc
                    def
                    """
            ).diff(
                """
                abc
                def
                """
            )
        #sourceLocation()

        XCTAssertEqual(testReporter.messages.count, 0)
    }

    func testDiffWhitespaceString() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """

                    """
            ).diff("test")
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:3: Strings don't match: difference starts here: Actual line reads 'test'

            Expected (between ---):

            ---

            ---

            Actual result (between ---):

            ---
            test
            ---

            Diff (between ---):

            ---
            test
            ^ Difference starts here
            ---
            """
        )
    }

    func testDiffLargerExpectedString() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    abc
                    def
                    ghi
                    """
            ).diff(
                """
                abc
                def
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:5: Strings don't match: difference starts here: Expected matching line 'ghi'

            Expected (between ---):

            ---
            abc
            def
            ghi
            ---

            Actual result (between ---):

            ---
            abc
            def
            ---

            Diff (between ---):

            ---
            abc
            def
            ~~~^ Difference starts here
            ---
            """
        )
    }

    func testDiffLargerExpectedStringWithMismatchInMiddle() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    abc
                    def
                    ghi
                    """
            ).diff(
                """
                abc
                xyz
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:4: Strings don't match: difference starts here: Actual line reads 'xyz'

            Expected (between ---):

            ---
            abc
            def
            ghi
            ---

            Actual result (between ---):

            ---
            abc
            xyz
            ---

            Diff (between ---):

            ---
            abc
            xyz
            ^ Difference starts here
            ---
            """
        )
    }

    func testDiffLargerResultString() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    abc
                    def
                    """
            ).diff(
                """
                abc
                def
                ghi
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:4: Strings don't match: difference starts here: Extraneous content after this line

            Expected (between ---):

            ---
            abc
            def
            ---

            Actual result (between ---):

            ---
            abc
            def
            ghi
            ---

            Diff (between ---):

            ---
            abc
            def
            ghi
            ~~~^ Difference starts here
            ---
            """
        )
    }

    func testDiffLargerExpectedStringWithChangeAtFirstLine() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    label:
                    if true {
                    }
                    """
            ).diff(
                """
                if true {
                    }
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:3: Strings don't match: difference starts here: Actual line reads 'if true {'

            Expected (between ---):

            ---
            label:
            if true {
            }
            ---

            Actual result (between ---):

            ---
            if true {
                }
            ---

            Diff (between ---):

            ---
            if true {
            ^ Difference starts here
                }
            ---
            """
        )
    }

    func testDiffLargeMultiLineStrings() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    line 1
                    line 2
                    line 3
                    line 4
                    line 5
                    line 6
                    line 7
                    line 8
                    line 9
                    line 10
                    """
            ).diff(
                """
                line 1
                line 2
                line 3
                line 4
                DIFFERENCE
                line 6
                line 7
                line 8
                line 9
                line 10
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:7: Strings don't match: difference starts here: Actual line reads 'DIFFERENCE'

            Expected (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            line 5
            line 6
            line 7
            line 8
            line 9
            line 10
            ---

            Actual result (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            DIFFERENCE
            line 6
            line 7
            line 8
            line 9
            line 10
            ---

            Diff (between ---):

            --- [2 lines omitted]
            line 3
            line 4
            DIFFERENCE
            ^ Difference starts here
            line 6
            line 7
            --- [3 lines omitted]
            """
        )
    }

    func testDiffLargeMultiLineStringsNoLinesOmittedBefore() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    line 1
                    line 2
                    line 3
                    line 4
                    line 5
                    """
            ).diff(
                """
                DIFFERENCE
                line 2
                line 3
                line 4
                line 5
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:3: Strings don't match: difference starts here: Actual line reads 'DIFFERENCE'

            Expected (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            line 5
            ---

            Actual result (between ---):

            ---
            DIFFERENCE
            line 2
            line 3
            line 4
            line 5
            ---

            Diff (between ---):

            ---
            DIFFERENCE
            ^ Difference starts here
            line 2
            line 3
            --- [2 lines omitted]
            """
        )
    }

    func testDiffLargeMultiLineStringsNoLinesOmittedAfter() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    line 1
                    line 2
                    line 3
                    line 4
                    line 5
                    """
            ).diff(
                """
                line 1
                line 2
                line 3
                line 4
                DIFFERENCE
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:7: Strings don't match: difference starts here: Actual line reads 'DIFFERENCE'

            Expected (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            line 5
            ---

            Actual result (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            DIFFERENCE
            ---

            Diff (between ---):

            --- [2 lines omitted]
            line 3
            line 4
            DIFFERENCE
            ^ Difference starts here
            ---
            """
        )
    }

    func testDiffOnly() {
        #sourceLocation(file: "test.swift", line: 1)
        testReporter
            .diffTest(
                expected: """
                    abc
                    def
                    """,
                diffOnly: true
            ).diff(
                """
                abc
                df
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:4: Strings don't match: difference starts here: Actual line reads 'df'

            Diff (between ---):

            ---
            abc
            df
            ~^ Difference starts here
            ---
            """
        )
    }
}

extension StringDiffTestingTests {
    public func diffTest(
        expected input: String,
        diffOnly: Bool = false,
        file: StaticString = #filePath,
        line: UInt = #line
    ) -> DiffingTest {

        let location = DiffLocation(file: file, line: line)
        let diffable = DiffableString(string: input, location: location)

        return DiffingTest(
            expected: diffable,
            testCase: testReporter,
            highlightLineInEditor: true,
            diffOnly: diffOnly
        )
    }
}

class TestDiffReporter: DiffTestCaseFailureReporter {
    var messages: [String] = []

    func _recordFailure(
        withDescription description: String,
        inFile filePath: StaticString,
        atLine lineNumber: UInt,
        expected: Bool
    ) {

        messages.append("\(filePath):\(lineNumber): " + description)
    }
}

private func assertLinesMatch(
    _ actual: String?,
    _ expected: String?,
    file: StaticString = #file,
    line: UInt = #line
) {
    guard actual != expected else {
        return
    }

    XCTFail(
        "Strings don't match:\n\(actual ?? "<nil>")\n\nvs\n\n\(expected ?? "<nil>")",
        file: file,
        line: line
    )
}

private extension Collection {
    subscript(safe index: Index) -> Element? {
        if indices.contains(index) {
            return self[index]
        }

        return nil
    }
}
