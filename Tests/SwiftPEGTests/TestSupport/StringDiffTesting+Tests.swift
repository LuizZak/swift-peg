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

            Actual result (between ---):

            ---
            abc
            df
            ---

            Expected (between ---):

            ---
            abc
            def
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

            Actual result (between ---):

            ---
            test
            ---

            Expected (between ---):

            ---

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

            Actual result (between ---):

            ---
            abc
            def
            ---

            Expected (between ---):

            ---
            abc
            def
            ghi
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

            Actual result (between ---):

            ---
            abc
            xyz
            ---

            Expected (between ---):

            ---
            abc
            def
            ghi
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

            Actual result (between ---):

            ---
            abc
            def
            ghi
            ---

            Expected (between ---):

            ---
            abc
            def
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

    func testDiffLargerResultStringWithMismatchInMiddle() {
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
                de
                ghi
                jkl
                """
            )
        #sourceLocation()

        assertLinesMatch(
            testReporter.messages[safe: 0],
            """
            test.swift:4: Strings don't match: difference starts here: Expected matching line 'def'

            Actual result (between ---):

            ---
            abc
            de
            ghi
            jkl
            ---

            Expected (between ---):

            ---
            abc
            def
            ---

            Diff (between ---):

            ---
            abc
            de
            ~~^ Difference starts here
            ghi
            jkl
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

            Actual result (between ---):

            ---
            if true {
                }
            ---

            Expected (between ---):

            ---
            label:
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

            Actual result (between ---):

            ---
            DIFFERENCE
            line 2
            line 3
            line 4
            line 5
            ---

            Expected (between ---):

            ---
            line 1
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

            Actual result (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            DIFFERENCE
            ---

            Expected (between ---):

            ---
            line 1
            line 2
            line 3
            line 4
            line 5
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
    guard let actual, let expected else {
        XCTAssertEqual(actual, expected, file: file, line: line)
        return
    }
    let linesActual = actual.split(separator: "\n", omittingEmptySubsequences: false)
    let linesExpected = expected.split(separator: "\n", omittingEmptySubsequences: false)

    guard linesActual.count > 1 && linesExpected.count > 1 else {
        XCTFail(
            "Strings don't match:\n\(actual)\n\nvs\n\n\(expected)",
            file: file,
            line: line
        )
        return
    }

    let firstChangedLine = zip(linesActual, linesExpected)
        .enumerated()
        .first(where: { $0.element.0 != $0.element.1 })

    guard let firstChangedLine else {
        XCTFail(
            "Strings don't match:\n\(actual)\n\nvs\n\n\(expected)",
            file: file,
            line: line
        )
        return
    }

    XCTFail("""
        Strings don't match starting at line \(firstChangedLine.offset):
        --
        \(firstChangedLine.element.0)
        --
        \(firstChangedLine.element.1)
        --
        Strings:
        
        \(actual)
        
        vs
        
        \(expected)
        """,
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
