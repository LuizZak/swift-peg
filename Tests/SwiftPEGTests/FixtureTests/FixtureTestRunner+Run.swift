import Foundation
import Console
import SwiftPEG

extension FixtureTestRunner {
    /// Runs all provided file fixtures.
    func runTests(_ fileFixtures: [FixtureTestsFile]) throws {
        // Detect focused files
        let focused = fileFixtures.filter({ $0.flags.contains(.focus) })
        guard focused.isEmpty else {
            for fileFixture in focused {
                try runTests(in: fileFixture)
            }

            let focusMeta = focused[0].grammar.test_focus_meta()
            recordFailure(
                "Failing tests while @focus flag is enabled in one or more files.",
                file: focused[0].file.path,
                line: focusMeta.map(_line(of:)) ?? 1,
                expected: true
            )
            return
        }

        for fileFixture in fileFixtures {
            try runTests(in: fileFixture)
        }
    }

    /// Run fixtures from a set of file fixtures.
    func runTests(in fileFixture: FixtureTestsFile) throws {
        logger.beginLogMessageScope(
            depth: 2,
            "Running \(fileName: fileFixture.file)..."
        )
        defer { logger.endLogMessageScope(depth: 2) }

        if fileFixture.fixtures.isEmpty {
            logger.logMessage("\("Warning:", color: .yellow) no test fixtures found in file \(fileName: fileFixture.file)?")
        }

        for fixture in fileFixture.fixtures {
            let failures = runTest(fixture, file: fileFixture.file)

            let result = FixtureTestResults(title: fixture.title, failures: failures)
            self.testResults.append(result)
        }
    }

    /// Runs a test fixture, returning the test failures.
    func runTest(_ test: FixtureTest, file: URL) -> [FixtureTestFailure] {
        let title: ConsoleString = "Test \(formatted: test.title)... "

        return _runTest(test, runStartMessage: title, file: file) { context in
            try test.testFunction(context)
        }
    }

    func _runTest(
        _ test: FixtureTest,
        runStartMessage: ConsoleString,
        file: URL,
        testBlock: (FixtureTestContext) throws -> Void
    ) -> [FixtureTestFailure] {

        let context = makeTestContext(for: test)

        logger.logMessage(runStartMessage, terminator: "")

        let stopwatch = Stopwatch.startNew()

        do {
            try testBlock(context)
        } catch {
            context.failures.append(
                .init(message: "Error running test: \(error)", file: file, line: 1)
            )
        }

        if context.hasFailures {
            logger.logMessage("\("Failed", color: .red) in \(elapsed: stopwatch)")
        } else {
            logger.logMessage("\("Passed", color: .green) in \(elapsed: stopwatch)")
        }

        context.flushFailureMessages()

        return context.failures
    }
}
