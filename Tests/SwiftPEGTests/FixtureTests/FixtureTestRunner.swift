import Foundation
import XCTest

@testable import SwiftPEG

private let basePath =
    URL(fileURLWithPath: #file)
        .deletingLastPathComponent()

private let fixturesPath =
    basePath
        .appendingPathComponent("Fixtures")

/// Class responsible for performing static fixture test execution based on contents
/// of a pre-configured fixtures folder URL.
class FixtureTestRunner {
    static let grammarProp = "grammar"
    static let expectedParserProp = "expectedParser"
    static let expectedTokenTypeProp = "expectedTokenType"
    // Test file flags
    static let focusProp = "focus"

    let tester: XCTestCase
    let logger: FixtureTestRunnerLogger

    /// List of test results from tests executed by this runner.
    var testResults: [FixtureTestResults] = []
    /// List of fixture URLs that failed to be parsed by some reason or another
    /// during fixture collection.
    var fixtureParseFailures: [URL] = []

    init(tester: XCTestCase) {
        self.tester = tester
        self.logger = FixtureTestRunnerLogger()
    }

    /// Starts fixture testing by finding all grammar files in the expected directory
    /// path, deriving test fixtures from their contents, and running each fixture.
    func runFixtures(file: StaticString = #file, line: UInt = #line) throws {
        logger.beginLogMessageScope("SwiftPEG Test Fixtures")
        defer { logger.endLogMessageScope() }

        // Find all fixtures
        logger.beginLogMessageScope("Finding test files...")
        let files = try collectFiles()

        logger.logMessage("Found \(files.count) test file(s)")
        let allFixtures = try collectFixtures(grammarFileUrls: files)
        logger.endLogMessageScope()

        try runTests(allFixtures)

        // Log results
        let failed = testResults.filter(\.failed).count
        let parseFailures = fixtureParseFailures.count
        logger.logMessage(
            """
            Ran \(testResults.count, color: .cyan) test(s), \
            \(failureCounter: failed) failed and \
            \(failureCounter: parseFailures) parsing failure(s).
            """
        )
    }

    /// Collects all .gram test file fixtures in the `fixturesPath` directory.
    func collectFiles() throws -> [URL] {
        let files = try FileManager.default.contentsOfDirectory(at: fixturesPath, includingPropertiesForKeys: nil)
        return files.filter { file in
            file.pathExtension == "gram"
        }
    }

    /// Collects all recognized test fixtures from all of the given grammar file
    /// URLs.
    func collectFixtures(
        grammarFileUrls: [URL]
    ) throws -> [FixtureTestsFile] {

        var result: [FixtureTestsFile] = []

        for grammarFileUrl in grammarFileUrls {
            let file = try collectFixtures(grammarFileUrl: grammarFileUrl)
            result.append(file)
        }

        return result
    }

    /// Collects all recognized test fixtures from a given grammar file URL.
    func collectFixtures(
        grammarFileUrl: URL
    ) throws -> FixtureTestsFile {
        logger.beginLogMessageScope(
            "Collecting fixtures from \(url: grammarFileUrl.test_relativeURL(to: basePath))..."
        )
        defer { logger.endLogMessageScope() }

        let source = try String(contentsOf: grammarFileUrl)
        let grammar = try parse(source: source)

        let fixtures = try collectFixtures(
            from: grammar,
            grammarFileUrl: grammarFileUrl
        )
        var flags: FixtureTestsFile.Flags = []
        if grammar.test_focus() {
            flags.insert(.focus)
        }

        return FixtureTestsFile(
            file: grammarFileUrl,
            fixtures: fixtures,
            flags: flags
        )
    }

    /// Collects all recognized test fixtures from a given grammar object, parsed
    /// from a given file URL.
    func collectFixtures(
        from grammar: SwiftPEGGrammar.Grammar,
        grammarFileUrl: URL
    ) throws -> [FixtureTest] {
        // Figure out if the test grammar is the file itself or if a provided
        // grammar source should be used, instead.
        let (grammarToTest, diagnostics) = try resolveGrammar(grammar, url: grammarFileUrl)
        var fixtures: [FixtureTest] = []

        if
            let test = expectedParserTest(
                file: grammar,
                grammarToTest: grammarToTest,
                diagnosticTarget: diagnostics
            )
        {
            fixtures.append(test)
        }
        if
            let test = expectedTokenTypeTest(
                file: grammar,
                grammarToTest: grammarToTest,
                diagnosticTarget: diagnostics
            )
        {
            fixtures.append(test)
        }

        return fixtures
    }

    /// Runs all provided file fixtures.
    func runTests(_ fileFixtures: [FixtureTestsFile]) throws {
        // Detect focused files
        let focused = fileFixtures.filter({ $0.flags.contains(.focus) })
        guard focused.isEmpty else {
            for fileFixture in focused {
                try runTests(in: fileFixture)
            }
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
            let failures: [FixtureTestFailure]
            do {
                failures = try runTest(fixture)
            } catch {
                failures = [
                    .init(message: "Error running test: \(error)", file: fileFixture.file.path, line: 1)
                ]
            }

            let result = FixtureTestResults(title: fixture.title, failures: failures)
            self.testResults.append(result)
        }
    }

    /// Runs a test fixture, returning the test failures.
    func runTest(_ test: FixtureTest) throws -> [FixtureTestFailure] {
        let context = makeTestContext(for: test)

        logger.logMessage("Test \(formatted: test.title)... ", terminator: "")

        try test.testFunction(context)

        if context.failed {
            logger.logMessage("\("Failed", color: .red)")
        } else {
            logger.logMessage("\("Passed", color: .green)")
        }

        context.flushFailureMessages()

        return context.failures
    }

    /// Processes a grammar object using a `GrammarProcessor`.
    func processGrammar(_ grammar: SwiftPEGGrammar.Grammar) throws -> ProcessedGrammar {
        let processor = GrammarProcessor(delegate: self)
        return try processor.process(grammar)
    }

    // MARK: -

    /// Returns `grammar` or attempts to parse a `@grammar` meta-property, returning
    /// the result of the parsing then.
    func resolveGrammar(
        _ grammar: SwiftPEGGrammar.Grammar,
        url: URL
    ) throws -> (SwiftPEGGrammar.Grammar, LineDiagnosticTarget) {
        if
            let grammarProp = grammar.test_metaProperty(named: Self.grammarProp),
            let value = grammarProp.value?.test_valueString
        {
            let parsed = try parse(source: value)
            return (parsed, GrammarLineContext.grammarMetaProperty(self, url, grammarProp))
        }

        return (grammar, GrammarLineContext.grammar(self, url))
    }

    func parse(source: String) throws -> SwiftPEGGrammar.Grammar {
        let tokenizer = GrammarRawTokenizer(source: source)
        let parser = GrammarParser(raw: tokenizer)

        guard let grammar = try parser.grammar(), tokenizer.isEOF else {
            throw parser.makeSyntaxError()
        }

        return grammar
    }

    /// Starts a multi-line string different test case with the given parameters.
    ///
    /// Errors are reported exclusively through the provided `diagnosticTarget`.
    func diffTest(
        expected input: String,
        lineOffset: Int,
        diagnosticTarget: any LineDiagnosticTarget,
        highlightLineInEditor: Bool = true,
        diffOnly: Bool = false
    ) -> DiffingTest {

        let location = DiffLocation(file: #file, line: UInt(lineOffset))
        let diffable = DiffableString(string: input, location: location)

        return FixtureDiffingTest(
            expected: diffable,
            testCase: tester,
            highlightLineInEditor: highlightLineInEditor,
            diffOnly: diffOnly,
            target: diagnosticTarget
        )
    }

    func _line(of node: Node) -> Int {
        (node.location as! FileSourceLocation).line
    }

    /// Creates a test context for a given fixture test.
    func makeTestContext(for test: FixtureTest) -> TestContext {
        TestContext(runner: self, diagnosticsTarget: test.diagnosticTarget)
    }

    func recordFailure(_ message: String, file: String, line: Int, expected: Bool) {
        tester.recordFailure(withDescription: message, inFile: file, atLine: line, expected: expected)
    }

    /// An unexpected error raised during fixture testing.
    enum Error: Swift.Error, CustomStringConvertible {
        case message(String)

        var description: String {
            switch self {
            case .message(let message):
                return message
            }
        }
    }

    /// Wraps a grammar or grammar's meta property so diagnostic messages can
    /// be pinpointed to the correct file/line during tests.
    enum GrammarLineContext: LineDiagnosticTarget {
        case grammar(FixtureTestRunner, URL)
        case grammarMetaProperty(FixtureTestRunner, URL, SwiftPEGGrammar.Meta)

        var fileUrl: URL {
            switch self {
            case .grammar(_, let url),
                .grammarMetaProperty(_, let url, _):
                return url
            }
        }

        func diagnoseError(message: String, line: Int) {
            switch self {
            case .grammar(let runner, let url):
                runner.tester.recordFailure(
                    withDescription: message,
                    inFile: url.path,
                    atLine: line,
                    expected: true
                )
            case .grammarMetaProperty(let runner, let url, let meta):
                let lineOffset =
                    if let location = meta.location as? FileSourceLocation {
                        location.line
                    } else {
                        0
                    }

                runner.tester.recordFailure(
                    withDescription: message,
                    inFile: url.path,
                    atLine: lineOffset + line,
                    expected: true
                )
            }
        }
    }

    /// Context passed to `FixtureTest` instances during tests.
    class TestContext: FixtureTestContext, LineDiagnosticTarget {
        var runner: FixtureTestRunner
        var diagnosticsTarget: LineDiagnosticTarget
        var failures: [FixtureTestFailure] = []
        var failed: Bool { !failures.isEmpty }

        var fileUrl: URL { diagnosticsTarget.fileUrl }

        init(
            runner: FixtureTestRunner,
            diagnosticsTarget: LineDiagnosticTarget
        ) {
            self.runner = runner
            self.diagnosticsTarget = diagnosticsTarget
        }

        /// Passes all collected fixture test failure messages to `diagnosticsTarget`.
        func flushFailureMessages() {
            for failure in failures {
                diagnosticsTarget.diagnoseError(message: failure.message, line: failure.line)
            }
        }

        func diffTest(
            expected input: String,
            lineOffset: Int,
            highlightLineInEditor: Bool,
            diffOnly: Bool
        ) -> DiffingTest {

            return runner.diffTest(
                expected: input,
                lineOffset: lineOffset,
                diagnosticTarget: self,
                highlightLineInEditor: highlightLineInEditor,
                diffOnly: diffOnly
            )
        }

        func processGrammar(_ grammar: SwiftPEGGrammar.Grammar) throws -> ProcessedGrammar {
            try runner.processGrammar(grammar)
        }

        func diagnoseError(message: String, line: Int) {
            let failure = FixtureTestFailure(
                message: message,
                file: diagnosticsTarget.fileUrl.path,
                line: line
            )
            failures.append(failure)
        }

        func sourceLine(of node: Node) -> Int {
            runner._line(of: node)
        }
    }
}

extension FixtureTestRunner: GrammarProcessor.Delegate {
    func grammarProcessor(
        _ processor: GrammarProcessor,
        loadTokensFileNamed name: String,
        ofGrammar grammar: SwiftPEGGrammar.Grammar
    ) throws -> String {

        let url = fixturesPath.appendingPathComponent(name)
        return try String(contentsOf: url)
    }
}

// MARK: - Test internals

private extension SwiftPEGGrammar.Grammar {
    /// `@grammar <value>`
    func test_grammar() -> String? {
        return test_stringOrIdentMetaValue(named: FixtureTestRunner.grammarProp)
    }

    /// `@focus`
    func test_focus() -> Bool {
        return test_metaProperty(named: FixtureTestRunner.focusProp) != nil
    }
}
