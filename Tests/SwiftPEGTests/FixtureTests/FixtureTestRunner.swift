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

    let tester: XCTestCase
    let logger: FixtureTestRunnerLogger

    /// List of tests executed by this runner.
    var tests: [FixtureTest] = []

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

        let files = try FileManager.default.contentsOfDirectory(at: fixturesPath, includingPropertiesForKeys: nil)

        logger.logMessage("Found \(files.count) test file(s)")
        logger.endLogMessageScope()

        try runFixtures(files)

        // Log results
        let failed = tests.filter(\.failed).count
        logger.logMessage("Ran \(tests.count, color: .cyan) test(s), \(failed, color: failed == 0 ? .green : .red) failed.")
    }

    /// Runs test fixtures detected from all of the provided grammar file URLs.
    func runFixtures(_ grammarFileUrls: [URL]) throws {
        logger.beginLogMessageScope("Running test fixtures...")
        defer { logger.endLogMessageScope() }

        for url in grammarFileUrls {
            do {
                try runFixture(url)
            } catch {
                recordFailure(
                    "Error parsing grammar file: \(error)",
                    file: url.path,
                    line: 1,
                    expected: false
                )
            }
        }
    }

    /// Runs test fixtures detected from a given grammar file URL.
    func runFixture(_ grammarFileUrl: URL) throws {
        logger.beginLogMessageScope(
            "Running \(url: grammarFileUrl.test_relativeURL(to: basePath))..."
        )
        defer { logger.endLogMessageScope() }

        let source = try String(contentsOf: grammarFileUrl)
        let grammar = try parse(source: source)

        let fixtures = try collectFixtures(
            from: grammar,
            grammarFileUrl: grammarFileUrl
        )

        for var fixture in fixtures {
            fixture.failures = try runTest(fixture)
            self.tests.append(fixture)
        }
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

        return fixtures
    }

    /// Runs a test fixture, returning the test failures.
    func runTest(_ test: FixtureTest) throws -> [FixtureTestFailure] {
        let context = makeTestContext(for: test)

        logger.logMessage("Running \(formatted: test.title) test... ", terminator: "")

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
}
