import Foundation
import Testing

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

    let logger: FixtureTestRunnerLogger

    /// List of test results from tests executed by this runner.
    var testResults: [FixtureTestResults] = []
    /// List of fixture URLs that failed to be parsed by some reason or another
    /// during fixture collection.
    var fixtureParseFailures: [URL] = []

    init() {
        self.logger = FixtureTestRunnerLogger()
    }

    /// Starts fixture testing by finding all grammar files in the expected directory
    /// path, deriving test fixtures from their contents, and running each fixture.
    func runFixtures(file: StaticString = #file, line: UInt = #line) throws {
        defer { logger.logResetToStandardOutput() }

        logger.beginLogMessageScope("SwiftPEG Test Fixtures")
        defer { logger.endLogMessageScope() }

        // Find all fixtures
        logger.beginLogMessageScope("Finding test files...")
        let files = try collectFiles()

        logger.logMessage("Found \(files.count) test file(s)")
        for file in files {
            logger.beginLogMessageScope(
                "\(url: file.test_relativeURL(to: basePath))"
            )
            logger.endLogMessageScope()
        }
        logger.endLogMessageScope()

        logger.beginLogMessageScope("Parsing test fixtures...")

        let allFixtures = try collectFixtures(grammarFileUrls: files)

        logger.endLogMessageScope()

        let stopwatch = Stopwatch.startNew()

        try runTests(allFixtures)

        // Log results
        let failed = testResults.filter(\.hasFailures).count
        let parseFailures = fixtureParseFailures.count
        logger.logMessage(
            """
            Ran \(testResults.count, color: .cyan) test(s), \
            \(failureCounter: failed) failed and \
            \(failureCounter: parseFailures) parsing failure(s) in \(elapsed: stopwatch).
            """
        )
    }

    /// Collects all .gram test file fixtures in the `fixturesPath` directory.
    func collectFiles() throws -> [URL] {
        guard
            let enumerator = FileManager.default.enumerator(
                at: fixturesPath,
                includingPropertiesForKeys: [.isSymbolicLinkKey, .isDirectoryKey],
                options: .skipsHiddenFiles
            )
        else {
            return []
        }

        var result: [URL] = []

        for case let url as URL in enumerator {
            if (try? url.resourceValues(forKeys: [.isSymbolicLinkKey]))?.isDirectory == true {
                continue
            }
            if (try? url.resourceValues(forKeys: [.isSymbolicLinkKey]))?.isSymbolicLink == true {
                continue
            }
            guard url.pathExtension == "gram" else {
                continue
            }

            result.append(url)
        }

        return result
    }

    /// Collects all recognized test fixtures from all of the given grammar file
    /// URLs.
    func collectFixtures(
        grammarFileUrls: [URL]
    ) throws -> [FixtureTestsFile] {

        var result: [FixtureTestsFile] = []

        /*
        for grammarFileUrl in grammarFileUrls {
            let file = try self.collectFixtures(grammarFileUrl: grammarFileUrl)
            result.append(file)
        }
        // */

        //*
        var _error: Swift.Error?
        let queue = OperationQueue()

        for grammarFileUrl in grammarFileUrls {
            queue.addOperation {
                do {
                    let file = try self.collectFixtures(grammarFileUrl: grammarFileUrl)
                    queue.addBarrierBlock {
                        result.append(file)
                    }
                } catch {
                    queue.addBarrierBlock {
                        _error = _error ?? error
                    }
                }
            }
        }

        queue.waitUntilAllOperationsAreFinished()

        if let _error {
            throw _error
        }
        // */

        return result
    }

    /// Collects all recognized test fixtures from a given grammar file URL.
    func collectFixtures(
        grammarFileUrl: URL
    ) throws -> FixtureTestsFile {
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
            grammar: grammar,
            fixtures: fixtures,
            flags: flags
        )
    }

    /// Processes a grammar object using a `GrammarProcessor`.
    func processGrammar(
        _ grammar: SwiftPEGGrammar.Grammar,
        basePath: URL
    ) throws -> ProcessedGrammar {
        let delegate = GrammarProcessorDelegate(basePath: basePath)
        let processor = GrammarProcessor(delegate: delegate)

        return try withExtendedLifetime(delegate) {
            return try processor.process(grammar)
        }
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
            let value = grammarProp.values.first?.test_valueString
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

        let location = DiffLocation(sourceLocation: .init(fileID: "", filePath: #file, line: lineOffset, column: 0))
        let diffable = DiffableString(string: input, location: location)

        return FixtureDiffingTest(
            expected: diffable,
            testCase: SwiftTestingDiffTestCaseFailureReporter(),
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

    func recordFailure(_ message: String, file: URL, line: Int, expected: Bool) {
        recordFailure(message, file: file.path, line: line, expected: expected)
    }

    func recordFailure(_ message: String, file: String, line: Int, expected: Bool) {
        //tester.recordFailure(withDescription: message, inFile: file, atLine: line, expected: expected)
        Issue.record(
            Comment(stringLiteral: message),
            sourceLocation: .init(
                fileID: #fileID,
                filePath: file,
                line: line,
                column: 0
            )
        )
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
            case .grammar(_, let url):
                Issue.record(
                    Comment(stringLiteral: message),
                    sourceLocation: .init(
                        fileID: #fileID,
                        filePath: url.path,
                        line: line,
                        column: 0
                    )
                )

            case .grammarMetaProperty(_, let url, let meta):
                let lineOffset =
                    if let location = meta.location as? FileSourceLocation {
                        location.line
                    } else {
                        0
                    }

                Issue.record(
                    Comment(stringLiteral: message),
                    sourceLocation: .init(
                        fileID: #fileID,
                        filePath: url.path,
                        line: lineOffset + line,
                        column: 0
                    )
                )
            }
        }
    }

    /// Context passed to `FixtureTest` instances during tests.
    class TestContext: FixtureTestContext, LineDiagnosticTarget {
        var runner: FixtureTestRunner
        var diagnosticsTarget: LineDiagnosticTarget
        var failures: [FixtureTestFailure] = []
        var hasFailures: Bool { !failures.isEmpty }

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
            try runner.processGrammar(
                grammar,
                basePath: diagnosticsTarget.fileUrl.deletingLastPathComponent()
            )
        }

        func diagnoseError(message: String, line: Int) {
            let failure = FixtureTestFailure(
                message: message,
                file: diagnosticsTarget.fileUrl,
                line: line
            )
            failures.append(failure)
        }

        func sourceLine(of node: Node) -> Int {
            runner._line(of: node)
        }
    }

    class GrammarProcessorDelegate: GrammarProcessor.Delegate {
        let basePath: URL

        init(basePath: URL) {
            self.basePath = basePath
        }

        func grammarProcessor(
            _ processor: GrammarProcessor,
            loadTokensFileNamed name: String,
            ofGrammar grammar: SwiftPEGGrammar.Grammar
        ) throws -> String {

            let url = basePath.appendingPathComponent(name)
            return try String(contentsOf: url)
        }

        func grammarProcessor(
            _ processor: GrammarProcessor,
            importFileNamed name: String,
            ofGrammar grammar: SwiftPEGGrammar.Grammar
        ) throws -> String {
            let url = basePath.appendingPathComponent(name)
            return try String(contentsOf: url)
        }
    }
}

// MARK: - Test internals

extension SwiftPEGGrammar.Grammar {
    /// `@grammar <value>`
    func test_grammar() -> String? {
        return test_stringOrIdentMetaValue(named: FixtureTestRunner.grammarProp)
    }

    /// `@focus`
    func test_focus() -> Bool {
        return test_focus_meta() != nil
    }

    /// `@focus`
    func test_focus_meta() -> SwiftPEGGrammar.Meta? {
        return test_metaProperty(named: FixtureTestRunner.focusProp)
    }
}
