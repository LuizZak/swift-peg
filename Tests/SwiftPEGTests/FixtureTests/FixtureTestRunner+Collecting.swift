@testable import SwiftPEG

extension FixtureTestRunner {
    /// Produces an `@expectedParser <value>` test based on a meta-property of
    /// a given grammar, along with a grammar to use to generate the parser code
    /// to test against.
    ///
    /// If `file` has no `@expectedParser` meta-property, `nil` is returned, instead.
    func expectedParserTest(
        file: SwiftPEGGrammar.Grammar,
        grammarToTest: SwiftPEGGrammar.Grammar,
        diagnosticTarget: any LineDiagnosticTarget
    ) -> FixtureTest? {

        guard let expectedParserProp = file.test_metaProperty(named: Self.expectedParserProp) else {
            return nil
        }
        guard let value = expectedParserProp.value?.test_valueString?.trimmingWhitespaceTrail() else {
            return nil
        }

        return FixtureTest(title: "\(name: expectedParserProp)", diagnosticTarget: diagnosticTarget) { context in
            let processed = try context.processGrammar(grammarToTest)
            let codeGen = SwiftCodeGen(from: processed)

            let parser = try codeGen.generateParser().trimmingWhitespaceTrail()

            context.diffTest(
                expected: value,
                lineOffset: context.sourceLine(of: expectedParserProp) - 1
            ).diff(parser)
        }
    }
}

extension SwiftPEGGrammar.Grammar {
    /// `@expectedParser <value>`
    func test_expectedParser() -> String? {
        return test_stringOrIdentMetaValue(named: FixtureTestRunner.expectedParserProp)
    }

    /// `@expectedTokenType <value>`
    func test_expectedTokenType() -> String? {
        return test_stringOrIdentMetaValue(named: FixtureTestRunner.expectedTokenTypeProp)
    }
}
