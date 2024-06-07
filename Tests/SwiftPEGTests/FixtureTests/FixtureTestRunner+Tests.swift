import Foundation

@testable import SwiftPEG

extension FixtureTestRunner {
    /// Collects all recognized test fixtures from a given grammar object, parsed
    /// from a given file URL.
    func collectFixtures(
        from grammar: SwiftPEGGrammar.Grammar,
        grammarFileUrl: URL
    ) throws -> [FixtureTest] {
        // Figure out if the test grammar is the file itself or if a provided
        // grammar source should be used, instead.
        let (grammarToTest, diagnostics) = try resolveGrammar(
            grammar,
            url: grammarFileUrl
        )
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

    /// Produces an `@expectedParser <value>` test based on a meta-property of
    /// a given grammar, along with a grammar to use to generate the parser code
    /// to test against.
    ///
    /// If `file` has no `@expectedParser` meta-property, `nil` is returned,
    /// instead.
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

        let target = GrammarLineContext.grammarMetaProperty(
            self,
            diagnosticTarget.fileUrl,
            expectedParserProp
        )

        return FixtureTest(title: "\(name: expectedParserProp)", diagnosticTarget: target) { context in
            let processed = try context.processGrammar(grammarToTest)
            let codeGen = SwiftCodeGen(from: processed)
            let settings = file.test_parserSettings()

            let parser = try codeGen.generateParser(settings: settings).trimmingWhitespaceTrail()

            context.diffTest(
                expected: value,
                lineOffset: 0
            ).diff(parser)
        }
    }

    /// Produces an `@expectedTokenType <value>` test based on a meta-property of
    /// a given grammar, along with a grammar to use to generate the token type
    /// code to test against.
    ///
    /// If `file` has no `@expectedTokenType` meta-property, `nil` is returned,
    /// instead.
    func expectedTokenTypeTest(
        file: SwiftPEGGrammar.Grammar,
        grammarToTest: SwiftPEGGrammar.Grammar,
        diagnosticTarget: any LineDiagnosticTarget
    ) -> FixtureTest? {

        guard let expectedTokenTypeProp = file.test_metaProperty(named: Self.expectedTokenTypeProp) else {
            return nil
        }
        guard let value = expectedTokenTypeProp.value?.test_valueString?.trimmingWhitespaceTrail() else {
            return nil
        }

        let target = GrammarLineContext.grammarMetaProperty(
            self,
            diagnosticTarget.fileUrl,
            expectedTokenTypeProp
        )

        return FixtureTest(title: "\(name: expectedTokenTypeProp)", diagnosticTarget: target) { context in
            let processed = try context.processGrammar(grammarToTest)
            let codeGen = SwiftCodeGen(from: processed)
            let settings = file.test_tokenTypeSettings()

            let parser = try codeGen.generateTokenType(settings: settings).trimmingWhitespaceTrail()

            context.diffTest(
                expected: value,
                lineOffset: 0
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

    /// `@omitRedundantMarkRestores <true/false>`
    func test_parserSettings() -> SwiftCodeGen.ParserGenSettings {
        var settings = SwiftCodeGen.ParserGenSettings.default

        if let omitRedundantMarkRestores = test_stringOrIdentMetaValue(named: "omitRedundantMarkRestores") {
            settings.omitRedundantMarkRestores = omitRedundantMarkRestores == "true"
        }
        if let emitTypesInBindings = test_stringOrIdentMetaValue(named: "emitTypesInBindings") {
            settings.emitTypesInBindings = emitTypesInBindings == "true"
        }

        return settings
    }

    /// Currently empty; might contain support for token type generation settings
    /// later.
    func test_tokenTypeSettings() -> SwiftCodeGen.TokenTypeGenSettings {
        let settings = SwiftCodeGen.TokenTypeGenSettings.default
        return settings
    }
}
