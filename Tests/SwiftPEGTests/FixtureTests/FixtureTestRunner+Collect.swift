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
        guard let value = expectedParserProp.values.first?.test_valueString?.trimmingWhitespaceTrail() else {
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
            let shortMessages = file.test_shortMessages()

            file.test_configureCodeGen(codeGen)

            let parser = try codeGen.generateParser(settings: settings).trimmingWhitespaceTrail()

            context.diffTest(
                expected: value,
                lineOffset: 0,
                diffOnly: shortMessages
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
        guard let value = expectedTokenTypeProp.values.first?.test_valueString?.trimmingWhitespaceTrail() else {
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
            let shortMessages = file.test_shortMessages()

            file.test_configureCodeGen(codeGen)

            let parser = try codeGen.generateTokenType(settings: settings).trimmingWhitespaceTrail()

            context.diffTest(
                expected: value,
                lineOffset: 0,
                diffOnly: shortMessages
            ).diff(parser)
        }
    }
}

extension SwiftPEGGrammar.Grammar {
    /// `@shortMessages`
    func test_shortMessages() -> Bool {
        return test_metaProperty(named: "shortMessages") != nil
    }

    /// `@expectedParser <value>`
    func test_expectedParser() -> String? {
        return test_stringOrIdentMetaValue(named: FixtureTestRunner.expectedParserProp)
    }

    /// `@expectedTokenType <value>`
    func test_expectedTokenType() -> String? {
        return test_stringOrIdentMetaValue(named: FixtureTestRunner.expectedTokenTypeProp)
    }

    /// `@omitRedundantMarkRestores <true/false>`
    /// `@emitTypesInBindings <true/false>`
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

    /// `@implicitBindings <true/false>`
    /// `@implicitReturns <true/false>`
    func test_configureCodeGen(_ codeGen: SwiftCodeGen) {
        if let implicitBindings = test_stringOrIdentMetaValue(named: "implicitBindings") {
            codeGen.implicitBindings = implicitBindings == "true"
        }
        if let implicitReturns = test_stringOrIdentMetaValue(named: "implicitReturns") {
            codeGen.implicitReturns = implicitReturns == "true"
        }
    }

    /// `@expectedTokenType_accessLevel <true/false>`
    /// `@expectedTokenType_emitInlinable <true/false>`
    /// `@expectedTokenType_emitLengthSwitchPhaseInTokenOcclusionSwitch <true/false>`
    func test_tokenTypeSettings() -> SwiftCodeGen.TokenTypeGenSettings {
        var settings = SwiftCodeGen.TokenTypeGenSettings.default

        if let accessLevel = test_stringOrIdentMetaValue(named: "expectedTokenType_accessLevel") {
            settings.accessLevel = accessLevel
        }
        if let emitInlinable = test_stringOrIdentMetaValue(named: "expectedTokenType_emitInlinable") {
            settings.emitInlinable = emitInlinable == "true"
        }
        if let emitLengthSwitchPhaseInTokenOcclusionSwitch = test_stringOrIdentMetaValue(named: "expectedTokenType_emitLengthSwitchPhaseInTokenOcclusionSwitch") {
            settings.emitLengthSwitchPhaseInTokenOcclusionSwitch = emitLengthSwitchPhaseInTokenOcclusionSwitch == "true"
        }

        return settings
    }
}
