import Testing

@testable import SwiftPEG

struct MetaPropertyManagerTests {
    @Test
    func diagnoseValue_acceptedValues_none_withValues() throws {
        let known = [
            makeKnown(name: "meta1", acceptedValues: []),
            makeKnown(name: "meta2", acceptedValues: [.none]),
        ]
        let metaProperties = try parseMetaProperties("""
        @meta1 a;
        @meta2 "b";
        """)
        let sut = makeSut(known)
        sut.add(contentsOf: metaProperties)

        sut.validateAll()

        assertDiagnosedMeta(sut, meta: metaProperties[0])
        assertDiagnosedMeta(sut, meta: metaProperties[1])
    }

    @Test
    func diagnoseValue_acceptedValues_string() throws {
        let known = [
            makeKnown(name: "meta1", acceptedValues: [.string()]),
            makeKnown(name: "meta2", acceptedValues: [.string()]),
            makeKnown(name: "meta3", acceptedValues: [.string()]),
        ]
        let metaProperties = try parseMetaProperties("""
        @meta1;
        @meta2 b;
        @meta3 "a";
        """)
        let sut = makeSut(known)
        sut.add(contentsOf: metaProperties)

        sut.validateAll()

        assertDiagnosedMeta(sut, meta: metaProperties[0])
        assertDiagnosedMeta(sut, meta: metaProperties[1])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[2])
    }

    @Test
    func diagnoseValue_acceptedValues_boolean() throws {
        let known = [
            makeKnown(name: "meta1", acceptedValues: [.boolean()]),
            makeKnown(name: "meta2", acceptedValues: [.boolean()]),
            makeKnown(name: "meta3", acceptedValues: [.boolean()]),
            makeKnown(name: "meta4", acceptedValues: [.boolean()]),
            makeKnown(name: "meta5", acceptedValues: [.boolean()]),
            makeKnown(name: "meta6", acceptedValues: [.boolean()]),
            makeKnown(name: "meta7", acceptedValues: [.boolean()]),
        ]
        let metaProperties = try parseMetaProperties("""
        @meta1;
        @meta2 "t";
        @meta3 t;
        @meta4 true;
        @meta5 false;
        @meta6 "true";
        @meta7 "false";
        """)
        let sut = makeSut(known)
        sut.add(contentsOf: metaProperties)

        sut.validateAll()

        assertDiagnosedMeta(sut, meta: metaProperties[0])
        assertDiagnosedMeta(sut, meta: metaProperties[1])
        assertDiagnosedMeta(sut, meta: metaProperties[2])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[3])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[4])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[5])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[6])
    }

    @Test
    func diagnoseValue_acceptedValues_mixedKinds() throws {
        let known = [
            makeKnown(name: "meta1", acceptedValues: [.boolean(), .string()], repeatMode: .always),
            makeKnown(name: "meta2", acceptedValues: [.none, .identifier()], repeatMode: .always),
        ]
        let metaProperties = try parseMetaProperties("""
        @meta1;
        @meta1 a;
        @meta1 true;
        @meta1 "notBoolean";

        @meta2 "string";
        @meta2;
        @meta2 b;
        """)
        let sut = makeSut(known)
        sut.add(contentsOf: metaProperties)

        sut.validateAll()

        assertDiagnosedMeta(sut, meta: metaProperties[0])
        assertDiagnosedMeta(sut, meta: metaProperties[1])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[2])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[3])
        assertDiagnosedMeta(sut, meta: metaProperties[4])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[5])
        assertDidNotDiagnoseMeta(sut, meta: metaProperties[6])
    }

    // MARK: Repeat mode

    @Test
    func diagnoseRepeated_always() throws {
        let known = [
            makeKnown(name: "meta", repeatMode: .always),
        ]
        let metaProperties = try parseMetaProperties("""
        @meta a;
        @meta b;
        @meta a;
        @meta a;
        """)
        let sut = makeSut(known)
        sut.add(contentsOf: metaProperties)

        sut.validateAll()

        assertNoDiagnostics(sut)
    }

    @Test
    func diagnoseRepeated_distinctValues_reportsAgainstFirstMetaPropertyOnly() throws {
        let known = [
            makeKnown(name: "meta", repeatMode: .distinctValues),
        ]
        let metaProperties = try parseMetaProperties("""
        @meta a;
        @meta b;
        @meta a;
        @meta a;
        """)
        let sut = makeSut(known)
        sut.add(contentsOf: metaProperties)

        sut.validateAll()

        assertDiagnosticCount(sut, expected: 2) { diag in
            switch diag {
            case .repeatedDefinitions(let first, let repeated)
                where first == metaProperties[0] && repeated == metaProperties[2]:
                return true
            case .repeatedDefinitions(let first, let repeated)
                where first == metaProperties[0] && repeated == metaProperties[3]:
                return true

            default:
                return false
            }
        }
    }
}

// MARK: - Test internals

private func makeSut(_ metaProperties: [MetaPropertyManager.KnownProperty] = []) -> MetaPropertyManager {
    let sut = MetaPropertyManager()
    for property in metaProperties {
        sut.registerKnownProperty(property)
    }
    return sut
}

private func makeKnown(
    name: String,
    description: String = "",
    acceptedValues: [MetaPropertyManager.KnownProperty.AcceptedValue] = MetaPropertyManager.KnownProperty.AcceptedValue.any,
    repeatMode: MetaPropertyManager.KnownProperty.RepeatMode = .always
) -> MetaPropertyManager.KnownProperty {

    .init(
        name: name,
        propertyDescription: description,
        acceptedValues: acceptedValues,
        repeatMode: repeatMode
    )
}

private func parseMetaProperties(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> [SwiftPEGGrammar.Meta] {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let metas = try parser.repeatOneOrMore({ try parser.meta() }) else {
        throw parser.makeSyntaxError()
    }

    return metas
}

private func assertNoDiagnostics(
    _ sut: MetaPropertyManager,
    sourceLocation: SourceLocation = #_sourceLocation
) {

    assertEmpty(
        sut.diagnostics,
        message: "Unexpected diagnostics",
        sourceLocation: sourceLocation
    )
}

private func assertDiagnosticCount(
    _ sut: MetaPropertyManager,
    expected: Int,
    matching predicate: (MetaPropertyManager.Diagnostic) -> Bool,
    sourceLocation: SourceLocation = #_sourceLocation
) {

    let diagnostics = sut.diagnostics.filter(predicate)
    let count = diagnostics.count

    assertEqual(
        count,
        expected,
        message: "Did not match expected diagnostics. All diagnostics: \(sut.diagnostics)",
        sourceLocation: sourceLocation
    )
}

private func assertDidNotDiagnoseMeta(
    _ sut: MetaPropertyManager,
    meta: SwiftPEGGrammar.Meta,
    sourceLocation: SourceLocation = #_sourceLocation
) {

    let diagnostics = sut.diagnostics.filter({ diag in
        switch diag {
        case .repeatedDefinitions(let metaProp1, let metaProp2):
            return metaProp1 == meta || metaProp2 == meta
        case .unexpectedValue(let metaProp, _):
            return metaProp == meta
        }
    })

    if !diagnostics.isEmpty {
        fail(
            "Expected no diagnostics for meta @\(meta.name.string) but found \(diagnostics). All diagnostics: \(sut.diagnostics)",
            sourceLocation: sourceLocation
        )
    }
}

private func assertDiagnosedMeta(
    _ sut: MetaPropertyManager,
    meta: SwiftPEGGrammar.Meta,
    count: Int? = nil,
    sourceLocation: SourceLocation = #_sourceLocation
) {

    let diagnostics = sut.diagnostics.filter({ diag in
        switch diag {
        case .repeatedDefinitions(let metaProp1, let metaProp2):
            return metaProp1 == meta || metaProp2 == meta
        case .unexpectedValue(let metaProp, _):
            return metaProp == meta
        }
    })

    if let count {
        if count != diagnostics.count {
            fail(
                "Expected \(count) diagnostics for meta @\(meta.name.string) but found \(diagnostics.count). All diagnostics: \(sut.diagnostics)",
                sourceLocation: sourceLocation
            )
        }
        return
    }

    if diagnostics.isEmpty {
        fail(
            "Expected diagnostics for meta @\(meta.name.string) but found none. All diagnostics: \(sut.diagnostics)",
            sourceLocation: sourceLocation
        )
    }
}

private extension MetaPropertyManager.MetaProperty {
    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.node === rhs.node && lhs.name == rhs.name && lhs.value == rhs.value
    }

    static func == (lhs: Self, rhs: SwiftPEGGrammar.Meta) -> Bool {
        lhs.node === rhs
    }

    static func == (lhs: SwiftPEGGrammar.Meta, rhs: Self) -> Bool {
        lhs === rhs.node
    }
}
