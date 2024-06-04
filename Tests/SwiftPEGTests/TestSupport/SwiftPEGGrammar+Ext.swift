import SwiftPEG

extension SwiftPEGGrammar.Grammar {
    /// Fetches a meta property by name within this grammar.
    func test_metaProperty(named name: String) -> SwiftPEGGrammar.Meta? {
        metas.first(where: { $0.name.string == name })
    }

    /// Fetches a meta property by name and returns its value with
    /// `SwiftPEGGrammar.MetaValue.test_valueString`.
    func test_stringOrIdentMetaValue(named name: String) -> String? {
        guard let meta = test_metaProperty(named: name) else {
            return nil
        }

        return meta.value?.test_valueString
    }
}

extension SwiftPEGGrammar.MetaValue {
    /// Returns the identifier name or string contents of this meta-property
    /// value instance.
    var test_valueString: String? {
        switch self {
        case let value as SwiftPEGGrammar.MetaIdentifierValue:
            return String(value.identifier.string)

        case let value as SwiftPEGGrammar.MetaStringValue:
            return value.string.rawContents()

        default:
            return nil
        }
    }
}
