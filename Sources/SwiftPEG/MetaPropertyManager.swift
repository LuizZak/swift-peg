/// Class used to more precisely manage meta properties in SwiftPEG grammars during
/// grammar processing.
public class MetaPropertyManager {
    var knownProperties: [KnownProperty] = []
    var metaProperties: [MetaProperty] = []

    var diagnostics: [Diagnostic] = []

    func registerKnownProperty(_ property: KnownProperty) {
        knownProperties.append(property)
    }

    /// Resolves meta-properties by loading them from a given grammar.
    func resolve(from grammar: SwiftPEGGrammar.Grammar) {
        for property in grammar.metas.map(MetaProperty.from) {
            add(property)
        }
    }

    func add(_ property: MetaProperty) {
        metaProperties.append(property)

        if let known = resolveAsKnown(property) {
            validate(property, known)
        }
    }

    func resolveAsKnown(_ property: MetaProperty) -> KnownProperty? {
        knownProperties.first(where: { $0.name == property.name })
    }

    func validate(_ property: MetaProperty, _ knownProperty: KnownProperty) {
        // Validate values
        if
            !knownProperty.acceptedValues.isEmpty,
            !knownProperty.acceptedValues.contains(where: { $0.accepts(property.value) })
        {
            diagnostics.append(
                .unexpectedValue(property, expected: knownProperty.acceptedValues)
            )
        }
    }

    /// A non-error diagnostic that is raised based on properties of known meta-property
    /// values.
    enum Diagnostic: CustomStringConvertible {
        /// A meta-property was defined multiple times when it was described to
        /// not be.
        case repeatedDefinitions(firstDefinition: MetaProperty, MetaProperty)

        /// A meta-property was defined with a value that differed from the expected
        /// kind.
        case unexpectedValue(MetaProperty, expected: [KnownProperty.AcceptedValue])

        var description: String {
            switch self {
            case .repeatedDefinitions(let firstDefinition, let other):
                return "Meta-property '\(other.name)' @ \(other.node.location) has been declared at least once @ \(firstDefinition.node.location)."

            case .unexpectedValue(let property, let expected):
                return ""
            }
        }
    }

    /// An evaluated meta-property value from a grammar file.
    struct MetaProperty {
        var node: SwiftPEGGrammar.Meta
        var name: String
        var value: Value

        static func from(_ node: SwiftPEGGrammar.Meta) -> Self {
            return .init(
                node: node,
                name: String(node.name.processedString),
                value: .from(node.value)
            )
        }

        enum Value: Equatable {
            case none
            case string(String)
            case identifier(String)

            static func from(_ node: SwiftPEGGrammar.MetaValue?) -> Self {
                switch node {
                case let node as SwiftPEGGrammar.MetaStringValue:
                    return .string(String(node.string.processedString))
                case let node as SwiftPEGGrammar.MetaIdentifierValue:
                    return .identifier(String(node.identifier.processedString))
                default:
                    return .none
                }
            }
        }
    }

    /// A static description of a known meta-property that can be defined in
    /// code.
    struct KnownProperty {
        /// The name of the meta property.
        var name: String

        /// A description of the meta property.
        var description: String

        /// A typed confirmation of the meta property's accepted values,
        /// including descriptions, if available.
        /// 
        /// If the array is empty, it equates to `AcceptedValue.none`.
        var acceptedValues: [AcceptedValue]

        /// The repeat mode acceptable for the described meta-property.
        var repeatMode: RepeatMode

        enum AcceptedValue: Equatable {
            /// No value, i.e. `@metaPropertyName ;`.
            case none

            /// A string literal value.
            case string(description: String? = nil)
            
            /// An identifier value.
            case identifier(description: String? = nil)
            
            /// A special case of an identifier that resolves to `true` or `false`.
            case boolean(description: String? = nil)

            var description: String {
                switch self {
                case .none:
                    return "No value"

                case .boolean(let description):
                    return "Identifiers or strings 'true' or 'false'. \(description ?? "")"
                
                case .string(let description):
                    return "A single, double, or triple-quoted string. \(description ?? "")"

                case .identifier(let description):
                    return "An identifier. \(description ?? "")"
                }
            }

            func accepts(_ value: MetaProperty.Value) -> Bool {
                switch self {
                case .none:
                    return value == .none

                case .boolean:
                    return value == .identifier("true")
                        || value == .identifier("false")
                        || value == .string("true")
                        || value == .string("false")
                
                case .string:
                    switch value {
                    case .string: return true
                    default: return false
                    }

                case .identifier:
                    switch value {
                    case .identifier: return true
                    default: return false
                    }
                }
            }
        }

        /// Describes the accepted repetition modes of a meta-property.
        enum RepeatMode {
            /// The meta-property can only be defined once per file; the results
            /// of defining them multiple times is dependant on the behavior of
            /// grammar processors and code generators.
            case never

            /// The meta-property can be defined multiple times, with distinct
            /// values each time.
            case distinctValues

            /// The meta-property can be defined multiple times, even with the
            /// same values.
            case always
        }
    }
}
