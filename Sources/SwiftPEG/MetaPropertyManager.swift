/// Class used to more precisely manage meta properties in SwiftPEG grammars during
/// grammar processing.
class MetaPropertyManager {
    var knownProperties: [KnownProperty] = []
    var metaProperties: [MetaProperty] = []

    var diagnostics: [Diagnostic] = []

    /// Registers a new known meta property into this meta property manager.
    ///
    /// If a meta-property has been previously been registered with the same
    /// name but different configurations, a fatal error is raised.
    func registerKnownProperty(_ knownProperty: KnownProperty) {
        for known in knownProperties {
            if known.name == knownProperty.name && known != knownProperty {
                fatalError("Known property registered before with different values: \(known) New: \(knownProperty)")
            }
        }

        knownProperties.append(knownProperty)
    }

    /// Resolves meta-properties by loading them from a given grammar.
    func resolve(from grammar: SwiftPEGGrammar.Grammar) {
        for property in grammar.metas.map(MetaProperty.from) {
            add(property)
        }
    }

    /// Adds a meta-property to this manager.
    func add(_ node: SwiftPEGGrammar.Meta) {
        add(MetaProperty.from(node))
    }

    /// Adds a range of meta-properties to this manager.
    func add(contentsOf properties: some Sequence<SwiftPEGGrammar.Meta>) {
        self.add(contentsOf: properties.map(MetaProperty.from))
    }

    /// Adds a meta-property to this manager.
    func add(_ property: MetaProperty) {
        metaProperties.append(property)
    }

    /// Adds a range of meta-properties to this manager.
    func add(contentsOf properties: some Sequence<MetaProperty>) {
        metaProperties.append(contentsOf: properties)
    }

    /// Validates all collected meta-properties, cross-referencing them with
    /// known meta-properties to produce diagnostics about their usage.
    func validateAll() {
        // Validate values
        for property in metaProperties {
            if let known = resolveAsKnown(property) {
                validateValue(property, known)
            }
        }

        // Validate repetition
        for knownProperty in knownProperties {
            let properties = propertyValues(fromKnown: knownProperty)

            switch knownProperty.repeatMode {
            case .always:
                continue

            case .distinctValues:
                validateDistinctValues(properties)

            case .never:
                for remaining in properties.dropFirst() {
                    diagnoseRepeated(original: properties[0], remaining)
                }
            }
        }
    }

    /// Matches a given meta-property with a known meta-property by name.
    func resolveAsKnown(_ property: MetaProperty) -> KnownProperty? {
        knownProperties.first(where: { $0.name == property.name })
    }

    /// Returns the meta-property with a given name in this manager.
    /// Returns `nil` if it hasn't been registered with `add()` beforehand.
    func property(ofName name: String) -> MetaProperty? {
        metaProperties.first(where: { $0.name == name })
    }

    /// Returns all meta-properties within this manager that are associated with
    /// a given known property by name.
    /// Returns an empty array if none have been registered with `add()` beforehand.
    func propertyValues(fromKnown knownProperty: KnownProperty) -> [MetaProperty] {
        metaProperties.filter { $0.name == knownProperty.name }
    }

    private func validateValue(_ property: MetaProperty, _ knownProperty: KnownProperty) {
        let acceptedValues = knownProperty.effectiveAccepted
        if !acceptedValues.contains(where: { $0.accepts(property.value) }) {
            diagnostics.append(
                .unexpectedValue(property, expected: knownProperty.acceptedValues)
            )
        }
    }

    private func validateDistinctValues(_ properties: [MetaProperty]) {
        let byValue = Dictionary(grouping: properties, by: \.value)

        for properties in byValue.values where properties.count > 1 {
            let original = properties[0]

            for remaining in properties.dropFirst() {
                diagnoseRepeatedValue(original: original, remaining)
            }
        }
    }

    private func diagnoseRepeatedValue(original: MetaProperty, _ next: MetaProperty) {
        diagnostics.append(.repeatedDefinitions(firstDefinition: original, next))
    }

    private func diagnoseRepeated(original: MetaProperty, _ next: MetaProperty) {
        diagnostics.append(.repeatedDefinitions(firstDefinition: original, next))
    }

    static func makeAcceptedValueDescription(_ expected: [KnownProperty.AcceptedValue]) -> String {
        func bufferForValue(_ value: KnownProperty.AcceptedValue) -> String {
            func terminate(_ suffix: String?) -> String {
                if let suffix { ": \(suffix)" }
                else { "." }
            }

            switch value {
            case .none:
                return "No value"
            case .string(let description):
                return "A string\(terminate(description))"

            case .identifier(let description):
                return "An identifier\(terminate(description))"

            case .boolean(let description):
                return "A string or identifier of 'true'/'false' value\(terminate(description))"
            }
        }
        if expected.isEmpty {
            return makeAcceptedValueDescription([.none])
        }
        if expected.count == 1 {
            return bufferForValue(expected[0])
        }

        return expected.map(bufferForValue(_:)).asNaturalLanguageList(options: .caseAware)
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
                return "Unexpected value '\(property.value)' for @\(property.name): expected: \(MetaPropertyManager.makeAcceptedValueDescription(expected))"
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

        enum Value: Hashable, CustomStringConvertible {
            case none
            case string(String)
            case identifier(String)

            var description: String {
                switch self {
                case .none: "<empty>"
                case .string(let value): #""\#(value)""#
                case .identifier(let value): value
                }
            }

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
    struct KnownProperty: Equatable, CustomStringConvertible {
        /// The name of the meta property.
        var name: String

        /// A description of the meta property.
        var propertyDescription: String

        /// A typed confirmation of the meta property's accepted values,
        /// including descriptions, if available.
        /// 
        /// If the array is empty, it equates to `AcceptedValue.none`.
        var acceptedValues: [AcceptedValue]

        /// The repeat mode acceptable for the described meta-property.
        var repeatMode: RepeatMode

        var description: String {
            "KnownProperty(name: \(name), propertyDescription: \(propertyDescription), acceptedValues: \(acceptedValues), repeatMode: \(repeatMode))"
        }

        /// Returns an always non-empty list of accepted values.
        /// If the list of accepted values is empty, `[AcceptedValue.none]` is
        /// returned, instead.
        internal var effectiveAccepted: [AcceptedValue] {
            if acceptedValues.isEmpty { return [.none] }
            return acceptedValues
        }

        enum AcceptedValue: Equatable {
            /// The list of accepted values that represent that any value for a
            /// meta-property, present or not, is accepted.
            static let any: [AcceptedValue] = [
                .none, .string(), .identifier(), .boolean()
            ]

            /// Returns a combination of `string` and `identifier` cases with a
            /// given description, for uses in cases where both string and identifier
            /// value kinds are acceptable.
            static func stringConvertible(description: String? = nil) -> [AcceptedValue] {
                return [
                    .string(description: description),
                    .identifier(description: description)
                ]
            } 

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
        enum RepeatMode: CustomStringConvertible {
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

            var description: String {
                switch self {
                case .never: "never"
                case .distinctValues: "distinctValues"
                case .always: "always"
                }
            }
        }
    }
}
