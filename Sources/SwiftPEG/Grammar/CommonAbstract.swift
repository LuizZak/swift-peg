/// A set of syntax construction structures that are neither unique to the grammar
/// processor nor so tied to the parser that they cannot be used elsewhere in the
/// code.
/// 
/// Namespaced to keep type pollution to a minimum.
public enum CommonAbstract {
    /// Describes the type of a grammar production, as a Swift type.
    /// 
    /// Represents the construct:
    /// ```
    /// swiftType:
    ///     | '[' key=swiftType ':' ~ value=swiftType ']'
    ///     | '[' ~ swiftType ']'
    ///     | swiftType '?'
    ///     | swiftType '.' IDENTIFIER '<' swiftTypeList '>'
    ///     | swiftType '.' IDENTIFIER
    ///     | IDENTIFIER '<' swiftTypeList '>'
    ///     | IDENTIFIER
    ///     ;
    /// ```
    /// 
    /// And in list form, as generic parameters of an enclosing generic Swift type
    /// separated by commas:
    /// ```
    /// swiftTypeList:
    ///     | ','.swiftType+
    ///     ;
    /// ```
    public indirect enum SwiftType: Hashable, CustomStringConvertible, ExpressibleByStringLiteral {
        /// A Swift dictionary type.
        ///
        /// `'[' key=swiftType ':' ~ value=swiftType ']'`
        case dictionary(key: Self, value: Self)

        /// A Swift array type.
        ///
        /// `'[' ~ swiftType ']'`
        case array(Self)

        /// A Swift optional type.
        ///
        /// `swiftType '?'`
        case optional(Self)

        /// A Swift nested type, with an identifier at its trail.
        ///
        /// ```
        /// swiftType '.' IDENTIFIER '<' swiftTypeList '>'
        /// swiftType '.' IDENTIFIER`
        /// ```
        case nested(Self, IdentifierSwiftType)

        /// A nominal Swift type.
        ///
        /// `IDENTIFIER`
        case nominal(IdentifierSwiftType)

        public var description: String {
            switch self {
            case .array(let element): "[\(element)]"
            case .dictionary(let key, let value): "[\(key): \(value)]"
            case .optional(let wrappedType): "\(wrappedType)?"
            case .nested(let base, let nominal): "\(base).\(nominal)"
            case .nominal(let nominal): nominal.description
            }
        }

        /// Initializes this `SwiftType` as a nominal type of the given identifier
        /// string.
        public init(stringLiteral value: String) {
            self = .nominal(.init(identifier: value))
        }
    }

    /// An auxiliary type to `SwiftType` that is used to describe a nominal Swift
    /// type, with optional generic arguments.
    public struct IdentifierSwiftType: Hashable, CustomStringConvertible {
        /// The identifier name of the type.
        public var identifier: String

        /// An optional set of generic arguments, in case this identifier is
        /// a generic type reference.
        public var genericArguments: [SwiftType]

        /// Returns a Swift-equivalent type string for this identifier type.
        public var description: String {
            if genericArguments.isEmpty {
                return String(identifier)
            } else {
                return "\(identifier)<\(genericArguments.map(\.description).joined(separator: ", "))>"
            }
        }

        public init(identifier: Substring, genericArguments: [SwiftType] = []) {
            self.identifier = String(identifier)
            self.genericArguments = genericArguments
        }

        public init(identifier: String, genericArguments: [SwiftType] = []) {
            self.identifier = identifier
            self.genericArguments = genericArguments
        }
    }
}

// MARK: Token syntax definition

extension CommonAbstract {
    struct TokSyntax: CustomStringConvertible {
        var components: [TokComponent]

        var description: String {
            return components.map(\.description).joined(separator: " | ")
        }
    }

    struct TokComponent: CustomStringConvertible {
        var terminals: [TokTerminal]

        var description: String {
            terminals.map(\.description).joined(separator: " ")
        }
    }

    enum TokTerminal: CustomStringConvertible {
        indirect case notLiteral(String, Self)
        indirect case notIdent(String, Self)
        case rangeLiteral(String, String)
        case literal(String)
        case ident(String)
        case any

        var description: String {
            switch self {
            case .notLiteral(let lookahead, let next):
                return #"!"\#(lookahead)" \#(next)"#
            case .notIdent(let lookahead, let next):
                return "!\(lookahead) \(next)"
            case .rangeLiteral(let start, let end):
                return #""\#(start)"..."\#(end)""#
            case .literal(let string):
                return #""\#(string)""#
            case .ident(let ident):
                return ident
            case .any:
                return "*"
            }
        }
    }
}
