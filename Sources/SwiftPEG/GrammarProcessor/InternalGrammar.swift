public enum InternalGrammar {
    /// ```
    /// tokenDefinition:
    ///     | name=IDENTIFIER '[' expectArgs=STRING ']' ':' literal=STRING ';'
    ///     | name=IDENTIFIER ':' literal=STRING ';'
    ///     ;
    /// ```
    public struct TokenDefinition: CustomStringConvertible {
        public var name: String
        public var expectArgs: String?
        
        /// String literal. Does not contains the quotes around the literal.
        public var string: String

        public var description: String {
            if let expectArgs {
                return #"\#(name)["\#(expectArgs)"]: "\#(string)" ;"#
            } else {
                return #"\#(name): "\#(string)" ;"#
            }
        }
        
        public static func from(
            _ node: SwiftPEGGrammar.TokenDefinition
        ) -> Self {

            .init(
                name: String(node.name.string),
                expectArgs: node.expectArgs.map({ String($0.processedString) }),
                string: String(node.literal.processedString)
            )
        }
    }

    /// ```
    /// grammar:
    ///     | metas rules
    ///     | rules
    ///     ;
    /// ```
    public struct Grammar {
        public var metas: [MetaProperty] = []
        public var rules: [Rule]

        public static func from(
            _ node: SwiftPEGGrammar.Grammar
        ) -> Self {

            .init(
                metas: node.metas.map(MetaProperty.from),
                rules: node.rules.map(Rule.from)
            )
        }
    }

    /// ```
    /// meta:
    ///     | "@" name=IDENT value=metaValue ';'
    ///     | "@" name=IDENT ';'
    ///     ;
    /// ```
    public struct MetaProperty: Hashable {
        public var name: String
        public var value: Value? = nil

        public static func from(
            _ node: SwiftPEGGrammar.Meta
        ) -> Self {

            .init(
                name: String(node.name.string),
                value: node.value.map(Value.from)
            )
        }

        public enum Value: Hashable {
            /// `IDENT`
            case identifier(String)

            /// `STRING`
            /// Note: Does not include quotes.
            case string(String)

            public static func from(
                _ node: SwiftPEGGrammar.MetaValue
            ) -> Self {

                switch node {
                case let value as SwiftPEGGrammar.MetaIdentifierValue:
                    return .identifier(String(value.identifier.string))

                case let value as SwiftPEGGrammar.MetaStringValue:
                    switch value.string {
                    case .string(.tripleQuote(let contents)) where contents.hasPrefix("\n"):
                        return .string(String(contents.dropFirst()))
                    default:
                        return .string(String(value.string.processedString))
                    }
                
                default:
                    fatalError("Unknown meta-property value type \(type(of: node))")
                }
            }
        }
    }

    /// ```
    /// rule:
    ///     | ruleName ":" '|' alts ';'
    ///     | ruleName ":" alts ';'
    ///     ;
    /// ```
    public struct Rule: Hashable {
        public var name: String
        public var type: SwiftType?
        public var alts: [Alt]
        public var isRecursive: Bool = false
        public var isRecursiveLeader: Bool = false

        public var isLoop: Bool { false }
        public var isGather: Bool { false }

        /// Flattens rules that have a single alt in parenthesis.
        public func flattened() -> Self {
            guard !isLoop else { return self }
            guard alts.count == 1 && alts[0].items.count == 1 else {
                return self
            }

            var copy = self

            switch alts[0].items[0] {
            case .item(_, .atom(.group(let alts)), _):
                copy.alts = alts
            default:
                break
            }

            return copy
        }

        public static func from(
            _ node: SwiftPEGGrammar.Rule
        ) -> Self {

            .init(
                name: String(node.name.name.string),
                type: node.name.type.map(SwiftType.from),
                alts: node.alts.map(Alt.from),
                isRecursive: node.isLeftRecursive,
                isRecursiveLeader: node.isLeftRecursiveLead
            )
        }
    }

    /// `namedItems action?`
    public struct Alt: Hashable, CustomStringConvertible {
        public var items: [NamedItem]
        public var action: Action? = nil

        public var description: String {
            let items = self.items.map(\.description).joined(separator: " ")
            if let action = action {
                return "\(items) \(action)"
            }
            return items
        }

        public static func from(
            _ node: SwiftPEGGrammar.Alt
        ) -> Self {
            .init(
                items: node.namedItems.map(NamedItem.from),
                action: node.action.map(Action.from)
            )
        }
    }

    /// `'{' balancedTokens '}'`
    public struct Action: Hashable, CustomStringConvertible {
        public var string: String

        public var description: String {
            return "{ \(string) }"
        }

        public static func from(
            _ node: SwiftPEGGrammar.Action
        ) -> Self {
            guard let tokens = node.balancedTokens?.tokens else {
                return .init(string: "")
            }

            return .init(
                string: tokens.map(\.token.string).joined().trimmingWhitespace()
            )
        }
    }

    public enum NamedItem: Hashable, CustomStringConvertible {
        /// `name=IDENT? item ('[' swiftType ']')?`
        case item(name: String? = nil, Item, type: SwiftType? = nil)
        /// ```
        /// lookahead:
        ///     | '&' ~ atom
        ///     | '!' ~ atom
        ///     | '~'
        ///     ;
        /// ```
        case lookahead(Lookahead)

        public var description: String {
            switch self {
            case .item(let name?, let item, let type?):
                return "\(name)=\(item)[\(type)]"
            case .item(let name?, let item, nil):
                return "\(name)=\(item)"
            case .item(nil, let item, let type?):
                return "\(item)[\(type)]"
            case .item(nil, let item, nil):
                return item.description
            case .lookahead(let lookahead):
                return lookahead.description
            }
        }

        /// Returns the alias for referencing the this named item in code generated
        /// by code generators.
        /// 
        /// Returns `nil` if no suitable alias was found.
        var alias: String? {
            switch self {
            case .item(let name?, _, _):
                return name
            case .item(_, let item, _):
                return item.alias
            case .lookahead:
                return nil
            }
        }

        public static func from(
            _ node: SwiftPEGGrammar.NamedItem
        ) -> Self {
            if let item = node.item {
                let name = (node.name?.string).map(String.init)

                return .item(name: name, .from(item), type: node.type.map(SwiftType.from))
            } else {
                return .lookahead(.from(node.lookahead!))
            }
        }
    }

    public enum Item: Hashable, CustomStringConvertible {
        /// `'[' alts ']'`
        case optionalItems([Alt])
        /// `atom '?'`
        case optional(Atom)
        /// `atom '*'`
        case zeroOrMore(Atom)
        /// `atom '+'`
        case oneOrMore(Atom)
        /// `sep=atom '.' node=atom '+'`
        case gather(sep: Atom, node: Atom)
        /// `atom`
        case atom(Atom)

        public var description: String {
            switch self {
            case .atom(let atom):
                return atom.description
            case .gather(let sep, let node):
                return "\(sep).\(node)+"
            case .zeroOrMore(let atom):
                return "\(atom)*"
            case .oneOrMore(let atom):
                return "\(atom)+"
            case .optional(let atom):
                return "\(atom)?"
            case .optionalItems(let alts):
                return "[\(alts.map(\.description).joined(separator: " | "))]"
            }
        }

        /// Returns the alias for referencing the this item in code generated by
        /// code generators.
        /// 
        /// Returns `nil` if no suitable alias was found.
        var alias: String? {
            switch self {
            case .atom(let atom),
                .zeroOrMore(let atom),
                .oneOrMore(let atom),
                .optional(let atom):
                return atom.alias
            
            case .gather(_, let node):
                return node.alias

            case .optionalItems:
                return nil
            }
        }

        public static func from(
            _ node: SwiftPEGGrammar.Item
        ) -> Self {

            switch node {
            case let item as SwiftPEGGrammar.OptionalItems:
                return .optionalItems(item.alts.map(Alt.from(_:)))

            case let item as SwiftPEGGrammar.OptionalItem:
                return .optional(Atom.from(item.atom))

            case let item as SwiftPEGGrammar.ZeroOrMoreItem:
                return .zeroOrMore(Atom.from(item.atom))

            case let item as SwiftPEGGrammar.OneOrMoreItem:
                return .oneOrMore(Atom.from(item.atom))

            case let item as SwiftPEGGrammar.GatherItem:
                return .gather(sep: Atom.from(item.sep), node: Atom.from(item.item))

            case let item as SwiftPEGGrammar.OptionalItem:
                return .optional(Atom.from(item.atom))

            case let item as SwiftPEGGrammar.AtomItem:
                return .atom(Atom.from(item.atom))

            default:
                fatalError("Unknown item type \(type(of: node))")
            }
        }
    }

    public indirect enum Lookahead: Hashable, CustomStringConvertible {
        /// `'!' atom`
        case negative(Atom)
        /// `'&' atom`
        case positive(Atom)
        /// `~`
        case cut

        public var description: String {
            switch self {
            case .negative(let atom):
                return "!\(atom)"
            case .positive(let atom):
                return "&\(atom)"
            case .cut:
                return "~"
            }
        }

        public static func from(
            _ node: SwiftPEGGrammar.LookaheadOrCut
        ) -> Self {

            switch node {
            case let positive as SwiftPEGGrammar.PositiveLookahead:
                return .positive(Atom.from(positive.atom))
            case let negative as SwiftPEGGrammar.NegativeLookahead:
                return .negative(Atom.from(negative.atom))
            case is SwiftPEGGrammar.Cut:
                return .cut
            default:
                fatalError("Unknown lookahead type \(type(of: node))")
            }
        }
    }

    public indirect enum Atom: Hashable, CustomStringConvertible {
        /// `'(' alts ')'`
        case group([Alt])

        /// `ident`
        case ruleName(String)

        /// `IDENT`
        case token(String)

        /// `STRING`
        case string(String, trimmed: String)

        public var description: String {
            switch self {
            case .group(let alts):
                return "(\(alts.map(\.description).joined(separator: " | ")))"
            case .ruleName(let ident):
                return ident
            case .token(let ident):
                return ident
            case .string(let str, _):
                return str
            }
        }

        public var isGroup: Bool {
            switch self {
            case .group: return true
            default: return false
            }
        }

        /// Returns the alias for referencing the this atom in code generated by
        /// code generators.
        /// 
        /// If this atom is a token, returns the token's identifier lowercased.
        /// If it's a rule name, return the rule name itself, otherwise returns
        /// `nil`.
        var alias: String? {
            switch self {
            case .token(let ident):
                return ident.lowercased()
                
            case .ruleName(let ident):
                return ident

            case .group, .string:
                return nil
            }
        }

        public static func from(
            _ node: SwiftPEGGrammar.Atom
        ) -> Self {

            switch node {
            case let group as SwiftPEGGrammar.GroupAtom:
                return .group(group.alts.map(Alt.from(_:)))

            case let ident as SwiftPEGGrammar.IdentAtom:
                let value = ident.identifier.string

                switch ident.identity {
                case .ruleName:
                    return .ruleName(String(value))
                case .token:
                    return .token(String(value))
                case .unresolved:
                    return .ruleName(String(value))
                }

            case let string as SwiftPEGGrammar.StringAtom:
                return .string(String(string.string.string), trimmed: String(string.string.processedString))

            default:
                fatalError("Unknown atom type \(type(of: node))")
            }
        }
    }

    public struct SwiftType: Hashable, CustomStringConvertible {
        public var name: String

        public var description: String { name }

        public static func from(
            _ node: SwiftPEGGrammar.SwiftType
        ) -> Self {
            .init(name: String(node.name))
        }
    }
}
