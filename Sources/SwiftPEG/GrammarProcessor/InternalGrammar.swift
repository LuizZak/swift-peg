public enum InternalGrammar {
    /// ```
    /// tokenDefinition:
    ///     | '$' name=IDENTIFIER '[' expectArgs=STRING ']' ':' ~ literal=STRING ';' 
    ///     | '$' name=IDENTIFIER '[' expectArgs=STRING ']' ';'
    ///     | '$' name=IDENTIFIER ':' ~ literal=STRING ';'
    ///     | '$' name=IDENTIFIER ';'
    ///     ;
    /// ```
    public struct TokenDefinition: CustomStringConvertible {
        public var name: String
        public var expectArgs: String?
        
        /// String literal. Does not contains the quotes around the literal.
        /// May not be provided; in which case the literal is assumed to be the
        /// same value as `name` wrapped in any type of quotes.
        public var string: String?

        /// Returns the computed literal for this token.
        ///
        /// If `self.string` is non-nil, returns its value, otherwise returns
        /// `name`.
        ///
        /// Does not contains the quotes around the literal.
        public var computedLiteral: String {
            if let string {
                return string
            } else {
                return name
            }
        }

        public var description: String {
            switch (expectArgs, string) {
            case (let expectArgs?, let string?):
                return #"$\#(name)["\#(expectArgs)"]: "\#(string)" ;"#
            case (let expectArgs?, nil):
                return #"$\#(name)["\#(expectArgs)"] ;"#
            case (nil, let string?):
                return #"$\#(name) : "\#(string)" ;"#
            case (nil, nil):
                return #"$\#(name) ;"#
            }
        }
        
        public static func from(
            _ node: SwiftPEGGrammar.TokenDefinition
        ) -> Self {

            .init(
                name: String(node.name.string),
                expectArgs: node.expectArgs.map({ String($0.processedString) }),
                string: node.literal.map { String($0.processedString) }
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

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            let items = items.compactMap(\.removingCuts)
            if items.isEmpty { return nil }

            return .init(items: items, action: action)
        }

        /// Returns `true` if both `self` and `other` execute equivalent productions,
        /// ignoring associated actions.
        func isEquivalent(to other: Self) -> Bool {
            let selfCut = self.removingCuts
            let otherCut = other.removingCuts
            if selfCut == nil && otherCut == nil {
                return true
            }

            guard let selfCut, let otherCut else {
                return false
            }

            if selfCut.items == otherCut.items { return true }
            if selfCut.items.count != otherCut.items.count { return false }

            return selfCut.items.elementsEqual(otherCut.items, by: { $0.isEquivalent(to: $1) })
        }

        /// Returns `true` if `self` executes a subset of the production of
        /// `other`, such that `self == other + [extra productions in other]...`.
        /// If `self` and `other` are equivalent, `true` is also returned.
        func isSubset(of other: Self) -> Bool {
            let selfCut = self.removingCuts
            let otherCut = other.removingCuts
            if selfCut == nil && otherCut == nil {
                return true
            }
            guard let selfCut, let otherCut else {
                return false
            }

            if selfCut.items == otherCut.items { return true }
            // Subsets require `self` to be at most the same size as `other`
            if selfCut.items.count > otherCut.items.count { return false }

            return selfCut.items.elementsEqual(otherCut.items[..<selfCut.items.count], by: { $0.isEquivalent(to: $1) })
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

        /// Returns `true` if `self` is a lookahead of Cut (`~`) type.
        var isCut: Bool {
            switch self {
            case .lookahead(.cut): return true
            default: return false
            }
        }

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            switch self {
            case .item(let name, let item, let type):
                return item.removingCuts.map { Self.item(name: name, $0, type: type) }

            case .lookahead(let lookahead):
                return lookahead.removingCuts.map(Self.lookahead)
            }
        }

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

        /// Returns `true` if both `self` and `other` execute equivalent productions,
        /// ignoring name and type.
        func isEquivalent(to other: Self) -> Bool {
            if self == other { return true }

            switch (self, other) {
            case (.item(_, let lhs, _), .item(_, let rhs, _)):
                return lhs.isEquivalent(to: rhs)

            case (.lookahead(let lhs), .lookahead(let rhs)):
                return lhs == rhs

            default:
                return false
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

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            switch self {
            case .atom(let atom):
                return atom.removingCuts.map(Self.atom)

            case .gather(let sep, let node):
                if let sep = sep.removingCuts, let node = node.removingCuts {
                    return Self.gather(sep: sep, node: node)
                }
                return nil

            case .zeroOrMore(let atom):
                return atom.removingCuts.map(Self.zeroOrMore)

            case .oneOrMore(let atom):
                return atom.removingCuts.map(Self.oneOrMore)

            case .optional(let atom):
                return atom.removingCuts.map(Self.optional)
                
            case .optionalItems(let alts):
                let alts = alts.compactMap(\.removingCuts)
                if alts.isEmpty {
                    return nil
                }

                return .optionalItems(alts)
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

        /// Returns `true` if both `self` and `other` execute equivalent productions.
        func isEquivalent(to other: Self) -> Bool {
            if self == other { return true }

            switch (self, other) {
            // Check `[ atom ]` and `atom ?`
            case (.optionalItems(let alts), .atom(let rhs)) where alts.count == 1 && alts[0].items.count == 1:
                return alts[0].items[0].isEquivalent(
                    to: .item(name: nil, .atom(rhs), type: nil)
                )
            // Check `atom ?` and `[ atom ]`
            case (.atom(let lhs), .optionalItems(let alts)) where alts.count == 1 && alts[0].items.count == 1:
                return alts[0].items[0].isEquivalent(
                    to: .item(name: nil, .atom(lhs), type: nil)
                )
            default:
                return false
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

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            switch self {
            case .negative(let atom):
                return atom.removingCuts.map(Self.negative)
            case .positive(let atom):
                return atom.removingCuts.map(Self.positive)
            case .cut:
                return nil
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

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            switch self {
            case .group(let alts):
                return .group(alts.compactMap(\.removingCuts))
            default:
                return self
            }
        }

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
