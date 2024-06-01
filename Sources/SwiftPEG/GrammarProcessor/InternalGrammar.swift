public enum InternalGrammar {
    /// ```
    /// tokenDefinition:
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER '[' staticToken=STRING ']' ':' ~ tokenSyntax ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER '[' staticToken=STRING ']' ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER ':' ~ tokenSyntax ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER ';'
    ///     ;
    ///
    /// tokenOrFragmentSpecifier:
    ///     | '$'
    ///     | '%'
    ///     ;
    /// ```
    public struct TokenDefinition: Equatable, CustomStringConvertible {
        public var name: String

        /// Whether this token is a fragment, or a part of another token's syntax
        /// without being exposed to the parser as a token kind or lexed on its
        /// own in a token's lexing function.
        public var isFragment: Bool

        public var staticToken: String?

        /// The syntax definition of this token.
        public var tokenSyntax: CommonAbstract.TokenSyntax?

        /// String literal. Does not contains the quotes around the literal.
        /// May not be provided; in which case the literal is assumed to be the
        /// same value as `name` wrapped in any type of quotes.
        public var string: String? {
            guard let tokenSyntax else { return nil }
            guard let alt = tokenSyntax.alts.first, tokenSyntax.alts.count == 1 else {
                return nil
            }
            guard let item = alt.items.first, alt.items.count == 1 else {
                return nil
            }
            guard case .atom(let atom) = item else {
                return nil
            }
            guard atom.excluded.isEmpty else {
                return nil
            }
            guard case .literal(let literal) = atom.terminal else {
                return nil
            }

            return literal.contents
        }

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
            let prefix = isFragment ? "%" : "$"

            switch (staticToken, tokenSyntax) {
            case (let staticToken?, let tokenSyntax?):
                return #"\#(prefix)\#(name)["\#(staticToken)"]: \#(tokenSyntax) ;"#
            case (let staticToken?, nil):
                return #"\#(prefix)\#(name)["\#(staticToken)"] ;"#
            case (nil, let tokenSyntax?):
                return #"\#(prefix)\#(name) : \#(tokenSyntax) ;"#
            case (nil, nil):
                return #"\#(prefix)\#(name) ;"#
            }
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.visit(self)
        }

        public static func from(
            _ node: SwiftPEGGrammar.TokenDefinition
        ) -> Self {

            .init(
                name: String(node.name.string),
                isFragment: node.isFragment,
                staticToken: node.staticToken.map({ $0.rawContents() }),
                tokenSyntax: node.tokenSyntax
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

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            try metas.forEach { try $0.accept(visitor) }
            try rules.forEach { try $0.accept(visitor) }

            try visitor.didVisit(self)
        }

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

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.visit(self)
        }

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
                    return .string(value.string.rawContents())

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
        public var type: CommonAbstract.SwiftType?
        public var alts: [Alt]

        /// Whether this rule has been marked as reachable from a starting rule
        /// during grammar processing.
        public var isReachable: Bool = true

        /// Whether this rule is contained within a left-recursive chain.
        public var isRecursive: Bool = false

        /// Whether this rule has been selected among its left-recursive chain
        /// to be memoized as a left-recursive lead.
        public var isRecursiveLeader: Bool = false

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            try alts.forEach { try $0.accept(visitor) }

            try visitor.didVisit(self)
        }

        /// Flattens rules that have a single alt in parenthesis.
        public func flattened() -> Self {
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
                type: node.name.type,
                alts: node.alts.map(Alt.from),
                isReachable: node.isReachable,
                isRecursive: node.isLeftRecursive,
                isRecursiveLeader: node.isLeftRecursiveLead
            )
        }
    }

    /// `namedItems action?`
    public struct Alt: Hashable, CustomStringConvertible {
        public var items: [NamedItem]
        public var action: Action? = nil
        public var failAction: Action? = nil

        public var description: String {
            var string = self.items.map(\.description).joined(separator: " ")
            if let action {
                string += " \(action)"
            }
            if let failAction {
                string += " !!\(failAction)"
            }
            return string
        }

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            let items = items.compactMap(\.removingCuts)
            if items.isEmpty { return nil }

            return .init(items: items, action: action, failAction: failAction)
        }

        /// Returns a copy of `self` with any optional productions elided.
        /// If this results in this production being empty, returns `nil`.
        var reduced: Self? {
            let items = items.compactMap(\.reduced)
            if items.isEmpty { return nil }

            return .init(items: items, action: action, failAction: failAction)
        }

        /// Flattens this alt. Returns a copy of `self` with nested productions
        /// collapsed to the top-level whenever possible.
        ///
        /// If this results in this production being empty, returns `nil`.
        func flattened() -> Self? {
            let items = items.compactMap({ $0.flattened() })
            if items.isEmpty && action == nil {
                return nil
            }

            return .init(items: items, action: action, failAction: failAction)
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            try items.forEach { try $0.accept(visitor) }
            try action?.accept(visitor)
            try failAction?.accept(visitor)

            try visitor.didVisit(self)
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
        /// `true` is also returned if `self` and `other` are equivalent.
        func isPrefix(of other: Self) -> Bool {
            let selfCut = self.removingCuts
            let otherCut = other.removingCuts
            if selfCut == nil && otherCut == nil {
                return true
            }
            guard let selfCut, let otherCut else {
                return false
            }

            if selfCut.items == otherCut.items { return true }
            // Prefixes require `self` to be at most the same size as `other`
            if selfCut.items.count > otherCut.items.count { return false }

            return selfCut.items.elementsEqual(otherCut.items[..<selfCut.items.count], by: { $0.isEquivalent(to: $1) })
        }

        public static func from(
            _ node: SwiftPEGGrammar.Alt
        ) -> Self {
            .init(
                items: node.namedItems.map(NamedItem.from),
                action: node.action.map(Action.from),
                failAction: node.failAction.map(Action.from)
            )
        }
    }

    /// `'{' balancedTokens '}'`
    public struct Action: Hashable, CustomStringConvertible {
        public var string: String

        public var description: String {
            return "{\(string)}"
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.visit(self)
        }

        public static func from(
            _ node: SwiftPEGGrammar.Action
        ) -> Self {
            return .init(string: node.rawAction)
        }
    }

    @GeneratedCaseChecks
    public enum NamedItem: Hashable, CustomStringConvertible {
        /// `name=IDENT? item ('[' swiftType ']')?`
        case item(name: String? = nil, Item, type: CommonAbstract.SwiftType? = nil)
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

        /// Returns a copy of `self` with any optional productions elided.
        /// If this results in this production being empty, returns `nil`.
        var reduced: Self? {
            switch self {
            case .item(let name, let item, let type):
                return item.reduced.map { Self.item(name: name, $0, type: type) }

            case .lookahead(let lookahead):
                return lookahead.reduced.map(Self.lookahead)
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

        /// Flattens this alt. Returns a copy of `self` with nested productions
        /// collapsed to the top-level whenever possible.
        ///
        /// If this results in this production being empty, returns `nil`.
        func flattened() -> Self? {
            switch self {
            case .item(let name, let item, let type):
                item.flattened().map { Self.item(name: name, $0, type: type) }

            case .lookahead(let lookahead):
                lookahead.flattened().map(Self.lookahead)
            }
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            switch self {
            case .item(_, let item, _):
                try item.accept(visitor)
            case .lookahead(let lookahead):
                try lookahead.accept(visitor)
            }

            try visitor.didVisit(self)
        }

        /// Returns the alias for referencing the this named item in code generated
        /// by code generators.
        ///
        /// Returns `nil` if no suitable alias was found.
        func alias(_ literalResolver: TokenLiteralResolver? = nil) -> String? {
            switch self {
            case .item(let name?, _, _):
                return name
            case .item(_, let item, _):
                return item.alias(literalResolver)
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

                return .item(name: name, .from(item), type: node.type)
            } else {
                return .lookahead(.from(node.lookahead!))
            }
        }
    }

    @GeneratedCaseChecks
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

        /// Returns a copy of `self` with any optional productions elided.
        /// If this results in this production being empty, returns `nil`.
        var reduced: Self? {
            switch self {
            case .zeroOrMore, .optional, .optionalItems:
                return nil

            case .oneOrMore(let atom):
                return atom.reduced.map(Self.oneOrMore)

            case .gather(let sep, let node):
                switch (sep.reduced, node.reduced) {
                case (let sep?, let node?):
                    return Self.gather(sep: sep, node: node)

                case (nil, let node?):
                    // A gather with nil separators is equivalent to a 'node+'
                    // production
                    return Self.oneOrMore(node)

                case (_?, nil):
                    // A gather with nil nodes is equivalent to a 'sep*'
                    // production, which is nullable
                    return nil

                case (nil, nil):
                    return nil
                }

            case .atom(let atom):
                return atom.reduced.map(Self.atom)
            }
        }

        /// Flattens this alt. Returns a copy of `self` with nested productions
        /// collapsed to the top-level whenever possible.
        ///
        /// If this results in this production being empty, returns `nil`.
        func flattened() -> Self? {
            switch self {
            case .optional(let atom):
                return atom.flattened().map(Self.optional)

            case .optionalItems(let alts):
                let alts = alts.compactMap({ $0.flattened() })
                if alts.isEmpty { return nil }
                if alts.count == 1 { return .optional(.group(alts)) }

                return .optionalItems(alts)

            case .zeroOrMore(let atom):
                return atom.flattened().map(Self.zeroOrMore)

            case .oneOrMore(let atom):
                return atom.flattened().map(Self.oneOrMore)

            case .gather(let sep, let node):
                switch (sep.flattened(), node.flattened()) {
                case (let sep?, let node?):
                    return Self.gather(sep: sep, node: node)

                case (nil, let node?):
                    // A gather with just nodes is equivalent to a 'node+'
                    // production
                    return Self.oneOrMore(node)

                case (let sep?, nil):
                    // A gather with just separators is equivalent to a 'sep*'
                    // production
                    return Self.zeroOrMore(sep)

                case (nil, nil):
                    return nil
                }

            case .atom(let atom):
                return atom.flattened().map(Self.atom)
            }
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            switch self {
            case .atom(let atom):
                try atom.accept(visitor)

            case .gather(let sep, let node):
                try sep.accept(visitor)
                try node.accept(visitor)

            case .zeroOrMore(let atom):
                try atom.accept(visitor)

            case .oneOrMore(let atom):
                try atom.accept(visitor)

            case .optional(let atom):
                try atom.accept(visitor)

            case .optionalItems(let alts):
                try alts.forEach {
                    try $0.accept(visitor)
                }
            }

            try visitor.didVisit(self)
        }

        /// Returns the alias for referencing the this item in code generated by
        /// code generators.
        ///
        /// Returns `nil` if no suitable alias was found.
        func alias(_ literalResolver: TokenLiteralResolver? = nil) -> String? {
            switch self {
            case .atom(let atom),
                .zeroOrMore(let atom),
                .oneOrMore(let atom),
                .optional(let atom):
                return atom.alias(literalResolver)

            case .gather(_, let node):
                return node.alias(literalResolver)

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

        /// Returns a copy of `self` with any optional productions elided.
        /// If this results in this production being empty, returns `nil`.
        var reduced: Self? {
            switch self {
            case .negative(let atom):
                return atom.reduced.map(Self.negative)
            case .positive(let atom):
                return atom.reduced.map(Self.positive)
            case .cut:
                return nil
            }
        }

        /// Flattens this alt. Returns a copy of `self` with nested productions
        /// collapsed to the top-level whenever possible.
        ///
        /// If this results in this production being empty, returns `nil`.
        func flattened() -> Self? {
            switch self {
            case .negative(let atom):
                return atom.flattened().map(Self.negative)
            case .positive(let atom):
                return atom.flattened().map(Self.positive)
            case .cut:
                return self
            }
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            switch self {
            case .positive(let atom):
                try atom.accept(visitor)
            case .negative(let atom):
                try atom.accept(visitor)
            default:
                break
            }

            try visitor.didVisit(self)
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
        @GeneratedIsCase(accessLevel: "public")
        case group([Alt])

        /// `ident`
        case ruleName(String)

        /// `IDENT`
        case token(String)

        /// `IDENT`, token has been defined before as the 'any token' construct.
        case anyToken(String)

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

        /// Returns a copy of `self` with any optional productions elided.
        /// If this results in this production being empty, returns `nil`.
        var reduced: Self? {
            switch self {
            case .group(let alts):
                let result = alts.compactMap(\.reduced)
                if result.isEmpty { return nil }
                return .group(result)
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
            case .anyToken(let ident):
                return ident
            case .string(let str, _):
                return str
            }
        }

        /// Flattens this alt. Returns a copy of `self` with nested productions
        /// collapsed to the top-level whenever possible.
        ///
        /// If this results in this production being empty, returns `nil`.
        func flattened() -> Self? {
            switch self {
            case .group(let alts):
                let flattened = alts.compactMap({ $0.flattened() })
                if flattened.isEmpty { return nil }
                return .group(flattened)

            default:
                return self
            }
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            if case .group(let alts) = self {
                try alts.forEach { try $0.accept(visitor) }
            }

            try visitor.didVisit(self)
        }

        /// Returns the alias for referencing the this atom in code generated by
        /// code generators.
        ///
        /// If this atom is a token, returns the token's identifier lowercased.
        /// If it's a rule name, return the rule name itself, otherwise returns
        /// `nil`.
        func alias(_ literalResolver: TokenLiteralResolver? = nil) -> String? {
            switch self {
            case .token(let ident):
                return ident.lowercased()

            case .anyToken(let ident):
                return ident.lowercased()

            case .ruleName(let ident):
                return ident

            case .group:
                return nil

            case .string(_, let literal):
                return literalResolver?.tokenName(ofRawLiteral: literal)
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
                case .anyToken:
                    return .anyToken(String(value))
                case .unresolved:
                    return .ruleName(String(value))
                }

            case let string as SwiftPEGGrammar.StringAtom:
                return .string(string.string.asStringLiteral(), trimmed: string.string.rawContents())

            default:
                fatalError("Unknown atom type \(type(of: node))")
            }
        }
    }

    /// Simplified visitor protocol for internal grammar trees.
    public protocol Visitor {
        func visit(_ node: TokenDefinition) throws

        func willVisit(_ node: Grammar) throws
        func visit(_ node: Grammar) throws
        func didVisit(_ node: Grammar) throws

        func visit(_ node: MetaProperty) throws

        func willVisit(_ node: Rule) throws
        func visit(_ node: Rule) throws
        func didVisit(_ node: Rule) throws

        func willVisit(_ node: Alt) throws
        func visit(_ node: Alt) throws
        func didVisit(_ node: Alt) throws

        func visit(_ node: Action) throws

        func willVisit(_ node: NamedItem) throws
        func visit(_ node: NamedItem) throws
        func didVisit(_ node: NamedItem) throws

        func willVisit(_ node: Item) throws
        func visit(_ node: Item) throws
        func didVisit(_ node: Item) throws

        func willVisit(_ node: Lookahead) throws
        func visit(_ node: Lookahead) throws
        func didVisit(_ node: Lookahead) throws

        func willVisit(_ node: Atom) throws
        func visit(_ node: Atom) throws
        func didVisit(_ node: Atom) throws
    }
}

/// Protocol for types capable of resolving a token string literal into a token
/// definition with an appropriate name.
///
/// Used by some of internal grammar classes' `alias()` methods.
protocol TokenLiteralResolver {
    /// Returns the name of a token that has a literal value matching the given
    /// (non-quoted) value, or `nil`, if none is known.
    func tokenName(ofRawLiteral literal: String) -> String?
}

public extension InternalGrammar.Visitor {
    func visit(_ node: InternalGrammar.TokenDefinition) throws { }

    func willVisit(_ node: InternalGrammar.Grammar) throws { }
    func visit(_ node: InternalGrammar.Grammar) throws { }
    func didVisit(_ node: InternalGrammar.Grammar) throws { }

    func visit(_ node: InternalGrammar.MetaProperty) throws { }

    func willVisit(_ node: InternalGrammar.Rule) throws { }
    func visit(_ node: InternalGrammar.Rule) throws { }
    func didVisit(_ node: InternalGrammar.Rule) throws { }

    func willVisit(_ node: InternalGrammar.Alt) throws { }
    func visit(_ node: InternalGrammar.Alt) throws { }
    func didVisit(_ node: InternalGrammar.Alt) throws { }

    func visit(_ node: InternalGrammar.Action) throws { }

    func willVisit(_ node: InternalGrammar.NamedItem) throws { }
    func visit(_ node: InternalGrammar.NamedItem) throws { }
    func didVisit(_ node: InternalGrammar.NamedItem) throws { }

    func willVisit(_ node: InternalGrammar.Item) throws { }
    func visit(_ node: InternalGrammar.Item) throws { }
    func didVisit(_ node: InternalGrammar.Item) throws { }

    func willVisit(_ node: InternalGrammar.Lookahead) throws { }
    func visit(_ node: InternalGrammar.Lookahead) throws { }
    func didVisit(_ node: InternalGrammar.Lookahead) throws { }

    func willVisit(_ node: InternalGrammar.Atom) throws { }
    func visit(_ node: InternalGrammar.Atom) throws { }
    func didVisit(_ node: InternalGrammar.Atom) throws { }
}
