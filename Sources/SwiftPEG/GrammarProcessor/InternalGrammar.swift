public enum InternalGrammar {
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
    ///     | "@" name=IDENT values=metaValue* ';'
    ///     ;
    /// ```
    public struct MetaProperty: Hashable {
        public var name: String
        public var values: [Value] = []

        public init(name: String, values: [Value] = []) {
            self.name = name
            self.values = values
        }

        public init(name: String, value: Value) {
            self.name = name
            self.values = [value]
        }

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
                values: node.values.map(Value.from)
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
    ///     | ruleName ruleParameters? ":" action? failAction? '|'? alts ';'
    ///     ;
    /// ```
    public struct Rule: Hashable {
        public var name: String
        public var ruleParameters: [RuleParameter]?
        public var type: CommonAbstract.SwiftType?
        public var action: Action?
        public var failAction: Action?
        public var alts: [Alt]

        /// Whether this rule has been marked as reachable from a starting rule
        /// during grammar processing.
        public var isReachable: Bool = true

        /// Whether this rule is contained within a left-recursive chain.
        public var isLeftRecursive: Bool = false

        /// Whether this rule has been selected among its left-recursive chain
        /// to be memoized as a left-recursive lead.
        public var isLeftRecursiveLeader: Bool = false

        /// Returns `true` if `self` and `other` are equivalent in terms of the
        /// productions they reference, except for their name.
        func isEquivalent(to other: Self) -> Bool {
            var other = other
            other.name = self.name
            return self == other
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            if let ruleParameters {
                try ruleParameters.forEach { try $0.accept(visitor) }
            }

            try alts.forEach { try $0.accept(visitor) }

            try visitor.didVisit(self)
        }

        /// Flattens rules that have a single alt in parenthesis.
        public func flattened() -> Self {
            guard alts.count == 1 && alts[0].namedItems.count == 1 else {
                return self
            }

            var copy = self

            switch alts[0].namedItems[0] {
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

            return .init(
                name: String(node.ruleName),
                ruleParameters: node.parameters.map({ $0.parameters.map(RuleParameter.from) }),
                type: node.name.type,
                action: node.action.map(Action.from),
                failAction: node.failAction.map(Action.from),
                alts: node.alts.map(Alt.from),
                isReachable: node.isReachable,
                isLeftRecursive: node.isLeftRecursive,
                isLeftRecursiveLeader: node.isLeftRecursiveLead
            )
        }
    }

    /// ```
    /// ruleParameter:
    ///     | IDENTIFIER ':' swiftType
    ///     ;
    /// ```
    public struct RuleParameter: Hashable, CustomStringConvertible {
        public var name: String
        public var type: CommonAbstract.SwiftType

        public var description: String {
            "\(name): \(type)"
        }

        public static func from(
            _ node: SwiftPEGGrammar.RuleParameter
        ) -> Self {
            .init(
                name: String(node.name.string),
                type: node.type
            )
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)
            try visitor.didVisit(self)
        }
    }

    /// `altLabel? namedItems action? failAction?`
    public struct Alt: Hashable, CustomStringConvertible {
        public var label: String?
        public var namedItems: [NamedItem]
        public var action: Action? = nil
        public var failAction: Action? = nil

        @available(*, deprecated, renamed: "namedItems")
        public var items: [NamedItem] {
            namedItems
        }

        public var description: String {
            var string = self.namedItems.map(\.description).joined(separator: " ")
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
            let namedItems = namedItems.compactMap(\.removingCuts)
            if namedItems.isEmpty { return nil }

            return .init(namedItems: namedItems, action: action, failAction: failAction)
        }

        /// Returns a copy of `self` with any optional productions elided.
        /// If this results in this production being empty, returns `nil`.
        var reduced: Self? {
            let namedItems = namedItems.compactMap(\.reduced)
            if namedItems.isEmpty { return nil }

            return .init(namedItems: namedItems, action: action, failAction: failAction)
        }

        /// Flattens this alt. Returns a copy of `self` with nested productions
        /// collapsed to the top-level whenever possible.
        ///
        /// If this results in this production being empty, returns `nil`.
        func flattened() -> Self? {
            let namedItems = namedItems.compactMap({ $0.flattened() })
            if namedItems.isEmpty && action == nil {
                return nil
            }

            return .init(namedItems: namedItems, action: action, failAction: failAction)
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)

            try namedItems.forEach { try $0.accept(visitor) }
            try action?.accept(visitor)
            try failAction?.accept(visitor)

            try visitor.didVisit(self)
        }

        /// Returns `true` if both `self` and `other` execute equivalent productions,
        /// ignoring associated actions and labels.
        func isEquivalent(to other: Self) -> Bool {
            let selfCut = self.removingCuts
            let otherCut = other.removingCuts
            if selfCut == nil && otherCut == nil {
                return true
            }

            guard let selfCut, let otherCut else {
                return false
            }

            if selfCut.namedItems == otherCut.namedItems { return true }
            if selfCut.namedItems.count != otherCut.namedItems.count { return false }

            return selfCut.namedItems.elementsEqual(otherCut.namedItems, by: { $0.isEquivalent(to: $1) })
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

            if selfCut.namedItems == otherCut.namedItems { return true }
            // Prefixes require `self` to be at most the same size as `other`
            if selfCut.namedItems.count > otherCut.namedItems.count { return false }

            return selfCut.namedItems.elementsEqual(otherCut.namedItems[..<selfCut.namedItems.count], by: { $0.isEquivalent(to: $1) })
        }

        public static func from(
            _ node: SwiftPEGGrammar.Alt
        ) -> Self {
            .init(
                label: (node.altLabel?.name.string).map(String.init),
                namedItems: node.namedItems.map(NamedItem.from),
                action: node.action.map(Action.from),
                failAction: node.failAction.map(Action.from)
            )
        }
    }

    /// `actionAttribute* '{' balancedTokens '}'`
    public struct Action: Hashable, CustomStringConvertible {
        public var attributes: [ActionAttribute]
        public var string: String

        public var description: String {
            if attributes.isEmpty {
                return "{\(string)}"
            } else {
                return "\(attributes.map(\.description).joined(separator: " ")) {\(string)}"
            }
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try attributes.forEach { try $0.accept(visitor) }
            try visitor.visit(self)
            try visitor.didVisit(self)
        }

        public static func from(
            _ node: SwiftPEGGrammar.Action
        ) -> Self {
            return .init(
                attributes: node.attributes.map(ActionAttribute.from),
                string: node.rawAction
            )
        }
    }

    public struct ActionAttribute: Hashable, CustomStringConvertible {
        public var name: String

        public var description: String {
            return "@\(name)"
        }

        public static func from(
            _ node: SwiftPEGGrammar.ActionAttribute
        ) -> Self {
            return .init(
                name: String(node.name.string)
            )
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)
            try visitor.didVisit(self)
        }
    }

    @GeneratedCaseChecks
    public enum NamedItem: Hashable, CustomStringConvertible {
        /// `name=IDENT? item ('[' swiftType ']')?`
        case item(name: String? = nil, Item, type: CommonAbstract.SwiftType? = nil)
        /// ```
        /// lookahead:
        ///     | '&''&' ~ atom
        ///     | '&' ~ atom
        ///     | '!' ~ atom
        ///     | '~'
        ///     ;
        /// ```
        case lookahead(Lookahead)

        /// Gets the item associated with this named item, if it is an item,
        /// otherwise returns `nil`.
        var asItem: Item? {
            switch self {
            case .item(_, let item, _):
                return item
            case .lookahead:
                return nil
            }
        }

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
        case zeroOrMore(Atom, repetitionMode: CommonAbstract.RepetitionMode = .standard)
        /// `atom '+'`
        case oneOrMore(Atom, repetitionMode: CommonAbstract.RepetitionMode = .standard)
        /// `sep=atom '.' node=atom '+'`
        case gather(sep: Atom, node: Atom, repetitionMode: CommonAbstract.RepetitionMode = .standard)
        /// `atom`
        case atom(Atom)

        public var description: String {
            switch self {
            case .atom(let atom):
                return atom.description
            case .gather(let sep, let node, let repetitionMode):
                return "\(sep).\(node)+\(repetitionMode._suffixString)"
            case .zeroOrMore(let atom, let repetitionMode):
                return "\(atom)*\(repetitionMode._suffixString)"
            case .oneOrMore(let atom, let repetitionMode):
                return "\(atom)+\(repetitionMode._suffixString)"
            case .optional(let atom):
                return "\(atom)?"
            case .optionalItems(let alts):
                return "[\(alts.map(\.description).joined(separator: " | "))]"
            }
        }

        /// If this Item is an atom or atom-related production, returns the
        /// associated value for the atom, otherwise returns `nil`.
        ///
        /// - note: For gather items, the atom returned is the `node` associated
        /// value.
        var atom: Atom? {
            switch self {
            case .atom(let atom):
                return atom
            case .gather(_, let node, _):
                return node
            case .zeroOrMore(let atom, _):
                return atom
            case .oneOrMore(let atom, _):
                return atom
            case .optional(let atom):
                return atom
            case .optionalItems:
                return nil
            }
        }

        /// Returns a copy of `self` with Cuts (`~`) removed.
        /// If this results in this production being empty, returns `nil`.
        var removingCuts: Self? {
            switch self {
            case .atom(let atom):
                return atom.removingCuts.map(Self.atom)

            case .gather(let sep, let node, let repetitionMode):
                if let sep = sep.removingCuts, let node = node.removingCuts {
                    return Self.gather(sep: sep, node: node, repetitionMode: repetitionMode)
                }
                return nil

            case .zeroOrMore(let atom, let repetitionMode):
                return atom.removingCuts.map {
                    Self.zeroOrMore($0, repetitionMode: repetitionMode)
                }

            case .oneOrMore(let atom, let repetitionMode):
                return atom.removingCuts.map {
                    Self.oneOrMore($0, repetitionMode: repetitionMode)
                }

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

            case .oneOrMore(let atom, let repetitionMode):
                return atom.reduced.map {
                    Self.oneOrMore($0, repetitionMode: repetitionMode)
                }

            case .gather(let sep, let node, let repetitionMode):
                switch (sep.reduced, node.reduced) {
                case (let sep?, let node?):
                    return Self.gather(sep: sep, node: node, repetitionMode: repetitionMode)

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

            case .zeroOrMore(let atom, let repetitionMode):
                return atom.flattened().map({
                    Self.zeroOrMore($0, repetitionMode: repetitionMode)
                })

            case .oneOrMore(let atom, let repetitionMode):
                return atom.flattened().map({
                    Self.oneOrMore($0, repetitionMode: repetitionMode)
                })

            case .gather(let sep, let node, let repetitionMode):
                switch (sep.flattened(), node.flattened()) {
                case (let sep?, let node?):
                    return Self.gather(sep: sep, node: node, repetitionMode: repetitionMode)

                case (nil, let node?):
                    // A gather with just nodes is equivalent to a 'node+'
                    // production
                    return Self.oneOrMore(node, repetitionMode: repetitionMode)

                case (let sep?, nil):
                    // A gather with just separators is equivalent to a 'sep*'
                    // production
                    return Self.zeroOrMore(sep, repetitionMode: repetitionMode)

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

            case .gather(let sep, let node, _):
                try sep.accept(visitor)
                try node.accept(visitor)

            case .zeroOrMore(let atom, _):
                try atom.accept(visitor)

            case .oneOrMore(let atom, _):
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

        /// Returns `true` if both `self` and `other` execute equivalent productions.
        func isEquivalent(to other: Self) -> Bool {
            if self == other { return true }

            switch (self, other) {
            // Check `[ atom ]` and `atom ?`
            case (.optionalItems(let alts), .atom(let rhs)) where alts.count == 1 && alts[0].namedItems.count == 1:
                return alts[0].namedItems[0].isEquivalent(
                    to: .item(name: nil, .atom(rhs), type: nil)
                )
            // Check `atom ?` and `[ atom ]`
            case (.atom(let lhs), .optionalItems(let alts)) where alts.count == 1 && alts[0].namedItems.count == 1:
                return alts[0].namedItems[0].isEquivalent(
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
                return .zeroOrMore(Atom.from(item.atom), repetitionMode: item.repetitionMode)

            case let item as SwiftPEGGrammar.OneOrMoreItem:
                return .oneOrMore(Atom.from(item.atom), repetitionMode: item.repetitionMode)

            case let item as SwiftPEGGrammar.GatherItem:
                return .gather(sep: Atom.from(item.sep), node: Atom.from(item.item), repetitionMode: item.repetitionMode)

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
        /// `'&''&' atom`
        case forced(Atom)
        /// `~`
        case cut

        public var description: String {
            switch self {
            case .negative(let atom):
                return "!\(atom)"
            case .positive(let atom):
                return "&\(atom)"
            case .forced(let atom):
                return "&&\(atom)"
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
            case .forced(let atom):
                return atom.removingCuts.map(Self.forced)
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
            case .forced(let atom):
                return atom.reduced.map(Self.forced)
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
            case .forced(let atom):
                return atom.flattened().map(Self.forced)
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
            case .forced(let atom):
                try atom.accept(visitor)
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
            case let forced as SwiftPEGGrammar.Forced:
                return .forced(Atom.from(forced.atom))
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

    /// ```
    /// atom:
    ///     | '(' alts ')'
    ///     | IDENTIFIER atomParameters?
    ///     | string
    ///     ;
    /// ```
    public indirect enum Atom: Hashable, CustomStringConvertible {
        /// `'(' alts ')'`
        @GeneratedIsCase(accessLevel: "public")
        case group([Alt])

        /// `ident`
        case ruleName(String, parameters: [AtomParameter]? = nil)

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

            case .ruleName(let ident, let parameters):
                if let parameters {
                    return "\(ident)(\(parameters.map(\.description).joined(separator: ",")))"
                }

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
            if case .ruleName(_, let parameters?) = self {
                try parameters.forEach { try $0.accept(visitor) }
            }

            try visitor.didVisit(self)
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
                    let parameters = ident.parameters.map({ $0.parameters.map(AtomParameter.from) })

                    return .ruleName(String(value), parameters: parameters)

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

    /// ```
    /// atomParameter:
    ///     | IDENTIFIER ':' action
    ///     ;
    /// ```
    public struct AtomParameter: Hashable, CustomStringConvertible {
        public var label: String
        public var action: Action

        public var description: String {
            "\(label): \(action)"
        }

        public static func from(
            _ node: SwiftPEGGrammar.AtomParameter
        ) -> Self {

            .init(
                label: String(node.label.string),
                action: .from(node.action)
            )
        }

        /// Accepts a given visitor, and recursively passes the visitor to nested
        /// property types within this object that can be visited, if present.
        public func accept(_ visitor: some Visitor) throws {
            try visitor.willVisit(self)
            try visitor.visit(self)
            try visitor.didVisit(self)
        }
    }

    /// ```
    /// tokenDefinition:
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER '[' tokenCodeReference=STRING ']' ':' ~ tokenSyntax ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER '[' tokenCodeReference=STRING ']' ';'
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

        public var tokenCodeReference: String?

        /// The syntax definition of this token.
        public var tokenSyntax: CommonAbstract.TokenSyntax?

        /// The channel associated with this token, or `nil`, if it belongs to
        /// the default channel.
        public var tokenChannel: String? = nil

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

            switch (tokenCodeReference, tokenSyntax) {
            case (let tokenCodeReference?, let tokenSyntax?):
                return #"\#(prefix)\#(name)[\#(tokenCodeReference.debugDescription)]: \#(tokenSyntax) ;"#
            case (let tokenCodeReference?, nil):
                return #"\#(prefix)\#(name)[\#(tokenCodeReference.debugDescription)] ;"#
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
                tokenCodeReference: node.tokenCodeReference.map({ $0.rawContents() }),
                tokenSyntax: node.tokenSyntax
            )
        }
    }

    /// ```
    /// tokenChannelDeclaration:
    ///     | '@' IDENTIFIER name=IDENTIFIER '~>' tokenChannelTarget ';'
    ///     | '@' IDENTIFIER name=IDENTIFIER? ';'
    ///     ;
    /// ```
    public struct TokenChannel: Equatable, CustomStringConvertible {
        public var name: String?
        public var target: Target?

        public var description: String {
            guard let name else {
                return "@channel ;"
            }
            if let target {
                return "@channel \(name) ~> \(target) ;"
            }
            return "@channel \(name) ;"
        }

        public static func from(
            _ node: SwiftPEGGrammar.TokenChannelDeclaration
        ) -> Self {

            .init(
                name: node.name.map(String.init),
                target: node.target.flatMap(Target.from)
            )
        }

        public enum Target: Equatable, CustomStringConvertible {
            case skip
            case regular

            public var description: String {
                switch self {
                case .skip: "skip"
                case .regular: "regular"
                }
            }

            static func from(
                _ node: SwiftPEGGrammar.TokenChannelTarget
            ) -> Self? {

                switch node.identifier.string {
                case "skip":
                    return .skip

                case "regular":
                    return .regular

                default:
                    return nil
                }
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

        func willVisit(_ node: RuleParameter) throws
        func visit(_ node: RuleParameter) throws
        func didVisit(_ node: RuleParameter) throws

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

        func willVisit(_ node: AtomParameter) throws
        func visit(_ node: AtomParameter) throws
        func didVisit(_ node: AtomParameter) throws
    }
}

public extension InternalGrammar.Visitor {
    func willVisit(_ node: InternalGrammar.Grammar) throws { }
    func visit(_ node: InternalGrammar.Grammar) throws { }
    func didVisit(_ node: InternalGrammar.Grammar) throws { }

    func visit(_ node: InternalGrammar.MetaProperty) throws { }

    func willVisit(_ node: InternalGrammar.Rule) throws { }
    func visit(_ node: InternalGrammar.Rule) throws { }
    func didVisit(_ node: InternalGrammar.Rule) throws { }

    func willVisit(_ node: InternalGrammar.RuleParameter) throws { }
    func visit(_ node: InternalGrammar.RuleParameter) throws { }
    func didVisit(_ node: InternalGrammar.RuleParameter) throws { }

    func willVisit(_ node: InternalGrammar.Alt) throws { }
    func visit(_ node: InternalGrammar.Alt) throws { }
    func didVisit(_ node: InternalGrammar.Alt) throws { }

    func willVisit(_ node: InternalGrammar.Action) throws { }
    func visit(_ node: InternalGrammar.Action) throws { }
    func didVisit(_ node: InternalGrammar.Action) throws { }

    func willVisit(_ node: InternalGrammar.ActionAttribute) throws { }
    func visit(_ node: InternalGrammar.ActionAttribute) throws { }
    func didVisit(_ node: InternalGrammar.ActionAttribute) throws { }

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

    func willVisit(_ node: InternalGrammar.AtomParameter) throws { }
    func visit(_ node: InternalGrammar.AtomParameter) throws { }
    func didVisit(_ node: InternalGrammar.AtomParameter) throws { }

    func visit(_ node: InternalGrammar.TokenDefinition) throws { }
}
