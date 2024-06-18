/// A set of syntax construction structures that are neither unique to the grammar
/// processor nor so tied to the parser that they cannot be used elsewhere in the
/// code.
///
/// Namespaced to keep type pollution to a minimum.
public enum CommonAbstract {
    /// A dual String representation that has carries a literal version and a
    /// as-per-source version containing the original escape sequences and quotes.
    public enum DualString: Hashable, ExpressibleByStringLiteral {
        /// A dual string that originated from parsing a SwiftPEG grammar file,
        /// with both the parsed contents and the original string, with quotes,
        /// as found in the source code.
        case fromSource(contents: String, original: String)

        /// A dual string that contains only a raw representation, and was generated
        /// by code.
        case fromCode(contents: String)

        // Convenience members

        public var contents: String {
            switch self {
            case .fromSource(let contents, _), .fromCode(let contents):
                return contents
            }
        }

        public var asStringLiteral: String {
            switch self {
            case .fromSource(_, let original):
                return original
            case .fromCode(let contents):
                return StringEscaping.escapeAsStringLiteral(contents)
            }
        }

        public init(stringLiteral value: String) {
            self = .fromCode(contents: value)
        }

        /// Appends two dual strings into a new, combined one.
        /// The result of the operation is always a `DualString.fromCode` value.
        public func appending(_ other: Self) -> Self {
            .fromCode(contents: contents + other.contents)
        }

        public func hash(into hasher: inout Hasher) {
            hasher.combine(contents)
        }

        public static func from(_ string: SwiftPEGGrammar.GrammarString) -> Self {
            return .fromSource(contents: string.rawContents(), original: string.asStringLiteral())
        }

        public static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.contents == rhs.contents
        }
    }

    // MARK: - SwiftType

    /// Describes the type of a grammar production, as a Swift type.
    ///
    /// Represents the construct:
    /// ```
    /// swiftType:
    ///     | '(' swiftTypeList? ')'
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
    ///
    /// And for tuple type elements:
    /// ```
    /// swiftTupleTypeList:
    ///     | ','.swiftTupleTypeElement+
    ///     ;
    /// swiftTupleTypeElement:
    ///     | label=IDENTIFIER ':' swiftType
    ///     | swiftType
    ///     ;
    /// ```
    public indirect enum SwiftType: Hashable, CustomStringConvertible, ExpressibleByStringLiteral {
        /// A Swift tuple type
        ///
        /// `'(' swiftTypeList? ')'`
        case tuple([TupleTypeElement])

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
            case .tuple(let elements): "(\(elements.map(\.description).joined(separator: ", ")))"
            case .dictionary(let key, let value): "[\(key): \(value)]"
            case .array(let element): "[\(element)]"
            case .optional(let wrappedType): "\(wrappedType)?"
            case .nested(let base, let nominal): "\(base).\(nominal)"
            case .nominal(let nominal): nominal.description
            }
        }

        /// If this Swift type is an optionally-wrapped type, returns the wrapped
        /// type, otherwise returns `self`.
        public var unwrapped: Self {
            switch self {
            case .optional(let wrapped):
                return wrapped
            default:
                return self
            }
        }

        /// Initializes this `SwiftType` as a nominal type of the given identifier
        /// string.
        public init(stringLiteral value: String) {
            self = .nominal(.init(identifier: value))
        }
    }

    /// An auxiliary type to `SwiftType` that is used to describe tuple types,
    /// with optional labels.
    ///
    /// ```
    /// swiftTupleTypeList:
    ///     | ','.swiftTupleTypeElement+
    ///     ;
    /// swiftTupleTypeElement:
    ///     | label=IDENTIFIER ':' swiftType
    ///     | swiftType
    ///     ;
    /// ```
    public enum TupleTypeElement: Hashable, CustomStringConvertible {
        indirect case unlabeled(SwiftType)
        indirect case labeled(label: String, SwiftType)

        /// Applies optional-wrapping to this tuple type element.
        var optionalWrapped: Self {
            switch self {
            case .labeled(let label, let type):
                return .labeled(label: label, .optional(type))
            case .unlabeled(let type):
                return .unlabeled(.optional(type))
            }
        }

        /// Applies optional-unwrapping to this tuple type element.
        var unwrapped: Self {
            switch self {
            case .labeled(let label, let type):
                return .labeled(label: label, type.unwrapped)
            case .unlabeled(let type):
                return .unlabeled(type.unwrapped)
            }
        }

        public var description: String {
            switch self {
            case .labeled(let label, let type):
                return "\(label): \(type)"
            case .unlabeled(let type):
                return type.description
            }
        }

        /// Returns the swift type associated with this tuple type element.
        public var swiftType: SwiftType {
            switch self {
            case .labeled(_, let type),
                .unlabeled(let type):
                return type
            }
        }

        /// Returns the label associated with this tuple type element, if present.
        public var label: String? {
            switch self {
            case .labeled(let label, _):
                return label
            case .unlabeled:
                return nil
            }
        }

        /// Convenience initializer for creating labeled/unlabeled tuple elements
        public init(label: String?, _ type: SwiftType) {
            if let label {
                self = .labeled(label: label, type)
            } else {
                self = .unlabeled(type)
            }
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

// MARK: Repetition mode

extension CommonAbstract {
    /// Specifies the mode for a repetition ('*'/'+') that is present in the middle
    /// of an alternative..
    public enum RepetitionMode {
        /// Repeats as long as the production succeeds, without giving up on tokens.
        case standard

        /// Starts capturing the least number of items with the repetition,
        /// before trying the rest of the items in the alternative.
        ///
        /// If the rest of the items fail, the alternative backtracks and tries
        /// to capture one more item in the repetition, repeating until either
        /// no more items can be capture, or the rest of the alternative succeeds.
        case minimal

        /// Starts capturing the maximal number of items with the repetition,
        /// before trying the rest of the items in the alternative.
        ///
        /// If the rest of the items fail, the alternative backtracks and tries
        /// to capture one less item in the repetition, repeating until either
        /// one ('+') or zero ('*') items have matched, or the rest of the
        /// alternative succeeds.
        case maximal

        var _suffixString: String {
            switch self {
            case .standard: ""
            case .maximal: ">"
            case .minimal: "<"
            }
        }
    }
}

// MARK: Token syntax definition

extension CommonAbstract {
    /// A token syntax description.
    ///
    /// ```
    /// tokenSyntax:
    ///     | '|'.tokenSyntaxAlt+
    ///     ;
    /// ```
    public struct TokenSyntax: Equatable, CustomStringConvertible {
        public var alts: [TokenAlt]

        public var description: String {
            return alts.map(\.description).joined(separator: " | ")
        }

        public init(alts: [CommonAbstract.TokenAlt]) {
            self.alts = alts
        }

        /// Returns `true` if this token syntax represents a static token, i.e.
        /// a token syntax that matches against a single literal terminal with
        /// no repetitions or alts.
        public func isStatic() -> Bool {
            staticTerminal() != nil
        }

        /// If this token syntax represents a static token, returns the terminal
        /// associated with that static token from within the syntax.
        ///
        /// - seealso: ``TokenSyntax.isStatic()``
        public func staticTerminal() -> DualString? {
            guard let alt = self.alts.first, self.alts.count == 1 else {
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

            return literal
        }

        /// Returns `true` if this token syntax represents a terminal, i.e.
        /// a token syntax that contains a single alt, with a single item, that
        /// is a terminal, with no exclusions.
        public func isTerminal() -> Bool {
            asTerminal() != nil
        }

        /// If this token syntax represents a single terminal, with no exclusions,
        /// returns the associated terminal.
        ///
        /// - seealso: ``TokenSyntax.isTerminal()``
        public func asTerminal() -> TokenTerminal? {
            guard let alt = self.alts.first, self.alts.count == 1 else {
                return nil
            }
            guard alt.trailExclusions.isEmpty else {
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

            return atom.terminal
        }

        /// Returns `true` if this token syntax can be considered a prefix of
        /// another, i.e. `other` always matches on the same inputs as `self`
        /// (but not necessarily the other way around), and matches on an input
        /// by this token syntax have a length that are always less than or equal
        /// to the length of the match on the same input in `other`.
        ///
        /// Two token syntaxes that are identical are considered to be prefixes
        /// of each other (non-strict prefix).
        public func isPrefix(of other: TokenSyntax) -> Bool {
            if self == other { return true }
            if alts.isEmpty || other.alts.isEmpty { return false }

            for alt in alts {
                for otherAlt in other.alts {
                    if !alt.isPrefix(of: otherAlt) {
                        return false
                    }
                }
            }

            return true
        }

        /// Flattens nested structures that can be simplified into shallower
        /// constructions.
        func flattened() -> Self {
            var flattenedAlts: [TokenAlt] = []
            for alt in alts {
                let flatAlt = alt.flattened()
                // If the alt is a single group with simple atoms, expand the
                // group
                if flatAlt.items.count == 1 && flatAlt.items[0].isGroup {
                    flattenedAlts.append(contentsOf:
                        flatAlt.items[0].atoms.map { atom in
                            TokenAlt(items: [.atom(atom)], trailExclusions: [])
                        }
                    )
                    continue
                }

                flattenedAlts.append(flatAlt)
            }

            return Self(alts: flattenedAlts)
        }
    }

    /// A token syntax alternative.
    ///
    /// ```
    /// tokenSyntaxAlt:
    ///     | tokenSyntaxItem+ tokenSyntaxExclusion*
    ///     ;
    /// ```
    public struct TokenAlt: Equatable, CustomStringConvertible {
        public var items: [TokenItem]
        public var trailExclusions: [TokenExclusion]

        public var description: String {
            (items.map(\.description) + trailExclusions.map(\.description)).joined(separator: " ")
        }

        public init(items: [TokenItem], trailExclusions: [TokenExclusion]) {
            self.items = items
            self.trailExclusions = trailExclusions
        }

        /// Returns `true` if this alt can be considered a prefix of another,
        /// i.e. `other` always matches on the same inputs as `self` (but not
        /// necessarily the other way around), and matches on an input by this
        /// alt have a length that are always less than or equal to the length
        /// of the match on the same input in `other`.
        ///
        /// Two alts that are identical are considered to be prefixes of each
        /// other (non-strict prefix).
        public func isPrefix(of other: TokenAlt) -> Bool {
            if self == other { return true }

            // Attempt to ignore leading identical items
            let itemEqualities = zip(self.items, other.items).map(==)
            let firstChangeIndex = itemEqualities.firstIndex(of: false) ?? itemEqualities.count

            guard firstChangeIndex < items.count else {
                // Items are equivalent?
                return true
            }
            guard firstChangeIndex < other.items.count else {
                // TODO: Validate this assumption
                // If `self` has more items, it will most likely be a longer match
                // than `other`.
                return false
            }

            return items[firstChangeIndex].isPrefix(of: other.items[firstChangeIndex])
        }

        /// Flattens nested structures that can be simplified into shallower
        /// constructions.
        func flattened() -> Self {
            var newItems: [TokenItem] = []

            // Merge sequential literal terminals
            for item in items.map({ $0.flattened() }) {
                guard let last = newItems.last else {
                    newItems.append(item)
                    continue
                }

                switch (last, item) {
                case (.atom(let last), .atom(let next))
                    where last.excluded.isEmpty && next.excluded.isEmpty:

                    if
                        let last = last.terminal.asLiteral,
                        let next = next.terminal.asLiteral
                    {
                        newItems[newItems.count - 1] = .atom(.init(
                            terminal: .literal(last.appending(next))
                        ))
                    } else {
                        newItems.append(item)
                    }
                default:
                    newItems.append(item)
                }
            }

            return Self(items: newItems, trailExclusions: trailExclusions)
        }
    }

    /// A token syntax item.
    ///
    /// ```
    /// tokenSyntaxItem:
    ///     | '(' '|'.tokenSyntaxAtom+ ')' '*'
    ///     | '(' '|'.tokenSyntaxAtom+ ')' '+'
    ///     | '(' '|'.tokenSyntaxAtom+ ')' '?'
    ///     | '(' '|'.tokenSyntaxAtom+ ')'
    ///     | tokenSyntaxAtom '*'
    ///     | tokenSyntaxAtom '+'
    ///     | tokenSyntaxAtom '?'
    ///     | tokenSyntaxAtom
    ///     ;
    /// ```
    @GeneratedCaseChecks(accessLevel: "public")
    public enum TokenItem: Equatable, CustomStringConvertible {
        /// `'(' '|'.tokenSyntaxAtom+ ')' '*'`
        case zeroOrMore([TokenAtom])

        /// `'(' '|'.tokenSyntaxAtom+ ')' '+'`
        case oneOrMore([TokenAtom])

        /// `'(' '|'.tokenSyntaxAtom+ ')' '?'`
        case optionalGroup([TokenAtom])

        /// `'(' '|'.tokenSyntaxAtom+ ')'`
        case group([TokenAtom])

        /// `tokenSyntaxAtom '?'`
        case optionalAtom(TokenAtom)

        /// `tokenSyntaxAtom`
        case atom(TokenAtom)

        /// Fetches all atoms contained within this token item.
        var atoms: [TokenAtom] {
            switch self {
            case .oneOrMore(let atoms), .zeroOrMore(let atoms),
                .optionalGroup(let atoms), .group(let atoms):
                return atoms

            case .optionalAtom(let atom), .atom(let atom):
                return [atom]
            }
        }

        public var description: String {
            switch self {
            case .oneOrMore(let atoms):
                return "(\(atoms.map(\.description).joined(separator: " | ")))+"

            case .zeroOrMore(let atoms):
                return "(\(atoms.map(\.description).joined(separator: " | ")))*"

            case .optionalGroup(let atoms):
                return "(\(atoms.map(\.description).joined(separator: " | ")))?"

            case .group(let atoms):
                return "(\(atoms.map(\.description).joined(separator: " | ")))"

            case .optionalAtom(let atom):
                return "\(atom)?"

            case .atom(let atom):
                return atom.description
            }
        }

        /// Returns `true` if this item can be considered a prefix of another,
        /// i.e. if `self` matching a token guarantees `other` does too (but not
        /// necessarily the other way around), those simultaneous matches are
        /// always of equal length or shorter in `self` than in `other`.
        ///
        /// Two items that are identical are considered to be prefixes of each
        /// other (non-strict prefix).
        ///
        /// If `other` consists of a non-excluded repeated `any` terminal, this
        /// method always returns `true`.
        public func isPrefix(of other: Self) -> Bool {
            if self == other { return true }

            /// Returns `true` iff `lhs` is a subset of `rhs`, or `rhs` is capable
            /// of matching all characters that `lhs` matches.
            func _isSubset(_ lhs: some Collection<TokenAtom>, _ rhs: some Collection<TokenAtom>) -> Bool {
                // lhs is a subset of rhs only if every atom in lhs is a subset
                // of some atom in rhs
                for lhs in lhs {
                    if !rhs.contains(where: { lhs.isSubset(of: $0) }) {
                        return false
                    }
                }

                return true
            }

            /// Returns `true` iff `lhs` is a prefix of `rhs`, or `rhs` is capable
            /// of matching all characters that `lhs` matches.
            func _isPrefix(_ lhs: some Collection<TokenAtom>, _ rhs: some Collection<TokenAtom>) -> Bool {
                // lhs is a prefix of rhs only if every atom in lhs is a prefix
                // of some atom in rhs
                for lhs in lhs {
                    if !rhs.contains(where: { lhs.isPrefix(of: $0) }) {
                        return false
                    }
                }

                return true
            }

            switch (self, other) {
            case (.optionalGroup, _), (.optionalAtom, _):
                // Optionals are prefix of all other items they are not optionally
                // prefixes to
                return _isPrefix(atoms, other.atoms)

            // Zero or more
            case (.zeroOrMore(let lhs), .zeroOrMore(let rhs)):
                return _isSubset(lhs, rhs)

            case (.zeroOrMore(let lhs), .oneOrMore(let rhs)):
                return _isSubset(lhs, rhs)

            case (.zeroOrMore, .group), (.zeroOrMore, .optionalGroup):
                return false

            case (.zeroOrMore, .atom), (.zeroOrMore, .optionalAtom):
                return false

            // One or more
            case (.oneOrMore(let lhs), .oneOrMore(let rhs)):
                return _isSubset(lhs, rhs)

            case (.oneOrMore(let lhs), .zeroOrMore(let rhs)):
                return _isSubset(lhs, rhs)

            case (.oneOrMore, .group), (.oneOrMore, .optionalGroup):
                return false

            case (.oneOrMore, .atom), (.oneOrMore, .optionalAtom):
                return false

            // Group
            case (.group(let lhs), .zeroOrMore(let rhs)):
                return _isSubset(lhs, rhs)

            case (.group(let lhs), .oneOrMore(let rhs)):
                return _isSubset(lhs, rhs)

            case (.group(let lhs), .group(let rhs)):
                return _isPrefix(lhs, rhs)

            case (.group(let lhs), .optionalGroup(let rhs)):
                return _isPrefix(lhs, rhs)

            case (.group(let lhs), .optionalAtom(let rhs)):
                return _isPrefix(lhs, [rhs])

            case (.group(let lhs), .atom(let rhs)):
                return _isPrefix(lhs, [rhs])

            // Atom
            case (.atom(let lhs), .zeroOrMore(let rhs)):
                return _isSubset([lhs], rhs) || _isPrefix([lhs], rhs)

            case (.atom(let lhs), .oneOrMore(let rhs)):
                return _isSubset([lhs], rhs) || _isPrefix([lhs], rhs)

            case (.atom(let lhs), .optionalGroup(let rhs)):
                return _isPrefix([lhs], rhs)

            case (.atom(let lhs), .group(let rhs)):
                return _isPrefix([lhs], rhs)

            case (.atom(let lhs), .optionalAtom(let rhs)):
                return lhs.isPrefix(of: rhs)

            case (.atom(let lhs), .atom(let rhs)):
                return lhs.isPrefix(of: rhs)
            }
        }

        /// Flattens nested structures that can be simplified into shallower
        /// constructions.
        func flattened() -> Self {
            switch self {
            case .group(let atoms) where atoms.count == 1:
                return .atom(atoms[0])
            case .optionalGroup(let atoms) where atoms.count == 1:
                return .optionalAtom(atoms[0])
            default:
                return self
            }
        }
    }

    /// A token syntax terminal element.
    ///
    /// ```
    /// tokenSyntaxAtom:
    ///     | tokenSyntaxExclusion* tokenSyntaxTerminal
    ///     ;
    /// ```
    public struct TokenAtom: Equatable, CustomStringConvertible {
        // TODO: Rework exclusion to be at the terminal level; exclusions don't make sense for some terminals like string literals

        /// A set of patterns to not match against.
        public var excluded: [TokenExclusion]

        /// The terminal that forms the main match for this atom.
        public var terminal: TokenTerminal

        /// Returns `true` if this atom describes a pair of terminal+exclusion
        /// that result in no input ever matching.
        ///
        /// Atoms that don't have exclusions cannot be unfulfillable.
        public var isUnfulfillable: Bool {
            guard !excluded.isEmpty else {
                return false
            }

            switch terminal {
            case .any:
                // No (reasonable) finite combination of exclusions can void
                // the any token
                return false

            case .identifier(let ident):
                return excluded.contains(.identifier(ident))

            case .literal(let literal):
                let literal = literal.contents

                for exclusion in excluded {
                    switch exclusion {
                    case .string(let exclusion):
                        if literal.hasPrefix(exclusion.contents) {
                            return true
                        }

                    case .rangeLiteral(let low, let high):
                        let low = low.contents
                        let high = high.contents
                        let trimmed = literal.prefix(max(low.count, high.count))

                        if (low...high).contains(String(trimmed)) {
                            return true
                        }

                    default:
                        break
                    }
                }

                return false

            case .rangeLiteral(let low, let high) where low == high:
                return excluded.contains(.string(low))

            case .rangeLiteral(let low, let high):
                let low = low.contents
                let high = high.contents

                // Detect excluded ranges fully containing the range literal
                let excludedRanges = excluded.compactMap(\.rangeLiteral).map{
                    ($0.0.contents...$0.1.contents)
                }

                if excludedRanges.contains(where: { $0.lowerBound <= low && $0.upperBound >= high }) {
                    return true
                }

                // Although technically a discrete space, there is no native way
                // to deal with striding and covering Character spaces in Swift;
                // for now, consider all ranges (except for single-item ranges)
                // non-voidable
                return false

            case .characterPredicate:
                return false
            }
        }

        public var description: String {
            let slices = excluded.map(\.description) + [terminal.description]
            return slices.joined(separator: " ")
        }

        public init(excluded: [TokenExclusion] = [], terminal: TokenTerminal) {
            self.excluded = excluded
            self.terminal = terminal
        }

        /// Returns `true` if this atom can be considered a subset of another,
        /// i.e. if this atom matching a token guarantees the other atom also
        /// does, but no necessarily the other way around.
        ///
        /// Subset relationship is defined in the following ways:
        ///
        /// - Any atom is a subset of another atom that has as terminal the 'any'
        ///     token with no exclusions;
        /// - An atom that is structurally equal to another is considered a
        ///     subset (non-strict subset);
        /// - An atom is not a subset of another if its terminal is not a subset
        ///     of the latter's;
        /// - An atom with more specific exclusions that another atom with the
        ///     same terminal is considered a strict subset of the other;
        /// - An atom with one or more identifier token exclusions cannot be a
        ///     subset of another atom, except for itself (non-strict subset).
        public func isSubset(of other: Self) -> Bool {
            if self == other {
                return true
            }
            if other.terminal.isAny && other.excluded.isEmpty {
                return true
            }

            guard !excluded.contains(where: \.isIdentifier) else {
                return false
            }
            guard !other.excluded.contains(where: \.isIdentifier) else {
                return false
            }
            guard terminal.isSubset(of: other.terminal) else {
                return false
            }

            if _isExcluded(other.excluded) {
                return false
            }

            let selfExcl = Set(excluded)
            return selfExcl.isSubset(of: other.excluded)
        }

        /// Returns `true` if `self` is a prefix on `other`, such that all matches
        /// of `self` have an equal or greater match length on `other`.
        ///
        /// Returns `true` if `self` is identical to `other`.
        public func isPrefix(of other: Self) -> Bool {
            if self == other {
                return true
            }

            if _isExcluded(other.excluded) {
                return false
            }

            return self.terminal.isPrefix(of: other.terminal)
        }

        private func _isExcluded(_ excludes: [TokenExclusion]) -> Bool {
            switch terminal {
            case .literal(let literal):
                if
                    excludes.contains(where: {
                        $0.asString.map(literal.contents.hasPrefix) == true ||
                        $0.asString.map { $0.hasPrefix(literal.contents) } == true
                    })
                {
                    return true
                }
            default:
                break
            }

            return false
        }
    }

    /// Specifies a negative lookahead prior to a token terminal.
    ///
    /// ```
    /// tokenSyntaxExclusion:
    ///     | '!' STRING
    ///     | '!' IDENTIFIER
    ///     | '!' STRING '...' STRING
    ///     ;
    /// ```
    @GeneratedCaseChecks(accessLevel: "public")
    public enum TokenExclusion: Hashable, CustomStringConvertible {
        case string(DualString)
        case identifier(String)
        case rangeLiteral(DualString, DualString)

        var asString: String? {
            switch self {
            case .string(let value):
                return value.contents
            default:
                return nil
            }
        }

        var rangeLiteral: (DualString, DualString)? {
            switch self {
            case .rangeLiteral(let low, let high):
                return (low, high)
            default:
                return nil
            }
        }

        public var description: String {
            switch self {
            case .string(let string):
                return "!\(string.asStringLiteral)"

            case .identifier(let ident):
                return "!\(ident)"

            case .rangeLiteral(let start, let end):
                return "!\(start)...\(end)"
            }
        }
    }

    /// A token syntax terminal element.
    ///
    /// ```
    /// tokenSyntaxTerminal:
    ///     | IDENTIFIER action
    ///     | string '...' string
    ///     | string
    ///     | IDENTIFIER
    ///     | '.'
    ///     ;
    /// ```
    public enum TokenTerminal: Equatable, CustomStringConvertible {
        /// `IDENTIFIER action`
        case characterPredicate(String, String)

        /// `string '...' string`
        case rangeLiteral(DualString, DualString)

        /// `string`
        case literal(DualString)

        /// IDENTIFIER
        case identifier(String)

        /// `'.'`
        @GeneratedIsCase(accessLevel: "public")
        case any

        fileprivate var _range: ClosedRange<Character>? {
            switch self {
            case .rangeLiteral(let lower, let upper):
                guard let lower = lower.contents.first, let upper = upper.contents.first else {
                    return nil
                }

                return lower...upper
            default:
                return nil
            }
        }

        /// If this `TokenTerminal` is a `literal()` case, returns the associated
        /// value, otherwise, returns `nil`.
        public var asLiteral: DualString? {
            switch self {
            case .literal(let value): return value
            default: return nil
            }
        }

        public var description: String {
            switch self {
            case .characterPredicate(let bind, let action):
                return "\(bind) {\(action)}"

            case .rangeLiteral(let start, let end):
                return "\(start.asStringLiteral)...\(end.asStringLiteral)"

            case .literal(let string):
                return "\(string.asStringLiteral)"

            case .identifier(let ident):
                return ident

            case .any:
                return "."
            }
        }

        /// Returns `true` if this terminal can be considered a subset of another,
        /// i.e. if this terminal matching a token guarantees the other terminal
        /// also does, but no necessarily the other way around.
        ///
        /// Subset relationship is defined in the following ways:
        ///
        /// - All terminals are a subset of `TokenTerminal.any`, including itself
        ///     (non-strict subset);
        /// - A terminal that is structurally equal to another is considered a
        ///     subset (non-strict subset);
        /// - An identifier terminal cannot be a subset of any other terminal,
        ///     except another identical identifier terminal, due to the above rule;
        /// - Likewise, a character predicate cannot be a subset of any other
        ///     terminal, except another identical character predicate terminal;
        /// - Literal terminals also cannot be subset of other literal terminals,
        ///     except those of identical values.
        /// - A literal terminal is a subset of a ranged terminal only if it is
        ///     a single character long, and contained within the ranged terminal's
        ///     bounds;
        /// - A ranged terminal is a subset of another ranged terminal only if
        ///     the former's bounds are fully contained within the latter's.
        public func isSubset(of other: Self) -> Bool {
            if self == other { return true }

            switch (self, other) {
            case (_, .any):
                return true

            case (.rangeLiteral, .rangeLiteral):
                guard let lhs = _range, let rhs = other._range else {
                    return false
                }

                return lhs.lowerBound >= rhs.lowerBound && lhs.upperBound <= rhs.upperBound

            case (.literal(let lit), .rangeLiteral):
                guard let char = lit.contents.first, let range = other._range, lit.contents.count == 1 else {
                    return false
                }

                return range.contains(char)

            case (.characterPredicate, _), (_, .characterPredicate):
                return false

            case (.literal, _), (_, .literal):
                return false

            case (.identifier, _), (_, .identifier):
                return false

            case (.any, _):
                return false
            }
        }

        /// Returns `true` if `self` is a prefix on `other`, such that all matches
        /// of `self` also match `other`, but not necessarily the other way around,
        /// and the matches of `self` always have an equal or greater match length
        /// on `other`.
        ///
        /// Returns `true` if `self` is identical to `other`.
        public func isPrefix(of other: Self) -> Bool {
            if self == other { return true }

            switch (self, other) {
            case (.literal(let literal), .any)
                where literal.contents.count == 1:
                return true

            case (.rangeLiteral(let low, let high), .any)
                where low.contents.count == 1 && high.contents.count == 1:
                return true

            case (.literal(let lhs), .literal(let rhs)):
                return rhs.contents.hasPrefix(lhs.contents)

            case (.literal(let lhs), .rangeLiteral(let low, let high))
                where lhs.contents.count <= low.contents.count:
                return (low.contents...high.contents).contains(lhs.contents)

            default:
                return false
            }
        }
    }
}
