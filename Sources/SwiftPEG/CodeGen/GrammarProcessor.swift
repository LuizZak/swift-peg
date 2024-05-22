/// Performs pre-code generation operations and returns an internal representation
/// grammar suitable to be consumed by code generators.
public class GrammarProcessor {
    /// Regex for validating rule names.
    public static let ruleNameGrammar = #"[A-Za-z][0-9A-Za-z_]*"#

    /// Set of identifiers that cannot be used as bare identifiers in Swift, and
    /// must be escaped with backticks (`)
    public static let invalidBareIdentifiers: Set<String> = [
        "_", "var", "let", "nil", "class", "struct", "func", "protocol", "enum",
        "try", "throws", "deinit", "init", "if", "for", "else", "while", "switch",
        "repeat", "do", "public", "private", "fileprivate", "internal", "static",
        "self",
    ]

    let grammar: Metagrammar.Grammar

    /// Rules remaining to be generated.
    var remaining: [String: Metagrammar.Rule] = [:]

    /// Stores information about locally-created variables when processing alts
    /// of rules.
    var localVariableStack: [[String]] = []

    /// A list of non-Error diagnostics issued during grammar analysis.
    private(set) public var diagnostics: [GrammarProcessorDiagnostic] = []

    /// If `true`, prints diagnostics into stdout.
    public var verbose: Bool

    /// Prepares a `GrammarProcessor` instance based on a given parsed grammar.
    public init(_ grammar: Metagrammar.Grammar, verbose: Bool = false) throws {
        self.grammar = grammar
        self.verbose = verbose

        let knownRules = try validateRuleNames()
        let tokens = try validateTokenReferences()
        try validateReferences(tokens: tokens)

        try computeNullables(knownRules)

        self.remaining = knownRules
    }

    /// Returns the reduced and tagged grammar for grammar analysis.
    public func generatedGrammar() -> Grammar {
        .from(grammar)
    }

    func validateRuleNames() throws -> [String: Metagrammar.Rule] {
        var knownRules: [String: Metagrammar.Rule] = [:]

        for rule in grammar.rules {
            let ruleName = try validateRuleName(rule)

            if let existing = knownRules[ruleName] {
                throw GrammarProcessorError.repeatedRuleName(ruleName, rule, prior: existing)
            } else {
                knownRules[ruleName] = rule
            }
        }

        return knownRules
    }

    func validateRuleName(_ rule: Metagrammar.Rule) throws -> String {
        let ruleName = rule.name.name.identifier
        if ruleName.isEmpty {
            throw GrammarProcessorError.invalidRuleName(desc: "Rule name cannot be empty", rule)
        }
        if try Regex(Self.ruleNameGrammar).wholeMatch(in: ruleName) == nil {
            throw GrammarProcessorError.invalidRuleName(desc: "Expected rule names to match regex '\(Self.ruleNameGrammar)'", rule)
        }
        if Self.invalidBareIdentifiers.contains(ruleName) {
            return "`\(ruleName)`"
        }

        return ruleName
    }

    func validateTokenReferences() throws -> Set<String> {
        var knownTokens: [String: Metagrammar.Meta] = [:]
        var issuedWarnings: Set<String> = []

        for token in tokenMetaProperties() {
            guard let value = token.value as? Metagrammar.MetaIdentifierValue else {
                diagnostics.append(.tokenMissingName(token))
                continue
            }

            let name = value.identifier.identifier
            guard !issuedWarnings.contains(name) else {
                continue
            }

            if let prior = knownTokens[name] {
                diagnostics.append(
                    .repeatedTokenDeclaration(name: name, token, prior: prior)
                )
                issuedWarnings.insert(name)
            } else {
                knownTokens[name] = token
            }
        }

        return Set(knownTokens.keys)
    }

    func validateReferences(tokens: Set<String>) throws {
        var knownNames: [(String, Metagrammar.IdentAtom.Identity)]
        knownNames = tokens.map {
            ($0, .token)
        }
        for rule in grammar.rules {
            let name = rule.name.name.identifier
            knownNames.append(
                (name, .ruleName)
            )
        }

        let visitor = ReferenceVisitor(knownIdentifiers: knownNames)

        let walker = NodeWalker(visitor: visitor)
        try walker.walk(grammar)

        guard !visitor.unknownReferences.isEmpty else {
            // Ok!
            return
        }

        for ref in visitor.unknownReferences {
            guard let rule: Metagrammar.Rule = ref.firstAncestor() else {
                fatalError("Found atom \(ref.name) @ \(ref.location) that has no Rule parent?")
            }

            throw GrammarProcessorError.unknownReference(ref, rule)
        }
    }

    func validateNamedItems() throws {
        let visitor = NamedItemVisitor { node in
            guard let name = node.name?.identifier else {
                return
            }
            
            if name == "_" {
                throw GrammarProcessorError.invalidNamedItem(
                    desc: "Name cannot be '_'",
                    node
                )
            }
            if name == "cut" {
                // TODO: Lift this restriction by dynamically deriving a suitable name for a cut local?
                throw GrammarProcessorError.invalidNamedItem(
                    desc: "Name cannot be 'cut' as it interferes with locals generated by parsers",
                    node
                )
            }
            if name.hasPrefix("_") {
                throw GrammarProcessorError.invalidNamedItem(
                    desc: "Names cannot start with '_'",
                    node
                )
            }
        }

        let walker = NodeWalker(visitor: visitor)
        try walker.walk(grammar)
    }

    /// Gets all @token meta-properties in the grammar.
    func tokenMetaProperties() -> [Metagrammar.Meta] {
        return grammar.metas.filter { $0.name.identifier == "token" }
    }

    func printIfVerbose(_ item: Any) {
        guard verbose else { return }

        print(item)
    }

    func toInternalRepresentation() -> [Rule] {
        grammar.rules.map(Rule.from)
    }

    /// An error that can be raised by `GrammarProcessor` during grammar analysis and parser
    /// generation.
    public enum GrammarProcessorError: Error, CustomStringConvertible {
        /// Rules found sharing the same name.
        case repeatedRuleName(String, Metagrammar.Rule, prior: Metagrammar.Rule)
        /// Rule found that has an invalid name.
        case invalidRuleName(desc: String, Metagrammar.Rule)
        /// Named item found that has an invalid name.
        case invalidNamedItem(desc: String, Metagrammar.NamedItem)
        /// An identifier was found in a rule that could not be resolved to a
        /// rule or token name.
        case unknownReference(Metagrammar.IdentAtom, Metagrammar.Rule)

        /// An attempt at resolving a left recursion and find a leader rule from
        /// a set of rules has failed.
        case unresolvedLeftRecursion(ruleNames: [String])

        /// A generic error with an attached message.
        case message(String)

        public var description: String {
            switch self {
            case .repeatedRuleName(let name, let rule, let prior):
                return "Rule '\(name)' re-declared @ \(rule.location). Original declaration @ \(prior.location)."
            case .invalidRuleName(let desc, let rule):
                return "Rule name '\(rule.name.name.identifier)' @ \(rule.location) is not valid: \(desc)"
            case .invalidNamedItem(let desc, let item):
                return "Named item '\(item.name?.identifier ?? "<nil>")' @ \(item.location) is not valid: \(desc)"
            case .unknownReference(let atom, let rule):
                return """
                Reference to unknown identifier '\(atom.name)' @ \(atom.location) in rule '\(rule.name.shortDebugDescription)'. \
                Did you forget to forward-declare a token with '@token \(atom.name);'?
                """
            case .unresolvedLeftRecursion(let ruleNames):
                return "Could not resolve left recursion with a lead rule in the set \(ruleNames)"
            case .message(let message):
                return message
            }
        }
    }

    /// A non-fatal GrammarProcessor diagnostic.
    public enum GrammarProcessorDiagnostic {
        /// A token has been declared multiple times with '@token'.
        /// Only reported the first time a duplicated token with a matching name
        /// is declared.
        case repeatedTokenDeclaration(name: String, Metagrammar.Meta, prior: Metagrammar.Meta)

        /// A '@token' meta-property is missing an identifier as its value.
        case tokenMissingName(Metagrammar.Meta)

        public var description: String {
            switch self {
            case .repeatedTokenDeclaration(let name, let meta, let prior):
                return "Token '\(name)' @ \(meta.location) has been declared at least once @ \(prior.location)."
            case .tokenMissingName(let meta):
                return "Expected token @ \(meta.location) to have an identifier name."
            }
        }
    }

    // MARK: - Internal representation

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
            _ node: Metagrammar.Grammar
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
            _ node: Metagrammar.Meta
        ) -> Self {

            .init(
                name: node.name.identifier,
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
                _ node: Metagrammar.MetaValue
            ) -> Self {

                switch node {
                case let value as Metagrammar.MetaIdentifierValue:
                    return .identifier(value.identifier.identifier)

                case let value as Metagrammar.MetaStringValue:
                    switch value.string.token {
                    case .string(.tripleQuote(let contents)) where contents.hasPrefix("\n"):
                        return .string(String(contents.dropFirst()))
                    default:
                        return .string(value.string.valueTrimmingQuotes)
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
            _ node: Metagrammar.Rule
        ) -> Self {

            .init(
                name: node.name.name.identifier,
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
            _ node: Metagrammar.Alt
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
            _ node: Metagrammar.Action
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

        public static func from(
            _ node: Metagrammar.NamedItem
        ) -> Self {
            if let item = node.item {
                return .item(name: node.name?.identifier, .from(item), type: node.type.map(SwiftType.from))
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
                return "[\(alts)]"
            }
        }

        public static func from(
            _ node: Metagrammar.Item
        ) -> Self {

            switch node {
            case let item as Metagrammar.OptionalItems:
                return .optionalItems(item.alts.map(Alt.from(_:)))

            case let item as Metagrammar.OptionalItem:
                return .optional(Atom.from(item.atom))

            case let item as Metagrammar.ZeroOrMoreItem:
                return .zeroOrMore(Atom.from(item.atom))

            case let item as Metagrammar.OneOrMoreItem:
                return .oneOrMore(Atom.from(item.atom))

            case let item as Metagrammar.GatherItem:
                return .gather(sep: Atom.from(item.sep), node: Atom.from(item.item))

            case let item as Metagrammar.OptionalItem:
                return .optional(Atom.from(item.atom))

            case let item as Metagrammar.AtomItem:
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
            _ node: Metagrammar.LookaheadOrCut
        ) -> Self {

            switch node {
            case let positive as Metagrammar.PositiveLookahead:
                return .positive(Atom.from(positive.atom))
            case let negative as Metagrammar.NegativeLookahead:
                return .negative(Atom.from(negative.atom))
            case is Metagrammar.Cut:
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
                return "(\(alts))"
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

        public static func from(
            _ node: Metagrammar.Atom
        ) -> Self {

            switch node {
            case let group as Metagrammar.GroupAtom:
                return .group(group.alts.map(Alt.from(_:)))

            case let ident as Metagrammar.IdentAtom:
                let value = ident.identifier.identifier

                switch ident.identity {
                case .ruleName:
                    return .ruleName(value)
                case .token:
                    return .token(value)
                case .unresolved:
                    return .ruleName(value)
                }

            case let string as Metagrammar.StringAtom:
                return .string(string.string.value, trimmed: string.string.valueTrimmingQuotes)

            default:
                fatalError("Unknown atom type \(type(of: node))")
            }
        }
    }

    public struct SwiftType: Hashable, CustomStringConvertible {
        public var name: String

        public var description: String { name }

        public static func from(
            _ node: Metagrammar.SwiftType
        ) -> Self {
            .init(name: node.name)
        }
    }
}

// MARK: - Visitors

private extension GrammarProcessor {
    /// Visitor used to validate named references in rules.
    final class ReferenceVisitor: Metagrammar.MetagrammarNodeVisitorType {
        private let knownIdentifiers: [(name: String, type: Metagrammar.IdentAtom.Identity)]
        var unknownReferences: [Metagrammar.IdentAtom] = []

        init(knownIdentifiers: some Sequence<(String, Metagrammar.IdentAtom.Identity)>) {
            self.knownIdentifiers = Array(knownIdentifiers)
        }

        func visit(_ node: Metagrammar.IdentAtom) throws -> NodeVisitChildrenResult {
            let ref = node.identifier.identifier

            if let identifier = knownIdentifiers.first(where: { $0.name == ref }) {
                node.identity = identifier.type
            } else {
                unknownReferences.append(node)
            }

            return .visitChildren
        }
    }

    /// Visitor used to validate named items in rules.
    final class NamedItemVisitor: Metagrammar.MetagrammarNodeVisitorType {
        var callback: (Metagrammar.NamedItem) throws -> Void

        init(_ callback: @escaping (Metagrammar.NamedItem) throws -> Void) {
            self.callback = callback
        }

        func visit(_ node: Metagrammar.NamedItem) throws -> NodeVisitChildrenResult {
            try callback(node)

            return .visitChildren
        }
    }
}
