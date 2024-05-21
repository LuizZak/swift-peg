/// Base class for code generators that operate on parsed SwiftPEG grammars.
public class CodeGen {
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

    /// A list of non-Error diagnostics issued during code generation.
    private(set) public var diagnostics: [CodeGenDiagnostic] = []

    /// If `true`, prints diagnostics into stdout.
    public var verbose: Bool

    /// Prepares a `CodeGen` instance based on a given parsed grammar.
    public init(_ grammar: Metagrammar.Grammar, verbose: Bool = false) throws {
        self.grammar = grammar
        self.verbose = verbose

        let knownRules = try validateRuleNames()
        let tokens = try validateTokenReferences()
        try validateReferences(tokens: tokens)

        try computeNullables(knownRules)

        self.remaining = knownRules
    }

    func collectRemaining() {
        var done: Set<String> = []

        while true {
            let fullRemaining = Array(self.remaining)

        }
    }

    func validateRuleNames() throws -> [String: Metagrammar.Rule] {
        var knownRules: [String: Metagrammar.Rule] = [:]

        for rule in grammar.rules {
            let ruleName = try validateRuleName(rule)

            if let existing = knownRules[ruleName] {
                throw CodeGenError.repeatedRuleName(ruleName, rule, prior: existing)
            } else {
                knownRules[ruleName] = rule
            }
        }

        return knownRules
    }

    func validateRuleName(_ rule: Metagrammar.Rule) throws -> String {
        let ruleName = rule.name.name.identifier
        if ruleName.isEmpty {
            throw CodeGenError.invalidRuleName(desc: "Rule name cannot be empty", rule)
        }
        if try Regex(Self.ruleNameGrammar).wholeMatch(in: ruleName) == nil {
            throw CodeGenError.invalidRuleName(desc: "Expected rule names to match regex '\(Self.ruleNameGrammar)'", rule)
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
        var knownNames: Set<String> = tokens
        for rule in grammar.rules {
            let name = rule.name.name.identifier
            knownNames.insert(name)
        }

        let visitor = ReferenceVisitor(knownIdentifiers: knownNames)

        let walker = NodeWalker(visitor: visitor)
        walker.walk(grammar)

        guard !visitor.unknownReferences.isEmpty else {
            // Ok!
            return
        }

        for ref in visitor.unknownReferences {
            guard let rule: Metagrammar.Rule = ref.firstAncestor() else {
                fatalError("Found atom \(ref.name) @ \(ref.location) that has no Rule parent?")
            }

            throw CodeGenError.unknownReference(ref, rule)
        }
    }

    /// Gets all @token meta-properties in the grammar.
    func tokenMetaProperties() -> [Metagrammar.Meta] {
        return grammar.metas.filter { $0.name.identifier == "token" }
    }

    func printIfVerbose(_ item: Any) {
        guard verbose else { return }

        print(item)
    }

    /// Visitor used to validate named references in rules.
    private class ReferenceVisitor: Metagrammar.MetagrammarNodeVisitorType {
        private let knownIdentifiers: Set<String>
        var unknownReferences: [Metagrammar.IdentAtom] = []

        init(knownIdentifiers: some Sequence<String>) {
            self.knownIdentifiers = Set(knownIdentifiers)
        }

        func visit(_ node: Metagrammar.IdentAtom) {
            let ref = node.identifier.identifier
            if !knownIdentifiers.contains(ref) {
                unknownReferences.append(node)
            }
        }
    }

    /// An error that can be raised by `CodeGen` during grammar analysis.
    public enum CodeGenError: Error, CustomStringConvertible {
        /// Rules found sharing the same name.
        case repeatedRuleName(String, Metagrammar.Rule, prior: Metagrammar.Rule)
        /// Rule found that has an invalid name.
        case invalidRuleName(desc: String, Metagrammar.Rule)
        /// An identifier was found in a rule that could not be resolved to a
        /// rule or token name.
        case unknownReference(Metagrammar.IdentAtom, Metagrammar.Rule)

        /// An attempt at resolving a left recursion and find a leader rule from
        /// a set of rules has failed.
        case unresolvedLeftRecursion(ruleNames: [String])

        public var description: String {
            switch self {
            case .repeatedRuleName(let name, let rule, let prior):
                return "Rule '\(name)' re-declared @ \(rule.location). Original declaration @ \(prior.location)."
            case .invalidRuleName(let desc, let rule):
                return "Rule name '\(rule.name.name.identifier)' @ \(rule.location) is not valid: \(desc)"
            case .unknownReference(let atom, let rule):
                return """
                Reference to unknown identifier '\(atom.name)' @ \(atom.location) in rule '\(rule.name.shortDebugDescription)'. \
                Did you forget to forward-declare a token with '@token \(atom.name);'?
                """
            case .unresolvedLeftRecursion(let ruleNames):
                return "Could not resolve left recursion with a lead rule in the set \(ruleNames)"
            }
        }
    }

    /// A non-fatal CodeGen diagnostic.
    public enum CodeGenDiagnostic {
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
}
