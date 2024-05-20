/// Base class for code generators that operate on parsed SwiftPEG grammars.
public class CodeGen {
    /// Regex for validating rule names.
    static let ruleGrammar = #/[A-Za-z_][0-9A-Za-z_]*/#

    /// Set of identifiers that cannot be used as bare identifiers in Swift, and
    /// must be escaped with backticks (`)
    static let invalidBareIdentifiers: Set<String> = [
        "_", "var", "let", "nil", "class", "struct", "func", "protocol", "enum",
        "try", "throws", "deinit", "init", "if", "for", "else", "while", "switch",
        "repeat", "do", "public", "private", "fileprivate", "internal", "static",
    ]

    let grammar: Metagrammar.Grammar

    /// Prepares a `CodeGen` instance based on a given parsed grammar.
    public init(_ grammar: Metagrammar.Grammar) throws {
        self.grammar = grammar

        try validateRuleNames()
    }

    func validateRuleNames() throws {
        var knownRules: [String: Metagrammar.Rule] = [:]

        for rule in grammar.rules {
            let ruleName = try validateRuleName(rule)

            if let existing = knownRules[ruleName] {
                throw CodeGenError.repeatedRuleName(ruleName, rule, prior: existing)
            } else {
                knownRules[ruleName] = rule
            }
        }
    }

    func validateRuleName(_ rule: Metagrammar.Rule) throws -> String {
        let ruleName = rule.name.name.identifier
        if ruleName.isEmpty {
            throw CodeGenError.invalidRuleName("Rule name cannot be empty", rule)
        }
        if Self.invalidBareIdentifiers.contains(ruleName) {
            return "`\(ruleName)`"
        }

        return ruleName
    }

    public enum CodeGenError: Error, CustomStringConvertible {
        case repeatedRuleName(String, Metagrammar.Rule, prior: Metagrammar.Rule)
        case invalidRuleName(String, Metagrammar.Rule)

        public var description: String {
            switch self {
            case .repeatedRuleName(let name, _, _):
                return "Rule '\(name)' re-declared."
            case .invalidRuleName(let name, _):
                return "Rule name '\(name)' is not valid."
            }
        }
    }
}
