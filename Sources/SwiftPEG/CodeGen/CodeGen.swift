/// Base class for code generators that operate on parsed SwiftPEG grammars.
public class CodeGen {
    let grammar: Metagrammar.Grammar

    /// Prepares a `CodeGen` instance based on a given parsed grammar.
    public init(_ grammar: Metagrammar.Grammar) throws {
        self.grammar = grammar

        try validateRuleNames()
    }

    func validateRuleNames() throws {
        
    }
}
