/// The result of processing a SwiftPEG grammar file.
public struct ProcessedGrammar {
    /// The reduced and tagged grammar for grammar analysis.
    public var grammar: InternalGrammar.Grammar

    /// The token definitions that where loaded from an associated tokens file.
    public let tokens: [InternalGrammar.TokenDefinition]

    internal init(grammar: InternalGrammar.Grammar, tokens: [InternalGrammar.TokenDefinition]) {
        self.grammar = grammar
        self.tokens = tokens
    }
}
