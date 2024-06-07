/// The result of processing a SwiftPEG grammar file.
public struct ProcessedGrammar {
    /// The reduced and tagged grammar for grammar analysis.
    public var grammar: InternalGrammar.Grammar

    /// The token definitions that where loaded from an associated tokens file.
    public var tokens: [InternalGrammar.TokenDefinition]

    /// The rule dependency graph for the grammar.
    public var ruleDependencyGraph: RuleDependencyGraph

    internal init(
        grammar: InternalGrammar.Grammar,
        tokens: [InternalGrammar.TokenDefinition],
        ruleDependencyGraph: RuleDependencyGraph
    ) {
        self.grammar = grammar
        self.tokens = tokens
        self.ruleDependencyGraph = ruleDependencyGraph
    }
}
