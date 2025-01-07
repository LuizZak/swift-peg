/// The result of processing a SwiftPEG grammar file.
public struct ProcessedGrammar {
    /// The reduced and tagged grammar for grammar analysis.
    public var grammar: InternalGrammar.Grammar

    /// The token definitions that where loaded from an associated tokens file.
    public var tokens: [InternalGrammar.TokenDefinition]

    /// The token channels that where loaded from an associated tokens file.
    public var tokenChannels: [InternalGrammar.TokenChannel]

    /// The rule dependency graph for the grammar.
    public var ruleDependencyGraph: RuleDependencyGraph

    /// The token occlusion graph for the grammar.
    ///
    /// Contains relationships between dynamic tokens and static tokens that they
    /// also parse as.
    public var tokenOcclusionGraph: TokenOcclusionGraph

    internal init(
        grammar: InternalGrammar.Grammar,
        tokens: [InternalGrammar.TokenDefinition],
        tokenChannels: [InternalGrammar.TokenChannel],
        ruleDependencyGraph: RuleDependencyGraph,
        tokenOcclusionGraph: TokenOcclusionGraph
    ) {
        self.grammar = grammar
        self.tokens = tokens
        self.tokenChannels = tokenChannels
        self.ruleDependencyGraph = ruleDependencyGraph
        self.tokenOcclusionGraph = tokenOcclusionGraph
    }
}
