/// Rule dependency graph type that grammar processors produce containing
/// relationship information between rules in a grammar file that where collected
/// during processing.
public struct RuleDependencyGraph {
    /// The type of node (or vertex) in this graph.
    /// Represents names of rules from a grammar file.
    public typealias Node = String

    /// Gets the set containing all the rule names within this graph.
    public var nodes: Set<Node>

    /// Gets a set of all edges within this graph.
    public var edges: Set<Edge>

    init(nodes: some Sequence<Node>, edges: some Sequence<Edge>) {
        self.nodes = Set(nodes)
        self.edges = Set(edges)
    }

    /// An edge in a rule dependency graph.
    public struct Edge: Hashable {
        public var start: String
        public var end: String

        init(start: String, end: String) {
            self.start = start
            self.end = end
        }
    }
}
