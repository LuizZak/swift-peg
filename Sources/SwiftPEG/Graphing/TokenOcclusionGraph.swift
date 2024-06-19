import MiniDigraph

/// Token occlusion graph type that grammar processors produce containing
/// information relating dynamic tokens in the syntax to static tokens they parse
/// successfully.
///
/// Used by code generators to generate appropriate code handling for token
/// occlusion.
public struct TokenOcclusionGraph {
    /// The type of node (or vertex) in this graph.
    /// Represents names of tokens from a tokens grammar file.
    public typealias Node = String

    /// Represents an empty graph, with no edges or nodes.
    public static let empty = Self(nodes: [], edges: [])

    /// Gets the set containing all the tokens names within this graph.
    public var nodes: Set<Node>

    /// Gets a set of all edges within this graph.
    public var edges: Set<Edge>

    init(nodes: some Sequence<Node>, edges: some Sequence<Edge>) {
        self.nodes = Set(nodes)
        self.edges = Set(edges)
    }

    init(graph: DirectedGraph<Node>) {
        self.nodes = graph.nodes
        self.edges = Set(graph.edges.map { Edge(start: $0.start, end: $0.end) })
    }

    /// An edge in a token dependency graph.
    public struct Edge: Hashable {
        public var start: String
        public var end: String

        init(start: String, end: String) {
            self.start = start
            self.end = end
        }
    }
}
