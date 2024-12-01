import MiniDigraph

public struct InternalCachingDirectedGraph<Node: Hashable> {
    public typealias Graph = AbstractDirectedGraph<Node, DirectedGraph<Node>.Edge>

    public typealias NodeCollection = Graph.NodeCollection
    public typealias EdgeCollection = Graph.EdgeCollection

    @usableFromInline
    var cache: Cache

    @usableFromInline
    var graph: Graph {
        cache.graph
    }

    /// Initializes this caching graph with a given input graph type.
    public init(graph: Graph) {
        self.cache = .init(graph: graph)
    }

    /// Initializes this caching graph with a given input graph type.
    public init(graph: InternalCachingDirectedGraph<Node>) {
        self.cache = graph.cache.copy()
    }

    mutating func ensureUnique() {
        if !isKnownUniquelyReferenced(&cache) {
            cache = cache.copy()
        }
    }

    @usableFromInline
    class Cache {
        typealias Node = Graph.Node
        typealias Edge = Graph.Edge

        @usableFromInline
        var graph: Graph

        @usableFromInline
        var nodes: Graph.NodeCollection {
            graph.nodes
        }
        @usableFromInline
        var edges: Graph.EdgeCollection {
            graph.edges
        }

        var edgesTowardsNode: [Node: Set<Edge>]
        var edgesFromNode: [Node: Set<Edge>]

        init(
            graph: Graph,
            edgesTowardsNode: [Node: Set<Edge>] = [:],
            edgesFromNode: [Node: Set<Edge>] = [:]
        ) {
            self.graph = graph
            self.edgesTowardsNode = edgesTowardsNode
            self.edgesFromNode = edgesFromNode
        }

        func copy() -> Cache {
            return .init(
                graph: graph,
                edgesTowardsNode: edgesTowardsNode,
                edgesFromNode: edgesFromNode
            )
        }

        func _addFromEdge(_ node: Node, _ edge: Edge) {
            precondition(nodes.contains(node))
            edgesFromNode[node, default: []].insert(edge)
        }

        func _removeFromEdge(_ node: Node, _ edge: Edge) {
            precondition(nodes.contains(node))
            edgesFromNode[node, default: []].remove(edge)
        }

        func _addTowardsEdge(_ node: Node, _ edge: Edge) {
            precondition(nodes.contains(node))
            edgesTowardsNode[node, default: []].insert(edge)
        }

        func _removeTowardsEdge(_ node: Node, _ edge: Edge) {
            precondition(nodes.contains(node))
            edgesTowardsNode[node, default: []].remove(edge)
        }
    }
}

extension InternalCachingDirectedGraph.Cache {
    func _addNode(_ node: Node) {
        graph.addNode(node)
        edgesFromNode[node] = []
        edgesTowardsNode[node] = []
    }

    func _removeNode(_ node: Node) {
        guard nodes.contains(node) else {
            preconditionFailure("Attempted to remove a node that was not part of this graph: \(node)")
        }

        for edge in edgesFromNode[node, default: []] {
            _removeEdge(edge)
        }
        for edge in edgesTowardsNode[node, default: []] {
            _removeEdge(edge)
        }

        edgesFromNode.removeValue(forKey: node)
        edgesTowardsNode.removeValue(forKey: node)
        graph.removeNode(node)
    }

    func _removeNodes(_ nodesToRemove: some Collection<Node>) {
        guard nodesToRemove.allSatisfy(nodes.contains(_:)) else {
            preconditionFailure("Attempted to remove one or more nodes that were not part of this graph: \(nodesToRemove)")
        }

        for node in nodesToRemove {
            let edgesFrom = edgesFromNode[node, default: []]
            let edgesTo = edgesTowardsNode[node, default: []]

            graph.removeEdges(edgesFrom)
            graph.removeEdges(edgesTo)

            edgesFromNode.removeValue(forKey: node)
            edgesTowardsNode.removeValue(forKey: node)

            for edgeFrom in edgesFrom {
                edgesTowardsNode[edgeFrom.end]?.remove(edgeFrom)
            }
            for edgeTo in edgesTo {
                edgesFromNode[edgeTo.start]?.remove(edgeTo)
            }
        }

        graph.removeNodes(nodesToRemove)
    }

    func _addEdge(_ edge: Edge) -> Edge {
        graph.addEdge(edge)

        _addFromEdge(edge.start, edge)
        _addTowardsEdge(edge.end, edge)

        return edge
    }

    func _removeEdge(_ edge: Edge) {
        guard graph.containsEdge(edge) else {
            preconditionFailure("Attempted to remove an edge that was not part of this graph: \(edge)")
        }

        graph.removeEdge(edge)
        _removeFromEdge(edge.start, edge)
        _removeTowardsEdge(edge.end, edge)
    }

    func _removeEdges(_ edges: some Sequence<Edge>) {
        for edge in edges {
            _removeEdge(edge)
        }
    }
}

extension InternalCachingDirectedGraph: DirectedGraphType {
    public var nodes: NodeCollection {
        graph.nodes
    }
    public var edges: EdgeCollection {
        graph.edges
    }

    public func startNode(for edge: Edge) -> Node {
        edge.start
    }

    public func endNode(for edge: Edge) -> Node {
        edge.end
    }

    public func edges(from node: Node) -> [Edge] {
        Array(cache.edgesFromNode[node, default: []])
    }

    public func edges(towards node: Node) -> [Edge] {
        Array(cache.edgesTowardsNode[node, default: []])
    }

    public func edges(from start: Node, to end: Node) -> [Edge] {
        cache.edgesFromNode[start]?.filter({ $0.end == end }) ?? []
    }
}

extension InternalCachingDirectedGraph: MutableDirectedGraphType where Graph: MutableDirectedGraphType {
    public init() {
        self.cache = .init(graph: Graph())
    }

    public mutating func addNode(_ node: Node) {
        ensureUnique()
        self.cache._addNode(node)
    }

    public mutating func removeNode(_ node: Node) {
        ensureUnique()
        self.cache._removeNode(node)
    }

    public mutating func removeNodes(_ nodesToRemove: some Sequence<Node>) {
        ensureUnique()
        self.cache._removeNodes(Array(nodesToRemove))
    }

    @discardableResult
    public mutating func addEdge(_ edge: Edge) -> Edge {
        ensureUnique()
        return self.cache._addEdge(edge)
    }

    public mutating func removeEdge(_ edge: Edge) {
        ensureUnique()
        self.cache._removeEdge(edge)
    }

    public mutating func removeEdges(_ edgesToRemove: some Sequence<Edge>) {
        ensureUnique()
        self.cache._removeEdges(edgesToRemove)
    }
}

extension InternalCachingDirectedGraph: MutableSimpleEdgeDirectedGraphType where Graph: MutableSimpleEdgeDirectedGraphType { }
