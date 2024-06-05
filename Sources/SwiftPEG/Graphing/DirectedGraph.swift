/// A protocol for representing directed graphs
protocol DirectedGraph {
    associatedtype Edge: DirectedGraphEdge
    associatedtype Node: DirectedGraphNode

    /// Convenience typealias for a visit for visit methods in this directed graph.
    typealias VisitElement = DirectedGraphVisitElement<Edge, Node>

    /// Gets a list of all nodes in this directed graph
    var nodes: [Node] { get }
    /// Gets a list of all edges in this directed graph
    var edges: [Edge] { get }

    /// Returns a subset of this directed graph containing only the nodes from
    /// a given sequence, and all edges present that match start/end nodes within
    /// the sequence.
    @inlinable
    func subgraph(of nodes: some Sequence<Node>) -> Self

    /// Returns `true` iff two node references represent the same underlying node
    /// in this graph.
    @inlinable
    func areNodesEqual(_ node1: Node, _ node2: Node) -> Bool

    /// Returns `true` iff two edges are equivalent (i.e. have the same start/end
    /// nodes).
    @inlinable
    func areEdgesEqual(_ edge1: Edge, _ edge2: Edge) -> Bool

    /// Returns the starting edge for a given node on this graph.
    @inlinable
    func startNode(for edge: Edge) -> Node

    /// Returns the ending edge for a given node on this graph.
    @inlinable
    func endNode(for edge: Edge) -> Node

    /// Returns all ingoing and outgoing edges for a given directed graph node.
    @inlinable
    func allEdges(for node: Node) -> [Edge]

    /// Returns all outgoing edges for a given directed graph node.
    @inlinable
    func edges(from node: Node) -> [Edge]

    /// Returns all ingoing edges for a given directed graph node.
    @inlinable
    func edges(towards node: Node) -> [Edge]

    /// Returns an existing edge between two nodes, or `nil`, if no edges between
    /// them currently exist.
    @inlinable
    func edge(from start: Node, to end: Node) -> Edge?

    /// Returns `true` if the two given nodes are connected with an edge.
    func areConnected(start: Node, end: Node) -> Bool

    /// Returns all graph nodes that are connected from a given directed graph
    /// node.
    @inlinable
    func nodesConnected(from node: Node) -> [Node]

    /// Returns all graph nodes that are connected towards a given directed graph
    /// node.
    @inlinable
    func nodesConnected(towards node: Node) -> [Node]

    /// Returns all graph nodes that are connected towards and from the given
    /// graph node.
    @inlinable
    func allNodesConnected(to node: Node) -> [Node]

    /// Returns `true` if the directed graph has a path between the two given nodes.
    func hasPath(from start: Node, to end: Node) -> Bool

    /// Returns the shortest number of edges that need to be traversed to get from
    /// the given start node to the given end node.
    ///
    /// If `start == end`, `0` is returned.
    ///
    /// In case the two nodes are not connected, or are connected in the opposite
    /// direction, `nil` is returned.
    @inlinable
    func shortestDistance(from start: Node, to end: Node) -> Int?

    /// Returns any of the shortest paths found between two nodes.
    ///
    /// If `start == end`, `[start]` is returned.
    ///
    /// In case the two nodes are not connected, or are connected in the opposite
    /// direction, `nil` is returned.
    @inlinable
    func shortestPath(from start: Node, to end: Node) -> [Node]?

    /// Performs a depth-first visiting of this directed graph, finishing once
    /// all nodes are visited, or when `visitor` returns false.
    ///
    /// In case a cycle is found, the previously-visited nodes are skipped.
    @inlinable
    func depthFirstVisit(start: Node, _ visitor: (VisitElement) -> Bool)

    /// Performs a breadth-first visiting of this directed graph, finishing once
    /// all nodes are visited, or when `visitor` returns false.
    ///
    /// In case a cycle is found, the previously-visited nodes are skipped.
    @inlinable
    func breadthFirstVisit(start: Node, _ visitor: (VisitElement) -> Bool)

    /// Computes and returns the strongly connected components of this directed
    /// graph.
    ///
    /// Each strongly connected component is returned as a set of nodes belonging
    /// to the component.
    ///
    /// A node in this graph only ever shows up once in one of the components.
    ///
    /// Nodes that are not strongly connected to any other node show up as a
    /// set containing that node only.
    @inlinable
    func stronglyConnectedComponents() -> [Set<Node>]

    /// Computes and returns the connected components of this directed graph.
    ///
    /// Each connected component is returned as a set of nodes belonging to the
    /// component, where for each node in a component, there exists an undirected
    /// path reaching every other node in the component.
    ///
    /// A node in this graph only ever shows up once in one of the components.
    ///
    /// Nodes that are not connected to any other node show up as a set containing
    /// that node only.
    @inlinable
    func connectedComponents() -> [Set<Node>]

    /// Returns cycles found within a this graph returned from a given start
    /// node.
    ///
    /// Returns an array of array of nodes that connects from `start` into a cycle,
    /// with the remaining nodes looping from the last index into an earlier index.
    @inlinable
    func findCycles(from start: Node) -> [[Node]]
}

/// Element for a graph visiting operation.
///
/// - start: The item represents the start of a visit.
/// - edge: The item represents an edge, pointing to a node of the graph. Also
/// contains information about the path leading up to that edge.
enum DirectedGraphVisitElement<E: DirectedGraphEdge, N: DirectedGraphNode>: Hashable {
    case start(N)
    indirect case edge(E, from: Self, towards: N)

    /// Gets the node at the end of this visit element.
    public var node: N {
        switch self {
        case .start(let node),
             .edge(_, _, let node):
            return node
        }
    }

    /// Gets the last edge that is associated with the visit.
    ///
    /// If this visit is not an `.edge` case, `nil` is returned instead.
    public var edge: E? {
        switch self {
        case .start:
            return nil
        case .edge(let edge, _, _):
            return edge
        }
    }

    /// Gets the list of all edges from this visit element.
    public var allEdges: [E] {
        switch self {
        case .start:
            return []
        case .edge(let edge, let from, _):
            return from.allEdges + [edge]
        }
    }

    /// Gets an array of all nodes from this visit element.
    public var allNodes: [N] {
        switch self {
        case .start(let node):
            return [node]
        case .edge(_, let from, let node):
            return from.allNodes + [node]
        }
    }

    /// Returns the length of the path represented by this visit element.
    ///
    /// Lengths start at 1 from `.start()`, and increase by one for every nested
    /// element in `.edge()`.
    public var length: Int {
        switch self {
        case .start:
            return 1
        case .edge(_, let from, _):
            return 1 + from.length
        }
    }
}

extension DirectedGraph {
    @inlinable
    func areEdgesEqual(_ edge1: Edge, _ edge2: Edge) -> Bool {
        areNodesEqual(startNode(for: edge1), startNode(for: edge2))
            && areNodesEqual(endNode(for: edge1), endNode(for: edge2))
    }

    @inlinable
    func allEdges(for node: Node) -> [Edge] {
        edges(towards: node) + edges(from: node)
    }

    @inlinable
    func areConnected(start: Node, end: Node) -> Bool {
        edge(from: start, to: end) != nil
    }

    @inlinable
    func nodesConnected(from node: Node) -> [Node] {
        edges(from: node).map(self.endNode(for:))
    }

    @inlinable
    func nodesConnected(towards node: Node) -> [Node] {
        edges(towards: node).map(self.startNode(for:))
    }

    @inlinable
    func allNodesConnected(to node: Node) -> [Node] {
        nodesConnected(towards: node) + nodesConnected(from: node)
    }

    @inlinable
    func hasPath(from start: Node, to end: Node) -> Bool {
        var found = false
        depthFirstVisit(start: start) { visit in
            if areNodesEqual(visit.node, end) {
                found = true
                return false
            }

            return true
        }

        return found
    }

    @inlinable
    func shortestDistance(from start: Node, to end: Node) -> Int? {
        if let path = shortestPath(from: start, to: end) {
            return path.count - 1
        }

        return nil
    }

    @inlinable
    func shortestPath(from start: Node, to end: Node) -> [Node]? {
        var paths: [VisitElement] = []

        breadthFirstVisit(start: start) { visit in
            if visit.node == end {
                paths.append(visit)
            }

            return true
        }

        if paths.isEmpty {
            return nil
        }

        return paths.sorted(by: { $0.length < $1.length }).first?.allNodes
    }

    @inlinable
    func depthFirstVisit(start: Node, _ visitor: (VisitElement) -> Bool) {
        var visited: Set<Node> = []
        var queue: [VisitElement] = []

        queue.append(.start(start))

        while let next = queue.popLast() {
            visited.insert(next.node)

            if !visitor(next) {
                return
            }

            for nextEdge in edges(from: next.node).reversed() {
                let node = endNode(for: nextEdge)
                if visited.contains(node) {
                    continue
                }

                queue.append(.edge(nextEdge, from: next, towards: node))
            }
        }
    }

    @inlinable
    func breadthFirstVisit(start: Node, _ visitor: (VisitElement) -> Bool) {
        var visited: Set<Node> = []
        var queue: [VisitElement] = []

        queue.append(.start(start))

        while !queue.isEmpty {
            let next = queue.removeFirst()
            visited.insert(next.node)

            if !visitor(next) {
                return
            }

            for nextEdge in edges(from: next.node) {
                let node = endNode(for: nextEdge)
                if visited.contains(node) {
                    continue
                }

                queue.append(.edge(nextEdge, from: next, towards: node))
            }
        }
    }

    @inlinable
    func stronglyConnectedComponents() -> [Set<Node>] {
        var result: [Set<Node>] = []

        var indices: [Node: Int] = [:]
        var stack: [Node] = []
        var index = 0

        func strongConnect(_ node: Node) -> Int {
            var lowLink = index
            indices[node] = index
            index += 1
            stack.append(node)

            for next in self.nodesConnected(from: node) {
                guard let nextIndex = indices[next] else {
                    // Not been visited
                    lowLink = min(lowLink, strongConnect(next))
                    continue
                }

                if stack.contains(next) {
                    lowLink = min(lowLink, nextIndex)
                }
            }

            if lowLink == indices[node], let idx = stack.lastIndex(of: node) {
                result.append(Set(stack[idx...]))
                stack.removeSubrange(idx...)
            }

            return lowLink
        }

        for node in nodes {
            if indices[node] == nil {
                _=strongConnect(node)
            }
        }

        return result
    }

    @inlinable
    func connectedComponents() -> [Set<Node>] {
        var result: [Set<Node>] = []
        var remaining = Set(self.nodes)

        while let start = remaining.popFirst() {
            var component: Set<Node> = []
            var nextNodes: [Node] = [start]

            while !nextNodes.isEmpty {
                let next = nextNodes.removeFirst()
                remaining.remove(next)
                guard component.insert(next).inserted else {
                    // Already visited
                    continue
                }

                let connected = allNodesConnected(to: next)
                nextNodes.append(contentsOf: connected)
            }

            result.append(component)
        }

        return result
    }

    @inlinable
    func findCycles(from start: Node) -> [[Node]] {
        assert(nodes.contains(start), "!nodes.contains(start)")

        var result: [[Node]] = []

        func inner(node: Node, path: [Node]) {
            if path.contains(node) {
                result.append(path + [node])
                return
            }

            let path = path + [node]
            for next in nodesConnected(from: node) {
                inner(node: next, path: path)
            }
        }

        inner(node: start, path: [])

        return result
    }
}

extension DirectedGraph {
    /// Returns a list which represents the [topologically sorted](https://en.wikipedia.org/wiki/Topological_sorting)
    /// nodes of this graph.
    ///
    /// Returns nil, in case it cannot be topologically sorted, e.g. when any
    /// cycles are found.
    ///
    /// - Returns: A list of the nodes from this graph, topologically sorted, or
    /// `nil`, in case it cannot be sorted.
    @inlinable
    func topologicalSorted() -> [Node]? {
        var permanentMark: Set<Node> = []
        var temporaryMark: Set<Node> = []

        var unmarkedNodes: [Node] = nodes
        var list: [Node] = []

        func visit(_ node: Node) -> Bool {
            if permanentMark.contains(node) {
                return true
            }
            if temporaryMark.contains(node) {
                return false
            }
            temporaryMark.insert(node)
            for next in nodesConnected(from: node) {
                if !visit(next) {
                    return false
                }
            }
            permanentMark.insert(node)
            list.insert(node, at: 0)
            return true
        }

        while let node = unmarkedNodes.popLast() {
            if !visit(node) {
                return nil
            }
        }

        return list
    }
}

/// A protocol for representing a directed graph's edge
protocol DirectedGraphEdge: Hashable {

}

/// A protocol for representing a directed graph's node
protocol DirectedGraphNode: Hashable {

}

extension DirectedGraphEdge where Self: AnyObject {
    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs === rhs
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}

extension DirectedGraphNode where Self: AnyObject {
    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs === rhs
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(ObjectIdentifier(self))
    }
}
