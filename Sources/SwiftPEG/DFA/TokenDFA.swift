import MiniDigraph

/// Contains information about a token syntax's characteristic deterministic
/// finite automata.
class TokenDFA {
    private var _nextId: Int = 0

    var nodes: Set<Node> = []
    var edges: Set<Edge> = []

    @discardableResult
    func addNode(isAccept: Bool = false) -> Node {
        let node = makeNode(isAccept: isAccept)
        self.nodes.insert(node)
        return node
    }

    func makeAccept(_ nodeId: Node.ID) {
        guard var node = nodes.first(where: { $0.id == nodeId }) else {
            assertionFailure("\(#function): node id \(nodeId) not found")
            return
        }
        guard !node.isAccept else {
            return
        }

        nodes.remove(node)
        node.isAccept = true
        nodes.insert(node)
    }

    func makeNode(isAccept: Bool) -> Node {
        defer { _nextId += 1 }

        return Node(id: _nextId, isAccept: isAccept)
    }

    @discardableResult
    func addEdge(from start: Node, to end: Node, label: String) -> Edge {
        addEdge(from: start.id, to: end.id, label: label)
    }

    @discardableResult
    func addEdge(from start: Node.ID, to end: Node.ID, label: String) -> Edge {
        let edge = makeEdge(from: start, to: end, label: label)
        edges.insert(edge)
        return edge
    }

    func makeEdge(from start: Node.ID, to end: Node.ID, label: String) -> Edge {
        Edge(start: start, end: end, label: label)
    }

    struct Node: Identifiable, Hashable {
        var id: Int

        /// Whether landing on this node is considered a success of the DFA.
        var isAccept: Bool
    }

    struct Edge: Hashable {
        var start: Node.ID
        var end: Node.ID
        var label: String
    }
}
