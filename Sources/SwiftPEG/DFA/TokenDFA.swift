import MiniDigraph

/// Contains information about a token syntax's characteristic deterministic
/// finite automata.
class TokenDFA: DirectedGraphType {
    private var _nextId: Int = 0

    /// Refers to the Id of the starting node for this DFA.
    var startNodeId: Int = -1

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
    func addEdge(from start: Node, to end: Node, condition: EdgeCondition) -> Edge {
        addEdge(from: start.id, to: end.id, condition: condition)
    }

    @discardableResult
    func addEdge(from start: Node.ID, to end: Node.ID, condition: EdgeCondition) -> Edge {
        let edge = makeEdge(from: start, to: end, condition: condition)
        edges.insert(edge)
        return edge
    }

    func startNode(for edge: Edge) -> Node {
        findNode(edge.start)!
    }

    func endNode(for edge: Edge) -> Node {
        findNode(edge.end)!
    }

    func edge(from start: Node, to end: Node) -> Edge? {
        edges.first(where: { $0.start == start.id && $0.end == end.id })
    }

    func edges(from node: Node) -> Set<Edge> {
        edges.filter({ $0.start == node.id })
    }

    func edges(towards node: Node) -> Set<Edge> {
        edges.filter({ $0.end == node.id })
    }

    func makeEdge(from start: Node.ID, to end: Node.ID, condition: EdgeCondition) -> Edge {
        Edge(start: start, end: end, condition: condition)
    }

    func findNode(_ nodeId: Node.ID) -> Node? {
        nodes.first(where: { $0.id == nodeId })
    }

    struct Node: Identifiable, Hashable {
        var id: Int

        /// Whether landing on this node is considered a success of the DFA.
        var isAccept: Bool
    }

    struct Edge: DirectedGraphEdge {
        var start: Node.ID
        var end: Node.ID
        var condition: EdgeCondition
    }

    /// Condition for transitioning between states in a DFA.
    enum EdgeCondition: Hashable, CustomStringConvertible {
        /// A terminal match against the input string.
        case terminal(CommonAbstract.TokenTerminal)

        /// An empty match that succeeds without consuming input.
        case epsilon

        var description: String {
            switch self {
            case .terminal(let terminal):
                return terminal.description

            case .epsilon:
                return "ε"
            }
        }
    }
}
