/// A graph for generic string node values.
final class StringDirectedGraph: DirectedGraphBase<StringDirectedGraph.Node, StringDirectedGraph.Edge> {
    @discardableResult
    func addNode(_ value: String) -> Node {
        let node = Node(value: value)
        addNode(node)

        return node
    }

    @discardableResult
    func addNodes(_ values: some Sequence<String>) -> [Node] {
        values.map(addNode)
    }

    @discardableResult
    func addEdge(from startRule: String, to endRule: String) -> Edge {
        guard let start = nodes.first(where: { $0.value == startRule }) else {
            fatalError("Node with value '\(startRule)' doesn't exist in this graph.")
        }
        guard let end = nodes.first(where: { $0.value == endRule }) else {
            fatalError("Node with value '\(endRule)' doesn't exist in this graph.")
        }

        return addEdge(from: start, to: end)
    }

    @discardableResult
    override func addEdge(from start: Node, to end: Node) -> Edge {
        let edge = Edge(start: start, end: end)

        edges.append(edge)

        return edge
    }

    final class Node: DirectedGraphNode, CustomStringConvertible, ExpressibleByStringLiteral {
        let value: String

        var description: String {
            "\(type(of: self))(value: \(value))"
        }

        init(value: String) {
            self.value = value
        }

        init(stringLiteral: String) {
            self.value = stringLiteral
        }
    }

    final class Edge: DirectedGraphBaseEdgeType {
        let start: Node
        let end: Node

        internal init(start: Node, end: Node) {
            self.start = start
            self.end = end
        }

        func copy() -> Self {
            return .init(start: start, end: end)
        }
    }
}
