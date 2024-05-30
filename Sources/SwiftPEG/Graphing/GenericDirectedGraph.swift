// TODO: Refactor DirectedGraph to DirectedGraphType or other name so we can use
// TODO: 'DirectedGraph' as the name of this class instead of this awkward name
final class GenericDirectedGraph<T>: DirectedGraphBase<GenericDirectedGraph<T>.Node, GenericDirectedGraph<T>.Edge> {
    @discardableResult
    func addNode(_ value: T) -> Node {
        let node = Node(value: value)
        addNode(node)

        return node
    }

    @discardableResult
    func addNodes(_ values: some Sequence<T>) -> [Node] {
        values.map(addNode)
    }

    @discardableResult
    func addEdge(from startRule: T, to endRule: T) -> Edge where T: Equatable {
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

    final class Node: DirectedGraphNode, CustomStringConvertible {
        let value: T

        var description: String {
            "\(type(of: self))(value: \(value))"
        }

        init(value: T) {
            self.value = value
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
