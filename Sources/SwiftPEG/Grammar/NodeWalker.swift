/// A walker that travels through `Node` hierarchies with a visitor type.
public class NodeWalker<Visitor: NodeVisitorType> {
    let visitor: Visitor

    public init(visitor: Visitor) {
        self.visitor = visitor
    }

    /// Starts walking in depth-first manner on a given node's hierarchy.
    /// The `node` itself is also visited.
    public func walk(_ node: Node) {
        visitor.willVisit(node)
        node.accept(visitor)

        for child in node.children {
            walk(child)
        }
        visitor.didVisit(node)
    }
}
