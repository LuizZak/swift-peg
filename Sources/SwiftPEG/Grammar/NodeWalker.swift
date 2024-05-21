/// A walker that travels through `Node` hierarchies with a visitor type.
public class NodeWalker<Visitor: NodeVisitorType> {
    let visitor: Visitor

    public init(visitor: Visitor) {
        self.visitor = visitor
    }

    /// Starts walking in depth-first manner on a given node's hierarchy.
    /// The `node` itself is also visited.
    public func walk(_ node: Node) throws {
        visitor.willVisit(node)

        if try node.accept(visitor) == .visitChildren {
            for child in node.children {
                try walk(child)
            }
        }

        visitor.didVisit(node)
    }
}
