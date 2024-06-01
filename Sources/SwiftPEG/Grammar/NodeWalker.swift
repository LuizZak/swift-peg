/// A walker that travels through `Node` hierarchies with a visitor type.
public class NodeWalker<Visitor: NodeVisitorType> {
    let visitor: Visitor

    public init(visitor: Visitor) {
        self.visitor = visitor
    }

    /// Starts walking in depth-first manner on a given node's hierarchy, accumulating
    /// the intermediary results of visits with a given closure.
    /// The `node` itself is also visited.
    public func walk(
        _ node: Node,
        accumulating accumulator: (Visitor.VisitResult, Visitor.VisitResult) -> Visitor.VisitResult
    ) throws -> Visitor.VisitResult {

        visitor.willVisit(node)
        var result = try node.accept(visitor)

        for child in node.children {
            let next = try walk(child, accumulating: accumulator)
            result = accumulator(result, next)
        }

        visitor.didVisit(node)

        return result
    }
}

public extension NodeWalker where Visitor.VisitResult == NodeVisitChildrenResult {
    /// Starts walking in depth-first manner on a given node's hierarchy.
    /// The `node` itself is also visited.
    ///
    /// The result of each visit, a value of `NodeVisitChildrenResult`, is checked
    /// before an attempt at walking a node's children is made.
    func walk(_ node: Node) throws {
        visitor.willVisit(node)

        if try node.accept(visitor) == .visitChildren {
            for child in node.children {
                try walk(child)
            }
        }

        visitor.didVisit(node)
    }
}

public extension NodeWalker where Visitor.VisitResult == Void {
    /// Starts walking in depth-first manner on a given node's hierarchy.
    /// The `node` itself is also visited.
    func walk(_ node: Node) throws {
        visitor.willVisit(node)
        try node.accept(visitor)

        for child in node.children {
            try walk(child)
        }

        visitor.didVisit(node)
    }
}
