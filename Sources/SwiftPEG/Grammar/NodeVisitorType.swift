/// Protocol for visitors of `Node` hierarchies.
public protocol NodeVisitorType {
    associatedtype VisitResult

    /// Called by a node walker to indicate that a node hierarchy will be visited.
    func willVisit(_ node: Node)

    /// Called by a generic node to indicate a visit within.
    func visit(_ node: Node) throws -> VisitResult

    /// Called by a node walker to indicate that a node hierarchy was fully
    /// visited.
    func didVisit(_ node: Node)
}

public extension NodeVisitorType {
    func willVisit(_ node: Node) { }
    func didVisit(_ node: Node) { }
}

public extension NodeVisitorType where VisitResult == NodeVisitChildrenResult {
    /// If result is `NodeVisitChildrenResult.skipChildren`, the node will be
    /// visited with `visit` but children of the node will not.
    func visit(_ node: Node) -> NodeVisitChildrenResult { .visitChildren }
}

/// Return of `NodeVisitorType.wilVisit` calls; indicates whether children of
/// nodes should be visited or skipped.
public enum NodeVisitChildrenResult {
    /// Walker will visit the children of `willVisit(node)`.
    case visitChildren
    /// Walker will skip the children of `willVisit(node)`.
    case skipChildren
}
