/// Protocol for visitors of `Node` hierarchies.
public protocol NodeVisitorType {
    /// Called by a node walker to indicate that a node hierarchy will be visited.
    func willVisit(_ node: Node)

    /// Called by a node to indicate a visit within.
    func visit(_ node: Node)

    /// Called by a node walker to indicate that a node hierarchy was fully
    /// visited.
    func didVisit(_ node: Node)
}

public extension NodeVisitorType {
    func willVisit(_ node: Node) { }
    func didVisit(_ node: Node) { }
}
