/// Node visitor that prints types of nodes as they are visited, with an indicator
/// for the depth of the visited node at each step.
public final class PrintingNodeVisitor: NodeVisitorType {
    var nodeStack: [Node] = []
        
    public init() {

    }

    public func willVisit(_ node: Node) {
        nodeStack.append(node)
    }

    public func visit(_ node: Node) -> NodeVisitChildrenResult {
        let depth = nodeStack.count
        let spacer = depth == 1 ? "" : String(repeating: "--", count: depth - 1)

        print("\(spacer)> \(type(of: node)) " + node.shortDebugDescription)

        return .visitChildren
    }

    public func didVisit(_ node: Node) {
        nodeStack.removeLast()
    }
}
