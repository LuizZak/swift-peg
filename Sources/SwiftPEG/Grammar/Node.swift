/// Base node for all grammar nodes.
open class Node {
    public weak var parent: Node? {
        willSet {
            if parent === newValue { return }
            
            if let parent, let newValue {
                preconditionFailure(
                    "Attempted to re-parent node that already has parent: \(self).parent == \(parent) new parent: \(newValue)"
                )
            }
        }
    }

    open var children: [Node] { [] }

    /// A short, one-line debug description for this node. By default is an empty
    /// string.
    /// Subclasses of `Node` can override this property to return custom metadata
    /// relating to the node.
    open var shortDebugDescription: String { "" }

    public init() {

    }

    /// Accepts a given visitor on this node, invoking its corresponding visit
    /// method.
    /// 
    /// Subclasses of `Node` can override this method to customize the visit call
    /// that is made to the visitor and support specialized `NodeVisitorType`
    /// types.
    open func accept<Visitor>(_ visitor: Visitor) where Visitor: NodeVisitorType {
        visitor.visit(self)
    }
}

public extension Node {
    /// Helper extension for generating node lists from various sources.
    @inlinable
    static func makeNodeList() -> [Node] {
        return []
    }

    /// Helper extension for generating node lists from various sources.
    @inlinable
    static func makeNodeList(nodes: Node...) -> [Node] {
        return nodes
    }

    /// Helper extension for generating node lists from various sources.
    @inlinable
    static func makeNodeList(nodes: Node?...) -> [Node] {
        return nodes.compactMap({ $0 })
    }

    /// Helper extension for generating node lists from various sources.
    @inlinable
    static func makeNodeList(lists: [Node]...) -> [Node] {
        return lists.flatMap({ $0 })
    }

    /// Helper extension for generating node lists from various sources.
    @inlinable
    static func makeNodeList(lists: [Node]..., nodes: Node...) -> [Node] {
        return lists.flatMap({ $0 }) + nodes
    }

    /// Helper extension for generating node lists from various sources.
    @inlinable
    static func makeNodeList(lists: [Node]..., nodes: Node?...) -> [Node] {
        return lists.flatMap({ $0 }) + nodes.compactMap({ $0 })
    }
}
