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

    public init() {

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
