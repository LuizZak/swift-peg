/// Base node for all grammar nodes.
open class Node {
    /// The parent of this node in the hierarchy.
    /// 
    /// Note that since nodes are reference types, they cannot be re-parented
    /// without first removing them from the hierarchy they belong, by e.g.
    /// replacing them with another node, or by initializing a new instance of
    /// `Node`.
    /// 
    /// Subclasses can ensure that an assertion isn't raised by assigning `nil`
    /// to a child node being replaced, before assigning `self` as the new child's
    /// `parent`. An assertion isn't raised if assigning `parent` the same value
    /// it already has.
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

    /// A collection of children of this node.
    /// 
    /// For every node `child` in `children`, `child.parent === self`.
    open var children: [Node] { [] }

    /// A short, one-line debug description for this node. By default is an empty
    /// string.
    /// Subclasses of `Node` can override this property to return custom metadata
    /// relating to the node.
    open var shortDebugDescription: String { "" }

    /// A location associated with this node when it was parsed by a tokenizer.
    /// 
    /// Initialized by default as `0`.
    open var location: any (Hashable & Comparable) = 0

    public init() {

    }

    /// Accepts a given visitor on this node, invoking its corresponding visit
    /// method.
    /// 
    /// Subclasses of `Node` can override this method to customize the visit call
    /// that is made to the visitor and support specialized `NodeVisitorType`
    /// types.
    open func accept<Visitor>(_ visitor: Visitor) -> NodeVisitChildrenResult where Visitor: NodeVisitorType {
        visitor.visit(self)
    }

    /// Returns the first ancestor of this node that can be type-cast into `T`.
    /// If no parent is present, or none can be cast to `T`, `nil` is returned,
    /// instead.
    public func firstAncestor<T>(ofType type: T.Type = T.self) -> T? where T: Node {
        var current: Node = self

        while let next = current.parent {
            if let cast = current as? T {
                return cast
            }

            current = next
        }

        return nil
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
