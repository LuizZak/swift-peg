import SwiftPEGMacros

/// A terminal Token node.
/// Must be subclassed by specific grammars to wrap the 
@NodeType<Node>
open class TokenNode: Node {
    /// The token value associated with this node.
    public let token: Any

    public init(token: Any) {
        self.token = token
    }
}
