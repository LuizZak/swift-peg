import SwiftPEGMacros

/// A terminal Token node.
/// Must be subclassed by specific grammars to wrap the tokens read by the tokenizer.
open class TokenNode: Node {
    /// The immutable token value associated with this node.
    public let token: any Hashable

    public init(token: some Hashable) {
        self.token = token
    }
}
