import SwiftPEGMacros

/// A terminal Token node.
/// Must be subclassed by specific grammars to wrap the tokens read by the tokenizer.
open class TokenNode<Token, Location>: Node where Token: TokenType, Location: Hashable & Comparable {
    /// The immutable token value associated with this node.
    public let token: Token

    public override var shortDebugDescription: String { #""\#(token.string)""# }

    public init(token: Token, location: Location) {
        self.token = token
        
        super.init()

        self.location = location
    }

    public init(_ token: (token: Token, location: Location)) {
        self.token = token.token

        super.init()

        self.location = location
    }

    public init<Raw>(_ token: Tokenizer<Raw>.TokenResult) where Raw.Token == Token, Raw.Location == Location {
        self.token = token.token

        super.init()

        self.location = location
    }
}
