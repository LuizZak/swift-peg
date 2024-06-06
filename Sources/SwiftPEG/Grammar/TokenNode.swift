import SwiftPEGMacros

/// A terminal Token node.
open class TokenNode<Token, Location>: Node where Token: TokenType, Location: Hashable & Comparable {
    /// The immutable token value associated with this node.
    public let rawToken: Token

    public override var shortDebugDescription: String { #""\#(rawToken.string)""# }

    public init(rawToken: Token, location: Location) {
        self.rawToken = rawToken

        super.init()

        self.location = location
    }

    public init(_ token: (rawToken: Token, location: Location)) {
        self.rawToken = token.rawToken

        super.init()

        self.location = location
    }

    public init<Raw>(_ token: Tokenizer<Raw>.TokenResult) where Raw.RawToken == Token, Raw.Location == Location {
        self.rawToken = token.rawToken

        super.init()

        self.location = location
    }
}
