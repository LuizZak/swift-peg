/// A terminal Token node.
open class TokenNode<RawTokenizer>: Node where RawTokenizer: RawTokenizerType {
    public typealias RawToken = RawTokenizer.RawToken
    public typealias Location = RawTokenizer.Location

    /// The immutable token value associated with this node.
    public let rawToken: RawToken

    public override var shortDebugDescription: String { #""\#(rawToken.string)""# }

    public init(rawToken: RawToken, location: Location) {
        self.rawToken = rawToken

        super.init()

        self.location = location
    }

    public init(_ token: (rawToken: RawToken, location: Location)) {
        self.rawToken = token.rawToken

        super.init()

        self.location = token.location
    }

    public init(_ token: Tokenizer<RawTokenizer>.Token) {
        self.rawToken = token.rawToken

        super.init()

        self.location = token.location
    }
}
