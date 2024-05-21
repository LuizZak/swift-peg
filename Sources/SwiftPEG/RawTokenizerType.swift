/// Protocol for types that sequentially produce raw tokens from some underlying
/// source. This type must be wrapped by a `Tokenizer` class in order to be used
/// by a parser.
public protocol RawTokenizerType {
    /// The token type that this tokenizer produces.
    associatedtype Token: TokenType

    /// An associated type that indicates locations of tokens that are produced
    /// by this tokenizer.
    /// 
    /// Normally used to diagnose parsing errors according to line/column or some
    /// other locatable, indexable value.
    associatedtype Location: Hashable, Comparable

    /// Gets whether any more tokens can be consumed from this tokenizer.
    var isEOF: Bool { get }

    /// Requests the next token from this raw tokenizer, including its location.
    /// Returns `nil` to signal EOF for consumers.
    /// 
    /// Errors can be thrown to indicate an unknown token type.
    mutating func next() throws -> (token: Token, location: Location)?
}
