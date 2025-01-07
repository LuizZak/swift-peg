/// Represents a raw token from a raw tokenizer.
///
/// Raw tokens contain structural information of a token but lack location
/// information, and can be created and used as a currency between the parser
/// and tokenizer.
public protocol RawTokenType: Hashable {
    /// A token kind that can be used to check the type of a token without
    /// knowing about its structural details, like its string contents.
    associatedtype TokenKind: TokenKindType

    /// The type that this token uses to return its string representation.
    associatedtype TokenString: StringProtocol = Substring

    /// Gets the kind of this token.
    var kind: TokenKind { get }

    /// The underlying string value representation of this token.
    /// Must be a string that was obtained via tokenization of a larger string
    /// stream.
    var string: TokenString { get }

    /// Gets the length of this token. The logical meaning is dependent on the
    /// underlying raw tokenizer that produced this token, but for string-based
    /// tokenization, this is the length in unicode grapheme clusters (i.e. Swift's
    /// `String.count` property).
    var length: Int { get }

    /// Produces a dummy token construction of a given type.
    /// The construction must have `RawTokenType.kind` be the same as the provided
    /// `kind`.
    ///
    /// The `RawTokenType.string` may be any arbitrary value.
    static func produceDummy(_ kind: TokenKind) -> Self
}

/// Specifies a kind of a token.
public protocol TokenKindType: Hashable, CustomStringConvertible {
    // TODO: CommonAbstract notion of whitespace into sets of tokens that can be skipped without affecting the parser under certain conditions.

    /// Gets the token type associated with whitespace, or other spacing tokens
    /// that can be skipped in regular parsing, unless they are queried for
    /// explicitly.
    static var whitespace: Self { get }
}
