/// Represents a token from a tokenizer.
public protocol TokenType: Hashable {
    /// A token kind that can be used to check the type of a token without
    /// knowing about its structural details, like its string contents.
    associatedtype TokenKind: TokenKindType

    /// The type that this token uses to return its string representation.
    associatedtype TokenString: StringProtocol

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
    /// The construction must have `TokenType.kind` be the same as the provided
    /// `kind`.
    ///
    /// The `TokenType.string` may be any arbitrary value.
    static func produceDummy(_ kind: TokenKind) -> Self
}

/// Specifies a kind of a token.
public protocol TokenKindType: Hashable, CustomStringConvertible {

}
