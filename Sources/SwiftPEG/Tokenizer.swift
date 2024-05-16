/// A tokenizer with built-in caching support.
public class Tokenizer<Raw: RawTokenizerType> {
    public typealias TokenType = Raw.TokenType
    private var _raw: Raw

    /// Stores the index at which `_raw` is at EOF.
    /// Cached to be re-served by `self.isEOF`.
    private var _rawEOFIndex: Int? = nil

    /// Cached tokens to be returned for index-based token requests
    private var _cachedTokens: [TokenType] = []

    /// Index into `_cachedTokens` that tokens are fetched from, currently.
    private var _tokenIndex: Int = 0

    /// Returns whether the current tokenizer position is at end-of-file, a.k.a.
    /// EOF.
    ///
    /// When at EOF, invocations of `next()` return `nil`.
    public var isEOF: Bool {
        return _raw.isEOF || _tokenIndex == _rawEOFIndex
    }

    public init(rawTokenizer: Raw) {
        self._raw = rawTokenizer
    }

    /// Peeks the next token from the underlying raw stream without advancing the
    /// token index.
    private func peekToken() throws -> TokenType? {
        // Look into cached tokens
        if _cachedTokens.count > _tokenIndex {
            return _cachedTokens[_tokenIndex]
        }

        // Prevent probing raw tokenizer past its EOF.
        if _tokenIndex == _rawEOFIndex {
            return nil
        }

        // Peek raw stream
        if let nextToken = try _raw.next() {
            _cachedTokens.append(nextToken)
            return nextToken
        } else {
            _rawEOFIndex = _tokenIndex
            return nil
        }
    }

    /// Returns the next token in the token sequence and advances the token index
    /// forward to the next token.
    /// Returns `nil` to indicate the end of the token stream.
    public func next() throws -> TokenType? {
        if let next = try peekToken() {
            _tokenIndex += 1
            return next
        }

        return nil
    }
}
