/// A tokenizer with built-in caching support.
open class Tokenizer<Raw: RawTokenizerType> {
    public typealias TokenType = Raw.TokenType
    private var _raw: Raw

    /// Stores the index at which `_raw` is at EOF.
    /// Cached to be re-served by `self.isEOF`.
    private var _rawEOFIndex: Int? = nil

    /// Cached tokens to be returned for index-based token requests
    private var _cachedTokens: [TokenType] = []

    /// Index into `_cachedTokens` that tokens are fetched from, currently.
    internal var tokenIndex: Int = 0

    /// Returns whether the current tokenizer position is at end-of-file, a.k.a.
    /// EOF.
    ///
    /// When at EOF, invocations of `next()` return `nil`.
    public var isEOF: Bool {
        return _raw.isEOF || tokenIndex == _rawEOFIndex
    }

    public init(rawTokenizer: Raw) {
        self._raw = rawTokenizer
    }

    /// Peeks the next token from the underlying raw stream without advancing the
    /// token index.
    open func peekToken() throws -> TokenType? {
        // Look into cached tokens
        if _cachedTokens.count > tokenIndex {
            return _cachedTokens[tokenIndex]
        }

        // Prevent probing raw tokenizer past its EOF.
        if tokenIndex == _rawEOFIndex {
            return nil
        }

        // Peek raw stream
        if let nextToken = try _raw.next() {
            _cachedTokens.append(nextToken)
            return nextToken
        } else {
            _rawEOFIndex = tokenIndex
            return nil
        }
    }

    /// Returns the next token in the token sequence and advances the token index
    /// forward to the next token.
    /// Returns `nil` to indicate the end of the token stream.
    open func next() throws -> TokenType? {
        if let next = try peekToken() {
            tokenIndex += 1
            return next
        }

        return nil
    }

    // MARK: - Seeking/restoring

    /// Returns a marker for the current tokenizer's position so that it can be
    /// rolled back at a later point.
    /// 
    /// - note: Markers from different tokenizer instances are not exchangeable.
    open func mark() -> Mark {
        return Mark(owner: self, index: self.tokenIndex)
    }

    /// Restores the tokenizer to a point indicated by `mark`.
    /// - precondition: `mark` was created by an invocation to this tokenizer
    /// instance's own `mark()` method.
    open func restore(_ mark: Mark) {
        precondition(
            mark.owner === self,
            "Attempt to restore() to a Mark created by a different tokenizer?"
        )

        if mark.index != self.tokenIndex {
            self.tokenIndex = mark.index
        }
    }

    /// A marker created by `Tokenizer.mark()`. Can be used to later restore the
    /// token stream to a previous location.
    public struct Mark: Hashable, Comparable {
        weak var owner: Tokenizer?
        var index: Int

        /// Hashes this marker into a given hasher.
        public func hash(into hasher: inout Hasher) {
            hasher.combine(index)
        }

        /// Returns `true` if `lhs` is a marker that precedes `rhs` on its tokenizer.
        ///
        /// - note: Always returns `false` for markers created by different
        /// tokenizer instances.
        public static func < (lhs: Mark, rhs: Mark) -> Bool {
            lhs.owner === rhs.owner && lhs.index < rhs.index
        }

        /// Returns `true` if `lhs` is a marker that points to the same location
        /// as `rhs` on its tokenizer.
        ///
        /// - note: Always returns `false` for markers created by different
        /// tokenizer instances.
        public static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.owner === rhs.owner && lhs.index == rhs.index
        }
    }
}
