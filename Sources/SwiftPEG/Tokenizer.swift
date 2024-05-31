import Foundation

/// A tokenizer with built-in caching support.
open class Tokenizer<Raw: RawTokenizerType> {
    public typealias Token = Raw.Token
    public typealias Location = Raw.Location

#if DEBUG
    /// Used for uniquely identifying tokenizers in debug builds for asserting
    /// that marks haven't been used across different tokenizer instances, which
    /// may be fault-prone.
    ///
    /// Is omitted in release builds.
    @usableFromInline
    internal let _uuid = UUID().uuidString
#endif

    @usableFromInline
    internal var _raw: Raw

    /// Stores the index at which `_raw` is at EOF.
    /// Cached to be re-served by `self.isEOF`.
    @usableFromInline
    internal var _rawEOFIndex: Int? = nil

    /// See: `Tokenizer.reach`.
    @usableFromInline
    internal var _reach: Int = 0

    /// Cached tokens to be returned for index-based token requests
    @usableFromInline
    internal var cachedTokens: [TokenResult] = []

    /// Index into `_cachedTokens` that tokens are fetched from, currently.
    @usableFromInline
    internal var tokenIndex: Int = 0

    /// Returns whether the current tokenizer position is at end-of-file, a.k.a.
    /// EOF.
    ///
    /// When at EOF, invocations of `next()` return `nil`.
    @inlinable
    public var isEOF: Bool {
        return _raw.isEOF || tokenIndex == _rawEOFIndex
    }

    /// Gets the reach of this tokenizer's internal buffer.
    /// The reach is an independent mark that records how far along a token stream
    /// tokens where requested without backtracking, and is used mostly for
    /// diagnostics of parsing errors.
    public var reach: Mark {
        Mark(owner: self, index: _reach)
    }

    @inlinable
    public init(rawTokenizer: Raw) {
        self._raw = rawTokenizer
    }

    /// Returns a human-readable location for a specified mark.
    @inlinable
    open func readableLocation(for mark: Mark) -> String {
        if mark.index < cachedTokens.count {
            let token = cachedTokens[mark.index]
            return "\(token.location)"
        } else {
            return "\(mark.index)"
        }
    }

    /// Returns the next token's location in the token sequence, as reported by
    /// the underlying raw tokenizer.
    ///
    /// Returns `nil` if at EOF.
    @inlinable
    open func location() throws -> Raw.Location? {
        try peekToken()?.location
    }

    /// Returns the location of the token pointed by `mark`.
    @inlinable
    open func location(at mark: Mark) -> Raw.Location {
        cachedTokens[mark.index].location
    }

    /// Peeks the next token from the underlying raw stream without advancing the
    /// token index.
    @inlinable
    open func peekToken() throws -> TokenResult? {
        _reach = max(_reach, tokenIndex + 1)

        // Look into cached tokens
        if cachedTokens.count > tokenIndex {
            return cachedTokens[tokenIndex]
        }

        // Prevent probing raw tokenizer past its EOF.
        if tokenIndex == _rawEOFIndex {
            return nil
        }

        // Peek raw stream
        if let nextToken = try _raw.next() {
            let result = TokenResult(token: nextToken.token, location: nextToken.location)
            cachedTokens.append(result)
            return result
        } else {
            _rawEOFIndex = tokenIndex
            return nil
        }
    }

    /// Returns the next token in the token sequence and advances the token index
    /// forward to the next token.
    /// Returns `nil` to indicate the end of the token stream.
    @inlinable
    open func next() throws -> TokenResult? {
        if let next = try peekToken() {
            tokenIndex += 1
            _reach = max(_reach, tokenIndex)
            return next
        }

        return nil
    }

    // MARK: - Seeking/restoring

    /// Returns a marker that points just before a given marker.
    @inlinable
    open func mark(before marker: Mark) -> Mark {
#if DEBUG
        assert(marker.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")
#endif

        return Mark(owner: self, index: marker.index - 1)
    }

    /// Returns a marker for the current tokenizer's position so that it can be
    /// rolled back at a later point.
    ///
    /// - note: Markers from different tokenizer instances are not exchangeable.
    @inlinable
    open func mark() -> Mark {
        return Mark(owner: self, index: self.tokenIndex)
    }

    /// Restores the tokenizer to a point indicated by `mark`.
    /// - precondition: `mark` was created by an invocation to this tokenizer
    /// instance's own `mark()` method.
    @inlinable
    open func restore(_ mark: Mark) {
#if DEBUG
        assert(mark.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")
#endif

        if mark.index != self.tokenIndex {
            self.tokenIndex = mark.index
        }
    }

    /// Controls the token reach value by setting it to be at least as far as
    /// `mark`. If `self.reach > mark`, the reach is not updated.
    @inlinable
    public func updateReach(_ mark: Mark) {
#if DEBUG
        assert(mark.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")
#endif

        _reach = max(_reach, mark.index)
    }

    /// Resets the reach value to point to a given mark, returning the old reach
    /// value.
    @inlinable
    public func resetReach(_ mark: Mark) -> Mark {
#if DEBUG
        assert(mark.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")
#endif

        let oldReach = reach
        _reach = max(_reach, mark.index)
        return oldReach
    }

    /// Type for results of parsing methods that query single tokens.
    public struct TokenResult {
        public var token: Token
        public var location: Location

        @inlinable
        public init(token: Token, location: Location) {
            self.token = token
            self.location = location
        }
    }

    /// A marker created by `Tokenizer.mark()`. Can be used to later restore the
    /// token stream to a previous location.
    public struct Mark: Hashable, Comparable {
#if DEBUG
        /// UUID used in debug builds to assert that a mark was used exclusively
        /// on the tokenizer instance that produced it.
        ///
        /// Is omitted in release builds.
        @usableFromInline
        var ownerUUID: String
#endif

        @usableFromInline
        var index: Int

        @usableFromInline
        internal init(owner: Tokenizer<Raw>?, index: Int) {
            #if DEBUG
            self.ownerUUID = owner?._uuid ?? ""
            #endif

            self.index = index
        }

        /// Hashes this marker into a given hasher.
        @inlinable
        public func hash(into hasher: inout Hasher) {
            hasher.combine(index)
        }

        /// Returns `true` if `lhs` is a marker that matches the same position
        /// as `rhs` on its tokenizer.
        ///
        /// - note: In debug builds, always returns `false` for markers created
        /// by different tokenizer instances.
        @inlinable
        public static func == (lhs: Mark, rhs: Mark) -> Bool {
#if DEBUG
            lhs.ownerUUID == rhs.ownerUUID && lhs.index == rhs.index
#else
            lhs.index == rhs.index
#endif
        }

        /// Returns `true` if `lhs` is a marker that precedes `rhs` on its tokenizer.
        ///
        /// - note: In debug builds, always returns `false` for markers created
        /// by different tokenizer instances.
        @inlinable
        public static func < (lhs: Mark, rhs: Mark) -> Bool {
#if DEBUG
            lhs.ownerUUID == rhs.ownerUUID && lhs.index < rhs.index
#else
            lhs.index < rhs.index
#endif
        }
    }
}

extension Tokenizer.TokenResult: CustomStringConvertible where Raw.Token: CustomStringConvertible {
    /// Returns `token.description`.
    @inlinable
    public var description: String {
        token.description
    }
}
