import Foundation

/// A tokenizer with built-in caching support.
open class Tokenizer<Raw: RawTokenizerType> {
    public typealias Token = Raw.Token

    /// Used for uniquely identifying tokenizers
    @usableFromInline
    internal let _uuid = UUID().uuidString

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
    internal var cachedTokens: [Token] = []

    /// Index into `_cachedTokens` that tokens are fetched from, currently.
    @usableFromInline
    internal var tokenIndex: Int = 0

    /// Returns whether the current tokenizer position is at end-of-file, a.k.a.
    /// EOF.
    ///
    /// When at EOF, invocations of `next()` return `nil`.
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
        "\(mark.index)"
    }

    /// Peeks the next token from the underlying raw stream without advancing the
    /// token index.
    @inlinable
    open func peekToken() throws -> Token? {
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
            cachedTokens.append(nextToken)
            return nextToken
        } else {
            _rawEOFIndex = tokenIndex
            return nil
        }
    }

    /// Returns the next token in the token sequence and advances the token index
    /// forward to the next token.
    /// Returns `nil` to indicate the end of the token stream.
    @inlinable
    open func next() throws -> Token? {
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
        assert(marker.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")

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
        precondition(
            mark.ownerUUID == self._uuid,
            "Attempt to restore() to a Mark created by a different tokenizer?"
        )

        if mark.index != self.tokenIndex {
            self.tokenIndex = mark.index
        }
    }

    /// Controls the token reach value by setting it to be at least as far as
    /// `mark`. If `self.reach > mark`, the reach is not updated.
    @inlinable
    public func updateReach(_ mark: Mark) {
        assert(mark.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")
        _reach = max(_reach, mark.index)
    }

    /// Resets the reach value to point to a given mark, returning the old reach
    /// value.
    @inlinable
    public func resetReach(_ mark: Mark) -> Mark {
        assert(mark.ownerUUID == self._uuid, "mark.ownerUUID != \(self._uuid)")

        let oldReach = reach
        _reach = max(_reach, mark.index)
        return oldReach
    }

    /// A marker created by `Tokenizer.mark()`. Can be used to later restore the
    /// token stream to a previous location.
    public struct Mark: Hashable, Comparable {
        @usableFromInline
        var ownerUUID: String
        @usableFromInline
        var index: Int

        @usableFromInline
        internal init(owner: Tokenizer<Raw>?, index: Int) {
            self.ownerUUID = owner?._uuid ?? ""
            self.index = index
        }

        /// Hashes this marker into a given hasher.
        @inlinable
        public func hash(into hasher: inout Hasher) {
            hasher.combine(index)
        }

        /// Returns `true` if `lhs` is a marker that precedes `rhs` on its tokenizer.
        ///
        /// - note: Always returns `false` for markers created by different
        /// tokenizer instances.
        @inlinable
        public static func < (lhs: Mark, rhs: Mark) -> Bool {
            lhs.ownerUUID == rhs.ownerUUID && lhs.index < rhs.index
        }

        /// Returns `true` if `lhs` is a marker that points to the same location
        /// as `rhs` on its tokenizer.
        ///
        /// - note: Always returns `false` for markers created by different
        /// tokenizer instances.
        @inlinable
        public static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.ownerUUID == rhs.ownerUUID && lhs.index == rhs.index
        }
    }
}
