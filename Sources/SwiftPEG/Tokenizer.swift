import Foundation

/// A tokenizer with built-in caching support.
public class Tokenizer<Raw: RawTokenizerType> {
    public typealias RawToken = Raw.RawToken
    public typealias Location = Raw.Location

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

    /// Returns `true` if the current token index is within the cached tokens
    /// list.
    @inlinable
    internal var isInCache: Bool {
        tokenIndex < cachedTokens.count
    }

    /// Returns whether the current tokenizer position is at end-of-file, a.k.a.
    /// EOF.
    ///
    /// When at EOF, invocations of `next()` return `nil`.
    @inlinable
    public var isEOF: Bool {
        return tokenIndex == _rawEOFIndex
    }

    /// Gets the reach of this tokenizer's internal buffer.
    /// The reach is an independent mark that records how far along a token stream
    /// tokens where requested without backtracking, and is used mostly for
    /// diagnostics of parsing errors.
    public var reach: Mark {
        Mark(index: _reach)
    }

    @inlinable
    public init(rawTokenizer: Raw) {
        self._raw = rawTokenizer
    }

    /// Updates `_rawEOFIndex` depending on the current state of the raw tokenizer.
    @inlinable
    func updateRawEOFIndex() {
        if !isEOF && _raw.isEOF {
            _rawEOFIndex = cachedTokens.count
        }
    }

    /// Returns a human-readable location for a specified mark.
    @inlinable
    public func readableLocation(for mark: Mark) -> String {
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
    public func location() throws -> Raw.Location? {
        try peekToken()?.location
    }

    /// Returns the location of the token pointed by `mark`.
    @inlinable
    public func location(at mark: Mark) -> Raw.Location {
        cachedTokens[mark.index].location
    }

    /// Peeks the next token from the underlying raw stream without advancing the
    /// token index.
    @inlinable
    public func peekToken() throws -> Token? {
        _reach = max(_reach, tokenIndex + 1)

        // Look into cached tokens
        if cachedTokens.count > tokenIndex {
            return cachedTokens[tokenIndex]
        }

        // Prevent probing raw tokenizer past its EOF.
        updateRawEOFIndex()
        if isEOF {
            return nil
        }

        // Peek raw stream
        if let nextToken = try _raw.next() {
            let result = Token(nextToken)
            cachedTokens.append(result)

            updateRawEOFIndex()

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
    public func next() throws -> Token? {
        if let next = try peekToken() {
            tokenIndex += 1
            _reach = max(_reach, tokenIndex)
            return next
        }

        return nil
    }

    /// Skips the current token in the stream without consuming it.
    @inlinable
    public func skip() throws {
        if isInCache {
            tokenIndex += 1
            _reach = max(_reach, tokenIndex)
        } else {
            _=try next()
        }
    }

    // MARK: - Seeking/restoring

    /// Returns a marker that points just before a given marker.
    @inlinable
    public func mark(before marker: Mark) -> Mark {
        return Mark(index: marker.index - 1)
    }

    /// Returns a marker for the current tokenizer's position so that it can be
    /// rolled back at a later point.
    ///
    /// - note: Markers from different tokenizer instances are not exchangeable.
    @inlinable
    public func mark() -> Mark {
        return Mark(index: self.tokenIndex)
    }

    /// Restores the tokenizer to a point indicated by `mark`.
    /// - precondition: `mark` was created by an invocation to this tokenizer
    /// instance's own `mark()` method.
    @inlinable
    public func restore(_ mark: Mark) {
        self.tokenIndex = mark.index
    }

    /// Controls the token reach value by setting it to be at least as far as
    /// `mark`. If `self.reach > mark`, the reach is not updated.
    @inlinable
    public func updateReach(_ mark: Mark) {
        _reach = max(_reach, mark.index)
    }

    /// Resets the reach value to point to a given mark, returning the old reach
    /// value.
    @inlinable
    public func resetReach(_ mark: Mark) -> Mark {
        let oldReach = reach
        _reach = max(_reach, mark.index)
        return oldReach
    }

    /// Type for tokens produced by a tokenizer.
    ///
    /// Pairs a raw token and location from the raw tokenizer.
    public struct Token: Hashable {
        public var rawToken: RawToken
        public var location: Location

        @inlinable
        public init(rawToken: RawToken, location: Location) {
            self.rawToken = rawToken
            self.location = location
        }

        @inlinable
        public init(_ token: (rawToken: RawToken, location: Location)) {
            self.init(rawToken: token.rawToken, location: token.location)
        }
    }

    /// A marker created by `Tokenizer.mark()`. Can be used to later restore the
    /// token stream to a previous location.
    public struct Mark: Hashable, Comparable {
        @usableFromInline
        var index: Int

        @usableFromInline
        internal init(index: Int) {
            self.index = index
        }

        /// Hashes this marker into a given hasher.
        @inlinable
        public func hash(into hasher: inout Hasher) {
            hasher.combine(index)
        }

        /// Returns `true` if `lhs` is a marker that matches the same position
        /// as `rhs` on its tokenizer.
        @inlinable
        public static func == (lhs: Mark, rhs: Mark) -> Bool {
            lhs.index == rhs.index
        }

        /// Returns `true` if `lhs` is a marker that precedes `rhs` on its tokenizer.
        @inlinable
        public static func < (lhs: Mark, rhs: Mark) -> Bool {
            lhs.index < rhs.index
        }
    }
}

extension Tokenizer.Token: CustomStringConvertible where Raw.RawToken: CustomStringConvertible {
    /// Returns `rawToken.description`.
    @inlinable
    public var description: String {
        rawToken.description
    }
}
