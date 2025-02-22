/// Base class for PEG parsers.
open class PEGParser<RawTokenizer: RawTokenizerType> {
    public typealias RawToken = RawTokenizer.RawToken
    public typealias Mark = Tokenizer<RawTokenizer>.Mark
    public typealias CacheEntry<T> = ParserCache<RawTokenizer>.CacheEntry<T>
    /// Alias for results of parsing methods that query single tokens.
    public typealias Token = Tokenizer<RawTokenizer>.Token

    /// The cache associated with this parser.
    public var cache: ParserCache<RawTokenizer> = ParserCache()

    /// The tokenizer associated with this parser.
    public let tokenizer: Tokenizer<RawTokenizer>

    /// Gets the reach of the underlying tokenizer.
    /// Convenience for `self.tokenizer.reach`.
    @inlinable
    public var reach: Mark {
        tokenizer.reach
    }

    public init(tokenizer: Tokenizer<RawTokenizer>) {
        self.tokenizer = tokenizer
    }

    public init(raw: RawTokenizer) {
        tokenizer = Tokenizer(rawTokenizer: raw)
    }

    /// Produces a syntax error description from the latest token position that
    /// the marker attempted to parse.
    @inlinable
    open func makeSyntaxError() -> ParserError {
        let mark = self.mark()
        defer { self.restore(mark) }

        typealias TokenKindType = RawToken.TokenKind

        let errorMark = tokenizer.mark(before: self.reach)
        let location = self.tokenizer.readableLocation(for: errorMark)

        guard let tokens = cache.fetchTokenKinds(at: errorMark), !tokens.isEmpty else {
            return SyntaxError.invalidSyntax(
                "Syntax error",
                errorMark,
                location: location
            )
        }

        let expectedTokensMsg = tokens.sorted(by: { $0.description < $1.description }).asNaturalLanguageList({ "\"\($0)\"" })

        // Check EOF
        tokenizer.restore(errorMark)
        guard let actual = try? tokenizer.peekToken() else {
            return SyntaxError.unexpectedEof(
                "Unexpected end-of-stream but expected: \(expectedTokensMsg)",
                errorMark,
                location: location,
                expected: tokens
            )
        }

        return SyntaxError.expectedToken(
            "Found \"\(actual.rawToken.string)\" but expected: \(expectedTokensMsg)",
            errorMark,
            location: location,
            received: actual.rawToken,
            expected: tokens
        )
    }

    /// Creates a syntax error that can be thrown during parsing by code actions.
    ///
    /// Optionally skips skippable tokens before pinning the syntax error to the
    /// next non-skippable available token.
    open func syntaxError(_ description: String, skipSkippable: Bool = true) -> SyntaxError {
        let mark = self.mark()
        defer { self.restore(mark) }

        if skipSkippable {
            do {
                try skipChannelSkipTokens()
            } catch {
            }
        }

        let errorMark = self.mark()
        let location = self.tokenizer.readableLocation(for: errorMark)

        return SyntaxError.invalidSyntax(description, errorMark, location: location)
    }

    /// Returns `true` if the tokenizer has reached the end of the relevant,
    /// non-whitespace tokens in the stream.
    ///
    /// Checking end-of-stream with the parser is preferred over checking the
    /// tokenizer since the parser is able to syntactically analyze trailing
    /// whitespace.
    ///
    /// Implicates that the underlying raw tokenizer also has reached the end of
    /// its stream.
    ///
    /// Method can throw as any remaining tokens are parsed by the tokenizer.
    open func isEOF() throws -> Bool {
        guard tokenizer._raw.isEOF else {
            return false
        }

        // Attempt to skip whitespace
        let mark = self.mark()
        defer { self.restore(mark) }

        try skipChannelSkipTokens()

        return tokenizer.isEOF
    }

    /// Skips tokens associated with '~> skip' channels, optionally not skipping
    /// a specific set of token kinds.
    open func skipChannelSkipTokens(_ except: Set<RawToken.TokenKind> = []) throws {
    }

    /// Convenience for `self.tokenizer.mark()`.
    @inlinable
    public func mark() -> Mark {
        tokenizer.mark()
    }

    /// Convenience for `self.tokenizer.restore(mark)`.
    @inlinable
    public func restore(_ mark: Mark) {
        tokenizer.restore(mark)
    }

    /// Controls the token reach value by setting it to be at least as far as
    /// `mark`. If `self.reach > mark`, the reach is not updated.
    ///
    /// Convenience for `tokenizer.updateReach(mark)`.
    @inlinable
    open func updateReach(_ mark: Mark) {
        tokenizer.updateReach(mark)
    }

    /// Resets the reach value to point to a given mark, returning the old reach
    /// value.
    ///
    /// Convenience for `tokenizer.resetReach(mark)`.
    @inlinable
    public func resetReach(_ mark: Mark) -> Mark {
        tokenizer.resetReach(mark)
    }

    /// Creates a cache key for the current tokenizer position and the given
    /// rule name/rule arguments combination.
    ///
    /// By default, `name` is the current callee's name via `#function`.
    @inlinable
    open func makeKey(
        _ name: String = #function,
        arguments: [AnyHashable]? = nil
    ) -> ParserCache<RawTokenizer>.Key {

        return .init(
            mark: tokenizer.mark(),
            ruleName: name,
            arguments: arguments
        )
    }

    /// Convenience for `self.tokenizer.location()`.
    @inlinable
    open func location() throws -> RawTokenizer.Location? {
        try tokenizer.location()
    }

    /// Updates a node's location to point to a given mark, optionally skipping
    /// leading whitespace.
    ///
    /// Returns `node` back.
    @inlinable
    open func setLocation<T>(
        _ node: T,
        at mark: Mark,
        skippingWhitespace: Bool = true
    ) -> T where T: Node {

        guard skippingWhitespace else {
            node.location = tokenizer.location(at: mark)
            return node
        }

        let prevMark = self.mark()
        defer { restore(prevMark) }

        restore(mark)

        do {
            try skipChannelSkipTokens()
        } catch {
        }

        node.location = tokenizer.location(at: self.mark())
        return node
    }

    /// Returns the kind of the next token on the tokenizer, if not at EOF.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func peekKind() throws -> RawToken.TokenKind? {
        return try tokenizer.peekToken()?.rawToken.kind
    }

    /// Fetches the next token in the stream and returns it unconditionally.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func nextToken() throws -> Token? {
        return try tokenizer.next()
    }

    /// Fetches the next token in the stream and returns it unconditionally.
    /// Can be used in conjunction with a token definition that specifies a blank
    /// 'expectArgs' case to indicate any token.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func expect() throws -> Token? {
        return try nextToken()
    }

    /// Fetches the next token in the stream and compares it to `token`, returning
    /// the token if it is equal. If the method fails, `nil` is returned and the
    /// tokenizer position is reset.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func expect(_ token: RawToken) throws -> Token? {
        let mark = self.mark()

        // If expected kind is not explicitly a skippable token, skip all skippable
        // tokens first
        try skipChannelSkipTokens([token.kind])

        self.cache.storeTokenKind(at: self.mark(), token.kind)

        if let next = try tokenizer.next(), next.rawToken == token {
            return next
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares its kind against `kind`,
    /// returning the token if it matches. If the method fails, `nil` is returned
    /// and the tokenizer position is reset.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func expect(kind: RawToken.TokenKind) throws -> Token? {
        let mark = self.mark()

        // If expected kind is not explicitly a skippable token, skip all skippable
        // tokens first
        try skipChannelSkipTokens([kind])

        self.cache.storeTokenKind(at: self.mark(), kind)

        if let next = try tokenizer.next(), next.rawToken.kind == kind {
            return next
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares its kind against a
    /// sequence of toke kinds `kinds`, returning the token if it matches one of
    /// them. If the method fails, `nil` is returned and the tokenizer position
    /// is reset.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func expect(oneOfKind kinds: Set<RawToken.TokenKind>) throws -> Token? {
        let mark = self.mark()

        // If expected kind is not explicitly a skippable token, skip all skippable
        // tokens first
        try skipChannelSkipTokens(kinds)

        self.cache.storeUniqueTokenKinds(at: self.mark(), kinds)

        if let next = try tokenizer.next(), kinds.contains(next.rawToken.kind) {
            return next
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares to `token`, and if it
    /// matches, consumes it and returns `true`, otherwise returns `false`.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func maybe(_ token: RawToken) throws -> Bool {
        if try tokenizer.peekToken()?.rawToken == token {
            _ = try tokenizer.next()
            return true
        }
        return false
    }

    /// Fetches the next token in the stream and compares its kind to `kind`,
    /// and if it matches, consumes it and returns `true`, otherwise returns
    /// `false`.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func maybe(kind: RawToken.TokenKind) throws -> Bool {
        if try tokenizer.peekToken()?.rawToken.kind == kind {
            _ = try tokenizer.next()
            return true
        }
        return false
    }

    /// Returns an expectation that a production occur, throwing an error if it
    /// doesn't. The result is an optional type for convenience when chaining into
    /// if-let statements.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func expectForced<T>(_ production: () throws -> T?, _ message: String) throws -> T? {
        let mark = self.mark()
        guard let production = try production() else {
            throw SyntaxError.expectedForcedFailed(
                "Expected \(message)",
                mark,
                location: self.tokenizer.readableLocation(for: mark)
            )
        }

        return production
    }

    /// Returns a doubly-wrapped optional value that wraps the optional result
    /// of a given production into `Optional<T>.some(<result>)`.
    /// This method can be used to chain optional productions into if-let statements
    /// without failing the whole check.
    @inlinable
    public func optional<T>(_ production: () throws -> T?) rethrows -> T?? {
        .some(try production())
    }

    /// Performs a given production repeatedly until it returns `nil`.
    ///
    /// Since it expects that the first production may be `nil`, it always succeeds,
    /// returning an empty array if the first production failed, but retains an
    /// optional return signature for convenience when generating Swift code.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func repeatZeroOrMore<T>(_ production: () throws -> T?) rethrows -> [T]? {
        var result: [T] = []

        while let next = try production() {
            result.append(next)
        }

        return result
    }

    /// Performs a given production repeatedly until it returns `nil`.
    ///
    /// Always expects at least one positive result, so the return is either `nil`
    /// or an array with at least one item.
    ///
    /// - note: Call is not memoized.
    @inlinable
    public func repeatOneOrMore<T>(_ production: () throws -> T?) rethrows -> [T]? {
        let mark = self.mark()

        if
            let first = try production()
        {
            var result: [T] = [first]

            while let next = try production() {
                result.append(next)
            }

            return result
        }

        self.restore(mark)
        return nil
    }

    /// Performs a 'gather' construction (`<sep>.<item>+`), which serves as an
    /// alias for `<item> (<sep> <item>)*`.
    ///
    /// The return is the collected array of `item` productions.
    @inlinable
    public func gather<S, T>(
        separator separatorProducer: () throws -> S?,
        item itemProducer: () throws -> T?
    ) throws -> [T]? {

        var mark = self.mark()

        guard let item = try itemProducer() else {
            self.restore(mark)
            return nil
        }

        mark = self.mark()

        var result: [T] = [item]

        while try separatorProducer() != nil {
            guard let next = try itemProducer() else {
                break
            }

            result.append(next)
            mark = self.mark()
        }

        self.restore(mark)
        return result
    }

    /// Performs a positive lookahead for a production, returning `true` if the
    /// result of `production()` is non-nil.
    ///
    /// Restores the position of the tokenizer to the previous position
    /// before the lookahead.
    @inlinable
    public func positiveLookahead<T>(_ production: () throws -> T?) rethrows -> Bool {
        let mark = self.mark()
        defer { restore(mark) }
        return try production() != nil
    }

    /// Performs a positive lookahead for a production, returning `true` if the
    /// result of `production()` is nil.
    ///
    /// Restores the position of the tokenizer to the previous position
    /// before the lookahead.
    @inlinable
    public func negativeLookahead<T>(_ production: () throws -> T?) rethrows -> Bool {
        let mark = self.mark()
        defer { restore(mark) }
        return try production() == nil
    }

    /// Stateful boolean flag used during parsing methods that use the cut syntax
    /// feature (`~`).
    public struct CutFlag {
        public var isOn: Bool = false

        @inlinable
        public init() {
        }

        /// Toggles the cut flag on and returns `true`, allowing parser methods
        /// to chain this method in a conditional expression.
        @inlinable
        public mutating func toggleOn() -> Bool {
            self.isOn = true
            return self.isOn
        }
    }

    /// A generic syntax error produced by a parser.
    public enum SyntaxError: ParserError, Equatable, CustomStringConvertible, CustomDebugStringConvertible {
        /// Generic syntax error.
        case invalidSyntax(String, _ mark: Mark, location: String)

        /// Found end-of-stream unexpectedly.
        case unexpectedEof(String, _ mark: Mark, location: String, expected: Set<RawToken.TokenKind>)

        /// An `expectForced` check failed at a given point.
        case expectedForcedFailed(String, _ mark: Mark, location: String)

        /// Found given token kind, expected one of the following token kinds
        /// instead.
        case expectedToken(String, _ mark: Mark, location: String, received: RawToken, expected: Set<RawToken.TokenKind>)

        public var location: String {
            switch self {
            case .invalidSyntax(_, _, let location),
                .unexpectedEof(_, _, let location, _),
                .expectedForcedFailed(_, _, let location),
                .expectedToken(_, _, let location, _, _):
                return location
            }
        }

        public var description: String {
            switch self {
            case .invalidSyntax(let desc, _, let location),
                .unexpectedEof(let desc, _, let location, _),
                .expectedForcedFailed(let desc, _, let location),
                .expectedToken(let desc, _, let location, _, _):
                return "Syntax error @ \(location): \(desc)"
            }
        }

        public var debugDescription: String {
            switch self {
            case .invalidSyntax(let desc, let mark, let location):
                return "\(Self.self).invalidSyntax(\"\(desc)\", \(mark), location: \(location))"

            case .unexpectedEof(let desc, let mark, let location, let expected):
                return "\(Self.self).unexpectedEof(\"\(desc)\", \(mark), location: \(location), expected: \(expected))"

            case .expectedForcedFailed(let desc, let mark, let location):
                return "\(Self.self).expectedForcedFailed(\"\(desc)\", \(mark), location: \(location))"

            case .expectedToken(let desc, let mark, let location, let received, let expected):
                return "\(Self.self).expectedToken(\"\(desc)\", \(mark), location: \(location), received: \(received), expected: \(expected))"
            }
        }
    }
}
