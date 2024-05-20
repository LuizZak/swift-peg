/// Base class for PEG parsers.
open class PEGParser<RawTokenizer: RawTokenizerType> {
    public typealias Token = RawTokenizer.Token
    public typealias Mark = Tokenizer<RawTokenizer>.Mark
    public typealias CacheEntry<T> = ParserCache<RawTokenizer>.CacheEntry<T>

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

    // TODO: Reduce duplication of methods bellow

    /// Produces a syntax error description from the latest token position that
    /// the marker attempted to parse.
    @inlinable
    open func makeSyntaxError() -> ParserError {
        let mark = self.mark()
        defer { self.restore(mark) }

        typealias TokenKindType = Token.TokenKind

        let errorMark = tokenizer.mark(before: reach)
        let errorLead = "Syntax error @ \(tokenizer.readableLocation(for: errorMark))"

        let keys = cache.fetchAllKeys(at: errorMark)
        if keys.isEmpty {
            return SyntaxError.invalidSyntax(errorLead, errorMark)
        }
        tokenizer.restore(errorMark)
        guard let actual = try? tokenizer.peekToken() else {
            return SyntaxError.unexpectedEof("\(errorLead): Unexpected end-of-stream", errorMark)
        }
        let expectEntries: [TokenKindType] = keys.compactMap { key -> TokenKindType? in
            guard let arguments = key.arguments else {
                return nil
            }
            guard key.ruleName == "expect" && arguments.count == 1 else {
                return nil
            }
            if let entry = arguments[0].base as? TokenKindType {
                return entry
            }
            if let entry = arguments[0].base as? Token {
                return entry.kind
            }

            return nil
        }

        return SyntaxError.expectedToken(
            "\(errorLead): Found \"\(actual.string)\" but expected: \(expectEntries.map({ "\"\($0)\"" }).joined(separator: ", "))",
            errorMark,
            received: actual,
            expected: expectEntries
        )
    }

    /// Produces a syntax error description from the latest token position that
    /// the marker attempted to parse.
    @inlinable
    open func makeSyntaxError() -> ParserError where Token.TokenKind: RawRepresentable, Token.TokenKind.RawValue == String {
        let mark = self.mark()
        defer { self.restore(mark) }

        typealias TokenKindType = Token.TokenKind

        let errorMark = tokenizer.mark(before: reach)
        let errorLead = "Syntax error @ \(tokenizer.readableLocation(for: errorMark))"

        let keys = cache.fetchAllKeys(at: errorMark)
        if keys.isEmpty {
            return SyntaxError.invalidSyntax(errorLead, errorMark)
        }
        tokenizer.restore(errorMark)
        guard let actual = try? tokenizer.peekToken() else {
            return SyntaxError.unexpectedEof("\(errorLead): Unexpected end-of-stream", errorMark)
        }
        let expectEntries: [TokenKindType] = keys.compactMap { key -> TokenKindType? in
            guard let arguments = key.arguments else {
                return nil
            }
            guard key.ruleName == "expect" && arguments.count == 1 else {
                return nil
            }
            if let entry = arguments[0].base as? TokenKindType {
                return entry
            }
            if let entry = arguments[0].base as? Token {
                return entry.kind
            }
            if
                let entry = arguments[0].base as? String,
                let kind = TokenKindType(rawValue: entry)
            {
                return kind
            }

            return nil
        }

        return SyntaxError.expectedToken(
            "\(errorLead): Found \"\(actual.string)\" but expected: \(expectEntries.map({ "\"\($0)\"" }).joined(separator: ", "))",
            errorMark,
            received: actual,
            expected: expectEntries
        )
    }

    /// Convenience for `self.tokenizer.mark()`.
    @inlinable
    open func mark() -> Mark {
        tokenizer.mark()
    }

    /// Convenience for `self.tokenizer.restore(mark)`.
    @inlinable
    open func restore(_ mark: Mark) {
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

    /// Returns the kind of the next token on the tokenizer, if not at EOF.
    /// 
    /// - note: Call is not memoized.
    @inlinable
    public func peekKind() throws -> Token.TokenKind? {
        return try tokenizer.peekToken()?.kind
    }

    /// Fetches the next token in the stream and compares its kind against `kind`,
    /// returning the token if it matches. If the method fails, `nil` is returned
    /// and the tokenizer position is reset.
    /// 
    /// - note: Call is not memoized.
    @inlinable
    public func expect(kind: Token.TokenKind) throws -> Token? {
        let mark = self.mark()
        if let next = try tokenizer.next(), next.kind == kind {
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
    public func expect(oneOfKind kinds: Set<Token.TokenKind>) throws -> Token? {
        let mark = self.mark()
        if let next = try tokenizer.next(), kinds.contains(next.kind) {
            return next
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares it to `token`, returning
    /// the token if it is equal. If the method fails, `nil` is returned and the
    /// tokenizer position is reset.
    /// 
    /// - note: Call is not memoized.
    @inlinable
    public func expect(_ token: Token) throws -> Token? {
        let mark = self.mark()
        if try tokenizer.next() == token {
            return token
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares to `token`, and if it
    /// matches, consumes it and returns `true`, otherwise returns `false`.
    /// 
    /// - note: Call is not memoized.
    @inlinable
    public func maybe(_ token: Token) throws -> Bool {
        if try tokenizer.peekToken() == token {
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
    public func maybe(kind: Token.TokenKind) throws -> Bool {
        if try tokenizer.peekToken()?.kind == kind {
            _ = try tokenizer.next()
            return true
        }
        return false
    }

    /// Fetches the next token in the stream and returns it unconditionally.
    /// 
    /// - note: Call is not memoized.
    @inlinable
    public func nextToken() throws -> Token? {
        return try tokenizer.next()
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

    /// Performs a given production repeatedly until it returns `nil`.
    /// 
    /// Since it expects that the first production may be `nil`, it always succeeds,
    /// returning an empty array if the first production failed.
    /// 
    /// - note: Call is not memoized.
    @inlinable
    public func repeatZeroOrMore<T>(_ production: () throws -> T?) rethrows -> [T] {
        var result: [T] = []

        while let next = try production() {
            result.append(next)
        }

        return result
    }

    /// Performs a positive lookahead for a token, returning `true` if the result
    /// of `production()` is non-nil.
    ///
    /// Restores the position of the tokenizer after the lookahead.
    @inlinable
    public func positiveLookahead<T>(_ production: () throws -> T?) rethrows -> Bool {
        let mark = self.mark()
        defer { restore(mark) }
        return try production() != nil
    }

    /// Performs a positive lookahead for a token, returning `true` if the result
    /// of `production()` is nil.
    ///
    /// Restores the position of the tokenizer after the lookahead.
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
        case invalidSyntax(String, _ mark: Mark)

        /// Found end-of-stream unexpectedly.
        case unexpectedEof(String, _ mark: Mark)

        /// Found given token kind, expected one of the following token kinds
        /// instead.
        case expectedToken(String, _ mark: Mark, received: Token, expected: [Token.TokenKind])

        public var description: String {
            switch self {
            case .invalidSyntax(let desc, _),
                .unexpectedEof(let desc, _),
                .expectedToken(let desc, _, _, _):
                return desc
            }
        }

        public var debugDescription: String {
            switch self {
            case .invalidSyntax(let desc, let mark):
                return "\(Self.self).invalidSyntax(\"\(desc)\", \(mark))"
            case .unexpectedEof(let desc, let mark):
                return "\(Self.self).unexpectedEof(\"\(desc)\", \(mark))"
            case .expectedToken(let desc, let mark, let received, let expected):
                return "\(Self.self).expectedToken(\"\(desc)\", \(mark), received: \(received), expected: \(expected))"
            }
        }
    }
}
