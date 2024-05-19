/// Base class for PEG parsers.
open class PEGParser<RawTokenizer: RawTokenizerType> {
    public typealias Token = RawTokenizer.Token
    public typealias Mark = Tokenizer<RawTokenizer>.Mark
    public typealias CacheEntry<T> = ParserCache<RawTokenizer>.CacheEntry<T>

    /// The cache associated with this parser.
    public let cache: ParserCache<RawTokenizer> = ParserCache()
    
    /// The tokenizer associated with this parser.
    public let tokenizer: Tokenizer<RawTokenizer>

    /// Gets the reach of the underlying tokenizer.
    /// Convenience for `self.tokenizer.reach`.
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
    open func makeSyntaxError() -> ParserError {
        typealias TokenKindType = Token.TokenKind

        let errorMark = tokenizer.mark(before: reach)
        let errorLead = "Syntax error @ \(tokenizer.readableLocation(for: errorMark))"

        let keys = cache.fetchAllKeys(at: errorMark)
        if keys.isEmpty {
            return SyntaxError.invalidSyntax(errorLead, errorMark)
        }
        let expectEntries: [TokenKindType] = keys.compactMap { key -> TokenKindType? in
            guard key.ruleName == "expect" && key.arguments.count == 1 else {
                return nil
            }
            if let entry = key.arguments[0] as? TokenKindType {
                return entry
            }

            return nil
        }

        return SyntaxError.expectedToken(
            "\(errorLead): Expected: \(expectEntries.map({ "\"\($0)\"" }).joined(separator: ", "))",
            errorMark,
            expected: expectEntries
        )
    }

    /// Produces a syntax error description from the latest token position that
    /// the marker attempted to parse.
    open func makeSyntaxError() -> ParserError where Token.TokenKind: RawRepresentable, Token.TokenKind.RawValue == String {
        typealias TokenKindType = Token.TokenKind

        let errorMark = tokenizer.mark(before: reach)
        let errorLead = "Syntax error @ \(tokenizer.readableLocation(for: errorMark))"

        let keys = cache.fetchAllKeys(at: errorMark)
        if keys.isEmpty {
            return SyntaxError.invalidSyntax(errorLead, errorMark)
        }
        let expectEntries: [TokenKindType] = keys.compactMap { key -> TokenKindType? in
            guard key.ruleName == "expect" && key.arguments.count == 1 else {
                return nil
            }
            if let entry = key.arguments[0].base as? TokenKindType {
                return entry
            }
            if let entry = key.arguments[0].base as? Token {
                return entry.kind
            }
            if
                let entry = key.arguments[0].base as? String,
                let kind = TokenKindType(rawValue: entry)
            {
                return kind
            }

            return nil
        }

        return SyntaxError.expectedToken(
            "\(errorLead): Expected: \(expectEntries.map({ "\"\($0)\"" }).joined(separator: ", "))",
            errorMark,
            expected: expectEntries
        )
    }

    /// Convenience for `self.tokenizer.mark()`.
    open func mark() -> Mark {
        tokenizer.mark()
    }

    /// Convenience for `self.tokenizer.restore(mark)`.
    open func restore(_ mark: Mark) {
        tokenizer.restore(mark)
    }

    /// Controls the token reach value by setting it to be at least as far as
    /// `mark`. If `self.reach > mark`, the reach is not updated.
    /// 
    /// Convenience for `tokenizer.updateReach(mark)`.
    open func updateReach(_ mark: Mark) {
        tokenizer.updateReach(mark)
    }

    /// Resets the reach value to point to a given mark, returning the old reach
    /// value.
    /// 
    /// Convenience for `tokenizer.resetReach(mark)`.
    public func resetReach(_ mark: Mark) -> Mark {
        tokenizer.resetReach(mark)
    }

    /// Creates a cache key for the current tokenizer position and the given
    /// rule name/rule arguments combination.
    /// 
    /// By default, `name` is the current callee's name via `#function`.
    open func makeKey(
        _ name: String = #function,
        arguments: [AnyHashable] = []
    ) -> ParserCache<RawTokenizer>.Key {

        return .init(
            mark: tokenizer.mark(),
            ruleName: name,
            arguments: arguments
        )
    }

    /// Fetches the next token in the stream and compares its kind against `kind`,
    /// returning the token if it matches. If the method fails, `nil` is returned
    /// and the tokenizer position is reset.
    @memoized("expect")
    public func __expect(kind: Token.TokenKind) throws -> Token? {
        let mark = self.mark()
        if let next = try tokenizer.next(), next.kind == kind {
            return next
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares it to `token`, returning
    /// the token if it is equal. If the method fails, `nil` is returned and the
    /// tokenizer position is reset.
    @memoized("expect")
    public func __expect(_ token: Token) throws -> Token? {
        let mark = self.mark()
        if try tokenizer.next() == token {
            return token
        }
        self.restore(mark)
        return nil
    }

    /// Fetches the next token in the stream and compares to `token`, and if it
    /// matches, consumes it and returns `true`, otherwise returns `false`.
    @memoized("maybe")
    public func __maybe(_ token: Token) throws -> Bool {
        if try tokenizer.peekToken() == token {
            _ = try tokenizer.next()
            return true
        }
        return false
    }

    /// Performs a positive lookahead for a token, returning `true` if the result
    /// of `production()` is non-nil.
    ///
    /// Restores the position of the tokenizer after the lookahead.
    public func positiveLookahead<T>(_ production: () throws -> T?) rethrows -> Bool {
        let mark = self.mark()
        defer { restore(mark) }
        return try production() != nil
    }

    /// Performs a positive lookahead for a token, returning `true` if the result
    /// of `production()` is nil.
    ///
    /// Restores the position of the tokenizer after the lookahead.
    public func negativeLookahead<T>(_ production: () throws -> T?) rethrows -> Bool {
        let mark = self.mark()
        defer { restore(mark) }
        return try production() == nil
    }

    /// Stateful boolean flag used during parsing methods that use the cut syntax
    /// feature (`~`).
    public struct CutFlag {
        public var isOn: Bool = false

        /// Toggles the cut flag on and returns `true`, allowing parser methods
        /// to chain this method in a conditional expression.
        public mutating func toggleOn() -> Bool {
            self.isOn = true
            return self.isOn
        }
    }

    /// A generic syntax error produced by a parser.
    public enum SyntaxError: ParserError, Equatable, CustomStringConvertible, CustomDebugStringConvertible {
        /// Generic syntax error.
        case invalidSyntax(String, _ mark: Mark)

        /// Expected one of the following token kinds, found none.
        case expectedToken(String, _ mark: Mark, expected: [Token.TokenKind])

        public var description: String {
            switch self {
            case .invalidSyntax(let desc, _),
                .expectedToken(let desc, _, _):
                return desc
            }
        }

        public var debugDescription: String {
            switch self {
            case .invalidSyntax(let desc, let mark):
                return "\(Self.self).invalidSyntax(\"\(desc)\", \(mark))"
            case .expectedToken(let desc, let mark, let expected):
                return "\(Self.self).expectedToken(\"\(desc)\", \(mark), expected: \(expected))"
            }
        }
    }
}
