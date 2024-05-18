/// Base class for PEG parsers.
open class PEGParser<RawTokenizer: RawTokenizerType> {
    public typealias TokenType = RawTokenizer.TokenType
    public typealias CacheEntry<T> = ParserCache<RawTokenizer>.CacheEntry<T>

    let cache: ParserCache<RawTokenizer> = ParserCache()
    
    /// The tokenizer associated with this parser.
    public let tokenizer: Tokenizer<RawTokenizer>

    public init(tokenizer: Tokenizer<RawTokenizer>) {
        self.tokenizer = tokenizer
    }
    
    public init(raw: RawTokenizer) {
        tokenizer = Tokenizer(rawTokenizer: raw)
    }

    /// Convenience for `self.tokenizer.mark()`.
    open func mark() -> Tokenizer<RawTokenizer>.Mark {
        tokenizer.mark()
    }

    /// Convenience for `self.tokenizer.restore(mark)`.
    open func restore(_ mark: Tokenizer<RawTokenizer>.Mark) {
        tokenizer.restore(mark)
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

    /// Fetches the next token in the stream and compares it to `token`, returning
    /// the token if it is equal. If the method fails, `nil` is returned and the
    /// token position is reset.
    @memoized("expect")
    public func __expect(_ token: TokenType) throws -> TokenType? {
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
    public func __maybe(_ token: TokenType) throws -> Bool {
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
}
