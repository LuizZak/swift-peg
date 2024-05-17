/// Base class for PEG parsers.
open class PEGParser<RawTokenizer: RawTokenizerType> {
    public typealias TokenType = RawTokenizer.TokenType
    public typealias CacheEntry<T> = ParserCache<RawTokenizer>.CacheEntry<T>

    let cache: ParserCache<RawTokenizer> = ParserCache()
    let tokenizer: Tokenizer<RawTokenizer>

    public init(tokenizer: Tokenizer<RawTokenizer>) {
        self.tokenizer = tokenizer
    }
    
    public init(raw: RawTokenizer) {
        tokenizer = Tokenizer(rawTokenizer: raw)
    }

    /// Convenience for `self.tokenizer.mark()`.
    public func mark() -> Tokenizer<RawTokenizer>.Mark {
        tokenizer.mark()
    }

    /// Convenience for `self.tokenizer.restore(mark)`.
    public func restore(_ mark: Tokenizer<RawTokenizer>.Mark) {
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
}
