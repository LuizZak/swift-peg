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
    /// rule name/rule parameters combination.
    /// 
    /// By default, `name` is the current callee's name via `#function`.
    open func makeKey(
        _ name: String = #function,
        parameters: [AnyHashable] = []
    ) -> ParserCache<RawTokenizer>.Key {

        return .init(
            mark: tokenizer.mark(),
            ruleName: name,
            parameters: parameters
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
}
