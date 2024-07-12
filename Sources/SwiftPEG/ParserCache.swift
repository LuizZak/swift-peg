/// An object for caching results of parser method invocations by tokenizer's mark
/// and optionally a parameter set for the arguments.
public struct ParserCache<RawTokenizer: RawTokenizerType> {
    public typealias Mark = Tokenizer<RawTokenizer>.Mark
    public typealias TokenKind = RawTokenizer.RawToken.TokenKind

    @usableFromInline
    internal var _cache: [Key: Any] = [:]
    @usableFromInline
    internal var _metadata: [String: Any] = [:]

    /// Dictionary to store requests for tokens within the parser.
    ///
    /// Used during error recovery/syntax error generation.
    @usableFromInline
    internal var _tokenHits: [Mark: [TokenKind]] = [:]

    /// Whether enable or disable the cache.
    /// When disabled, `has()`, `fetch()`, and `fetchAny()` behave as if the cache
    /// was empty. Calls to `store()`, `removeValue()` and to metadata functions
    /// remain unchanged.
    public var enabled: Bool = true

    /// Fetches or stores a cached token hit list on a given marker within this
    /// cache. Returns `nil` if no token hits have occurred at that point.
    @inlinable
    public subscript(tokenAt mark: Mark) -> [TokenKind]? {
        get {
            fetchTokenKinds(at: mark)
        }
        set {
            replaceTokenKinds(at: mark, newValue)
        }
    }

    /// Fetches or stores a cached entry with a given key within this cache.
    /// Returns `nil` if no entries are stored with the given key.
    @inlinable
    public subscript<Value>(key: Key) -> CacheEntry<Value?>? {
        get { fetch(key) }
        set {
            if let newValue {
                store(key, value: newValue)
            } else {
                removeValue(forKey: key)
            }
        }
    }

    // MARK: - Merging

    /// Merges failures of a given cache into this cache instance, optionally
    /// merging token hits and metadata as well. When a non-failure result is found,
    /// instead of replacing the value from this cache, it is removed from this
    /// cache altogether, requiring re-caching of the value.
    ///
    /// Failures are only merged for cache entries that are of `CacheEntry<T>`
    /// type, where the appropriate initializer for the cache entry type was
    /// used during initialization.
    public mutating func mergeFailures(
        _ other: Self,
        mergeTokenHits: Bool,
        mergeMetadata: Bool
    ) {
        for (key, value) in other._cache {
            guard let cacheEntry = value as? CacheEntryType else {
                continue
            }

            if cacheEntry.isNilValue {
                _cache[key] = DummyCacheFailure(
                    mark: key.mark,
                    reach: key.mark
                )
            } else {
                _cache.removeValue(forKey: key)
            }
        }

        if mergeTokenHits {
            for (key, value) in other._tokenHits {
                storeUniqueTokenKinds(at: key, value)
            }
        }

        if mergeMetadata {
            for (key, value) in other._metadata {
                storeMetadata(key, value)
            }
        }
    }

    // MARK: - Token requests

    /// Returns `true` if there are token requests at a specified mark within this
    /// cache.
    @inlinable
    public func hasTokenKinds(at mark: Mark) -> Bool {
        return _tokenHits[mark] != nil
    }

    /// Stores a request for a token of the given kind at a given point in this
    /// cache.
    @inlinable
    public mutating func storeTokenKind(at mark: Mark, _ tokenKind: TokenKind) {
        _tokenHits[mark, default: []].append(tokenKind)
    }

    /// Stores a request for a token of the given kind at a given point in this
    /// cache, ignoring the request if the token kind is already present at that
    /// point.
    @inlinable
    public mutating func storeUniqueTokenKind(at mark: Mark, _ tokenKind: TokenKind) {
        guard _tokenHits[mark]?.contains(tokenKind) != true else {
            return
        }

        storeTokenKind(at: mark, tokenKind)
    }

    /// Stores a set of token requests at a given point in this cache, adding
    /// them to the set of existing tokens.
    @inlinable
    public mutating func storeTokenKinds(at mark: Mark, _ tokenKinds: some Sequence<TokenKind>) {
        _tokenHits[mark, default: []].append(contentsOf: tokenKinds)
    }

    /// Stores a set of token requests at a given point in this cache, adding
    /// them to the set of existing tokens, while ignoring duplicated entries.
    @inlinable
    public mutating func storeUniqueTokenKinds(at mark: Mark, _ tokenKinds: some Sequence<TokenKind>) {
        if let existing = _tokenHits[mark] {
            let newTokens = Set(tokenKinds).subtracting(existing)

            _tokenHits[mark] = existing + newTokens
        } else {
            _tokenHits[mark] = Array(tokenKinds)
        }
    }

    /// Returns a list of all cached token fetches that occurred at a given index.
    /// If no token fetch has happened at that point, `nil` is returned, instead.
    @inlinable
    public func fetchTokenKinds(at mark: Mark) -> [TokenKind]? {
        _tokenHits[mark]
    }

    /// Replaces the cached token kinds at a given point in this cache.
    @inlinable
    public mutating func replaceTokenKinds(at mark: Mark, _ tokenKinds: [TokenKind]?) {
        _tokenHits[mark] = tokenKinds
    }

    /// Removes all cached token kind hits that occurred at a given mark from
    /// this cache. Returns the cached value, if present.
    @inlinable
    @discardableResult
    public mutating func removeTokenKinds(at mark: Mark) -> [TokenKind]? {
        _tokenHits.removeValue(forKey: mark)
    }

    // MARK: - Rule production

    /// Returns `true` if cache contains a given key.
    @inlinable
    public func has(_ key: Key) -> Bool {
        enabled && _cache[key] != nil
    }

    /// Stores the given key/value pair in this cache.
    @inlinable
    public mutating func store<T>(_ key: consuming Key, value: CacheEntry<T>) {
        _cache[key] = value
    }

    /// Performs an untyped cache fetch.
    @inlinable
    public func fetchAny(_ key: Key) -> Any?? {
        if enabled { _cache[key] }
        else { nil }
    }

    /// Performs a typed cache fetch.
    @inlinable
    public func fetch<Value>(_ key: Key) -> CacheEntry<Value?>? {
        guard enabled, let cached = _cache[key] else {
            return nil
        }

        if let cached = cached as? DummyCacheFailure {
            return CacheEntry<Value?>(
                mark: cached.mark,
                reach: cached.reach,
                result: nil
            )
        }

        return cached as? CacheEntry<Value?>
    }

    /// Fetches all keys that point at a given mark.
    @inlinable
    public func fetchAllKeys(at mark: Mark) -> [Key] {
        return _cache.keys.filter { key in
            key.mark == mark
        }
    }

    /// Removes an entry from this cache with a given key.
    @inlinable
    @discardableResult
    public mutating func removeValue(forKey key: Key) -> Any?? {
        _cache.removeValue(forKey: key)
    }

    /// Removes all cached entries within the cache that have a mark greater than
    /// `mark`.
    @inlinable
    public mutating func removePast(mark: Mark) {
        // Tokens
        removeTokenKinds(at: mark)

        // Productions
        for key in _cache.keys {
            if key.mark > mark {
                removeValue(forKey: key)
            }
        }
    }

    // MARK: - Metadata

    /// If the metadata key associated with `key` is an Int, increment it by
    /// one. If it it doesn't exist yet, creates and stores 1.
    @inlinable
    public mutating func incrementMetadata(_ key: String) {
        if let value: Int = fetchMetadata(key) {
            storeMetadata(key, value + 1)
        } else if _metadata[key] == nil {
            storeMetadata(key, 1)
        }
    }

    /// Stores metadata on this cache.
    @inlinable
    public mutating func storeMetadata(_ key: consuming String, _ value: consuming Any) {
        _metadata[key] = value
    }

    /// Fetches a metadata with a given key.
    /// Returns is `nil` if `key` is not present in the metadata cache.
    @inlinable
    public func fetchMetadataAny(_ key: String) -> Any? {
        return _metadata[key]
    }

    /// Fetches a metadata with a given key, conditionally casted to `T`.
    /// Returns is `nil` if `key` is not present in the metadata cache, or if its
    /// value cannot be cast to `T`.
    @inlinable
    public func fetchMetadata<T>(_ key: String) -> T? {
        return _metadata[key] as? T
    }

    public struct CacheEntry<T>: CacheEntryType {
        /// The tokenizer position after this cached entry's original parsing.
        public var mark: Mark
        /// The furthest reach point in the tokenizer. Used for diagnostic
        /// purposes.
        public var reach: Mark
        /// The result of this cached entry.
        public var result: T
        /// Whether the value stored within this cached entry is an optional of
        /// `nil` value.
        public var isNilValue: Bool

        @inlinable
        public init(mark: Mark, reach: Mark, result: T) {
            self.mark = mark
            self.reach = reach
            self.result = result
            self.isNilValue = false
        }

        @inlinable
        public init<U>(mark: Mark, reach: Mark, result: U?) where T == U? {
            self.mark = mark
            self.reach = reach
            self.result = result
            self.isNilValue = result == nil
        }
    }

    /// A non-realized cache failure that is awaiting to be fetches in a strong-typed
    /// context to be materialized.
    public struct DummyCacheFailure: CacheEntryType {
        /// The tokenizer position after this cached entry's original parsing.
        public var mark: Mark
        /// The furthest reach point in the tokenizer. Used for diagnostic
        /// purposes.
        public var reach: Mark
        /// Whether the value stored within this cached entry is an optional of
        /// `nil` value.
        public var isNilValue: Bool

        @inlinable
        public init(mark: Mark, reach: Mark) {
            self.mark = mark
            self.reach = reach
            self.isNilValue = true
        }
    }

    public struct Key: Hashable {
        /// The mark at which the parsing attempt that this key is based of was
        /// made.
        public var mark: Mark
        /// The name of the rule that produced this key.
        public var ruleName: String
        /// The arguments for the particular call to the rule that produced this
        /// key.
        public var arguments: [AnyHashable]?

        @inlinable
        public init(mark: Mark, ruleName: String, arguments: [AnyHashable]?) {
            self.mark = mark
            self.ruleName = ruleName
            self.arguments = arguments
        }
    }
}

/// A type-erased wrapper for `CacheEntry<T>` values.
public protocol CacheEntryType {
    /// Whether the value stored within this cached entry is an optional of
    /// `nil` value.
    var isNilValue: Bool { get }
}
