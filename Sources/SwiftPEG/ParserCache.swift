/// A class for caching results of parser method invocations by tokenizer's mark
/// and optionally a parameter set for the arguments.
public class ParserCache<RawTokenizer: RawTokenizerType> {
    public typealias Mark = Tokenizer<RawTokenizer>.Mark

    private var _cache: [Key: Any] = [:]
    private var _metadata: [String: Any] = [:]

    /// Whether enable or disable the cache.
    /// When disabled, `has()`, `fetch()`, and `fetchAny()` behave as if the cache
    /// was empty. Calls to `store()`, `removeValue()` and to metadata functions
    /// remain unchanged.
    public var enabled: Bool = true

    /// Returns `true` if cache contains a given key.
    public func has(_ key: Key) -> Bool {
        enabled && _cache[key] != nil
    }

    /// Stores the given key/value pair in this cache.
    public func store<T>(_ key: Key, value: CacheEntry<T>?) {
        _cache[key] = value
    }

    /// Performs an untyped cache fetch.
    public func fetchAny(_ key: Key) -> Any?? {
        if enabled { _cache[key] }
        else { nil }
    }

    /// Performs a typed cache fetch.
    public func fetch<Value>(_ key: Key) -> CacheEntry<Value>? {
        guard enabled, let cached = _cache[key] else {
            return nil
        }
        
        return cached as? CacheEntry<Value>
    }

    /// Fetches all keys that point at a given mark.
    public func fetchAllKeys(at mark: Mark) -> [Key] {
        return _cache.keys.filter { key in
            key.mark == mark
        }
    }

    /// Removes an entry to this cache with a given key.
    @discardableResult
    public func removeValue(forKey key: Key) -> Any?? {
        _cache.removeValue(forKey: key)
    }

    /// Removes all cached entries within the cache that have a mark greater than
    /// `mark`.
    public func removePast(mark: Mark) {
        for key in _cache.keys {
            if key.mark > mark {
                removeValue(forKey: key)
            }
        }
    }

    // MARK: - Metadata

    /// If the metadata key associated with `key` is an Int, increment it by
    /// one. If it it doesn't exist yet, creates and stores 1.
    public func incrementMetadata(_ key: String) {
        if let value: Int = fetchMetadata(key) {
            storeMetadata(key, value + 1)
        } else if _metadata[key] == nil {
            storeMetadata(key, 1)
        }
    }

    /// Stores metadata on this cache.
    public func storeMetadata(_ key: String, _ value: Any) {
        _metadata[key] = value
    }

    /// Fetches a metadata with a given key.
    /// Returns is `nil` if `key` is not present in the metadata cache.
    public func fetchMetadataAny(_ key: String) -> Any? {
        return _metadata[key]
    }

    /// Fetches a metadata with a given key, conditionally casted to `T`.
    /// Returns is `nil` if `key` is not present in the metadata cache, or if its
    /// value cannot be cast to `T`.
    public func fetchMetadata<T>(_ key: String) -> T? {
        return _metadata[key] as? T
    }

    public struct CacheEntry<T> {
        /// The tokenizer position after this cached entry's original parsing.
        public var mark: Mark
        /// The furthest reach point in the tokenizer. Used for diagnostic
        /// purposes.
        public var reach: Mark
        /// The result of this cached entry.
        public var result: T

        public init(mark: Mark, reach: Mark, result: T) {
            self.mark = mark
            self.reach = reach
            self.result = result
        }
    }

    public struct Key: Hashable {
        /// The mark at which the parsing attempt that this key is based of was
        /// made.
        public var mark: Mark
        /// The name of the rule that produced this key.
        public var ruleName: String
        /// The arguments for the particular call to the rule that produced this key.
        public var arguments: [AnyHashable]
        
        public init(mark: Mark, ruleName: String, arguments: [AnyHashable]) {
            self.mark = mark
            self.ruleName = ruleName
            self.arguments = arguments
        }
    }
}
