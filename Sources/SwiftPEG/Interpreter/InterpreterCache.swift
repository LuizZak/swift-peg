/// An object for caching results of grammar interpreters.
struct InterpreterCache {
    typealias Mark = InterpreterTokenizer.Mark

    @usableFromInline
    internal var _cache: [Key: CacheEntry] = [:]

    /// Whether enable or disable the cache.
    /// When disabled, `has()`, `fetch()`, and `fetchAny()` behave as if the cache
    /// was empty. Calls to `store()`, `removeValue()` and to metadata functions
    /// remain unchanged.
    var enabled: Bool = true

    /// Fetches or stores a cached entry with a given key within this cache.
    /// Returns `nil` if no entries are stored with the given key.
    @inlinable
    subscript(key: Key) -> CacheEntry? {
        get { fetch(key) }
        set {
            if let newValue {
                store(key, value: newValue)
            } else {
                removeValue(forKey: key)
            }
        }
    }

    // MARK: - Rule production

    /// Returns `true` if cache contains a given key.
    @inlinable
    func has(_ key: Key) -> Bool {
        enabled && _cache[key] != nil
    }

    /// Stores the given key/value pair in this cache.
    @inlinable
    mutating func store(_ key: consuming Key, value: CacheEntry) {
        _cache[key] = value
    }

    /// Performs a cache fetch.
    @inlinable
    func fetch(_ key: Key) -> CacheEntry? {
        guard enabled, let cached = _cache[key] else {
            return nil
        }

        return cached
    }

    /// Fetches all keys that point at a given mark.
    @inlinable
    func fetchAllKeys(at mark: Mark) -> [Key] {
        return _cache.keys.filter { key in
            key.mark == mark
        }
    }

    /// Removes an entry from this cache with a given key.
    @inlinable
    @discardableResult
    mutating func removeValue(forKey key: Key) -> CacheEntry?? {
        _cache.removeValue(forKey: key)
    }

    /// Removes all cached entries within the cache that have a mark greater than
    /// `mark`.
    @inlinable
    mutating func removePast(mark: Mark) {
        // Productions
        for key in _cache.keys {
            if key.mark > mark {
                removeValue(forKey: key)
            }
        }
    }

    struct CacheEntry {
        /// The tokenizer position after this cached entry's original parsing.
        var mark: Mark
        /// The furthest reach point in the tokenizer. Used for diagnostic
        /// purposes.
        var reach: Mark
        /// The result of this cached entry.
        var result: Any?

        @inlinable
        init(mark: Mark, reach: Mark, result: Any?) {
            self.mark = mark
            self.reach = reach
            self.result = result
        }
    }

    struct Key: Hashable {
        /// The mark at which the parsing attempt that this key is based of was
        /// made.
        var mark: Mark
        /// The name of the rule that produced this key.
        var ruleName: String

        @inlinable
        init(mark: Mark, ruleName: String) {
            self.mark = mark
            self.ruleName = ruleName
        }
    }
}
