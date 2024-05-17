/// A class for caching results of parser method invocations by tokenizer's mark
/// and optionally a parameter set for the arguments.
public class ParserCache<RawTokenizer: RawTokenizerType> {
    public typealias Mark = Tokenizer<RawTokenizer>.Mark

    private var _cache: [Key: Any] = [:]

    /// Returns `true` if cache contains a given key.
    public func has(_ key: Key) -> Bool {
        _cache[key] != nil
    }

    /// Stores the given key/value pair in this cache.
    public func store<T>(_ key: Key, value: CacheEntry<T>?) {
        _cache[key] = value
    }

    /// Performs an untyped cache fetch.
    public func fetchAny(_ key: Key) -> Any?? {
        _cache[key]
    }

    /// Performs a typed cache fetch.
    public func fetch<Value>(_ key: Key) -> CacheEntry<Value>? {
        guard let cached = _cache[key] else {
            return nil
        }
        
        return cached as? CacheEntry<Value>
    }

    /// Removes an entry to this cache with a given key.
    public func removeValue(forKey key: Key) -> Any?? {
        _cache.removeValue(forKey: key)
    }

    public struct CacheEntry<T> {
        var mark: Mark
        var result: T?
    }

    public struct Key: Hashable {
        var mark: Mark
        var ruleName: String
        var parameters: [AnyHashable]
    }
}
