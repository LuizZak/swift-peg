/// Provides interpreting capabilities to `InternalGrammar.Grammar` objects.
public class GrammarInterpreter {
    /// Type of tokens produced by delegates and consumed by the interpreter.
    /// The length of the token is measured in grapheme clusters (ie. Swift's
    /// `String.count` value).
    public typealias TokenResult = (token: Any, length: Int)
    typealias Mark = InterpreterTokenizer.Mark

    let context = InterpretedRuleContextManager()
    weak var _delegate: Delegate?

    var delegate: Delegate {
        get throws {
            guard let delegate = _delegate else {
                throw Error.delegateReleased
            }

            return delegate
        }
    }

    var cache: InterpreterCache

    let tokenizer: InterpreterTokenizer
    let grammar: InternalGrammar.Grammar
    let source: String

    /// The number of re-entrant recursions that where made during parsing.
    public internal(set) var recursionCount: Int = 0

    /// Number of re-entrant recursions to attempt before giving up with a thrown
    /// error.
    ///
    /// This limit is shared across any recursive rule, and is reset when `produce()`
    /// is invoked again.
    public var recursionLimit: UInt = 1_000

    /// Whether to automatically skip all whitespace and newlines during parsing
    /// before feeding the delegate string sections to tokenize.
    public var skipWhitespace: Bool {
        get { tokenizer.skipWhitespace }
        set { tokenizer.skipWhitespace = newValue }
    }

    /// Initializes a new `GrammarInterpreter` with a given grammar, source string
    /// and delegate.
    public init(
        grammar: InternalGrammar.Grammar,
        source: String,
        delegate: Delegate?
    ) {
        self._delegate = delegate
        self.grammar = grammar
        self.source = source
        self.tokenizer = InterpreterTokenizer(source: source)
        self.cache = InterpreterCache()
    }

    /// Produces the `start` rule in the grammar that was provided to this interpreter.
    public func produce() throws -> Any? {
        recursionCount = 0

        return try produce(ruleName: "start")
    }

    func mark() -> Mark {
        tokenizer.mark()
    }

    func restore(_ mark: Mark) {
        tokenizer.restore(mark)
    }

    func next() throws -> TokenResult? {
        try self.tokenizer.next(self.delegate)
    }

    func makeKey(ruleName: String) -> InterpreterCache.Key {
        .init(mark: self.mark(), ruleName: ruleName)
    }

    func produce(ruleName: String) throws -> Any? {
        // Validation
        guard let rule = grammar.rule(named: ruleName) else {
            throw Error.ruleNotFound(ruleName)
        }
        guard !rule.alts.isEmpty else {
            throw Error.invalidGrammar("Rule \(ruleName) has no alternatives to match!")
        }

        return try produce(rule)
    }

    func produce(_ rule: InternalGrammar.Rule) throws -> Any? {
        if rule.isRecursiveLeader {
            return try produceRecursive(rule)
        }

        let mark = self.mark()
        let key = makeKey(ruleName: rule.name)
        if let cached = self.cache[key] {
            self.restore(cached.mark)
            return cached.result
        }

        let result = try produceUncached(rule)
        self.cache[key] = .init(mark: mark, reach: mark, result: result)
        return result
    }

    /// Attempts to produce a given rule without reading or storing the result
    /// in the cache.
    func produceUncached(_ rule: InternalGrammar.Rule) throws -> Any? {
        return try tryAlts(rule.alts)
    }

    /// Executes a recursively-reentrant rule with cache priming in an attempt to
    /// produce the longest result, in terms of tokens consume from the tokenizer.
    func produceRecursive(_ rule: InternalGrammar.Rule) throws -> Any? {
        let key = makeKey(ruleName: rule.name)
        if let cached = self.cache[key] {
            self.restore(cached.mark)
            return cached.result
        }

        // Prime cache with failed result to start with
        let mark = self.mark()

        self.cache[key] = .init(mark: mark, reach: mark, result: nil)
        var lastResult: Any?
        var lastMark = mark

        while true {
            self.restore(mark)

            recursionCount += 1
            if recursionCount >= self.recursionLimit {
                throw Error.reachedRecursionLimit
            }

            let result = try produceUncached(rule)

            let endMark = self.mark()

            if result == nil {
                break
            }
            if endMark <= lastMark {
                break
            }

            lastResult = result
            lastMark = endMark
            self.cache[key] = .init(mark: lastMark, reach: lastMark, result: result)
        }

        self.restore(lastMark)

        let result = lastResult
        let endMark: Mark
        if result != nil {
            endMark = self.mark()
        } else {
            endMark = mark
            self.restore(endMark)
        }

        self.cache[key] = .init(mark: endMark, reach: endMark, result: result)

        return result
    }

    func tryAlts(_ alts: [InternalGrammar.Alt]) throws -> Any? {
        context.push()
        defer { context.pop() }

        for alt in alts {
            let mark = self.mark()

            if let ctx = try tryAlt(alt) {
                return try self.delegate.produceResult(for: alt, context: ctx)
            }

            self.restore(mark)

            if context.isCutOn { return nil }
        }

        return nil
    }

    func tryAlt(_ alt: InternalGrammar.Alt) throws -> AltContext? {
        var ctx = AltContext(valueNames: [], values: [])

        for item in alt.items {
            let alias = item.alias()

            switch item {
            case .item(_, let item, _):
                guard let result = try tryItem(item) else {
                    return nil
                }

                ctx.valueNames.append(alias)
                ctx.values.append(result)

            case .lookahead(.forced(let atom)):
                guard let result = try self.tryAtom(atom) else {
                    throw Error.expectForceFailed(
                        "Expected \(atom.description.debugDescription)"
                    )
                }

                ctx.valueNames.append(alias)
                ctx.values.append(result)

            case .lookahead(.positive(let atom)):
                if try self.tryAtom(atom) == nil {
                    return nil
                }

            case .lookahead(.negative(let atom)):
                if try self.tryAtom(atom) != nil {
                    return nil
                }

            case .lookahead(.cut):
                context.toggleCutOn()
            }
        }

        return ctx
    }

    func tryItem(_ item: InternalGrammar.Item) throws -> Any? {
        switch item {
        case .optional(let atom):
            return Any??.some(try tryAtom(atom)) as Any?

        case .optionalItems(let alts):
            return Any??.some(try tryAlts(alts)) as Any?

        case .oneOrMore(let atom):
            guard let first = try self.tryAtom(atom) else {
                return nil
            }

            var mark = self.mark()
            var result: [Any] = [first]

            while let next = try self.tryAtom(atom) {
                result.append(next)
                mark = self.mark()
            }

            self.restore(mark)
            return result

        case .zeroOrMore(let atom):
            var mark = self.mark()
            var result: [Any] = []

            while let next = try self.tryAtom(atom) {
                result.append(next)
                mark = self.mark()
            }

            self.restore(mark)
            return result

        case .gather(let sep, let node):
            guard let first = try self.tryAtom(node) else {
                return nil
            }

            var mark = self.mark()
            var result: [Any] = [first]

            while try self.tryAtom(sep) != nil {
                guard let next = try self.tryAtom(node) else {
                    break
                }

                result.append(next)
                mark = self.mark()
            }

            self.restore(mark)
            return result

        case .atom(let atom):
            return try self.tryAtom(atom)
        }
    }

    func tryAtom(_ atom: InternalGrammar.Atom) throws -> Any? {
        switch atom {
        case .group(let alts):
            return try tryAlts(alts)

        case .ruleName(let ruleName):
            return try produce(ruleName: ruleName)

        case .token(let tokenName):
            return try expect(tokenName: tokenName)?.token

        case .anyToken:
            return try next()?.token

        case .string(_, let string):
            return try expect(tokenLiteral: string)?.token
        }
    }

    func expect(tokenName: String) throws -> TokenResult? {
        let mark = self.mark()

        guard
            let next = try self.next(),
            try self.delegate.tokenResult(next, matchesTokenName: tokenName)
        else {
            self.restore(mark)
            return nil
        }

        return next
    }

    func expect(tokenLiteral: String) throws -> TokenResult? {
        let mark = self.mark()

        guard
            let next = try self.next(),
            try self.delegate.tokenResult(next, matchesTokenLiteral: tokenLiteral)
        else {
            self.restore(mark)
            return nil
        }

        return next
    }

    /// Context for invocations of alts that match and require a return value
    /// from a delegate.
    ///
    /// Contains information about the matched alt's items.
    public struct AltContext {
        /// Gets the name of values that matched with the alt.
        ///
        /// Matches the named items that where matched on the alt. Entries are
        /// `nil` if no name could be derived from the named item.
        public var valueNames: [String?]

        /// Gets the values that matched with the alt.
        ///
        /// Matches the named items that where matched on the alt.
        public var values: [Any]

        /// Type of value in an alt context.
        private enum _Value: CustomStringConvertible {
            /// The value references a token construct.
            case token(TokenResult)

            /// The value is the result of an alt result value being delegated
            /// to a delegate.
            case rule(Any)

            /// Returns the associated value of `rule(Any)`, if this value is one,
            /// otherwise returns `nil`.
            public var asRule: Any? {
                switch self {
                case .rule(let value):
                    return value
                case .token:
                    return nil
                }
            }

            /// Returns the associated value of `token(TokenResult)`, if this
            /// value is one, otherwise returns `nil`.
            public var asToken: TokenResult? {
                switch self {
                case .token(let token):
                    return token
                case .rule:
                    return nil
                }
            }

            public var description: String {
                switch self {
                case .rule(let value):
                    return "\(value)"
                case .token(let tok):
                    return "\(tok.token)"
                }
            }
        }
    }

    public protocol Delegate: AnyObject {
        /// Requests that the delegate produce the resulting value for an alt
        /// that was matched.
        ///
        /// Errors thrown during result production abort further parsing of the
        /// syntax.
        func produceResult(
            for alt: InternalGrammar.Alt,
            context: AltContext
        ) throws -> Any

        /// Requests that a token be produced from a given substring, returning
        /// the token value and an associated length that indicates how much of
        /// the input substring the token occupies, in grapheme cluster count
        /// (ie. Swift's `String.count` value).
        ///
        /// The interpreter expects that the token be read from the beginning of
        /// the given `Substring`, and that an error be thrown if the token is
        /// not valid.
        ///
        /// Returning `nil` indicates that end-of-file has been reached.
        func produceToken(
            string: Substring
        ) throws -> TokenResult?

        /// Requests that the result of a token fetch from a tokenizer be compared
        /// to a token name found within the grammar, returning a boolean value
        /// indicating whether the interpreter should consider the token name
        /// a match.
        func tokenResult(
            _ tokenResult: TokenResult,
            matchesTokenName tokenName: String
        ) -> Bool

        /// Requests that the result of a token fetch from a tokenizer be compared
        /// to a token literal found within the grammar, returning a boolean value
        /// indicating whether the interpreter should consider the token literal
        /// a match.
        func tokenResult(
            _ tokenResult: TokenResult,
            matchesTokenLiteral tokenLiteral: String
        ) -> Bool
    }

    /// Errors that can be raised during grammar interpretation.
    public enum Error: Swift.Error, CustomStringConvertible {
        case ruleNotFound(String)
        case invalidGrammar(String)
        case expectForceFailed(String)
        case delegateReleased
        case reachedRecursionLimit

        public var description: String {
            switch self {
            case .ruleNotFound(let message): return "Could not find rule named '\(message)'"
            case .invalidGrammar(let message): return message
            case .expectForceFailed(let message): return message
            case .delegateReleased: return "Delegate is nil: Class reference was released unexpectedly!"
            case .reachedRecursionLimit: return "Reached recursion limit while parsing."
            }
        }
    }
}

internal extension InternalGrammar.Grammar {
    func metaProperty(named name: String) -> InternalGrammar.MetaProperty? {
        metas.first(where: { $0.name == name })
    }

    func rule(named name: String) -> InternalGrammar.Rule? {
        rules.first(where: { $0.name == name })
    }
}

/// Internal interpreter context-keeping object.
class InterpretedRuleContextManager {
    private var stack: [Context] = []

    var isCutOn: Bool {
        stack.last?.isCutOn == true
    }

    func toggleCutOn() {
        stack[stack.count - 1].isCutOn = true
    }

    /// Pushes a context level
    func push() {
        stack.append(Context())
    }

    /// Pops a context level
    func pop() {
        _=stack.popLast()
    }

    struct Context {
        var isCutOn: Bool = false
    }
}

/// Internal tokenizer used for interpreters.
class InterpreterTokenizer {
    typealias TokenResult = GrammarInterpreter.TokenResult
    typealias Mark = Int

    var cachedTokens: [TokenResult] = []
    var cacheIndex: Int = 0

    let source: String
    var stringIndex: String.Index
    var isEOF: Bool = false

    var skipWhitespace: Bool = true

    init(source: String) {
        self.source = source
        self.stringIndex = source.startIndex
    }

    func mark() -> Mark {
        return cacheIndex
    }

    func restore(_ mark: Mark) {
        self.cacheIndex = mark
    }

    /// Fetches the next token in the stream, either from the currently cached
    /// token set or by requesting that a given delegate parse the next token
    /// from the stream.
    func next(_ delegate: GrammarInterpreter.Delegate) throws -> TokenResult? {
        guard cacheIndex >= cachedTokens.count else {
            defer { cacheIndex += 1 }
            return cachedTokens[cacheIndex]
        }

        guard !isEOF else {
            return nil
        }

        if skipWhitespace {
            while stringIndex < source.endIndex, source[stringIndex].isWhitespace {
                source.formIndex(after: &stringIndex)
            }
        }

        guard let next = try delegate.produceToken(string: source[stringIndex...]) else {
            isEOF = true
            return nil
        }

        cachedTokens.append(next)
        cacheIndex += 1
        source.formIndex(&stringIndex, offsetBy: next.length)

        return next
    }
}
