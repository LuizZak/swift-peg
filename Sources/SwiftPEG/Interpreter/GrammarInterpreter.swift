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
    let bindingEngine: BindingEngine

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
    public convenience init(
        grammar: InternalGrammar.Grammar,
        tokens: [InternalGrammar.TokenDefinition] = [],
        source: String,
        delegate: Delegate?
    ) {
        self.init(
            processedGrammar: .init(grammar: grammar, tokens: tokens),
            source: source,
            delegate: delegate
        )
    }

    /// Initializes a new `GrammarInterpreter` with a given processed grammar,
    /// source string and delegate.
    public init(
        processedGrammar: ProcessedGrammar,
        source: String,
        delegate: Delegate?
    ) {
        self._delegate = delegate
        self.grammar = processedGrammar.grammar
        self.source = source
        self.tokenizer = InterpreterTokenizer(source: source)
        self.cache = InterpreterCache()
        self.bindingEngine = BindingEngine(processedGrammar: processedGrammar)
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

            if
                let ctx = try tryAlt(alt),
                let result = try self.delegate.produceResult(for: alt, context: ctx)
            {
                return result
            }

            self.restore(mark)

            if context.isCutOn { return nil }
        }

        return nil
    }

    func tryAlt(_ alt: InternalGrammar.Alt) throws -> AltContext? {
        let ctx = AltContext()
        return try tryNamedItems(ctx: ctx, namedItems: alt.namedItems)
    }

    func tryNamedItems(ctx: AltContext, namedItems: [InternalGrammar.NamedItem]) throws -> AltContext? {
        var ctx = ctx

        let nonStandardIndex = SwiftCodeGen.nonStandardRepetitionIndex(in: namedItems)

        for (index, namedItem) in namedItems.enumerated() {
            if index == nonStandardIndex {
                return try tryAltNonStandardRepetition(
                    ctx,
                    repetition: namedItem,
                    remaining: Array(namedItems[(index + 1)...])
                )
            }

            let alias = alias(namedItem)
            switch try tryNamedItem(namedItem) {
            case .none:
                return nil

            case .value(let value):
                ctx.valueNames.append(alias)
                ctx.values.append(value)

            case .boolean(let value):
                if !value {
                    return nil
                }

            case .toggleCut:
                context.toggleCutOn()
            }
        }

        return ctx
    }

    func tryAltNonStandardRepetition(
        _ ctx: AltContext,
        repetition: InternalGrammar.NamedItem,
        remaining: [InternalGrammar.NamedItem]
    ) throws -> AltContext? {

        var ctx = ctx
        let alias = alias(repetition)

        assert(!remaining.isEmpty, "!remaining.isEmpty")

        func tryRemaining() throws -> AltContext? {
            let mark = self.mark()
            let restCtx = AltContext()
            if let result = try tryNamedItems(ctx: restCtx, namedItems: remaining) {
                return result
            }
            self.restore(mark)
            return nil
        }

        switch repetition {
        // *<
        case .item(_, .zeroOrMore(let atom, .minimal), _):
            var collected: [Any] = []

            while !tokenizer.isEOF {
                if let rest = try tryRemaining() {
                    ctx.valueNames.append(alias)
                    ctx.values.append(collected)
                    return ctx.merged(with: rest)
                }
                if let value = try tryAtom(atom) {
                    collected.append(value)
                } else {
                    return nil
                }
            }

            return nil

        // *>
        case .item(_, .zeroOrMore(let atom, .maximal), _):
            let startMark = self.mark()
            guard
                var collected: [(Mark, Any)] = try zeroOrMore(production: {
                    if let atom = try self.tryAtom(atom) { (self.mark(), atom) }
                    else { nil }
                })
            else {
                return nil
            }

            while true {
                let mark = collected.last?.0 ?? startMark
                self.restore(mark)

                if let rest = try tryRemaining() {
                    ctx.valueNames.append(alias)
                    ctx.values.append(collected.map(\.1))
                    return ctx.merged(with: rest)
                }
                if collected.isEmpty {
                    return nil
                }

                collected.removeLast()
            }

        // +<
        case .item(_, .oneOrMore(let atom, .minimal), _):
            guard let first = try tryAtom(atom) else {
                return nil
            }

            var collected: [Any] = [first]

            while !tokenizer.isEOF {
                if let rest = try tryRemaining() {
                    ctx.valueNames.append(alias)
                    ctx.values.append(collected)
                    return ctx.merged(with: rest)
                }
                if let value = try tryAtom(atom) {
                    collected.append(value)
                } else {
                    return nil
                }
            }

            return nil

        // +>
        case .item(_, .oneOrMore(let atom, .maximal), _):
            guard
                var collected: [(Mark, Any)] = try oneOrMore(production: {
                    if let atom = try self.tryAtom(atom) { (self.mark(), atom) }
                    else { nil }
                })
            else {
                return nil
            }

            while let last = collected.last {
                let mark = last.0
                self.restore(mark)

                if let rest = try tryRemaining() {
                    ctx.valueNames.append(alias)
                    ctx.values.append(collected.map(\.1))
                    return ctx.merged(with: rest)
                }
                if collected.count < 1 {
                    return nil
                }

                collected.removeLast()
            }

            return nil

        // sep.node+<
        case .item(_, .gather(let sep, let atom, .minimal), _):
            guard let first = try tryAtom(atom) else {
                return nil
            }

            var collected: [Any] = [first]

            while !tokenizer.isEOF {
                if let rest = try tryRemaining() {
                    ctx.valueNames.append(alias)
                    ctx.values.append(collected)
                    return ctx.merged(with: rest)
                }
                guard let _ = try tryAtom(sep) else {
                    break
                }
                if let value = try tryAtom(atom) {
                    collected.append(value)
                } else {
                    return nil
                }
            }

            return nil

        // sep.node+>
        case .item(_, .gather(let sep, let atom, .maximal), _):
            guard
                var collected: [(Mark, Any)] = try gather(separator: { try self.tryAtom(sep) }, production: {
                    if let atom = try self.tryAtom(atom) { (self.mark(), atom) }
                    else { nil }
                })
            else {
                return nil
            }

            while let last = collected.last {
                let mark = last.0
                self.restore(mark)

                if let rest = try tryRemaining() {
                    ctx.valueNames.append(alias)
                    ctx.values.append(collected.map(\.1))
                    return ctx.merged(with: rest)
                }
                if collected.count < 1 {
                    return nil
                }

                collected.removeLast()
            }

            return nil

        default:
            preconditionFailure("item \(repetition) is not a non-standard repetition")
        }
    }

    func tryNamedItem(_ namedItem: InternalGrammar.NamedItem) throws -> NamedItemResult {
        switch namedItem {
        case .item(_, let item, _):
            guard let result = try tryItem(item) else {
                return .none
            }

            return .value(result)

        case .lookahead(.forced(let atom)):
            guard let result = try self.tryAtom(atom) else {
                throw Error.expectForceFailed(
                    "Expected \(atom.description.debugDescription)"
                )
            }

            return .value(result)

        case .lookahead(.positive(let atom)):
            return .boolean(try self.tryAtom(atom) != nil)

        case .lookahead(.negative(let atom)):
            return .boolean(try self.tryAtom(atom) == nil)

        case .lookahead(.cut):
            return .toggleCut
        }
    }

    func tryItem(_ item: InternalGrammar.Item) throws -> Any? {
        switch item {
        case .optional(let atom):
            return Any??.some(try tryAtom(atom)) as Any?

        case .optionalItems(let alts):
            return Any??.some(try tryAlts(alts)) as Any?

        case .zeroOrMore(let atom, _):
            return try zeroOrMore(atom)

        case .oneOrMore(let atom, _):
            return try oneOrMore(atom)

        case .gather(let sep, let node, _):
            return try gather(sep, node)

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

    func zeroOrMore(_ atom: InternalGrammar.Atom) throws -> [Any]? {
        return try zeroOrMore {
            try self.tryAtom(atom)
        }
    }

    func oneOrMore(_ atom: InternalGrammar.Atom) throws -> [Any]? {
        return try oneOrMore {
            try self.tryAtom(atom)
        }
    }

    func gather(_ sep: InternalGrammar.Atom, _ node: InternalGrammar.Atom) throws -> [Any]? {
        try gather(separator: { try self.tryAtom(sep) }) {
            try self.tryAtom(node)
        }
    }

    func zeroOrMore<T>(production: () throws -> T?) throws -> [T]? {
        var mark = self.mark()
        var result: [T] = []

        while let next = try production() {
            result.append(next)
            mark = self.mark()
        }

        self.restore(mark)
        return result
    }

    func oneOrMore<T>(production: () throws -> T?) throws -> [T]? {
        guard let first = try production() else {
            return nil
        }

        var mark = self.mark()
        var result: [T] = [first]

        while let next = try production() {
            result.append(next)
            mark = self.mark()
        }

        self.restore(mark)
        return result
    }

    func gather<U, T>(separator: () throws -> U?, production: () throws -> T?) throws -> [T]? {
        guard let first = try production() else {
            return nil
        }

        var mark = self.mark()
        var result: [T] = [first]

        while try separator() != nil {
            guard let next = try production() else {
                break
            }

            result.append(next)
            mark = self.mark()
        }

        self.restore(mark)
        return result
    }

    //

    func alias(_ namedItem: InternalGrammar.NamedItem) -> String? {
        switch namedItem {
        case .item(let name?, _, _):
            return name
        case .item(_, let item, _):
            return alias(item)
        case .lookahead:
            return nil
        }
    }

    func alias(_ item: InternalGrammar.Item) -> String? {
        switch item {
        case .atom(let atom),
            .zeroOrMore(let atom, _),
            .oneOrMore(let atom, _),
            .optional(let atom):
            return alias(atom)

        case .gather(_, let node, _):
            return alias(node)

        case .optionalItems:
            return nil
        }
    }

    func alias(_ atom: InternalGrammar.Atom) -> String? {
        switch atom {
        case .token(let ident):
            return ident.lowercased()

        case .anyToken(let ident):
            return ident.lowercased()

        case .ruleName(let ident):
            return ident

        case .group:
            return nil

        case .string(_, let literal):
            return bindingEngine.tokenName(ofRawLiteral: literal)
        }
    }

    enum NamedItemResult {
        /// Named item computed as a no value; indicates a matching fail for the
        /// named item.
        case none

        /// Named item computed to a value.
        case value(Any)

        /// Named item computed to a boolean value.
        case boolean(Bool)

        /// Named item requested a cut to be toggled on.
        case toggleCut
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
        public var valueNames: [String?] = []

        /// Gets the values that matched with the alt.
        ///
        /// Matches the named items that where matched on the alt.
        public var values: [Any] = []

        func merged(with other: Self) -> Self {
            .init(
                valueNames: self.valueNames + other.valueNames,
                values: self.values + other.values
            )
        }

        mutating func merge(with other: Self) {
            self = self.merged(with: other)
        }

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
        /// If the delegate returns `nil`, the alternative is failed as if it
        /// didn't match its items.
        ///
        /// Errors thrown during result production abort further parsing of the
        /// syntax.
        func produceResult(
            for alt: InternalGrammar.Alt,
            context: AltContext
        ) throws -> Any?

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
