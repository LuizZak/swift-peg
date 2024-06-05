import XCTest

@testable import SwiftPEG

class SwiftCodeGen_RepetitionsTests: XCTestCase {
    func testGenerateParser_zeroOrMore_minimal() throws {
        let grammar = try parseGrammar("""
        @token d; @token e; @tokenCallKind "expectKind" ;
        start: a b*< c;
        a[A]: 'a' ;
        b[B]: 'b' ;
        c[C]: 'c'? 'e' ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.emitTypesInBindings, value: true)
        )

        diffTest(expected: """
            extension Parser {
                /// ```
                /// start:
                ///     | a b*< c
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, c?) = try self._start_nsr()
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// a[A]:
                ///     | 'a'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> A? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("a")
                    {
                        return A()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// b[B]:
                ///     | 'b'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> B? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("b")
                    {
                        return B()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// c[C]:
                ///     | 'c'? 'e'
                ///     ;
                /// ```
                @memoized("c")
                @inlinable
                public func __c() throws -> C? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: TokenResult = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start_nsr[(b: [B], c: C)]:
                ///     | b*< _start_nsr_tail
                ///     ;
                /// ```
                @memoized("_start_nsr")
                @inlinable
                public func ___start_nsr() throws -> (b: [B]?, c: C?) {
                    var _current: [B] = []

                    while true {
                        let _mark = self.mark()

                        if
                            let c: C = try self._start_nsr_tail()
                        {
                            return (_current, c)
                        }

                        self.restore(_mark)

                        // Collect an extra item and try again
                        if
                            let b: B = try self.b()
                        {
                            _current.append(b)
                        } else {
                            break
                        }
                    }

                    return (nil, nil)
                }

                /// ```
                /// _start_nsr_tail[C]:
                ///     | c { c }
                ///     ;
                /// ```
                @memoized("_start_nsr_tail")
                @inlinable
                public func ___start_nsr_tail() throws -> C? {
                    let _mark = self.mark()

                    if
                        let c: C = try self.c()
                    {
                        return c
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_zeroOrMore_maximal() throws {
        let grammar = try parseGrammar("""
        @token d; @token e; @tokenCallKind "expectKind" ;
        start: a b*> c;
        a[A]: 'a' ;
        b[B]: 'b' ;
        c[C]: 'c'? 'e' ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.emitTypesInBindings, value: true)
        )

        diffTest(expected: #"""
            extension Parser {
                /// ```
                /// start:
                ///     | a b*> c
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, c?) = try self._start_nsr()
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// a[A]:
                ///     | 'a'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> A? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("a")
                    {
                        return A()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// b[B]:
                ///     | 'b'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> B? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("b")
                    {
                        return B()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// c[C]:
                ///     | 'c'? 'e'
                ///     ;
                /// ```
                @memoized("c")
                @inlinable
                public func __c() throws -> C? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: TokenResult = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start_nsr[(b: [B], c: C)]:
                ///     | b*> _start_nsr_tail
                ///     ;
                /// ```
                @memoized("_start_nsr")
                @inlinable
                public func ___start_nsr() throws -> (b: [B]?, c: C?) {
                    let _mark = self.mark()

                    // Start by fetching as many productions as possible
                    guard
                        var _current: [(Mark, B)] = try self.repeatZeroOrMore({
                            if let b: B = try self.b() { return (self.mark(), b) }
                            return nil
                        })
                    else {
                        return (nil, nil)
                    }

                    while true {
                        let _endMark = _current.last?.0 ?? _mark
                        self.restore(_endMark)

                        if
                            let c: C = try self._start_nsr_tail()
                        {
                            return (_current.map(\.1), c)
                        } else if _current.isEmpty {
                            return (nil, nil)
                        }

                        // Drop an item, backtrack the parser, and try again
                        _current.removeLast()
                    }

                    return (nil, nil)
                }

                /// ```
                /// _start_nsr_tail[C]:
                ///     | c { c }
                ///     ;
                /// ```
                @memoized("_start_nsr_tail")
                @inlinable
                public func ___start_nsr_tail() throws -> C? {
                    let _mark = self.mark()

                    if
                        let c: C = try self.c()
                    {
                        return c
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """#).diff(result)
    }
    func testGenerateParser_oneOrMore_minimal() throws {
        let grammar = try parseGrammar("""
        @token d; @tokenCallKind "expectKind" ;
        start: a b+< c;
        a[A]: 'a' ;
        b[B]: 'b' ;
        c[C]: 'c'? 'd' ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.emitTypesInBindings, value: true)
        )

        diffTest(expected: """
            extension Parser {
                /// ```
                /// start:
                ///     | a b+< c
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, c?) = try self._start_nsr()
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// a[A]:
                ///     | 'a'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> A? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("a")
                    {
                        return A()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// b[B]:
                ///     | 'b'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> B? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("b")
                    {
                        return B()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// c[C]:
                ///     | 'c'? 'd'
                ///     ;
                /// ```
                @memoized("c")
                @inlinable
                public func __c() throws -> C? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: TokenResult = try self.expect("d")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start_nsr[(b: [B], c: C)]:
                ///     | b+< _start_nsr_tail
                ///     ;
                /// ```
                @memoized("_start_nsr")
                @inlinable
                public func ___start_nsr() throws -> (b: [B]?, c: C?) {
                    var _current: [B] = []

                    while
                        let b: B = try self.b()
                    {
                        _current.append(b)
                        let _mark = self.mark()

                        if
                            let c: C = try self._start_nsr_tail()
                        {
                            return (_current, c)
                        }

                        self.restore(_mark)
                    }

                    return (nil, nil)
                }

                /// ```
                /// _start_nsr_tail[C]:
                ///     | c { c }
                ///     ;
                /// ```
                @memoized("_start_nsr_tail")
                @inlinable
                public func ___start_nsr_tail() throws -> C? {
                    let _mark = self.mark()

                    if
                        let c: C = try self.c()
                    {
                        return c
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_oneOrMore_maximal() throws {
        let grammar = try parseGrammar("""
        @token d; @token e; @tokenCallKind "expectKind" ;
        start: a b+> c;
        a[A]: 'a' ;
        b[B]: 'b' ;
        c[C]: 'c'? 'e' ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.emitTypesInBindings, value: true)
        )

        diffTest(expected: #"""
            extension Parser {
                /// ```
                /// start:
                ///     | a b+> c
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, c?) = try self._start_nsr()
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// a[A]:
                ///     | 'a'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> A? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("a")
                    {
                        return A()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// b[B]:
                ///     | 'b'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> B? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult = try self.expect("b")
                    {
                        return B()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// c[C]:
                ///     | 'c'? 'e'
                ///     ;
                /// ```
                @memoized("c")
                @inlinable
                public func __c() throws -> C? {
                    let _mark = self.mark()

                    if
                        let _: TokenResult? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: TokenResult = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start_nsr[(b: [B], c: C)]:
                ///     | b+> _start_nsr_tail
                ///     ;
                /// ```
                @memoized("_start_nsr")
                @inlinable
                public func ___start_nsr() throws -> (b: [B]?, c: C?) {
                    // Start by fetching as many productions as possible
                    guard
                        var _current: [(Mark, B)] = try self.repeatZeroOrMore({
                            if let b: B = try self.b() { return (self.mark(), b) }
                            return nil
                        })
                    else {
                        return (nil, nil)
                    }

                    while let _end = _current.last {
                        self.restore(_end.0)

                        if
                            let c: C = try self._start_nsr_tail()
                        {
                            return (_current.map(\.1), c)
                        } else if _current.count <= 1 {
                            return (nil, nil)
                        }

                        // Drop an item, backtrack the parser, and try again
                        _current.removeLast()
                    }

                    return (nil, nil)
                }

                /// ```
                /// _start_nsr_tail[C]:
                ///     | c { c }
                ///     ;
                /// ```
                @memoized("_start_nsr_tail")
                @inlinable
                public func ___start_nsr_tail() throws -> C? {
                    let _mark = self.mark()

                    if
                        let c: C = try self.c()
                    {
                        return c
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """#).diff(result)
    }
}

// MARK: - Test internals

private func makeSut(_ grammar: InternalGrammar.Grammar, _ tokens: [InternalGrammar.TokenDefinition] = []) -> SwiftCodeGen {
    SwiftCodeGen(grammar: grammar, tokenDefinitions: tokens)
}

private func parseGrammar(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> InternalGrammar.Grammar {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let grammar = try parser.start(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    let processor = GrammarProcessor(delegate: nil)
    return try processor.process(grammar).grammar
}
