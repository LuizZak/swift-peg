import Testing

@testable import SwiftPEG

struct SwiftCodeGenTests {
    @Test
    func generateParser_emptyGrammar() throws {
        let grammar = makeGrammar([])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
            }

            """).diff(result)
    }

    @Test
    func generateParser_generationKind_class_emptyGrammar() throws {
        let grammar = makeGrammar([], metas: [
            .init(name: "generationKind", value: .identifier("class")),
            .init(name: "tokenTypeName", value: .identifier("MyToken")),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            public class TestParser<RawTokenizer: RawTokenizerType>: PEGParser<RawTokenizer> where RawTokenizer.RawToken == MyToken, RawTokenizer.Location == FileSourceLocation {
                public override func skipChannelSkipTokens(_ except: Set<RawToken.TokenKind>) throws -> Void {
                    let skipKinds: Set<RawToken.TokenKind> = []

                    repeat {
                        let next: Token? = try tokenizer.peekToken()

                        guard
                            let kind = next?.rawToken.kind,
                            skipKinds.contains(kind)
                        else {
                            break
                        }

                        if except.contains(kind) {
                            break
                        }

                        _ = try tokenizer.next()
                    } while !tokenizer.isEOF
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_anyTokenAtom_returnsNextToken() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item(.atom(.anyToken("ANY"))),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | ANY
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let any = try self.nextToken() {
                        return any
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_anyReturn_returnsNode() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "Any", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a[Any]:
                ///     | '+'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Any? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect("+") {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_omitUnreachableRules_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                ]),
            ]).with(\.isReachable, value: false),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.omitUnreachable, value: true)
        )

        diffTest(expected: """
            // TestParser
            extension TestParser {
            }

            """).diff(result)
    }

    @Test
    func generateParser_omitUnreachableRules_false() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                ]),
            ]).with(\.isReachable, value: false),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.omitUnreachable, value: false)
        )

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let b = try self.b() {
                        return b
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_emitTypesInBindings() throws {
        let grammar = try parseInternalGrammar("""
        start: a ;
        a: 'a' | 'b' c+ ;
        b[B]: 'aa' ;
        c: ','.d+ ;
        d[D]: 'a'? b? ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.emitTypesInBindings, value: true)
        )

        diffTest(expected: """
            extension Parser {
                /// ```
                /// start:
                ///     | a
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let a: Node = try self.a() {
                        return a
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// a:
                ///     | 'a'
                ///     | 'b' c+
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("a") {
                        return Node()
                    }

                    self.restore(_mark)

                    if
                        let _: Token = try self.expect("b"),
                        let c: [Node] = try self.repeatOneOrMore({
                            try self.c()
                        })
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// b[B]:
                ///     | 'aa'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> B? {
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("aa") {
                        return B()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// c:
                ///     | ','.d+
                ///     ;
                /// ```
                @memoized("c")
                @inlinable
                public func __c() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let d: [D] = try self.gather(separator: {
                            try self.expect(",")
                        }, item: {
                            try self.d()
                        })
                    {
                        return d
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// d[D]:
                ///     | 'a'? b?
                ///     ;
                /// ```
                @memoized("d")
                @inlinable
                public func __d() throws -> D? {
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("a"),
                        case let b: B? = try self.b()
                    {
                        return D()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_implicitBindings_false() throws {
        let grammar = try parseInternalGrammar("""
        @implicitBindings "false" ;

        start: a ;
        a: a='a' | 'b' c+ ;
        b[B]: 'aa' ;
        c: d=','.d+ ;
        d[D]: 'a'? b? ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser(
            settings: .default.with(\.emitTypesInBindings, value: true)
        )

        diffTest(expected: """
            extension Parser {
                /// ```
                /// start:
                ///     | a
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let _: Node = try self.a() {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// a:
                ///     | a='a'
                ///     | 'b' c+
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let a: Token = try self.expect("a") {
                        return a
                    }

                    self.restore(_mark)

                    if
                        let _: Token = try self.expect("b"),
                        let _: [Node] = try self.repeatOneOrMore({
                            try self.c()
                        })
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// b[B]:
                ///     | 'aa'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> B? {
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("aa") {
                        return B()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// c:
                ///     | d=','.d+
                ///     ;
                /// ```
                @memoized("c")
                @inlinable
                public func __c() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let d: [D] = try self.gather(separator: {
                            try self.expect(",")
                        }, item: {
                            try self.d()
                        })
                    {
                        return d
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// d[D]:
                ///     | 'a'? b?
                ///     ;
                /// ```
                @memoized("d")
                @inlinable
                public func __d() throws -> D? {
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("a"),
                        case _: B? = try self.b()
                    {
                        return D()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_implicitBindings_true_bindTokenLiterals_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ]),
            ]),
        ], metas: [
            .init(name: "implicitBindings", value: .identifier("true")),
            .init(name: "bindTokenLiterals", value: .identifier("true")),
        ])
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | '+'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let ADD = try self.expect(.ADD) {
                        return ADD
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_implicitBindings_false_bindTokenLiterals_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ]),
            ]),
        ], metas: [
            .init(name: "implicitBindings", value: .identifier("false")),
            .init(name: "bindTokenLiterals", value: .identifier("true")),
        ])
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | '+'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect(.ADD) {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_omitRedundantMarkRestores_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ]),
            ]),
            .init(name: "b", alts: [
                .init(namedItems: [
                    .item("'*'"),
                    .item("a"),
                ]),
                .init(namedItems: [
                    .item("'-'"),
                ]),
            ]),
        ])
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
            makeTokenDef(name: "MUL", literal: "*"),
            makeTokenDef(name: "SUB", literal: "-"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser(
            settings: .default.with(\.omitRedundantMarkRestores, value: true)
        )

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | '+'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    if let _ = try self.expect(.ADD) {
                        return Node()
                    }

                    return nil
                }

                /// ```
                /// b:
                ///     | '*' a
                ///     | '-'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let _ = try self.expect(.MUL),
                        let a = try self.a()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    if let _ = try self.expect(.SUB) {
                        return Node()
                    }

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_emitProducerProtocol_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ]),
            ]),
            .init(name: "b", alts: [
                .init(
                    namedItems: [
                        .item("MUL"),
                        .item("a"),
                    ],
                    action: "action1()"
                ),
                .init(namedItems: [
                    .item("'-'"),
                ]),
            ]),
            .init(name: "aAndB", alts: [
                .init(namedItems: [
                    .item("a"),
                    .item("b"),
                ]),
                .init(namedItems: [
                    .item("a"),
                    .item("b"),
                    .item("a"),
                ]),
            ]),
        ])
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
            makeTokenDef(name: "MUL", literal: "*"),
            makeTokenDef(name: "SUB", literal: "-"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser(
            settings: .default.with(\.emitProducerProtocol, value: true)
        )

        diffTest(expected: """
            // TestParser
            public protocol TestParserProducer {
                associatedtype Mark
                associatedtype Token
                associatedtype AProduction
                associatedtype BProduction
                associatedtype AAndBProduction

                func produce_a_alt1(_mark: Mark) throws -> AProduction
                func produce_b_alt1(_mark: Mark, mul: Token, a: AProduction) throws -> BProduction
                func produce_b_alt2(_mark: Mark) throws -> BProduction
                func produce_aAndB_alt1(_mark: Mark, a: AProduction, b: BProduction) throws -> AAndBProduction
                func produce_aAndB_alt2(_mark: Mark, a: AProduction, b: BProduction, a1: AProduction) throws -> AAndBProduction
            }
            
            public class DefaultTestParserProducer<RawTokenizer: RawTokenizerType>: TestParserProducer {
                public typealias Mark = Tokenizer<RawTokenizer>.Mark
                public typealias Token = Tokenizer<RawTokenizer>.Token
                public typealias AProduction = Node
                public typealias BProduction = Node
                public typealias AAndBProduction = Node

                @inlinable
                public func produce_a_alt1(_mark: Mark) throws -> AProduction {
                    return Node()
                }

                @inlinable
                public func produce_b_alt1(_mark: Mark, mul: Token, a: AProduction) throws -> BProduction {
                    return action1()
                }

                @inlinable
                public func produce_b_alt2(_mark: Mark) throws -> BProduction {
                    return Node()
                }

                @inlinable
                public func produce_aAndB_alt1(_mark: Mark, a: AProduction, b: BProduction) throws -> AAndBProduction {
                    return Node()
                }

                @inlinable
                public func produce_aAndB_alt2(_mark: Mark, a: AProduction, b: BProduction, a1: AProduction) throws -> AAndBProduction {
                    return Node()
                }
            }

            extension TestParser {
                /// ```
                /// a:
                ///     | '+'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Producer.AProduction? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect(.ADD) {
                        return try _producer.produce_a_alt1(_mark: _mark)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// b:
                ///     | MUL a {action1()}
                ///     | '-'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> Producer.BProduction? {
                    let _mark: Mark = self.mark()

                    if
                        let mul = try self.expect(.MUL),
                        let a = try self.a()
                    {
                        return try _producer.produce_b_alt1(_mark: _mark, mul: mul, a: a)
                    }

                    self.restore(_mark)

                    if let _ = try self.expect(.SUB) {
                        return try _producer.produce_b_alt2(_mark: _mark)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// aAndB:
                ///     | a b
                ///     | a b a
                ///     ;
                /// ```
                @memoized("aAndB")
                @inlinable
                public func __aAndB() throws -> Producer.AAndBProduction? {
                    let _mark: Mark = self.mark()

                    if
                        let a = try self.a(),
                        let b = try self.b()
                    {
                        return try _producer.produce_aAndB_alt1(_mark: _mark, a: a, b: b)
                    }

                    self.restore(_mark)

                    if
                        let a = try self.a(),
                        let b = try self.b(),
                        let a1 = try self.a()
                    {
                        return try _producer.produce_aAndB_alt2(_mark: _mark, a: a, b: b, a1: a1)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_emitProducerProtocol_true_escapesIdentifiers() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("inout"),
                ]),
                .init(namedItems: [
                    .item("in"),
                ]),
            ]),
        ])
        let tokens = [
            makeTokenDef(name: "inout", literal: "inout"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser(
            settings: .default.with(\.emitProducerProtocol, value: true)
        )

        diffTest(expected: """
            // TestParser
            public protocol TestParserProducer {
                associatedtype Mark
                associatedtype Token
                associatedtype AProduction

                func produce_a_alt1(_mark: Mark, `inout`: Node) throws -> AProduction
                func produce_a_alt2(_mark: Mark, `in`: Node) throws -> AProduction
            }

            public class DefaultTestParserProducer<RawTokenizer: RawTokenizerType>: TestParserProducer {
                public typealias Mark = Tokenizer<RawTokenizer>.Mark
                public typealias Token = Tokenizer<RawTokenizer>.Token
                public typealias AProduction = Node

                @inlinable
                public func produce_a_alt1(_mark: Mark, `inout`: Node) throws -> AProduction {
                    return `inout`
                }

                @inlinable
                public func produce_a_alt2(_mark: Mark, `in`: Node) throws -> AProduction {
                    return `in`
                }
            }

            extension TestParser {
                /// ```
                /// a:
                ///     | inout
                ///     | in
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Producer.AProduction? {
                    let _mark: Mark = self.mark()

                    if let `inout` = try self.`inout`() {
                        return try _producer.produce_a_alt1(_mark: _mark, `inout`: `inout`)
                    }

                    self.restore(_mark)

                    if let `in` = try self.`in`() {
                        return try _producer.produce_a_alt2(_mark: _mark, in: `in`)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_emitProducerProtocol_true_optionalGroup() throws {
        let grammar = try parseInternalGrammar(#"""
        @parserName TestParser ;
        @token ADD ; @token MUL ;
        a: (ADD+)? ;
        """#, entryRuleName: "a")
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser(
            settings: .default.with(\.emitProducerProtocol, value: true)
        )

        diffTest(expected: """
            public protocol TestParserProducer {
                associatedtype Mark
                associatedtype Token
                associatedtype AProduction

                func produce_a_alt1(_mark: Mark, add: [Token]?) throws -> AProduction
            }
            
            public class DefaultTestParserProducer<RawTokenizer: RawTokenizerType>: TestParserProducer {
                public typealias Mark = Tokenizer<RawTokenizer>.Mark
                public typealias Token = Tokenizer<RawTokenizer>.Token
                public typealias AProduction = Node

                @inlinable
                public func produce_a_alt1(_mark: Mark, add: [Token]?) throws -> AProduction {
                    return add
                }
            }

            extension TestParser {
                /// ```
                /// a:
                ///     | (ADD+)?
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Producer.AProduction? {
                    let _mark: Mark = self.mark()

                    if case let add = try self._a__group_() {
                        return try _producer.produce_a_alt1(_mark: _mark, add: add)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _a__group_[[Token]]:
                ///     | ADD+ { add }
                ///     ;
                /// ```
                @memoized("_a__group_")
                @inlinable
                public func ___a__group_() throws -> [Token]? {
                    let _mark: Mark = self.mark()

                    if
                        let add = try self.repeatOneOrMore({
                            try self.expect(.ADD)
                        })
                    {
                        return add
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_emitProducerProtocol_true_emitVoidProducer_true() throws {
        let grammar = try parseInternalGrammar(#"""
        @parserName TestParser ;
        @token ADD ; @token MUL ;
        a: (ADD+)? ;
        """#, entryRuleName: "a")
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser(
            settings: .default
                .with(\.emitProducerProtocol, value: true)
                .with(\.emitVoidProducer, value: true)
        )

        diffTest(expected: """
            public protocol TestParserProducer {
                associatedtype Mark
                associatedtype Token
                associatedtype AProduction

                func produce_a_alt1(_mark: Mark, add: [Token]?) throws -> AProduction
            }
            
            public class DefaultTestParserProducer<RawTokenizer: RawTokenizerType>: TestParserProducer {
                public typealias Mark = Tokenizer<RawTokenizer>.Mark
                public typealias Token = Tokenizer<RawTokenizer>.Token
                public typealias AProduction = Node

                @inlinable
                public func produce_a_alt1(_mark: Mark, add: [Token]?) throws -> AProduction {
                    return add
                }
            }
            
            public class VoidTestParserProducer<RawTokenizer: RawTokenizerType>: TestParserProducer {
                public typealias Mark = Tokenizer<RawTokenizer>.Mark
                public typealias Token = Tokenizer<RawTokenizer>.Token
                public typealias AProduction = Void

                @inlinable
                public func produce_a_alt1(_mark: Mark, add: [Token]?) throws -> AProduction {
                }
            }

            extension TestParser {
                /// ```
                /// a:
                ///     | (ADD+)?
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Producer.AProduction? {
                    let _mark: Mark = self.mark()

                    if case let add = try self._a__group_() {
                        return try _producer.produce_a_alt1(_mark: _mark, add: add)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _a__group_[[Token]]:
                ///     | ADD+ { add }
                ///     ;
                /// ```
                @memoized("_a__group_")
                @inlinable
                public func ___a__group_() throws -> [Token]? {
                    let _mark: Mark = self.mark()

                    if
                        let add = try self.repeatOneOrMore({
                            try self.expect(.ADD)
                        })
                    {
                        return add
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_respectsTokenCallMeta() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    "'+'",
                    .item("c"),
                ]),
            ]),
        ], metas: [
            .init(name: "tokenCall", value: .identifier("expectKind"))
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b '+' c
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect(kind: "+"),
                        let c = try self.c()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_escapesBackSlashLiterals() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    #"'\'"#,
                    .item("c"),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b '\' c
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect("\\"),
                        let c = try self.c()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_escapesInvalidSwiftIdentifiers() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("default"),
                    #"'\'"#,
                    .item("switch"),
                ]),
                .init(namedItems: [
                    .item("default"),
                ]),
                .init(namedItems: [
                    .item(.oneOrMore("default", repetitionMode: .minimal)),
                    #"'\'"#,
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | default '\' switch
                ///     | default
                ///     | default+< '\'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let `default` = try self.`default`(),
                        let _ = try self.expect("\\"),
                        let `switch` = try self.`switch`()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    if let `default` = try self.`default`() {
                        return `default`
                    }

                    self.restore(_mark)

                    if let `default` = try self._a_nsr() {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _a_nsr[[Node]]:
                ///     | default+< '\'
                ///     ;
                /// ```
                @memoized("_a_nsr")
                @inlinable
                public func ___a_nsr() throws -> [Node]? {
                    let _mark: Mark = self.mark()

                    var _current: [Node] = []

                    while let `default` = try self.`default`() {
                        _current.append(`default`)

                        let _mark1: Mark = self.mark()

                        if let _ = try self.expect("\\") {
                            return _current
                        }

                        self.restore(_mark1)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_usesTokensStaticToken() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    "'+'",
                    .item("c"),
                    #"'\'"#,
                ]),
            ]),
            .init(name: "b", alts: [
                .init(namedItems: [
                    .item("b"),
                    "ADD",
                    .item("c"),
                    "'*'",
                ]),
            ]),
        ])
        let tokens = makeTokenDefs([
            makeTokenDef(name: "ADD", tokenCodeReference: "custom: .add", literal: "+"),
            makeTokenDef(name: "BACKSLASH", tokenCodeReference: "custom: .back", literal: #"\"#),
        ])
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b '+' c '\'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect(custom: .add),
                        let c = try self.c(),
                        let _ = try self.expect(custom: .back)
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// b:
                ///     | b ADD c '*'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let add = try self.expect(custom: .add),
                        let c = try self.c(),
                        let _ = try self.expect("*")
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_useImplicitTokenNameForSyntaxTokens() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    "'+'",
                    .item("c"),
                    "BACKSLASH",
                ]),
            ]),
            .init(name: "b", alts: [
                .init(namedItems: [
                    .item("b"),
                    "ADD",
                    .item("c"),
                    "'*'",
                ]),
            ]),
        ], metas: [
            .init(name: "tokenCall", value: .identifier("expectKind"))
        ])
        let tokens = makeTokenDefs([
            makeTokenDef(name: "ADD", tokenSyntax: nil),
            makeTokenDef(name: "BACKSLASH", tokenSyntax: "\\"),
        ])
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b '+' c BACKSLASH
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect(kind: "+"),
                        let c = try self.c(),
                        let backslash = try self.expect(kind: .BACKSLASH)
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// b:
                ///     | b ADD c '*'
                ///     ;
                /// ```
                @memoized("b")
                @inlinable
                public func __b() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let add = try self.ADD(),
                        let c = try self.c(),
                        let _ = try self.expect(kind: "*")
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_respectsImplicitReturns_false() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "Any", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ], action: " anAction "),
            ]),
        ], metas: [
            .init(name: "implicitReturns", value: .string("false")),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a[Any]:
                ///     | '+' { anAction }
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Any? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect("+") {
                        anAction
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_respectsImplicitReturns_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "Any", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ], action: " anAction "),
            ]),
        ], metas: [
            .init(name: "implicitReturns", value: .string("true")),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a[Any]:
                ///     | '+' { anAction }
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Any? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect("+") {
                        return anAction
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_deduplicatesIdentifiers() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    .item(name: "_", "c"),
                    .item("b"),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b _=c b
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let b1 = try self.b()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_deduplication_runsPerAlt() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    .item(name: "_", "c"),
                    .item("b"),
                ]),
                .init(namedItems: [
                    .item("b"),
                    .item(name: "_", "c"),
                    .item("b"),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b _=c b
                ///     | b _=c b
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let b1 = try self.b()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let b1 = try self.b()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_wildcardName() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("b"),
                    .item(name: "_", "c"),
                    .item(name: "_", "d"),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b _=c _=d
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let _ = try self.d()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_tokenLiteral_expectKind_withStaticToken() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("ADD"),
                ]),
            ]),
        ], metas: [
            .init(name: "tokenCall", value: .identifier("expectKind")),
        ])
        let tokens = [
            makeTokenDef(name: "ADD", tokenCodeReference: ".add", literal: "+"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | ADD
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let add = try self.expect(kind: .add) {
                        return add
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_soleTokenRule_doesNotEmitTokenNameAsReturnAction() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item("'+'"),
                ]),
            ]),
        ])
        let tokens = [
            makeTokenDef(name: "ADD", literal: "+"),
        ]
        let sut = makeSut(grammar, tokens)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | '+'
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect(.ADD) {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_altReturnsSingleNamedItemIfNoActionSpecified() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "SomeType", alts: [
                .init(namedItems: ["b"])
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a[SomeType]:
                ///     | b
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> SomeType? {
                    let _mark: Mark = self.mark()

                    if let b = try self.b() {
                        return b
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_gather() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [.item(.gather(sep: "b", node: "c"))]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b.c+
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let c = try self.gather(separator: {
                            try self.b()
                        }, item: {
                            try self.c()
                        })
                    {
                        return c
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_altAction() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: ["b"], action: " CustomAction() ")
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b { CustomAction() }
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let b = try self.b() {
                        return CustomAction()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_altFailAction() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: ["b"], failAction: " CustomAction() "),
                .init(namedItems: ["c"], action: " CAction() ", failAction: " OtherCustomAction() "),
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b !!{ CustomAction() }
                ///     | c { CAction() } !!{ OtherCustomAction() }
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let b = try self.b() {
                        return b
                    }

                    self.restore(_mark)

                    CustomAction()

                    if let c = try self.c() {
                        return CAction()
                    }

                    self.restore(_mark)

                    OtherCustomAction()

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_expectForced() throws {
        let grammar = try parseInternalGrammar("""
        start:
            | &&('a' | 'b')
            ;
        """)
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            extension Parser {
                /// ```
                /// start:
                ///     | &&('a' | 'b')
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        try self.expectForced({
                            try self._start__group_()
                        }, "(\'a\' | \'b\')")
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _start__group_[Void]:
                ///     | 'a' { () }
                ///     | 'b' { () }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> Void? {
                    let _mark: Mark = self.mark()

                    if let _ = try self.expect("a") {
                        return ()
                    }

                    self.restore(_mark)

                    if let _ = try self.expect("b") {
                        return ()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_singleRule() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "ANode", alts: [
                ["b", "c", .item(.optional("d"))],
            ])
        ])
        let sut = makeSut(grammar)
        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a[ANode]:
                ///     | b c d?
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> ANode? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        case let d = try self.d()
                    {
                        return ANode()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_groupInAtom() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "ANode", alts: [
                ["b", "c", .item(.atom(.group([["d", "e"]])))],
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a[ANode]:
                ///     | b c (d e)
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> ANode? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        case let (d?, e?) = self.shuffleTuple(try self._a__group_())
                    {
                        return ANode()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _a__group_[(Node, Node)]:
                ///     | d e { (d: d, e: e) }
                ///     ;
                /// ```
                @memoized("_a__group_")
                @inlinable
                public func ___a__group_() throws -> (Node, Node)? {
                    let _mark: Mark = self.mark()

                    if
                        let d = try self.d(),
                        let e = try self.e()
                    {
                        return (d: d, e: e)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_groupInAtom_exposesBindings() throws {
        let grammar = try parseInternalGrammar("""
        @token d; @tokenCallKind "expectKind" ;
        start: a (b+ cBind=c 'd') ;
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
                ///     | a (b+ cBind=c 'd')
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, cBind?) = self.shuffleTuple(try self._start__group_())
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("a") {
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("b") {
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
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("c"),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _start__group_[([B], C)]:
                ///     | b+ cBind=c 'd' { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> ([B], C)? {
                    let _mark: Mark = self.mark()

                    if
                        let b: [B] = try self.repeatOneOrMore({
                            try self.b()
                        }),
                        let cBind: C = try self.c(),
                        let _: Token = try self.expect("d")
                    {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_optionalAtom_exposesNestedBindings() throws {
        let grammar = try parseInternalGrammar("""
        @token d; @tokenCallKind "expectKind" ;
        start: a (b+ cBind=c 'd')? ;
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
                ///     | a (b+ cBind=c 'd')?
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b, cBind) = self.shuffleTuple(try self._start__group_())
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("a") {
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("b") {
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
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("c"),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _start__group_[([B], C)]:
                ///     | b+ cBind=c 'd' { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> ([B], C)? {
                    let _mark: Mark = self.mark()

                    if
                        let b: [B] = try self.repeatOneOrMore({
                            try self.b()
                        }),
                        let cBind: C = try self.c(),
                        let _: Token = try self.expect("d")
                    {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_groupWithMultipleAlts_exposesCommonBindingsOnly() throws {
        let grammar = try parseInternalGrammar("""
        @token d; @token e; @tokenCallKind "expectKind" ;
        start: a (b+ cBind=c 'd' | b=e cBind=c) ;
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
                ///     | a (b+ cBind=c 'd' | b=e cBind=c)
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let a: A = try self.a(),
                        let cBind: C = try self._start__group_()
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("a") {
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("b") {
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
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("c"),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _start__group_[C]:
                ///     | b+ cBind=c 'd' { cBind }
                ///     | b=e cBind=c { cBind }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> C? {
                    let _mark: Mark = self.mark()

                    if
                        let b: [B] = try self.repeatOneOrMore({
                            try self.b()
                        }),
                        let cBind: C = try self.c(),
                        let _: Token = try self.expect("d")
                    {
                        return cBind
                    }

                    self.restore(_mark)

                    if
                        let b: Token = try self.e(),
                        let cBind: C = try self.c()
                    {
                        return cBind
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_repetitionWithMultipleBindings() throws {
        let grammar = try parseInternalGrammar("""
        @tokenCallKind "expectKind" ;
        start: a (b c)+ ;
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
                ///     | a (b c)+
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let a: A = try self.a(),
                        let _: [(b: B, c: C)] = try self.repeatOneOrMore({
                            try self._start__group_()
                        })
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("a") {
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("b") {
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
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("c"),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _start__group_[(B, C)]:
                ///     | b c { (b: b, c: c) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> (B, C)? {
                    let _mark: Mark = self.mark()

                    if
                        let b: B = try self.b(),
                        let c: C = try self.c()
                    {
                        return (b: b, c: c)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_deeplyNestedOptionalGroups() throws {
        let grammar = try parseInternalGrammar("""
        @token d; @token e; @tokenCallKind "expectKind" ;
        start: a (((b+ cBind=c 'd')?)?)? ;
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
                ///     | a (((b+ cBind=c 'd')?)?)?
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b, cBind) = self.shuffleTuple(try self._start__group_())
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("a") {
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
                    let _mark: Mark = self.mark()

                    if let _: Token = try self.expect("b") {
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
                    let _mark: Mark = self.mark()

                    if
                        case _: Token? = try self.expect("c"),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _start__group_[([B]??, C??)]:
                ///     | ((b+ cBind=c 'd')?)? { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> ([B]??, C??)? {
                    let _mark: Mark = self.mark()

                    if case let (b, cBind) = self.shuffleTuple(try self.__start__group___group_()) {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// __start__group___group_[([B]?, C?)]:
                ///     | (b+ cBind=c 'd')? { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("__start__group___group_")
                @inlinable
                public func ____start__group___group_() throws -> ([B]?, C?)? {
                    let _mark: Mark = self.mark()

                    if case let (b, cBind) = self.shuffleTuple(try self.___start__group___group___group_()) {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// ___start__group___group___group_[([B], C)]:
                ///     | b+ cBind=c 'd' { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("___start__group___group___group_")
                @inlinable
                public func _____start__group___group___group_() throws -> ([B], C)? {
                    let _mark: Mark = self.mark()

                    if
                        let b: [B] = try self.repeatOneOrMore({
                            try self.b()
                        }),
                        let cBind: C = try self.c(),
                        let _: Token = try self.expect("d")
                    {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_generateCut() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: ["b", .lookahead(.cut), "c"]),
                .init(namedItems: ["b", "d"]),
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b ~ c
                ///     | b d
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    var _cut: CutFlag = CutFlag()

                    if
                        let b = try self.b(),
                        _cut.toggleOn(),
                        let c = try self.c()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    if _cut.isOn {
                        return nil
                    }

                    if
                        let b = try self.b(),
                        let d = try self.d()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_leftRecursiveRule() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: ["b"])
            ]).with(\.isLeftRecursiveLeader, value: true)
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b
                ///     ;
                /// ```
                @memoizedLeftRecursive("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let b = try self.b() {
                        return b
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_nestedLeftRecursiveRule() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: ["b"])
            ]).with(\.isLeftRecursive, value: true).with(\.isLeftRecursiveLeader, value: true),
            .init(name: "b", alts: [
                .init(namedItems: ["a"])
            ]).with(\.isLeftRecursive, value: true).with(\.isLeftRecursiveLeader, value: false),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b
                ///     ;
                /// ```
                @memoizedLeftRecursive("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let b = try self.b() {
                        return b
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// b:
                ///     | a
                ///     ;
                /// ```
                @inlinable
                public func b() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if let a = try self.a() {
                        return a
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """).diff(result)
    }

    @Test
    func generateParser_optionalGroup() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item(.optionalItems([
                        .init(namedItems: [
                            .item("b"),
                            #"'\'"#,
                            .item("c"),
                        ]),
                    ])),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | [b '\' c]
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if case let (b, c) = self.shuffleTuple(try self._a__opt()) {
                        return Node()
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _a__opt[(Node, Node)]:
                ///     | b '\' c { (b: b, c: c) }
                ///     ;
                /// ```
                @memoized("_a__opt")
                @inlinable
                public func ___a__opt() throws -> (Node, Node)? {
                    let _mark: Mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect("\\"),
                        let c = try self.c()
                    {
                        return (b: b, c: c)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_optionalGroup_named() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(namedItems: [
                    .item(name: "name", .optionalItems([
                        .init(namedItems: [
                            "'+'",
                            .item(name: "nameInner", #"'\'"#),
                            "'-'",
                        ], action: " nameInner "),
                    ])),
                ]),
            ]),
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: #"""
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | name=['+' nameInner='\' '-' { nameInner }]
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let _mark: Mark = self.mark()

                    if case let name = try self._a__opt() {
                        return name
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// _a__opt[Token]:
                ///     | '+' nameInner='\' '-' { nameInner }
                ///     ;
                /// ```
                @memoized("_a__opt")
                @inlinable
                public func ___a__opt() throws -> Token? {
                    let _mark: Mark = self.mark()

                    if
                        let _ = try self.expect("+"),
                        let nameInner = try self.expect("\\"),
                        let _ = try self.expect("-")
                    {
                        return nameInner
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """#).diff(result)
    }

    @Test
    func generateParser_fullGrammar() throws {
        let parserHeader = #"""
        // HEADS UP! Automatically generated by SwiftCodeGenTests.swift
        @testable import SwiftPEG

        @usableFromInline
        enum TestGrammarAST {
            @usableFromInline
            indirect enum Expr: CustomStringConvertible {
                case add(Self, Term)
                case sub(Self, Term)
                case term(Term)

                @usableFromInline
                var description: String {
                    switch self {
                    case .add(let l, let r): return "\(l) + \(r)"
                    case .sub(let l, let r): return "\(l) - \(r)"
                    case .term(let term): return term.description
                    }
                }
            }

            @usableFromInline
            indirect enum Term: CustomStringConvertible {
                case mul(Self, Factor)
                case div(Self, Factor)
                case factor(Factor)

                @usableFromInline
                var description: String {
                    switch self {
                    case .mul(let l, let r): return "\(l) * \(r)"
                    case .div(let l, let r): return "\(l) / \(r)"
                    case .factor(let factor): return factor.description
                    }
                }
            }

            @usableFromInline
            indirect enum Factor: CustomStringConvertible {
                case expr(Expr)
                case atom(Atom)

                @usableFromInline
                var description: String {
                    switch self {
                    case .expr(let expr): return "(\(expr.description))"
                    case .atom(let atom): return atom.description
                    }
                }
            }

            @usableFromInline
            enum Atom: CustomStringConvertible {
                case name(Substring)
                case number(Double)

                @usableFromInline
                var description: String {
                    switch self {
                    case .name(let name): return String(name)
                    case .number(let number): return number.description
                    }
                }
            }

            @usableFromInline
            enum Token: RawTokenType, ExpressibleByStringLiteral {
                @usableFromInline
                typealias TokenKind = TestGrammarAST.TokenKind
                @usableFromInline
                typealias TokenString = Substring

                case whitespace(Substring)
                case name(Substring)
                case number(Double, Substring)
                case newline
                case add
                case sub
                case mul
                case div
                case lp
                case rp

                @inlinable
                var length: Int {
                    string.count
                }

                @inlinable
                var kind: TestGrammarAST.TokenKind {
                    switch self {
                    case .whitespace: return .whitespace
                    case .name: return .name
                    case .number: return .number
                    case .newline: return .newline
                    case .add: return .add
                    case .sub: return .sub
                    case .mul: return .mul
                    case .div: return .div
                    case .lp: return .lp
                    case .rp: return .rp
                    }
                }

                @inlinable
                var string: Substring {
                    switch self {
                    case .whitespace(let value): return value
                    case .name(let value): return value
                    case .number(_, let value): return value
                    case .newline: return "\n"
                    default:
                        return Substring(kind.description)
                    }
                }

                @inlinable
                init(stringLiteral value: String) {
                    if let value = Self.from(value[...]) {
                        self = value
                    } else {
                        fatalError("Invalid token literal \(value)")
                    }
                }

                @inlinable
                static func from(_ string: Substring) -> Self? {
                    guard let first = string.first else {
                        return nil
                    }

                    switch first {
                    case "+": return .add
                    case "-": return .sub
                    case "*": return .mul
                    case "/": return .div
                    case "(": return .lp
                    case ")": return .rp
                    case "\n": return .newline
                    case let c where c.isWhitespace:
                        if let match = Self._parseWhitespace(string) {
                            return .whitespace(match)
                        }
                        return nil
                    case "0"..."9":
                        if let number = Self._parseNumber(string), let double = Double(number) {
                            return .number(double, number)
                        }
                        return nil
                    default:
                        // Try name
                        if let ident = Self._parseName(string) {
                            return .name(ident)
                        }
                        return nil
                    }
                }

                @inlinable
                static func _parseWhitespace<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
                    var stream = StringStream(source: string)
                    guard !stream.isEof else { return nil }

                    switch stream.next() {
                    case let c where c.isWhitespace:
                        break
                    default:
                        return nil
                    }

                    loop:
                    while !stream.isEof {
                        switch stream.peek() {
                        case let c where c.isWhitespace:
                            stream.advance()
                        default:
                            break loop
                        }
                    }

                    return stream.substring
                }

                @inlinable
                static func _parseName<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
                    var stream = StringStream(source: string)
                    guard !stream.isEof else { return nil }

                    switch stream.next() {
                    case let c where c.isLetter:
                        break
                    case "_":
                        break
                    default:
                        return nil
                    }

                    loop:
                    while !stream.isEof {
                        switch stream.peek() {
                        case let c where c.isLetter:
                            stream.advance()
                        case let c where c.isWholeNumber:
                            stream.advance()
                        case "_":
                            stream.advance()
                        default:
                            break loop
                        }
                    }

                    return stream.substring
                }

                @inlinable
                static func _parseNumber<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
                    var stream = StringStream(source: string)
                    guard !stream.isEof else { return nil }

                    switch stream.next() {
                    case "0"..."9":
                        break
                    default:
                        return nil
                    }

                    loop:
                    while !stream.isEof {
                        switch stream.peek() {
                        case "0"..."9":
                            stream.advance()
                        default:
                            break loop
                        }
                    }

                    if !stream.isEof && stream.advanceIfNext(".") {
                        loop:
                        while !stream.isEof {
                            switch stream.peek() {
                            case "0"..."9":
                                stream.advance()
                            default:
                                break loop
                            }
                        }
                    }

                    return stream.substring
                }

                @inlinable
                static func produceDummy(_ kind: TestGrammarAST.TokenKind) -> TestGrammarAST.Token {
                    switch kind {
                    case .whitespace: return .whitespace(" ")
                    case .name: return .name("<dummy>")
                    case .number: return .number(0.0, "0.0")
                    case .newline: return .newline
                    case .add: return .add
                    case .sub: return .sub
                    case .mul: return .mul
                    case .div: return .div
                    case .lp: return .lp
                    case .rp: return .rp
                    }
                }
            }

            @usableFromInline
            enum TokenKind: String, TokenKindType {
                case whitespace = "<whitespace>"
                case name = "<name>"
                case number = "<number>"
                case newline = "<newline>"
                case add = "+"
                case sub = "-"
                case mul = "*"
                case div = "/"
                case lp = "("
                case rp = ")"

                @inlinable
                var description: String { self.rawValue }
            }
        }

        @usableFromInline
        final class TestGrammarRawTokenizer: RawTokenizerType {
            @usableFromInline
            typealias RawToken = TestGrammarAST.Token
            @usableFromInline
            typealias Location = FileSourceLocation

            @usableFromInline
            internal var _source: String
            @usableFromInline
            internal var _index: String.Index

            /// Internal source location tracker
            @usableFromInline
            internal var _location: FileSourceLocation

            @inlinable
            var isEOF: Bool {
                _index >= _source.endIndex
            }

            @inlinable
            init(source: String) {
                self._source = source
                _index = source.startIndex
                _location = FileSourceLocation(line: 1, column: 1)
            }

            @inlinable
            func next() throws -> (rawToken: RawToken, location: Location)? {
                skipToContent()

                guard _index < _source.endIndex else {
                    return nil
                }

                guard
                    let token = RawToken.from(_source[_index...]),
                    token.length > 0
                else {
                    throw Error.unknownToken(index: _index)
                }

                defer { advance(by: token.length) }

                return (token, _location)
            }

            @inlinable
            internal func skipToContent() {
                var lastIndex = _index
                repeat {
                    lastIndex = _index
                    skipComments()
                } while _index < _source.endIndex && lastIndex != _index
            }

            @inlinable
            internal func skipLine() {
                while _index < _source.endIndex && _source[_index] != "\n" {
                    advance(by: 1)
                }
                if _index < _source.endIndex {
                    advance(by: 1) // Skip linefeed character
                }
            }

            @inlinable
            internal func skipComments() {
                while _index < _source.endIndex && _source[_index] == "#" {
                    skipLine()
                }
            }

            @inlinable
            internal func advance(by count: Int) {
                for _ in 0..<count {
                    if _source[_index] == "\n" {
                        _location.column = 0
                        _location.line += 1
                    }

                    _index = _source.utf8.index(after: _index)
                    _location.column += 1
                }
            }

            @usableFromInline
            enum Error: TokenizerError {
                case unknownToken(index: String.Index)

                @usableFromInline
                var description: String {
                    switch self {
                    case .unknownToken(let index):
                        return "Unknown token @ \(index)"
                    }
                }
            }
        }

        final class TestGrammarParser<Raw: RawTokenizerType>: PEGParser<Raw> where Raw.Token == TestGrammarAST.Token {
            @inlinable
            func NAME() throws -> Substring? {
                if let token = try self.expect(kind: .name) {
                    return token.rawToken.string
                }
                return nil
            }

            @inlinable
            func NUMBER() throws -> Double? {
                if
                    let token = try self.expect(kind: .number),
                    case .number(let value, _) = token.rawToken
                {
                    return value
                }
                return nil
            }

            @inlinable
            func NEWLINE() throws -> Substring? {
                if let token = try self.expect(kind: .newline) {
                    return token.rawToken.string
                }
                return nil
            }
        }
        """#

        let grammar = try parseInternalGrammar(#"""
        @parserName "TestGrammarParser" ;
        @parserHeader """
        \#(StringEscaping.escape(parserHeader, terminator: "\"\"\""))
        """ ;
        @token NAME ;
        @token NUMBER ;
        @token NEWLINE ;

        start[TestGrammarAST.Expr]:
            | expr _=NEWLINE? { expr }
            ;

        expr[TestGrammarAST.Expr]:
            | expr '+' term { .add(expr, term) }
            | expr '-' term { .sub(expr, term) }
            | term { .term(term) }
            ;

        term[TestGrammarAST.Term]:
            | term '*' factor { .mul(term, factor) }
            | term '/' factor { .div(term, factor) }
            | factor { .factor(factor) }
            ;

        factor[TestGrammarAST.Factor]:
            | '(' expr ')' { .expr(expr) }
            | atom { .atom(atom) }
            ;

        atom[TestGrammarAST.Atom]:
            | NAME { .name(name) }
            | NUMBER { .number(number) }
            ;
        """#)
        let sut = makeSut(grammar)

        let result = try assertNoThrow({ try sut.generateParser() })

        diffTest(expected: """
            \(parserHeader)

            extension TestGrammarParser {
                /// ```
                /// start[TestGrammarAST.Expr]:
                ///     | expr _=NEWLINE? { expr }
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> TestGrammarAST.Expr? {
                    let _mark: Mark = self.mark()

                    if
                        let expr = try self.expr(),
                        case _ = try self.NEWLINE()
                    {
                        return expr
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// expr[TestGrammarAST.Expr]:
                ///     | expr '+' term { .add(expr, term) }
                ///     | expr '-' term { .sub(expr, term) }
                ///     | term { .term(term) }
                ///     ;
                /// ```
                @memoizedLeftRecursive("expr")
                @inlinable
                public func __expr() throws -> TestGrammarAST.Expr? {
                    let _mark: Mark = self.mark()

                    if
                        let expr = try self.expr(),
                        let _ = try self.expect("+"),
                        let term = try self.term()
                    {
                        return .add(expr, term)
                    }

                    self.restore(_mark)

                    if
                        let expr = try self.expr(),
                        let _ = try self.expect("-"),
                        let term = try self.term()
                    {
                        return .sub(expr, term)
                    }

                    self.restore(_mark)

                    if let term = try self.term() {
                        return .term(term)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// term[TestGrammarAST.Term]:
                ///     | term '*' factor { .mul(term, factor) }
                ///     | term '/' factor { .div(term, factor) }
                ///     | factor { .factor(factor) }
                ///     ;
                /// ```
                @memoizedLeftRecursive("term")
                @inlinable
                public func __term() throws -> TestGrammarAST.Term? {
                    let _mark: Mark = self.mark()

                    if
                        let term = try self.term(),
                        let _ = try self.expect("*"),
                        let factor = try self.factor()
                    {
                        return .mul(term, factor)
                    }

                    self.restore(_mark)

                    if
                        let term = try self.term(),
                        let _ = try self.expect("/"),
                        let factor = try self.factor()
                    {
                        return .div(term, factor)
                    }

                    self.restore(_mark)

                    if let factor = try self.factor() {
                        return .factor(factor)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// factor[TestGrammarAST.Factor]:
                ///     | '(' expr ')' { .expr(expr) }
                ///     | atom { .atom(atom) }
                ///     ;
                /// ```
                @memoized("factor")
                @inlinable
                public func __factor() throws -> TestGrammarAST.Factor? {
                    let _mark: Mark = self.mark()

                    if
                        let _ = try self.expect("("),
                        let expr = try self.expr(),
                        let _ = try self.expect(")")
                    {
                        return .expr(expr)
                    }

                    self.restore(_mark)

                    if let atom = try self.atom() {
                        return .atom(atom)
                    }

                    self.restore(_mark)

                    return nil
                }

                /// ```
                /// atom[TestGrammarAST.Atom]:
                ///     | NAME { .name(name) }
                ///     | NUMBER { .number(number) }
                ///     ;
                /// ```
                @memoized("atom")
                @inlinable
                public func __atom() throws -> TestGrammarAST.Atom? {
                    let _mark: Mark = self.mark()

                    if let name = try self.NAME() {
                        return .name(name)
                    }

                    self.restore(_mark)

                    if let number = try self.NUMBER() {
                        return .number(number)
                    }

                    self.restore(_mark)

                    return nil
                }
            }

            """, highlightLineInEditor: false).diff(result)
    }
}

// MARK: - Test internals

private func makeSut(_ grammar: InternalGrammar.Grammar, _ tokens: [InternalGrammar.TokenDefinition] = []) -> SwiftCodeGen {
    SwiftCodeGen(grammar: grammar, tokenDefinitions: tokens)
}

private func makeGrammar(
    parserName: String = "TestParser",
    parserHeader: String = "// TestParser",
    _ rules: [InternalGrammar.Rule],
    metas: [InternalGrammar.MetaProperty] = []
) -> InternalGrammar.Grammar {

    .init(
        metas: [
            .init(name: SwiftCodeGen.parserHeader, value: .string(parserHeader)),
            .init(name: SwiftCodeGen.parserName, value: .string(parserName)),
        ] + metas,
        rules: rules
    )
}

private func makeTokenDefs(
    _ tokens: [InternalGrammar.TokenDefinition]
) -> [InternalGrammar.TokenDefinition] {
    return tokens
}

private func makeTokenDef(
    name: String,
    isFragment: Bool = false,
    tokenCodeReference: String? = nil,
    literal: String
) -> InternalGrammar.TokenDefinition {

    .init(name: name, isFragment: isFragment, tokenCodeReference: tokenCodeReference, tokenSyntax: .init(stringLiteral: literal))
}

private func makeTokenDef(
    name: String,
    isFragment: Bool = false,
    tokenCodeReference: String? = nil,
    tokenSyntax: CommonAbstract.TokenSyntax?
) -> InternalGrammar.TokenDefinition {

    .init(name: name, isFragment: isFragment, tokenCodeReference: tokenCodeReference, tokenSyntax: tokenSyntax)
}

private func parseInternalGrammar(
    _ grammar: String,
    entryRuleName: String = "start",
    file: StaticString = #file,
    line: UInt = #line
) throws -> InternalGrammar.Grammar {

    let grammar = try parseGrammar(grammar, file: file, line: line)

    let processor = GrammarProcessor(delegate: nil)
    return try processor.process(grammar, entryRuleName: entryRuleName).grammar
}
