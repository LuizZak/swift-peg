import XCTest

@testable import SwiftPEG

class SwiftCodeGenTests: XCTestCase {
    func testGenerateParser_emptyGrammar() throws {
        let grammar = makeGrammar([])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
            }
            """).diff(result)
    }

    func testGenerateParser_anyTokenAtom_returnsNextToken() throws {
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
                    let _mark = self.mark()

                    if
                        let any = try self.nextToken()
                    {
                        return any
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_anyReturn_returnsNode() throws {
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
                    let _mark = self.mark()

                    if
                        let _ = try self.expect("+")
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_omitUnreachableRules_true() throws {
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

    func testGenerateParser_omitUnreachableRules_false() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_emitTypesInBindings() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

                    if
                        let a: Node = try self.a()
                    {
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
                    let _mark = self.mark()

                    if
                        let _: Token = try self.expect("a")
                    {
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
                    let _mark = self.mark()

                    if
                        let _: Token = try self.expect("aa")
                    {
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
                    let _mark = self.mark()

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
                    let _mark = self.mark()

                    if
                        let _: Token? = try self.optional({
                            try self.expect("a")
                        }),
                        let b: B? = try self.optional({
                            try self.b()
                        })
                    {
                        return D()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_implicitBindings_false() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

                    if
                        let _: Node = try self.a()
                    {
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
                    let _mark = self.mark()

                    if
                        let a: Token = try self.expect("a")
                    {
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
                    let _mark = self.mark()

                    if
                        let _: Token = try self.expect("aa")
                    {
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
                    let _mark = self.mark()

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
                    let _mark = self.mark()

                    if
                        let _: Token? = try self.optional({
                            try self.expect("a")
                        }),
                        let _: B? = try self.optional({
                            try self.b()
                        })
                    {
                        return D()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_implicitBindings_true_bindTokenLiterals_true() throws {
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
                    let _mark = self.mark()

                    if
                        let ADD = try self.expect(.ADD)
                    {
                        return ADD
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_implicitBindings_false_bindTokenLiterals_true() throws {
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
                    let _mark = self.mark()

                    if
                        let _ = try self.expect(.ADD)
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_respectsTokenCallMeta() throws {
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
                    let _mark = self.mark()

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

    func testGenerateParser_escapesBackSlashLiterals() throws {
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
                    let _mark = self.mark()

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

    func testGenerateParser_escapesInvalidSwiftIdentifiers() throws {
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
                    let _mark = self.mark()

                    if
                        let `default` = try self.`default`(),
                        let _ = try self.expect("\\"),
                        let `switch` = try self.`switch`()
                    {
                        return Node()
                    }

                    self.restore(_mark)

                    if
                        let `default` = try self.`default`()
                    {
                        return `default`
                    }

                    self.restore(_mark)

                    if
                        let `default` = try self._a_nsr()
                    {
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
                    let _mark = self.mark()

                    var _current: [Node] = []

                    while
                        let `default` = try self.`default`()
                    {
                        _current.append(`default`)
                        let _mark1 = self.mark()

                        if
                            let _ = try self.expect("\\")
                        {
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

    func testGenerateParser_usesTokensStaticToken() throws {
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
            makeTokenDef(name: "ADD", staticToken: "custom: .add", literal: "+"),
            makeTokenDef(name: "BACKSLASH", staticToken: "custom: .back", literal: #"\"#),
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
                    let _mark = self.mark()

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
                    let _mark = self.mark()

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

    func testGenerateParser_useImplicitTokenNameForSyntaxTokens() throws {
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
                    let _mark = self.mark()

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
                    let _mark = self.mark()

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

    func testGenerateParser_respectsImplicitReturns_false() throws {
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
                    let _mark = self.mark()

                    if
                        let _ = try self.expect("+")
                    {
                        anAction
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_respectsImplicitReturns_true() throws {
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
                    let _mark = self.mark()

                    if
                        let _ = try self.expect("+")
                    {
                        return anAction
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_deduplicatesIdentifiers() throws {
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
                    let _mark = self.mark()

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

    func testGenerateParser_deduplication_runsPerAlt() throws {
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
                    let _mark = self.mark()

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

    func testGenerateParser_wildcardName() throws {
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
                    let _mark = self.mark()

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

    func testGenerateParser_tokenLiteral_expectKind_withStaticToken() throws {
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
            makeTokenDef(name: "ADD", staticToken: ".add", literal: "+"),
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
                    let _mark = self.mark()

                    if
                        let add = try self.expect(kind: .add)
                    {
                        return add
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_soleTokenRule_doesNotEmitTokenNameAsReturnAction() throws {
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
                    let _mark = self.mark()

                    if
                        let _ = try self.expect(.ADD)
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_altReturnsSingleNamedItemIfNoActionSpecified() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_gather() throws {
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
                    let _mark = self.mark()

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

    func testGenerateParser_altAction() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return CustomAction()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_altFailAction() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(_mark)
                    CustomAction()

                    if
                        let c = try self.c()
                    {
                        return CAction()
                    }

                    self.restore(_mark)
                    OtherCustomAction()
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_expectForced() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

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
                /// _start__group_[Any]:
                ///     | 'a' { () }
                ///     | 'b' { () }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> Any? {
                    let _mark = self.mark()

                    if
                        let _ = try self.expect("a")
                    {
                        return ()
                    }

                    self.restore(_mark)

                    if
                        let _ = try self.expect("b")
                    {
                        return ()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """#).diff(result)
    }

    func testGenerateParser_singleRule() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        let d = try self.optional({
                            try self.d()
                        })
                    {
                        return ANode()
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_groupInAtom() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        case let (d?, e?) = try self._a__group_()
                    {
                        return ANode()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _a__group_[(d: Node, e: Node)]:
                ///     | d e { (d: d, e: e) }
                ///     ;
                /// ```
                @memoized("_a__group_")
                @inlinable
                public func ___a__group_() throws -> (d: Node?, e: Node?) {
                    let _mark = self.mark()

                    if
                        let d = try self.d(),
                        let e = try self.e()
                    {
                        return (d: d, e: e)
                    }

                    self.restore(_mark)
                    return (nil, nil)
                }
            }
            """).diff(result)
    }

    func testGenerateParser_groupInAtom_exposesBindings() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, cBind?) = try self._start__group_()
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
                        let _: Token = try self.expect("a")
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
                        let _: Token = try self.expect("b")
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
                        let _: Token? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start__group_[(b: [B], cBind: C)]:
                ///     | b+ cBind=c 'd' { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> (b: [B]?, cBind: C?) {
                    let _mark = self.mark()

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
                    return (nil, nil)
                }
            }
            """).diff(result)
    }

    func testGenerateParser_optionalAtom_exposesNestedBindings() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, cBind?) = try self.optional({
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
                    let _mark = self.mark()

                    if
                        let _: Token = try self.expect("a")
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
                        let _: Token = try self.expect("b")
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
                        let _: Token? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start__group_[(b: [B], cBind: C)]:
                ///     | b+ cBind=c 'd' { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> (b: [B]?, cBind: C?) {
                    let _mark = self.mark()

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
                    return (nil, nil)
                }
            }
            """).diff(result)
    }

    func testGenerateParser_groupWithMultipleAlts_exposesCommonBindingsOnly() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

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
                    let _mark = self.mark()

                    if
                        let _: Token = try self.expect("a")
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
                        let _: Token = try self.expect("b")
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
                        let _: Token? = try self.optional({
                            try self.expect("c")
                        }),
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
                    let _mark = self.mark()

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

    func testGenerateParser_deeplyNestedOptionalGroups() throws {
        let grammar = try parseGrammar("""
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
                    let _mark = self.mark()

                    if
                        let a: A = try self.a(),
                        case let (b?, cBind?) = try self.optional({
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
                    let _mark = self.mark()

                    if
                        let _: Token = try self.expect("a")
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
                        let _: Token = try self.expect("b")
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
                        let _: Token? = try self.optional({
                            try self.expect("c")
                        }),
                        let _: Token = try self.expect("e")
                    {
                        return C()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _start__group_[(b: [B], cBind: C)]:
                ///     | ((b+ cBind=c 'd')?)? { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("_start__group_")
                @inlinable
                public func ___start__group_() throws -> (b: [B]?, cBind: C?) {
                    let _mark = self.mark()

                    if
                        case let (b?, cBind?) = try self.optional({
                            try self.__start__group___group_()
                        })
                    {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)
                    return (nil, nil)
                }

                /// ```
                /// __start__group___group_[(b: [B], cBind: C)]:
                ///     | (b+ cBind=c 'd')? { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("__start__group___group_")
                @inlinable
                public func ____start__group___group_() throws -> (b: [B]?, cBind: C?) {
                    let _mark = self.mark()

                    if
                        case let (b?, cBind?) = try self.optional({
                            try self.___start__group___group___group_()
                        })
                    {
                        return (b: b, cBind: cBind)
                    }

                    self.restore(_mark)
                    return (nil, nil)
                }

                /// ```
                /// ___start__group___group___group_[(b: [B], cBind: C)]:
                ///     | b+ cBind=c 'd' { (b: b, cBind: cBind) }
                ///     ;
                /// ```
                @memoized("___start__group___group___group_")
                @inlinable
                public func _____start__group___group___group_() throws -> (b: [B]?, cBind: C?) {
                    let _mark = self.mark()

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
                    return (nil, nil)
                }
            }
            """).diff(result)
    }

    func testGenerateParser_generateCut() throws {
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
                    let _mark = self.mark()
                    var _cut = CutFlag()

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

    func testGenerateParser_leftRecursiveRule() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_nestedLeftRecursiveRule() throws {
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
                    let _mark = self.mark()

                    if
                        let b = try self.b()
                    {
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
                    let _mark = self.mark()

                    if
                        let a = try self.a()
                    {
                        return a
                    }

                    self.restore(_mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_optionalGroup() throws {
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
                    let _mark = self.mark()

                    if
                        case let (b?, c?) = try self.optional({
                            try self._a__opt()
                        })
                    {
                        return Node()
                    }

                    self.restore(_mark)
                    return nil
                }

                /// ```
                /// _a__opt[(b: Node, c: Node)]:
                ///     | b '\' c { (b: b, c: c) }
                ///     ;
                /// ```
                @memoized("_a__opt")
                @inlinable
                public func ___a__opt() throws -> (b: Node?, c: Node?) {
                    let _mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect("\\"),
                        let c = try self.c()
                    {
                        return (b: b, c: c)
                    }

                    self.restore(_mark)
                    return (nil, nil)
                }
            }
            """#).diff(result)
    }

    func testGenerateParser_optionalGroup_named() throws {
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
                    let _mark = self.mark()

                    if
                        let name = try self.optional({
                            try self._a__opt()
                        })
                    {
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
                    let _mark = self.mark()

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

    func testGenerateParser_fullGrammar() throws {
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
                    case let c where c.isWholeNumber:
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
                    case let c where c.isWholeNumber:
                        break
                    default:
                        return nil
                    }

                    loop:
                    while !stream.isEof {
                        switch stream.peek() {
                        case let c where c.isWholeNumber:
                            stream.advance()
                        default:
                            break loop
                        }
                    }

                    if !stream.isEof && stream.advanceIfNext(".") {
                        loop:
                        while !stream.isEof {
                            switch stream.peek() {
                            case let c where c.isWholeNumber:
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

        let grammar = try parseGrammar(#"""
        @parserName "TestGrammarParser" ;
        @parserHeader """
        \#(StringEscaping.escape(parserHeader, terminator: "\"\"\""))
        """ ;
        @token NAME ;
        @token NUMBER ;
        @token NEWLINE ;

        start[TestGrammarAST.Expression]:
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
                /// start[TestGrammarAST.Expression]:
                ///     | expr _=NEWLINE? { expr }
                ///     ;
                /// ```
                @memoized("start")
                @inlinable
                public func __start() throws -> TestGrammarAST.Expression? {
                    let _mark = self.mark()

                    if
                        let expr = try self.expr(),
                        let _ = try self.optional({
                            try self.NEWLINE()
                        })
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
                    let _mark = self.mark()

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

                    if
                        let term = try self.term()
                    {
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
                    let _mark = self.mark()

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

                    if
                        let factor = try self.factor()
                    {
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
                    let _mark = self.mark()

                    if
                        let _ = try self.expect("("),
                        let expr = try self.expr(),
                        let _ = try self.expect(")")
                    {
                        return .expr(expr)
                    }

                    self.restore(_mark)

                    if
                        let atom = try self.atom()
                    {
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
                    let _mark = self.mark()

                    if
                        let name = try self.NAME()
                    {
                        return .name(name)
                    }

                    self.restore(_mark)

                    if
                        let number = try self.NUMBER()
                    {
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
    staticToken: String? = nil,
    literal: String
) -> InternalGrammar.TokenDefinition {

    .init(name: name, isFragment: isFragment, staticToken: staticToken, tokenSyntax: .init(stringLiteral: literal))
}

private func makeTokenDef(
    name: String,
    isFragment: Bool = false,
    staticToken: String? = nil,
    tokenSyntax: CommonAbstract.TokenSyntax?
) -> InternalGrammar.TokenDefinition {

    .init(name: name, isFragment: isFragment, staticToken: staticToken, tokenSyntax: tokenSyntax)
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
