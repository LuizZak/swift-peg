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

    func testGenerateParser_omitUnreachableRules_true() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
                    .item("b"),
                ]),
            ]).with(\.isReachable, value: false),
        ], metas: [
            .init(name: "tokenCall", value: .identifier("expectKind"))
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
                .init(items: [
                    .item("b"),
                ]),
            ]).with(\.isReachable, value: false),
        ], metas: [
            .init(name: "tokenCall", value: .identifier("expectKind"))
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
                    let mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_respectsTokenCallMeta() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect(kind: "+"),
                        let c = try self.c()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_escapesBackSlashLiterals() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect("\\"),
                        let c = try self.c()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """#).diff(result)
    }

    func testGenerateParser_escapesInvalidSwiftIdentifiers() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
                    .item("default"),
                    #"'\'"#,
                    .item("switch"),
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
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let mark = self.mark()

                    if
                        let `default` = try self.`default`(),
                        let _ = try self.expect("\\"),
                        let `switch` = try self.`switch`()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """#).diff(result)
    }

    func testGenerateParser_usesTokenExpectArgs() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
                    .item("b"),
                    "'+'",
                    .item("c"),
                    #"'\'"#,
                ]),
            ]),
            .init(name: "b", alts: [
                .init(items: [
                    .item("b"),
                    "ADD",
                    .item("c"),
                    "'*'",
                ]),
            ]),
        ])
        let tokens = makeTokenDefs([
            makeTokenDef(name: "ADD", expectArgs: "custom: .add", literal: "+"),
            makeTokenDef(name: "BACKSLASH", expectArgs: "custom: .back", literal: #"\"#),
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.expect(custom: .add),
                        let c = try self.c(),
                        let _ = try self.expect(custom: .back)
                    {
                        return Node()
                    }

                    self.restore(mark)
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let add = try self.expect(custom: .add),
                        let c = try self.c(),
                        let _ = try self.expect("*")
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """#).diff(result)
    }

    func testGenerateParser_deduplicatesIdentifiers() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let b1 = try self.b()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_deduplication_runsPerAlt() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
                    .item("b"),
                    .item(name: "_", "c"),
                    .item("b"),
                ]),
                .init(items: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let b1 = try self.b()
                    {
                        return Node()
                    }

                    self.restore(mark)

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let b1 = try self.b()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_wildcardName() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let _ = try self.c(),
                        let _ = try self.d()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_altReturnsSingleNamedItemIfNoActionSpecified() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: "SomeType", alts: [
                .init(items: ["b"])
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
                    let mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_gather() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: [.item(.gather(sep: "b", node: "c"))]),
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
                    let mark = self.mark()

                    if
                        let c = try self.gather(separator: {
                            try self.b()
                        }, item: {
                            try self.c()
                        })
                    {
                        return c
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_altAction() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: ["b"], action: "CustomAction()")
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
                    let mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return CustomAction()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_singleRule() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: .init(name: "ANode"), alts: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        let d = try self.optional({
                            try self.d()
                        })
                    {
                        return ANode()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_groupInAtom() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: .init(name: "ANode"), alts: [
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
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        let _ = try self._a__group_()
                    {
                        return ANode()
                    }

                    self.restore(mark)
                    return nil
                }

                /// ```
                /// _a__group_:
                ///     | d e
                ///     ;
                /// ```
                @memoized("_a__group_")
                @inlinable
                public func ___a__group_() throws -> Node? {
                    let mark = self.mark()

                    if
                        let d = try self.d(),
                        let e = try self.e()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_generateCut() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: ["b", .lookahead(.cut), "c"], action: "CustomAction()")
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                /// ```
                /// a:
                ///     | b ~ c { CustomAction() }
                ///     ;
                /// ```
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let mark = self.mark()
                    var cut = CutFlag()

                    if
                        let b = try self.b(),
                        cut.toggleOn(),
                        let c = try self.c()
                    {
                        return CustomAction()
                    }

                    self.restore(mark)

                    if cut.isOn {
                        return nil
                    }
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_leftRecursiveRule() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: ["b"])
            ]).with(\.isRecursiveLeader, value: true)
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
                    let mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return b
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_fullGrammar() throws {
        let parserHeader = #"""
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
            enum Token: TokenType, ExpressibleByStringLiteral {
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
            typealias Token = TestGrammarAST.Token
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
            func next() throws -> (token: Token, location: Location)? {
                skipToContent()

                guard _index < _source.endIndex else {
                    return nil
                }

                guard
                    let token = Token.from(_source[_index...]),
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
                    return token.token.string
                }
                return nil
            }

            @inlinable
            func NUMBER() throws -> Double? {
                if
                    let token = try self.expect(kind: .number),
                    case .number(let value, _) = token.token
                {
                    return value
                }
                return nil
            }

            @inlinable
            func NEWLINE() throws -> Substring? {
                if let token = try self.expect(kind: .newline) {
                    return token.token.string
                }
                return nil
            }
        }
        """#

        let grammar = try parseGrammar(#"""
        @parserName "TestGrammarParser" ;
        @parserHeader """
        \#(parserHeader)
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
                    let mark = self.mark()

                    if
                        let expr = try self.expr(),
                        let _ = try self.optional({
                            try self.NEWLINE()
                        })
                    {
                        return expr
                    }

                    self.restore(mark)
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
                    let mark = self.mark()

                    if
                        let expr = try self.expr(),
                        let _ = try self.expect("+"),
                        let term = try self.term()
                    {
                        return .add(expr, term)
                    }

                    self.restore(mark)

                    if
                        let expr = try self.expr(),
                        let _ = try self.expect("-"),
                        let term = try self.term()
                    {
                        return .sub(expr, term)
                    }

                    self.restore(mark)

                    if
                        let term = try self.term()
                    {
                        return .term(term)
                    }

                    self.restore(mark)
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
                    let mark = self.mark()

                    if
                        let term = try self.term(),
                        let _ = try self.expect("*"),
                        let factor = try self.factor()
                    {
                        return .mul(term, factor)
                    }

                    self.restore(mark)

                    if
                        let term = try self.term(),
                        let _ = try self.expect("/"),
                        let factor = try self.factor()
                    {
                        return .div(term, factor)
                    }

                    self.restore(mark)

                    if
                        let factor = try self.factor()
                    {
                        return .factor(factor)
                    }

                    self.restore(mark)
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
                    let mark = self.mark()

                    if
                        let _ = try self.expect("("),
                        let expr = try self.expr(),
                        let _ = try self.expect(")")
                    {
                        return .expr(expr)
                    }

                    self.restore(mark)

                    if
                        let atom = try self.atom()
                    {
                        return .atom(atom)
                    }

                    self.restore(mark)
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
                    let mark = self.mark()

                    if
                        let name = try self.NAME()
                    {
                        return .name(name)
                    }

                    self.restore(mark)

                    if
                        let number = try self.NUMBER()
                    {
                        return .number(number)
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
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
    expectArgs: String?,
    literal: String
) -> InternalGrammar.TokenDefinition {

    .init(name: name, expectArgs: expectArgs, string: literal)
}

private func parseGrammar(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> InternalGrammar.Grammar {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let grammar = try parser.start() else {
        throw parser.makeSyntaxError()
    }

    let processor = GrammarProcessor(delegate: nil)
    return try processor.process(grammar).grammar
}
