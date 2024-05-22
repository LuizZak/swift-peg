import SwiftPEG

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
        case name(String)
        case number(Double)

        @usableFromInline
        var description: String {
            switch self {
            case .name(let name): return name
            case .number(let number): return number.description
            }
        }
    }

    @usableFromInline
    enum Token: TokenType, ExpressibleByStringLiteral {
        @usableFromInline
        static let whitespace_pattern = #/([^\S\n])+/#
        @usableFromInline
        static let name_pattern = #/[A-Za-z_][0-9A-Za-z_]*/#
        @usableFromInline
        static let number_pattern = #/[0-9]+(\.[0-9]+)?/#

        @usableFromInline
        typealias TokenKind = TestGrammarAST.TokenKind
        @usableFromInline
        typealias TokenString = String

        case whitespace(String)
        case name(String)
        case number(Double, String)
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
        var string: String {
            switch self {
            case .whitespace(let value): return value
            case .name(let value): return value
            case .number(_, let value): return "\(value)"
            case .newline: return "\n"
            default:
                return kind.description
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
            switch string.first {
            case "+": return .add
            case "-": return .sub
            case "*": return .mul
            case "/": return .div
            case "(": return .lp
            case ")": return .rp
            case "\n": return .newline
            default:
                if let match = try? whitespace_pattern.prefixMatch(in: string) {
                    return .whitespace(String(match.0))
                }
                if let match = try? name_pattern.prefixMatch(in: string) {
                    return .name(String(match.0))
                }
                if
                    let match = try? number_pattern.prefixMatch(in: string),
                    let double = Double(match.0)
                {
                    return .number(double, String(match.0))
                }
            }

            return nil
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
class TestGrammarRawTokenizer: RawTokenizerType {
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

class TestGrammarParser<Raw: RawTokenizerType>: PEGParser<Raw> where Raw.Token == TestGrammarAST.Token {
    @inlinable
    func NAME() throws -> String? {
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
    func NEWLINE() throws -> String? {
        if let token = try self.expect(kind: .newline) {
            return token.token.string
        }
        return nil
    }
}

extension TestGrammarParser {
    /// ```
    /// start[TestGrammarAST.Expr]:
    ///     | expr NEWLINE? { expr }
    ///     ;
    /// ```
    @memoized("start")
    @inlinable
    public func __start() throws -> TestGrammarAST.Expr? {
        let mark = self.mark()

        if
            let expr = try self.expr(),
            let newline = try self.optional({
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
