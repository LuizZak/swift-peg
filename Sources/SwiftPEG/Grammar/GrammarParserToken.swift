// HEADS UP! This is a generated file

public struct GrammarParserToken: RawTokenType, CustomStringConvertible {
    public var kind: TokenKind
    public var string: Substring

    @inlinable
    public var length: Int {
        string.count
    }

    @inlinable
    public var description: String {
        String(string)
    }

    @inlinable
    public init(kind: TokenKind, string: Substring) {
        self.kind = kind
        self.string = string
    }

    @inlinable
    public static func produceDummy(_ kind: TokenKind) -> Self {
        .init(kind: kind, string: "<dummy>")
    }

    @inlinable
    public static func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring {
        guard !stream.isEof else { return nil }
        stream.markSubstringStart()

        if consume_WHITESPACE(from: &stream) {
            return .init(kind: .whitespace, string: stream.substring)
        }
        if consume_AMPERSAND(from: &stream) {
            return .init(kind: .ampersand, string: stream.substring)
        }
        if consume_AT(from: &stream) {
            return .init(kind: .at, string: stream.substring)
        }
        if consume_BACKSLASH(from: &stream) {
            return .init(kind: .backslash, string: stream.substring)
        }
        if consume_BACKTICK(from: &stream) {
            return .init(kind: .backtick, string: stream.substring)
        }
        if consume_BAR(from: &stream) {
            return .init(kind: .bar, string: stream.substring)
        }
        if consume_COLON(from: &stream) {
            return .init(kind: .colon, string: stream.substring)
        }
        if consume_COMMA(from: &stream) {
            return .init(kind: .comma, string: stream.substring)
        }
        if consume_DOLLAR(from: &stream) {
            return .init(kind: .dollarSign, string: stream.substring)
        }
        if consume_DOUBLEEXCLAMATIONMARK(from: &stream) {
            return .init(kind: .doubleExclamationMark, string: stream.substring)
        }
        if consume_ELLIPSIS(from: &stream) {
            return .init(kind: .ellipsis, string: stream.substring)
        }
        if consume_EQUALS(from: &stream) {
            return .init(kind: .equals, string: stream.substring)
        }
        if consume_EXCLAMATIONMARK(from: &stream) {
            return .init(kind: .exclamationMark, string: stream.substring)
        }
        if consume_FORWARDSLASH(from: &stream) {
            return .init(kind: .forwardSlash, string: stream.substring)
        }
        if consume_LEFTANGLE(from: &stream) {
            return .init(kind: .leftAngle, string: stream.substring)
        }
        if consume_LEFTBRACE(from: &stream) {
            return .init(kind: .leftBrace, string: stream.substring)
        }
        if consume_LEFTPAREN(from: &stream) {
            return .init(kind: .leftParen, string: stream.substring)
        }
        if consume_LEFTSQUARE(from: &stream) {
            return .init(kind: .leftSquare, string: stream.substring)
        }
        if consume_MINUS(from: &stream) {
            return .init(kind: .minus, string: stream.substring)
        }
        if consume_PERCENT(from: &stream) {
            return .init(kind: .percent, string: stream.substring)
        }
        if consume_PERIOD(from: &stream) {
            return .init(kind: .period, string: stream.substring)
        }
        if consume_PLUS(from: &stream) {
            return .init(kind: .plus, string: stream.substring)
        }
        if consume_QUESTIONMARK(from: &stream) {
            return .init(kind: .questionMark, string: stream.substring)
        }
        if consume_RIGHTANGLE(from: &stream) {
            return .init(kind: .rightAngle, string: stream.substring)
        }
        if consume_RIGHTBRACE(from: &stream) {
            return .init(kind: .rightBrace, string: stream.substring)
        }
        if consume_RIGHTPAREN(from: &stream) {
            return .init(kind: .rightParen, string: stream.substring)
        }
        if consume_RIGHTSQUARE(from: &stream) {
            return .init(kind: .rightSquare, string: stream.substring)
        }
        if consume_SEMICOLON(from: &stream) {
            return .init(kind: .semicolon, string: stream.substring)
        }
        if consume_STAR(from: &stream) {
            return .init(kind: .star, string: stream.substring)
        }
        if consume_TILDE(from: &stream) {
            return .init(kind: .tilde, string: stream.substring)
        }
        if consume_DIGITS(from: &stream) {
            return .init(kind: .digits, string: stream.substring)
        }
        if consume_IDENTIFIER(from: &stream) {
            return .init(kind: .identifier, string: stream.substring)
        }
        if consume_STRING(from: &stream) {
            return .init(kind: .string, string: stream.substring)
        }

        return nil
    }

    public enum TokenKind: TokenKindType {
        /// `(" " | "\t" | "\n" | "\r")+`
        case whitespace

        /// `"&"`
        case ampersand

        /// `"@"`
        case at

        /// `"\\"`
        case backslash

        /// `"`"`
        case backtick

        /// `"|"`
        case bar

        /// `":"`
        case colon

        /// `","`
        case comma

        /// `"$"`
        case dollarSign

        /// `"!!"`
        case doubleExclamationMark

        /// `"..."`
        case ellipsis

        /// `"="`
        case equals

        /// `"!"`
        case exclamationMark

        /// `"/"`
        case forwardSlash

        /// `"<"`
        case leftAngle

        /// `"{"`
        case leftBrace

        /// `"("`
        case leftParen

        /// `"["`
        case leftSquare

        /// `"-"`
        case minus

        /// `"%"`
        case percent

        /// `"."`
        case period

        /// `"+"`
        case plus

        /// `"?"`
        case questionMark

        /// `">"`
        case rightAngle

        /// `"}"`
        case rightBrace

        /// `")"`
        case rightParen

        /// `"]"`
        case rightSquare

        /// `";"`
        case semicolon

        /// `"*"`
        case star

        /// `"~"`
        case tilde

        /// `("0"..."9")+`
        case digits

        /// `identifierHead ("0"..."9" | "a"..."z" | "A"..."Z" | "_")*`
        case identifier

        /// ```
        /// STRING[".string"]:
        ///     | "\"\"\"" ("\\\"\"\"" | "\\\\" | "\\" | !"\"\"\"" .)* "\"\"\""
        ///     | "\"" ("\\\"" | "\\\\" | "\\" | !"\"" !"\n" .)* "\""
        ///     | "'" ("\\'" | "\\\\" | "\\" | !"'" !"\n" .)* "'"
        ///     ;
        /// ```
        case string

        @inlinable
        public var description: String {
            switch self {
            case .whitespace: "WHITESPACE"
            case .ampersand: "&"
            case .at: "@"
            case .backslash: "\\"
            case .backtick: "`"
            case .bar: "|"
            case .colon: ":"
            case .comma: ","
            case .dollarSign: "$"
            case .doubleExclamationMark: "!!"
            case .ellipsis: "..."
            case .equals: "="
            case .exclamationMark: "!"
            case .forwardSlash: "/"
            case .leftAngle: "<"
            case .leftBrace: "{"
            case .leftParen: "("
            case .leftSquare: "["
            case .minus: "-"
            case .percent: "%"
            case .period: "."
            case .plus: "+"
            case .questionMark: "?"
            case .rightAngle: ">"
            case .rightBrace: "}"
            case .rightParen: ")"
            case .rightSquare: "]"
            case .semicolon: ";"
            case .star: "*"
            case .tilde: "~"
            case .digits: "DIGITS"
            case .identifier: "IDENTIFIER"
            case .string: "STRING"
            }
        }
    }

    /// ```
    /// WHITESPACE[".whitespace"]:
    ///     | (" " | "\t" | "\n" | "\r")+
    ///     ;
    /// ```
    @inlinable
    public static func consume_WHITESPACE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        guard !stream.isEof else { return false }

        alt:
        do {
            switch stream.peek() {
            case " ", "\t", "\n", "\r":
                stream.advance()
            default:
                return false
            }

            loop:
            while !stream.isEof {
                switch stream.peek() {
                case " ", "\t", "\n", "\r":
                    stream.advance()
                default:
                    break loop
                }
            }

            return true
        }
    }

    /// ```
    /// AMPERSAND[".ampersand"]:
    ///     | "&"
    ///     ;
    /// ```
    @inlinable
    public static func consume_AMPERSAND<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("&")
    }

    /// ```
    /// AT[".at"]:
    ///     | "@"
    ///     ;
    /// ```
    @inlinable
    public static func consume_AT<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("@")
    }

    /// ```
    /// BACKSLASH[".backslash"]:
    ///     | "\\"
    ///     ;
    /// ```
    @inlinable
    public static func consume_BACKSLASH<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("\\")
    }

    /// ```
    /// BACKTICK[".backtick"]:
    ///     | "`"
    ///     ;
    /// ```
    @inlinable
    public static func consume_BACKTICK<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("`")
    }

    /// ```
    /// BAR[".bar"]:
    ///     | "|"
    ///     ;
    /// ```
    @inlinable
    public static func consume_BAR<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("|")
    }

    /// ```
    /// COLON[".colon"]:
    ///     | ":"
    ///     ;
    /// ```
    @inlinable
    public static func consume_COLON<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext(":")
    }

    /// ```
    /// COMMA[".comma"]:
    ///     | ","
    ///     ;
    /// ```
    @inlinable
    public static func consume_COMMA<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext(",")
    }

    /// ```
    /// DOLLAR[".dollarSign"]:
    ///     | "$"
    ///     ;
    /// ```
    @inlinable
    public static func consume_DOLLAR<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("$")
    }

    /// ```
    /// DOUBLEEXCLAMATIONMARK[".doubleExclamationMark"]:
    ///     | "!!"
    ///     ;
    /// ```
    @inlinable
    public static func consume_DOUBLEEXCLAMATIONMARK<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("!!")
    }

    /// ```
    /// ELLIPSIS[".ellipsis"]:
    ///     | "..."
    ///     ;
    /// ```
    @inlinable
    public static func consume_ELLIPSIS<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("...")
    }

    /// ```
    /// EQUALS[".equals"]:
    ///     | "="
    ///     ;
    /// ```
    @inlinable
    public static func consume_EQUALS<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("=")
    }

    /// ```
    /// EXCLAMATIONMARK[".exclamationMark"]:
    ///     | "!"
    ///     ;
    /// ```
    @inlinable
    public static func consume_EXCLAMATIONMARK<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("!")
    }

    /// ```
    /// FORWARDSLASH[".forwardSlash"]:
    ///     | "/"
    ///     ;
    /// ```
    @inlinable
    public static func consume_FORWARDSLASH<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("/")
    }

    /// ```
    /// LEFTANGLE[".leftAngle"]:
    ///     | "<"
    ///     ;
    /// ```
    @inlinable
    public static func consume_LEFTANGLE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("<")
    }

    /// ```
    /// LEFTBRACE[".leftBrace"]:
    ///     | "{"
    ///     ;
    /// ```
    @inlinable
    public static func consume_LEFTBRACE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("{")
    }

    /// ```
    /// LEFTPAREN[".leftParen"]:
    ///     | "("
    ///     ;
    /// ```
    @inlinable
    public static func consume_LEFTPAREN<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("(")
    }

    /// ```
    /// LEFTSQUARE[".leftSquare"]:
    ///     | "["
    ///     ;
    /// ```
    @inlinable
    public static func consume_LEFTSQUARE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("[")
    }

    /// ```
    /// MINUS[".minus"]:
    ///     | "-"
    ///     ;
    /// ```
    @inlinable
    public static func consume_MINUS<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("-")
    }

    /// ```
    /// PERCENT[".percent"]:
    ///     | "%"
    ///     ;
    /// ```
    @inlinable
    public static func consume_PERCENT<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("%")
    }

    /// ```
    /// PERIOD[".period"]:
    ///     | "."
    ///     ;
    /// ```
    @inlinable
    public static func consume_PERIOD<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext(".")
    }

    /// ```
    /// PLUS[".plus"]:
    ///     | "+"
    ///     ;
    /// ```
    @inlinable
    public static func consume_PLUS<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("+")
    }

    /// ```
    /// QUESTIONMARK[".questionMark"]:
    ///     | "?"
    ///     ;
    /// ```
    @inlinable
    public static func consume_QUESTIONMARK<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("?")
    }

    /// ```
    /// RIGHTANGLE[".rightAngle"]:
    ///     | ">"
    ///     ;
    /// ```
    @inlinable
    public static func consume_RIGHTANGLE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext(">")
    }

    /// ```
    /// RIGHTBRACE[".rightBrace"]:
    ///     | "}"
    ///     ;
    /// ```
    @inlinable
    public static func consume_RIGHTBRACE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("}")
    }

    /// ```
    /// RIGHTPAREN[".rightParen"]:
    ///     | ")"
    ///     ;
    /// ```
    @inlinable
    public static func consume_RIGHTPAREN<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext(")")
    }

    /// ```
    /// RIGHTSQUARE[".rightSquare"]:
    ///     | "]"
    ///     ;
    /// ```
    @inlinable
    public static func consume_RIGHTSQUARE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("]")
    }

    /// ```
    /// SEMICOLON[".semicolon"]:
    ///     | ";"
    ///     ;
    /// ```
    @inlinable
    public static func consume_SEMICOLON<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext(";")
    }

    /// ```
    /// STAR[".star"]:
    ///     | "*"
    ///     ;
    /// ```
    @inlinable
    public static func consume_STAR<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("*")
    }

    /// ```
    /// TILDE[".tilde"]:
    ///     | "~"
    ///     ;
    /// ```
    @inlinable
    public static func consume_TILDE<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        stream.advanceIfNext("~")
    }

    /// ```
    /// DIGITS[".digits"]:
    ///     | ("0"..."9")+
    ///     ;
    /// ```
    @inlinable
    public static func consume_DIGITS<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        guard !stream.isEof else { return false }

        alt:
        do {
            switch stream.peek() {
            case "0"..."9":
                stream.advance()
            default:
                return false
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

            return true
        }
    }

    /// ```
    /// IDENTIFIER[".identifier"]:
    ///     | identifierHead ("0"..."9" | "a"..."z" | "A"..."Z" | "_")*
    ///     ;
    /// ```
    @inlinable
    public static func consume_IDENTIFIER<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        guard !stream.isEof else { return false }

        alt:
        do {
            guard consume_identifierHead(from: &stream) else {
                return false
            }


            loop:
            while !stream.isEof {
                switch stream.peek() {
                case "0"..."9", "a"..."z", "A"..."Z", "_":
                    stream.advance()
                default:
                    break loop
                }
            }

            return true
        }
    }

    /// ```
    /// STRING[".string"]:
    ///     | "\"\"\"" ("\\\"\"\"" | "\\\\" | "\\" | !"\"\"\"" .)* "\"\"\""
    ///     | "\"" ("\\\"" | "\\\\" | "\\" | !"\"" !"\n" .)* "\""
    ///     | "'" ("\\'" | "\\\\" | "\\" | !"'" !"\n" .)* "'"
    ///     ;
    /// ```
    @inlinable
    public static func consume_STRING<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        guard !stream.isEof else { return false }
        let state = stream.save()

        alt:
        do {
            guard stream.isNext("\"\"\"") else {
                break alt
            }
            stream.advance(3)

            loop:
            while !stream.isEof {
                if stream.isNext("\\\"\"\"") {
                    stream.advance(4)
                } else if stream.isNext("\\\\") {
                    stream.advance(2)
                } else if stream.isNext("\\") {
                    stream.advance()
                } else if !stream.isNext("\"\"\""), !stream.isEof {
                    stream.advance()
                } else {
                    break loop
                }
            }

            guard stream.isNext("\"\"\"") else {
                break alt
            }
            stream.advance(3)

            return true
        }

        stream.restore(state)

        alt:
        do {
            guard stream.isNext("\"") else {
                break alt
            }
            stream.advance()

            loop:
            while !stream.isEof {
                if stream.isNext("\\\"") {
                    stream.advance(2)
                } else if stream.isNext("\\\\") {
                    stream.advance(2)
                } else if stream.isNext("\\") {
                    stream.advance()
                } else if !stream.isNext("\""), !stream.isNext("\n"), !stream.isEof {
                    stream.advance()
                } else {
                    break loop
                }
            }

            guard stream.isNext("\"") else {
                break alt
            }
            stream.advance()

            return true
        }

        stream.restore(state)

        alt:
        do {
            guard stream.isNext("'") else {
                return false
            }
            stream.advance()

            loop:
            while !stream.isEof {
                if stream.isNext("\\'") {
                    stream.advance(2)
                } else if stream.isNext("\\\\") {
                    stream.advance(2)
                } else if stream.isNext("\\") {
                    stream.advance()
                } else if !stream.isNext("'"), !stream.isNext("\n"), !stream.isEof {
                    stream.advance()
                } else {
                    break loop
                }
            }

            guard stream.isNext("'") else {
                break alt
            }
            stream.advance()

            return true
        }

        stream.restore(state)

        return false
    }

    /// ```
    /// identifierHead:
    ///     | "a"..."z"
    ///     | "A"..."Z"
    ///     | "_"
    ///     ;
    /// ```
    @inlinable
    public static func consume_identifierHead<StringType>(from stream: inout StringStream<StringType>) -> Bool {
        guard !stream.isEof else { return false }
        let state = stream.save()

        alt:
        do {
            guard !stream.isEof, stream.isNextInRange("a"..."z") else {
                break alt
            }
            stream.advance()

            return true
        }

        stream.restore(state)

        alt:
        do {
            guard !stream.isEof, stream.isNextInRange("A"..."Z") else {
                break alt
            }
            stream.advance()

            return true
        }

        stream.restore(state)

        alt:
        do {
            guard stream.isNext("_") else {
                return false
            }
            stream.advance()

            return true
        }
    }
}