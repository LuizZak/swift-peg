extension SwiftPEGGrammar {
    /// A token in a grammar.
    public enum GrammarToken: TokenType, ExpressibleByStringLiteral, CustomStringConvertible, CustomDebugStringConvertible {
        public typealias TokenKind = GrammarTokenKind
        public typealias TokenString = Substring

        /// `*`
        ///
        /// Alias for `Self.star`
        public static let asterisk = Self.star

        /// `.`
        ///
        /// Alias for `Self.period`
        public static let dot = Self.period

        /// A set of whitespace or newlines, with no other non-whitespace character
        /// in between.
        case whitespace(Substring)

        /// A Swift-compatible identifier token.
        case identifier(Substring)

        /// A digit sequence.
        case digits(Substring)

        /// A string literal token.
        /// Includes the quotes.
        case string(Substring)

        /// `(`
        case leftParen
        /// `)`
        case rightParen

        /// `{`
        case leftBrace
        /// `}`
        case rightBrace

        /// `[`
        case leftSquare
        /// `]`
        case rightSquare

        /// `<`
        case leftAngle
        /// `>`
        case rightAngle

        /// `:`
        case colon
        /// `;`
        case semicolon
        /// `|`
        case bar

        /// `=`
        case equals
        /// `~`
        case tilde
        /// `*`
        case star
        /// `+`
        case plus
        /// `-`
        case minus

        /// `?`
        case questionMark
        /// `!`
        case exclamationMark
        /// `!!`
        case doubleExclamationMark
        /// `&`
        case ampersand
        /// `,`
        case comma
        /// `.`
        case period
        /// `...`
        case ellipsis
        /// `@`
        case at
        /// `$`
        case dollarSign

        /// `/`
        case forwardSlash
        /// `\`
        case backslash
        /// `'`
        case singleQuote
        /// `"`
        case doubleQuote
        /// `"""`
        case tripleQuote
        /// `
        case backtick

        @inlinable
        public var kind: GrammarTokenKind {
            switch self {
            case .whitespace: return .whitespace
            case .identifier: return .identifier
            case .digits: return .digits
            case .string: return .string
            case .leftParen: return .leftParen
            case .rightParen: return .rightParen
            case .leftBrace: return .leftBrace
            case .rightBrace: return .rightBrace
            case .leftSquare: return .leftSquare
            case .rightSquare: return .rightSquare
            case .leftAngle: return .leftAngle
            case .rightAngle: return .rightAngle
            case .colon: return .colon
            case .semicolon: return .semicolon
            case .bar: return .bar
            case .equals: return .equals
            case .tilde: return .tilde
            case .star: return .star
            case .plus: return .plus
            case .minus: return .minus
            case .questionMark: return .questionMark
            case .exclamationMark: return .exclamationMark
            case .doubleExclamationMark: return .doubleExclamationMark
            case .ampersand: return .ampersand
            case .comma: return .comma
            case .period: return .period
            case .ellipsis: return .ellipsis
            case .at: return .at
            case .dollarSign: return .dollarSign
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            case .singleQuote: return .singleQuote
            case .doubleQuote: return .doubleQuote
            case .tripleQuote: return .tripleQuote
            case .backtick: return .backtick
            }
        }

        @inlinable
        public var string: TokenString {
            switch self {
            case .whitespace(let value): return value
            case .identifier(let value): return value
            case .digits(let value): return value
            case .string(let value): return value
            case .leftParen: return "("
            case .rightParen: return ")"
            case .leftBrace: return "{"
            case .rightBrace: return "}"
            case .leftSquare: return "["
            case .rightSquare: return "]"
            case .leftAngle: return "<"
            case .rightAngle: return ">"
            case .colon: return ":"
            case .semicolon: return ";"
            case .bar: return "|"
            case .equals: return "="
            case .tilde: return "~"
            case .star: return "*"
            case .plus: return "+"
            case .minus: return "-"
            case .questionMark: return "?"
            case .exclamationMark: return "!"
            case .doubleExclamationMark: return "!!"
            case .ampersand: return "&"
            case .comma: return ","
            case .period: return "."
            case .ellipsis: return "..."
            case .at: return "@"
            case .dollarSign: return "$"
            case .forwardSlash: return "/"
            case .backslash: return "\\"
            case .singleQuote: return "'"
            case .doubleQuote: return "\""
            case .tripleQuote: return "\"\"\""
            case .backtick: return "`"
            }
        }

        /// Returns a version of `self.string` that is suitable to be used as
        /// a bare literal value. Removes quotes from string literals.
        @inlinable
        public var processedString: TokenString {
            switch self {
            case .whitespace(let value): return value
            case .identifier(let value): return value
            case .digits(let value): return value
            case .string(let value): return value
            case .leftParen: return "("
            case .rightParen: return ")"
            case .leftBrace: return "{"
            case .rightBrace: return "}"
            case .leftSquare: return "["
            case .rightSquare: return "]"
            case .leftAngle: return "<"
            case .rightAngle: return ">"
            case .colon: return ":"
            case .semicolon: return ";"
            case .bar: return "|"
            case .equals: return "="
            case .tilde: return "~"
            case .star: return "*"
            case .plus: return "+"
            case .minus: return "-"
            case .questionMark: return "?"
            case .exclamationMark: return "!"
            case .doubleExclamationMark: return "!!"
            case .ampersand: return "&"
            case .comma: return ","
            case .period: return "."
            case .ellipsis: return "..."
            case .at: return "@"
            case .dollarSign: return "$"
            case .forwardSlash: return "/"
            case .backslash: return "\\"
            case .singleQuote: return "'"
            case .doubleQuote: return "\""
            case .tripleQuote: return "\"\"\""
            case .backtick: return "`"
            }
        }

        /// Returns the UTF8 length of this token.
        @inlinable
        public var tokenUTF8Length: Int {
            string.utf8.count
        }

        @inlinable
        public var length: Int {
            switch self {
            case .whitespace(let value):
                return value.count

            case .identifier(let value):
                return value.count

            case .digits(let value):
                return value.count

            case .string(let value):
                return value.count

            case .ellipsis, .tripleQuote:
                return 3

            case .doubleExclamationMark:
                return 2

            case
                .leftParen, .rightParen, .leftBrace, .rightBrace, .leftSquare,
                .rightSquare, .leftAngle, .rightAngle,.colon, .semicolon, .bar,
                .equals, .tilde, .star, .plus, .minus, .questionMark, .exclamationMark,
                .ampersand, .comma, .period, .at, .dollarSign, .forwardSlash,
                .backslash, .singleQuote, .doubleQuote, .backtick:
                return 1
            }
        }

        /// Returns `self.processedString` for string interpolation usage.
        public var description: String {
            return String(processedString)
        }

        public var debugDescription: String {
            switch self {
            case .whitespace(let value): return ".whitespace(\(value.debugDescription))"
            case .identifier(let value): return ".identifier(\(value.debugDescription))"
            case .digits(let value): return ".digits(\(value.debugDescription))"
            case .string(let value): return ".string(\(value.debugDescription))"
            case .leftParen: return ".leftParen"
            case .rightParen: return ".rightParen"
            case .leftBrace: return ".leftBrace"
            case .rightBrace: return ".rightBrace"
            case .leftSquare: return ".leftSquare"
            case .rightSquare: return ".rightSquare"
            case .leftAngle: return ".leftAngle"
            case .rightAngle: return ".rightAngle"
            case .colon: return ".colon"
            case .semicolon: return ".semicolon"
            case .bar: return ".bar"
            case .equals: return ".equals"
            case .tilde: return ".tilde"
            case .star: return ".star"
            case .plus: return ".plus"
            case .minus: return ".minus"
            case .questionMark: return ".questionMark"
            case .exclamationMark: return ".exclamationMark"
            case .doubleExclamationMark: return ".doubleExclamationMark"
            case .ampersand: return ".ampersand"
            case .comma: return ".comma"
            case .period: return ".period"
            case .ellipsis: return ".ellipsis"
            case .at: return ".at"
            case .dollarSign: return ".dollarSign"
            case .forwardSlash: return ".forwardSlash"
            case .backslash: return ".backslash"
            case .singleQuote: return ".singleQuote"
            case .doubleQuote: return ".doubleQuote"
            case .tripleQuote: return ".tripleQuote"
            case .backtick: return ".backtick"
            }
        }

        /// Attempts to construct a token from a given string literal value.
        ///
        /// - Note: If the construction fails, an assertion is raised, and should
        /// only be used as a convenience within a parser.
        @inlinable
        public init(stringLiteral value: String) {
            var stream = StringStream(source: value)
            guard let token = try! Self.from(stream: &stream) else {
                fatalError("\(Self.self): Unknown token literal '\(value)'")
            }

            self = token
        }

        @inlinable
        public static func produceDummy(_ kind: TokenKind) -> Self {
            switch kind {
            case .whitespace: return .whitespace(" ")
            case .identifier: return .identifier("<dummy>")
            case .digits: return .digits("<dummy>")
            case .string: return .string("<dummy>")
            case .leftParen: return .leftParen
            case .rightParen: return .rightParen
            case .leftBrace: return .leftBrace
            case .rightBrace: return .rightBrace
            case .leftSquare: return .leftSquare
            case .rightSquare: return .rightSquare
            case .leftAngle: return .leftAngle
            case .rightAngle: return .rightAngle
            case .colon: return .colon
            case .semicolon: return .semicolon
            case .bar: return .bar
            case .equals: return .equals
            case .tilde: return .tilde
            case .star: return .star
            case .plus: return .plus
            case .minus: return .minus
            case .questionMark: return .questionMark
            case .exclamationMark: return .exclamationMark
            case .doubleExclamationMark: return .doubleExclamationMark
            case .ampersand: return .ampersand
            case .comma: return .comma
            case .period: return .period
            case .ellipsis: return .ellipsis
            case .at: return .at
            case .dollarSign: return .dollarSign
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            case .singleQuote: return .singleQuote
            case .doubleQuote: return .doubleQuote
            case .tripleQuote: return .tripleQuote
            case .backtick: return .backtick
            }
        }

        /// Returns a parsed token from the given string stream.
        /// If the token is not recognized, `nil` is returned, instead.
        @inlinable
        public static func from<S>(stream: inout StringStream<S>) throws -> Self? where S.SubSequence == Substring {
            if stream.isEof { return nil }

            switch stream.peek() {
            case "(": return .leftParen
            case ")": return .rightParen
            case "{": return .leftBrace
            case "}": return .rightBrace
            case "[": return .leftSquare
            case "]": return .rightSquare
            case "<": return .leftAngle
            case ">": return .rightAngle
            case ":": return .colon
            case ";": return .semicolon
            case "|": return .bar
            case "=": return .equals
            case "~": return .tilde
            case "*": return .star
            case "+": return .plus
            case "-": return .minus
            case "?": return .questionMark
            case "!":
                if stream.isNext("!!") {
                    return .doubleExclamationMark
                }

                return .exclamationMark
            case "&": return .ampersand
            case ",": return .comma
            case ".":
                if stream.isNext("...") {
                    return .ellipsis
                }
                return .period
            case "@": return .at
            case "$": return .dollarSign
            case "/": return .forwardSlash
            case "\\":
                return .backslash
            // String
            case "'", "\"":
                if consume_STRING(from: &stream) {
                    return .string(stream.substring)
                }

                return nil
            // Whitespace
            case let c where c.isWhitespace:
                if let match = Self._parseWhitespace(&stream) {
                    return .whitespace(match)
                }
                return nil
            // Digits
            case let c where c.isWholeNumber:
                if let digits = Self._parseDigits(&stream) {
                    return .digits(digits)
                }
                return nil
            // Identifier
            default:
                if let ident = Self._parseIdentifier(&stream) {
                    return .identifier(ident)
                }
                return nil
            }
        }

        @inlinable
        static func _parseWhitespace<S: StringProtocol>(_ stream: inout StringStream<S>) -> Substring? where S.SubSequence == Substring {
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
        static func _parseIdentifier<S: StringProtocol>(_ stream: inout StringStream<S>) -> Substring? where S.SubSequence == Substring {
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
                case let c where c.isLetter || c.isWholeNumber:
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
        static func _parseDigits<S: StringProtocol>(_ stream: inout StringStream<S>) -> Substring? where S.SubSequence == Substring {
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

            return stream.substring
        }

        /// ```
        /// STRING[".string"]:
        ///     | "\"\"\"" ("\\\"\"\"" | !"\"\"\"" .)* "\"\"\""
        ///     | "\"" ("\\\"" | !"\"" .)* "\""
        ///     | "'" ("\\'" | !"'" .)* "'"
        ///     ;
        /// ```
        @inlinable
        static func consume_STRING<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                guard stream.isNext("\"\"\"") else {
                    break alt
                }
                stream.advance(3)

                while !stream.isEof {
                    if stream.isNext("\\") {
                        stream.advance(2)
                    } else if stream.isNext("\\\"\"\"") {
                        stream.advance(4)
                    } else if !stream.isNext("\"\"\""), !stream.isEof {
                        stream.advance()
                    } else {
                        break
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

                while !stream.isEof {
                    if stream.isNext("\\") {
                        stream.advance(2)
                    } else if stream.isNext("\\\"") {
                        stream.advance(2)
                    } else if !stream.isNext("\""), !stream.isEof {
                        stream.advance()
                    } else {
                        break
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
                    break alt
                }
                stream.advance()

                while !stream.isEof {
                    if stream.isNext("\\") {
                        stream.advance(2)
                    } else if stream.isNext("\\'") {
                        stream.advance(2)
                    } else if !stream.isNext("'"), !stream.isEof {
                        stream.advance()
                    } else {
                        break
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

        @inlinable
        static func _stringSegment<S: StringProtocol>(_ stream: inout StringStream<S>) -> Substring? where S.SubSequence == Substring {
            guard !stream.isEof else { return nil }

            switch stream.peek() {
            case "\\", "'", "\"":
                return nil
            default:
                stream.advance()
            }

            loop:
            while !stream.isEof {
                switch stream.peek() {
                case "\\", "'", "\"":
                    break loop
                default:
                    stream.advance()
                }
            }

            return stream.substring
        }
    }

    /// Specifies kinds for grammar tokens.
    public enum GrammarTokenKind: String, TokenKindType, CaseIterable, ExpressibleByStringLiteral {
        /// `*`
        ///
        /// Alias for `Self.star`
        public static let asterisk = Self.star

        /// `.`
        ///
        /// Alias for `Self.period`
        public static let dot = Self.period

        /// Whitespace characters.
        case whitespace = "WHITESPACE"

        /// A Swift-compatible identifier token.
        case identifier = "IDENTIFIER"

        /// A digit sequence.
        case digits = "DIGITS"

        /// A string literal token.
        /// Includes the quotes.
        case string = "STRING"

        /// `(`
        case leftParen = "("
        /// `)`
        case rightParen = ")"

        /// `{`
        case leftBrace = "{"
        /// `}`
        case rightBrace = "}"

        /// `[`
        case leftSquare = "["
        /// `]`
        case rightSquare = "]"

        /// `<`
        case leftAngle = "<"
        /// `>`
        case rightAngle = ">"

        /// `:`
        case colon = ":"
        /// `;`
        case semicolon = ";"
        /// `|`
        case bar = "|"

        /// `=`
        case equals = "="
        /// `~`
        case tilde = "~"
        /// `*`
        case star = "*"
        /// `+`
        case plus = "+"
        /// `-`
        case minus = "-"

        /// `?`
        case questionMark = "?"
        /// `!`
        case exclamationMark = "!"
        /// `!!`
        case doubleExclamationMark = "!!"
        /// `&`
        case ampersand = "&"
        /// `,`
        case comma = ","
        /// `.`
        case period = "."
        /// `...`
        case ellipsis = "..."
        /// `@`
        case at = "@"
        /// `$`
        case dollarSign = "$"

        /// `/`
        case forwardSlash = "/"
        /// `\`
        case backslash = "\\"

        /// `'`
        case singleQuote = "'"
        /// `"`
        case doubleQuote = "\""
        /// `"""`
        case tripleQuote = "\"\"\""
        /// `
        case backtick = "`"

        @inlinable
        public var description: String {
            self.rawValue
        }

        @inlinable
        public init(stringLiteral: String) {
            guard let value = Self(rawValue: stringLiteral) else {
                fatalError("Unknown grammar token kind '\(stringLiteral)'")
            }

            self = value
        }
    }

    /// A generic error produced by a token during lexing of an input string.
    public enum LexingError: TokenizerError {
        case message(String, String.Index)
        case missingTerminator(terminator: String, start: String.Index)

        public var description: String {
            switch self {
            case .message(let message, let offset):
                return "Lexer error @ \(offset): \(message)"
            case .missingTerminator(let terminator, let start):
                return "Missing terminator '\(terminator)' to match @ \(start)"
            }
        }
    }
}
