extension SwiftPEGGrammar {
    /// A token in a grammar.
    public enum GrammarToken: TokenType, ExpressibleByStringLiteral {
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
        case string(StringLiteral)

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
        /// `@`
        case at
        /// `$`
        case dollarSign

        /// `/`
        case forwardSlash
        /// `\`
        case backslash

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
            case .at: return .at
            case .dollarSign: return .dollarSign
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            }
        }

        @inlinable
        public var string: TokenString {
            switch self {
            case .whitespace(let value): return value
            case .identifier(let value): return value
            case .digits(let value): return value
            case .string(let value): return value.quotedContents
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
            case .at: return "@"
            case .dollarSign: return "$"
            case .forwardSlash: return "/"
            case .backslash: return "\\"
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
            case .string(let value): return value.contents
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
            case .at: return "@"
            case .dollarSign: return "$"
            case .forwardSlash: return "/"
            case .backslash: return "\\"
            }
        }

        /// Returns the UTF8 length of this token.
        @inlinable
        public var tokenUTF8Length: Int {
            string.utf8.count
        }

        @inlinable
        public var length: Int {
            string.count
        }

        /// Attempts to construct a token from a given string literal value.
        ///
        /// - Note: If the construction fails, an assertion is raised, and should
        /// only be used as a convenience within a parser.
        @inlinable
        public init(stringLiteral value: String) {
            guard let token = Self.from(string: value[...]) else {
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
            case .string: return .string(.singleQuote("<dummy>"))
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
            case .at: return .at
            case .dollarSign: return .dollarSign
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            }
        }

        /// Returns a parsed token from the given substring.
        /// If the token is not recognized, `nil` is returned, instead.
        @inlinable
        public static func from(string: Substring) -> Self? {
            guard let first = string.first else {
                return nil
            }

            switch first {
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
                if string.dropFirst().first == "!" {
                    return .doubleExclamationMark
                }

                return .exclamationMark
            case "&": return .ampersand
            case ",": return .comma
            case ".": return .period
            case "@": return .at
            case "$": return .dollarSign
            case "/": return .forwardSlash
            case "\\": return .backslash
            case "'", "\"":
                if let string = StringLiteral.from(string: string) {
                    return .string(string)
                }
                return nil
            case let c where c.isWhitespace:
                if let match = Self._parseWhitespace(string) {
                    return .whitespace(match)
                }
                return nil
            case let c where c.isWholeNumber:
                if let digits = Self._parseDigits(string) {
                    return .digits(digits)
                }
                return nil
            default:
                // Try identifier
                if let ident = Self._parseIdentifier(string) {
                    return .identifier(ident)
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
        static func _parseIdentifier<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
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
        static func _parseDigits<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
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

            return stream.substring
        }

        /// Specifies a variant of a string literal.
        /// 
        /// Associated values represent the string's contents, not including the
        /// quotes.
        public enum StringLiteral: Hashable, CustomStringConvertible {
            /// `'<...>'`
            case singleQuote(Substring)
            
            /// `"<...>"`
            case doubleQuote(Substring)

            /// `"""<...>"""`
            /// Supports newlines within
            case tripleQuote(Substring)

            /// Returns contents of the string, without surrounding quotes.
            @inlinable
            public var contents: Substring {
                switch self {
                case .singleQuote(let string):
                    return string.dropFirst().dropLast()
                case .doubleQuote(let string):
                    return string.dropFirst().dropLast()
                case .tripleQuote(let string):
                    // Ignore first newline past triple quote
                    if string.hasPrefix("\"\"\"\n") {
                        return string.dropFirst(4).dropLast(3)
                    }
                    return string.dropFirst(3).dropLast(3)
                }
            }

            /// Returns the full representation of this literal, including quotes.
            @inlinable
            public var quotedContents: Substring {
                switch self {
                case .singleQuote(let string),
                    .doubleQuote(let string),
                    .tripleQuote(let string):
                    return string
                }
            }

            /// Returns the full representation of this literal, including quotes.
            @inlinable
            public var description: String {
                String(quotedContents)
            }

            /// Returns a parsed string literal from the given substring.
            /// If no string literal is recognized, `nil` is returned, instead.
            @inlinable
            public static func from(string: Substring) -> Self? {
                var stream = StringStream(source: string)
                guard !stream.isEof else { return nil }

                let terminator: String
                let multiline: Bool

                if stream.advanceIfNext("\"\"\"") {
                    terminator = "\"\"\""
                    multiline = true
                } else if stream.advanceIfNext("\"") {
                    terminator = "\""
                    multiline = false
                } else if stream.advanceIfNext("'") {
                    terminator = "'"
                    multiline = false
                } else {
                    return nil
                }

                var expectsEscapeSequence = false
                while !stream.isEof {
                    if expectsEscapeSequence {
                        // TODO: Handle escape sequences appropriately
                        stream.advance()
                        expectsEscapeSequence = false
                    } else {
                        if stream.advanceIfNext(terminator) {
                            if terminator == "\"\"\"" {
                                return .tripleQuote(stream.substring)
                            } else if terminator == "\"" {
                                return .doubleQuote(stream.substring)
                            } else if terminator == "'" {
                                return .singleQuote(stream.substring)
                            }
                        }

                        let next = stream.next()

                        // Newlines are only allowed in triple-quoted strings
                        if next == "\n" {
                            if !multiline {
                                return nil
                            }
                        }
                        // Check escape sequence
                        else if next == "\\" {
                            expectsEscapeSequence = true
                        }
                    }
                }

                // Missing terminator?
                return nil
            }
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
        /// `@`
        case at = "@"
        /// `$`
        case dollarSign = "$"

        /// `/`
        case forwardSlash = "/"
        /// `\`
        case backslash = "\\"

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
}
