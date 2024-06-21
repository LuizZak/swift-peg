extension SwiftPEGGrammar {
    /// A string that was parsed from a grammar file.
    public struct GrammarString: Hashable {
        internal var pieces: [Piece]
        internal var quote: Quote
        internal var location: any (Hashable & Comparable)

        public init(
            pieces: [Piece],
            quote: Self.Quote,
            location: any (Hashable & Comparable)
        ) {
            self.pieces = pieces
            self.quote = quote
            self.location = location
        }

        /// Returns the contents of this grammar string as a raw string, with all
        /// escape sequences expanded, and without quotes.
        public func rawContents() -> String {
            pieces.map(\.raw).joined()
        }

        /// Returns the contents of this grammar string as a Swift string literal.
        public func asStringLiteral() -> String {
            "\(quote.raw)\(pieces.map({$0.escaped(self.quote)}).joined())\(quote.raw)"
        }

        public func hash(into hasher: inout Hasher) {
            hasher.combine(quote)
            hasher.combine(pieces)
        }

        public static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.quote == rhs.quote && lhs.pieces == rhs.pieces
        }

        /// Attempts to construct a grammar string from a given string grammar
        /// token.
        ///
        /// Expects the token to contain the surrounding quotes.
        ///
        /// Throws errors if the token is not of 'string' token kind, if the
        /// terminators surrounding the string are not recognized, or if the
        /// string contains unrecognized escape sequences.
        public static func fromStringToken(
            _ token: SwiftPEGGrammar.Token,
            _ location: any (Hashable & Comparable)
        ) throws -> Self {

            guard token.kind == .string else {
                throw Error.unrecognizedStringToken
            }

            let section: Substring
            let quote: Quote

            if token.string.hasPrefix("\"\"\"") {
                // Triple quoted strings must ignore a leading newline immediately
                // after the opening quotes, if one is present
                if token.string.hasPrefix("\"\"\"\n") {
                    section = token.string.dropFirst(4).dropLast(3)
                } else {
                    section = token.string.dropFirst(3).dropLast(3)
                }

                quote = .tripleQuote
            } else if token.string.hasPrefix("\"") {
                section = token.string.dropFirst().dropLast()
                quote = .doubleQuote
            } else if token.string.hasPrefix("'") {
                section = token.string.dropFirst().dropLast()
                quote = .singleQuote
            } else {
                throw Error.unrecognizedTerminators(
                    String(token.string.prefix(1)), at: token.string.startIndex
                )
            }

            // Escape the string
            var pieces: [Piece] = []
            var leadLiteral = ""
            var stream = StringStream(source: section)
            var inEscapeSequence = false
            while !stream.isEof {
                if inEscapeSequence {
                    switch stream.peek() {
                    case "\\": pieces.append(.escapeSequence("\\"))
                    case "n": pieces.append(.escapeSequence("n"))
                    case "r": pieces.append(.escapeSequence("r"))
                    case "t": pieces.append(.escapeSequence("t"))
                    case "(": pieces.append(.literal(#"\("#))
                    case _ where stream.advanceIfNext(quote.raw):
                        pieces.append(.escapeSequence(quote.raw))
                        inEscapeSequence = false
                        continue
                    default:
                        throw Error.unknownEscapeSequence("\\\(stream.peek())", at: stream.index)
                    }

                    stream.advance()
                    inEscapeSequence = false
                } else {
                    if stream.advanceIfNext("\\") {
                        pieces.append(.literal(leadLiteral))
                        leadLiteral = ""

                        inEscapeSequence = true
                    } else {
                        leadLiteral.append(stream.next())
                    }
                }
            }

            if !leadLiteral.isEmpty {
                pieces.append(.literal(leadLiteral))
            }

            return .init(pieces: pieces, quote: quote, location: location)
        }

        public static func fromStringToken<Raw>(
            _ token: Tokenizer<Raw>.Token
        ) throws -> Self where Raw.RawToken == SwiftPEGGrammar.Token {

            try .fromStringToken(token.rawToken, token.location)
        }

        /// Describes the quotes surrounding a grammar string literal.
        public enum Quote: Hashable {
            case singleQuote
            case doubleQuote
            case tripleQuote

            var raw: String {
                switch self {
                case .singleQuote: "'"
                case .doubleQuote: "\""
                case .tripleQuote: "\"\"\""
                }
            }
        }

        /// A string segment piece.
        public enum Piece: Hashable {
            case literal(String)
            case escapeSequence(String)

            var raw: String {
                switch self {
                case .escapeSequence(let escape):
                    switch escape {
                    case "n": return "\n"
                    case "r": return "\r"
                    case "t": return "\t"
                    case "\"": return "\""
                    case "\"\"\"": return "\"\"\""
                    case "'": return "'"
                    case "\\": return "\\"
                    default: fatalError("Unknown escape sequence \(escape)")
                    }
                case .literal(let literal):
                    return literal
                }
            }

            func escaped(_ quote: GrammarString.Quote = .doubleQuote) -> String {
                switch self {
                case .escapeSequence(let escape):
                    return "\\\(escape)"

                case .literal(let literal):
                    return literal
                }
            }
        }

        /// An error that can be raised when parsing a token with
        /// `GrammarString.fromStringToken()`.
        public enum Error: Swift.Error, CustomStringConvertible {
            /// Reports an escape sequence that was not recognized.
            case unknownEscapeSequence(String, at: String.Index)

            /// A token that is not a string token was provided.
            case unrecognizedStringToken

            /// The terminators within the string where not recognized.
            case unrecognizedTerminators(String, at: String.Index)

            public var description: String {
                switch self {
                case .unknownEscapeSequence(let sequence, _):
                    return "Unknown escape sequence \(sequence)"
                case .unrecognizedStringToken:
                    return "Expected token with kind SwiftPEGGrammar.Token.TokenKind._string"
                case .unrecognizedTerminators(let message, _):
                    return "Unrecognized string terminators: \(message)"
                }
            }
        }
    }
}
