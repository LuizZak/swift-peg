extension SwiftPEGGrammar {
    /// Represents a raw sequence of tokens from a grammar file.
    public struct TokenSequence {
        internal var pieces: [Piece]

        init(pieces: [Piece]) {
            self.pieces = pieces
        }

        func raw() -> String {
            self.pieces.map(\.raw).joined()
        }

        //

        public func appending(_ segment: Self) -> Self {
            .init(pieces: pieces + segment.pieces)
        }

        public func appending(contentsOf segments: some Sequence<Self>) -> Self {
            .init(pieces: pieces + segments.flatMap(\.pieces))
        }

        public static func from(_ token: SwiftPEGGrammar.Token) -> Self {
            .init(pieces: [.token(String(token.string))])
        }

        public static func from(_ string: SwiftPEGGrammar.GrammarString) -> Self {
            .init(pieces: [.string(string)])
        }

        public static func from<L>(_ token: TokenNode<SwiftPEGGrammar.Token, L>) -> Self {
            .from(token.rawToken)
        }

        public static func from<Raw>(_ token: Tokenizer<Raw>.TokenResult) -> Self where Raw.RawToken == SwiftPEGGrammar.Token {
            .from(token.rawToken)
        }

        public static func from(_ segments: some Sequence<Self>) -> Self {
            .init(pieces: segments.flatMap(\.pieces))
        }

        public static func from(interpolated: some Sequence<Self>) -> Self {
            .init(pieces: interpolated.flatMap(\.pieces))
        }

        public static func + (lhs: Self, rhs: Self) -> Self {
            lhs.appending(rhs)
        }

        enum Piece {
            case token(String)
            case string(GrammarString)
            case literal(String)

            internal var raw: String {
                switch self {
                case .literal(let literal):
                    return literal
                case .string(let string):
                    return string.asStringLiteral()
                case .token(let token):
                    return token
                }
            }
        }
    }
}
