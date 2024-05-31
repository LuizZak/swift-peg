import XCTest

@testable import SwiftPEG

class SwiftCodeGen_TokenTests: XCTestCase {
    func testGenerateTokenParser_modifiers() throws {
        let tokens = try parseTokenDefinitions(#"""
        $leftSquare: '[' ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default, modifiers: ["private", "static"])

        diffTest(expected: #"""
        /// ```
        /// leftSquare:
        ///     | "["
        ///     ;
        /// ```
        private static func consume_leftSquare<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            stream.advanceIfNext("[")
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_singleCharacter() throws {
        let tokens = try parseTokenDefinitions(#"""
        $leftSquare: '[' ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// leftSquare:
        ///     | "["
        ///     ;
        /// ```
        func consume_leftSquare<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            stream.advanceIfNext("[")
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_anyPattern() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | '[' (']' | .)
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "[" ("]" | .)
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                guard stream.isNext("[") else {
                    break alt
                }
                stream.advance()

                switch stream.peek() {
                case "]":
                    stream.advance()
                default:
                    stream.advance()
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_identifierToken() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | 'a' (b | 'c')+
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "a" (b | "c")+
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                guard stream.isNext("a") else {
                    break alt
                }
                stream.advance()

                if consume_b(from: &stream) {
                } else if stream.isNext("c") {
                    stream.advance()
                } else {
                    break alt
                }

                loop:
                while !stream.isEof {
                    if consume_b(from: &stream) {
                    } else if stream.isNext("c") {
                        stream.advance()
                    } else {
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_identifierExclusion() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | 'a' (!b 'c' | 'd')+
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "a" (!b "c" | "d")+
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                guard stream.isNext("a") else {
                    break alt
                }
                stream.advance()

                if stream.negativeLookahead(consume_b(from:)), stream.isNext("c") {
                    stream.advance()
                } else if stream.isNext("d") {
                    stream.advance()
                } else {
                    break alt
                }

                loop:
                while !stream.isEof {
                    if stream.negativeLookahead(consume_b(from:)), stream.isNext("c") {
                        stream.advance()
                    } else if stream.isNext("d") {
                        stream.advance()
                    } else {
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_mergeAtoms() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | ('a'...'j' | '_' | 'j'...'p' | 'p' | 'p'...'z')
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | ("a"..."j" | "_" | "j"..."p" | "p" | "p"..."z")
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                switch stream.peek() {
                case "a"..."j", "_", "j"..."z":
                    stream.advance()
                default:
                    break alt
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_characterRanges() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | ('A'...'Z' | 'a'...'z' | '_') ('0'...'9' | 'A'...'Z' | 'a'...'z' | '_')*
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | ("A"..."Z" | "a"..."z" | "_") ("0"..."9" | "A"..."Z" | "a"..."z" | "_")*
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                switch stream.peek() {
                case "A"..."Z", "a"..."z", "_":
                    stream.advance()
                default:
                    break alt
                }

                loop:
                while !stream.isEof {
                    switch stream.peek() {
                    case "0"..."9", "A"..."Z", "a"..."z", "_":
                        stream.advance()
                    default:
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_characterActionOnly() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | c { c.isLetter || c == "_" } (c { c.isLetter || c.isWholeNumber || c == "_" })*
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | c { c.isLetter || c == "_" } (c { c.isLetter || c.isWholeNumber || c == "_" })*
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                guard let c = stream.safePeek(), c.isLetter || c == "_" else {
                    break alt
                }
                stream.advance()

                loop:
                while !stream.isEof {
                    switch stream.peek() {
                    case let c where c.isLetter || c.isWholeNumber || c == "_":
                        stream.advance()
                    default:
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_mixedLiteralAndCharacterActions() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | c { c.isLetter || c == "_" } ("_" | "A"..."Z" | c { c.isLetter || c.isWholeNumber || c == "_" } | "0"..."9")*
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | c { c.isLetter || c == "_" } ("_" | "A"..."Z" | c { c.isLetter || c.isWholeNumber || c == "_" } | "0"..."9")*
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else { return false }
            let state = stream.save()

            alt:
            do {
                guard let c = stream.safePeek(), c.isLetter || c == "_" else {
                    break alt
                }
                stream.advance()

                loop:
                while !stream.isEof {
                    switch stream.peek() {
                    case "_", "A"..."Z":
                        stream.advance()
                    case let c where c.isLetter || c.isWholeNumber || c == "_":
                        stream.advance()
                    case "0"..."9":
                        stream.advance()
                    default:
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenParser_stringSyntax() throws {
        let tokens = try parseTokenDefinitions(#"""
        $stringLiteral:
            # Triple quote + any character that is not an unescaped triple quote + Triple quote
            | '"""' (!'"""' . | '\\"""')* '"""'
            # Quote + any character that is not an unescaped quote or newline + quote
            | "'" (!"'" !"\n" . | "\\'")* "'"
            # Quote + any character that is not an unescaped quote or newline + quote
            | '"' (!'"' !'\n' . | '\\"')* '"'
            ;
        """#)
        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0], settings: .default)

        diffTest(expected: #"""
        /// ```
        /// stringLiteral:
        ///     | "\"\"\"" (!"\"\"\"" . | "\\\"\"\"")* "\"\"\""
        ///     | "'" (!"'" !"\n" . | "\\'")* "'"
        ///     | "\"" (!"\"" !"\n" . | "\\\"")* "\""
        ///     ;
        /// ```
        func consume_stringLiteral<StringType>(from stream: inout StringStream<StringType>) -> Bool {
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
                    if !stream.isNext("\"\"\""), !stream.isEof {
                        stream.advance()
                    } else if stream.isNext("\\\"\"\"") {
                        stream.advance(4)
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
                guard stream.isNext("'") else {
                    break alt
                }
                stream.advance()

                loop:
                while !stream.isEof {
                    if !stream.isNext("'"), !stream.isNext("\n"), !stream.isEof {
                        stream.advance()
                    } else if stream.isNext("\\'") {
                        stream.advance(2)
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

            alt:
            do {
                guard stream.isNext("\"") else {
                    break alt
                }
                stream.advance()

                loop:
                while !stream.isEof {
                    if !stream.isNext("\""), !stream.isNext("\n"), !stream.isEof {
                        stream.advance()
                    } else if stream.isNext("\\\"") {
                        stream.advance(2)
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

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
    }

    func testGenerateTokenType_usesStaticTokenDefinition() throws {
        let tokens = try parseTokenDefinitions(#"""
        $TOKEN[".tokenName"]: 'a' ;
        $TOKEN2["A.tokenName2"]: 'b';
        $TOKEN3["tokenName3"]: 'c';
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        struct ParserToken: TokenType, CustomStringConvertible {
            var kind: TokenKind
            var string: Substring

            var length: Int {
                string.count
            }

            var description: String {
                String(string)
            }

            static func produceDummy(_ kind: TokenKind) -> Self {
                .init(kind: kind, string: "<dummy>")
            }

            static func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring {
                guard !stream.isEof else { return nil }
                stream.markSubstringStart()

                if consume_TOKEN(from: &stream) {
                    return .init(kind: .tokenName, string: stream.substring)
                }
                if consume_TOKEN2(from: &stream) {
                    return .init(kind: .TOKEN2, string: stream.substring)
                }
                if consume_TOKEN3(from: &stream) {
                    return .init(kind: .tokenName3, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"a"`
                case tokenName

                /// `"b"`
                case TOKEN2

                /// `"c"`
                case tokenName3

                var description: String {
                    switch self {
                    case .tokenName: "a"
                    case .TOKEN2: "b"
                    case .tokenName3: "c"
                    }
                }
            }

            /// ```
            /// TOKEN[".tokenName"]:
            ///     | "a"
            ///     ;
            /// ```
            static func consume_TOKEN<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("a")
            }

            /// ```
            /// TOKEN2["A.tokenName2"]:
            ///     | "b"
            ///     ;
            /// ```
            static func consume_TOKEN2<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("b")
            }

            /// ```
            /// TOKEN3["tokenName3"]:
            ///     | "c"
            ///     ;
            /// ```
            static func consume_TOKEN3<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("c")
            }
        }
        """#).diff(result)
    }

    func testGenerateTokenType_emitInlinable() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        struct ParserToken: TokenType, CustomStringConvertible {
            var kind: TokenKind
            var string: Substring

            @inlinable
            var length: Int {
                string.count
            }

            @inlinable
            var description: String {
                String(string)
            }

            @inlinable
            static func produceDummy(_ kind: TokenKind) -> Self {
                .init(kind: kind, string: "<dummy>")
            }

            @inlinable
            static func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring {
                guard !stream.isEof else { return nil }
                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"abc"`
                case tok

                @inlinable
                var description: String {
                    switch self {
                    case .tok: "abc"
                    }
                }
            }

            /// ```
            /// tok:
            ///     | "abc"
            ///     ;
            /// ```
            @inlinable
            static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("abc")
            }
        }
        """#).diff(result)
    }

    func testGenerateTokenType_accessLevel() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: TokenType, CustomStringConvertible {
            public var kind: TokenKind
            public var string: Substring

            public var length: Int {
                string.count
            }

            public var description: String {
                String(string)
            }

            public init(kind: TokenKind, string: Substring) {
                self.kind = kind
                self.string = string
            }

            public static func produceDummy(_ kind: TokenKind) -> Self {
                .init(kind: kind, string: "<dummy>")
            }

            public static func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring {
                guard !stream.isEof else { return nil }
                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            public enum TokenKind: TokenKindType {
                /// `"abc"`
                case tok

                public var description: String {
                    switch self {
                    case .tok: "abc"
                    }
                }
            }

            /// ```
            /// tok:
            ///     | "abc"
            ///     ;
            /// ```
            public static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("abc")
            }
        }
        """#).diff(result)
    }

    func testGenerateTokenType_emitInlinable_accessLevel() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")
                .with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: TokenType, CustomStringConvertible {
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

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            public enum TokenKind: TokenKindType {
                /// `"abc"`
                case tok

                @inlinable
                public var description: String {
                    switch self {
                    case .tok: "abc"
                    }
                }
            }

            /// ```
            /// tok:
            ///     | "abc"
            ///     ;
            /// ```
            @inlinable
            public static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("abc")
            }
        }
        """#).diff(result)
    }

    func testGenerateTokenType() throws {
        let tokens = try parseTokenDefinitions(#"""
        $leftSquare: '[' ;
        $rightSquare: ']' ;
        $assign: '=' ;
        $logicalNot: '!' ;
        $identical: '===' ;
        $notIdentical: '!==' ;
        $equals: '==' ;
        $notEquals: '!=' ;
        $string: '"' (!'"' .)+ '"' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        struct ParserToken: TokenType, CustomStringConvertible {
            var kind: TokenKind
            var string: Substring

            var length: Int {
                string.count
            }

            var description: String {
                String(string)
            }

            static func produceDummy(_ kind: TokenKind) -> Self {
                .init(kind: kind, string: "<dummy>")
            }

            static func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring {
                guard !stream.isEof else { return nil }
                stream.markSubstringStart()

                if consume_leftSquare(from: &stream) {
                    return .init(kind: .leftSquare, string: stream.substring)
                }
                if consume_rightSquare(from: &stream) {
                    return .init(kind: .rightSquare, string: stream.substring)
                }
                if consume_identical(from: &stream) {
                    return .init(kind: .identical, string: stream.substring)
                }
                if consume_notIdentical(from: &stream) {
                    return .init(kind: .notIdentical, string: stream.substring)
                }
                if consume_equals(from: &stream) {
                    return .init(kind: .equals, string: stream.substring)
                }
                if consume_assign(from: &stream) {
                    return .init(kind: .assign, string: stream.substring)
                }
                if consume_notEquals(from: &stream) {
                    return .init(kind: .notEquals, string: stream.substring)
                }
                if consume_logicalNot(from: &stream) {
                    return .init(kind: .logicalNot, string: stream.substring)
                }
                if consume_string(from: &stream) {
                    return .init(kind: .string, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"["`
                case leftSquare

                /// `"]"`
                case rightSquare

                /// `"="`
                case assign

                /// `"!"`
                case logicalNot

                /// `"==="`
                case identical

                /// `"!=="`
                case notIdentical

                /// `"=="`
                case equals

                /// `"!="`
                case notEquals

                /// `"\"" (!"\"" .)+ "\""`
                case string

                var description: String {
                    switch self {
                    case .leftSquare: "["
                    case .rightSquare: "]"
                    case .assign: "="
                    case .logicalNot: "!"
                    case .identical: "==="
                    case .notIdentical: "!=="
                    case .equals: "=="
                    case .notEquals: "!="
                    case .string: "string"
                    }
                }
            }

            /// ```
            /// leftSquare:
            ///     | "["
            ///     ;
            /// ```
            static func consume_leftSquare<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("[")
            }

            /// ```
            /// rightSquare:
            ///     | "]"
            ///     ;
            /// ```
            static func consume_rightSquare<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("]")
            }

            /// ```
            /// assign:
            ///     | "="
            ///     ;
            /// ```
            static func consume_assign<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("=")
            }

            /// ```
            /// logicalNot:
            ///     | "!"
            ///     ;
            /// ```
            static func consume_logicalNot<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("!")
            }

            /// ```
            /// identical:
            ///     | "==="
            ///     ;
            /// ```
            static func consume_identical<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("===")
            }

            /// ```
            /// notIdentical:
            ///     | "!=="
            ///     ;
            /// ```
            static func consume_notIdentical<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("!==")
            }

            /// ```
            /// equals:
            ///     | "=="
            ///     ;
            /// ```
            static func consume_equals<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("==")
            }

            /// ```
            /// notEquals:
            ///     | "!="
            ///     ;
            /// ```
            static func consume_notEquals<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("!=")
            }

            /// ```
            /// string:
            ///     | "\"" (!"\"" .)+ "\""
            ///     ;
            /// ```
            static func consume_string<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else { return false }
                let state = stream.save()

                alt:
                do {
                    guard stream.isNext("\"") else {
                        break alt
                    }
                    stream.advance()

                    if !stream.isNext("\""), !stream.isEof {
                        stream.advance()
                    } else {
                        break alt
                    }

                    loop:
                    while !stream.isEof {
                        if !stream.isNext("\""), !stream.isEof {
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

                return false
            }
        }
        """#).diff(result)
    }
}

// MARK: - Test internals

private func makeSut(
    _ tokens: [InternalGrammar.TokenDefinition]
) -> SwiftCodeGen {

    SwiftCodeGen(grammar: .init(rules: []), tokenDefinitions: tokens)
}

private func parseTokenDefinitions(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> [InternalGrammar.TokenDefinition] {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let tokens = try parser.tokensFile(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return tokens.map(InternalGrammar.TokenDefinition.from)
}
