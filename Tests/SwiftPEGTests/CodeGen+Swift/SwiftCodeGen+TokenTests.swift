import Testing
import SwiftAST

@testable import SwiftPEG

struct SwiftCodeGen_TokenTests {
    @Test
    func generateTokenParseCheck_noDependantTokens() throws {
        let tokens = try parseTokenDefinitions(#"""
        $leftSquare: '[' ;
        """#)
        let sut = makeSut(tokens)
        var emittedTokenNames: Set<String> = []

        let result = try sut.generateTokenParseCheck(
            settings: .default,
            tokens[0],
            emittedTokenNames: &emittedTokenNames
        ).emit()

        assertEqual(emittedTokenNames, ["leftSquare"])
        diffTest(expected: #"""
        if consume_leftSquare(from: &stream) {
            return .init(kind: .leftSquare, string: stream.substring)
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParseCheck_withDependantTokens() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier: ("a"..."z")+ ;
        $keyword: "keyword" ;
        """#)
        let tokenOcclusionGraph = TokenOcclusionGraph(
            nodes: ["keyword", "identifier"],
            edges: [.init(start: "identifier", end: "keyword")]
        )
        let sut = makeSut(tokens, tokenOcclusionGraph: tokenOcclusionGraph)
        var emittedTokenNames: Set<String> = []

        let result = try sut.generateTokenParseCheck(
            settings: .default,
            tokens[0],
            emittedTokenNames: &emittedTokenNames
        ).emit()

        assertEqual(emittedTokenNames, ["identifier", "keyword"])
        diffTest(expected: #"""
        if consume_identifier(from: &stream) {
            switch stream.substring {
            case "keyword":
                return .init(kind: .keyword, string: stream.substring)
            default:
                return .init(kind: .identifier, string: stream.substring)
            }
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParseCheck_withDependantTokens_convertsQuotes() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier: ("a"..."z")+ ;
        $keyword: 'keyword' ;
        """#)
        let tokenOcclusionGraph = TokenOcclusionGraph(
            nodes: ["keyword", "identifier"],
            edges: [.init(start: "identifier", end: "keyword")]
        )
        let sut = makeSut(tokens, tokenOcclusionGraph: tokenOcclusionGraph)
        var emittedTokenNames: Set<String> = []

        let result = try sut.generateTokenParseCheck(
            settings: .default,
            tokens[0],
            emittedTokenNames: &emittedTokenNames
        ).emit()

        assertEqual(emittedTokenNames, ["identifier", "keyword"])
        diffTest(expected: #"""
        if consume_identifier(from: &stream) {
            switch stream.substring {
            case "keyword":
                return .init(kind: .keyword, string: stream.substring)
            default:
                return .init(kind: .identifier, string: stream.substring)
            }
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParseCheck_ignoresFragments() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier: ("a"..."z")+ ;
        %keyword: 'keyword' ;
        """#)
        let tokenOcclusionGraph = TokenOcclusionGraph(
            nodes: ["keyword", "identifier"],
            edges: [.init(start: "identifier", end: "keyword")]
        )
        let sut = makeSut(tokens, tokenOcclusionGraph: tokenOcclusionGraph)
        var emittedTokenNames: Set<String> = []

        let result = try sut.generateTokenParseCheck(
            settings: .default,
            tokens[0],
            emittedTokenNames: &emittedTokenNames
        ).emit()

        assertEqual(emittedTokenNames, ["identifier", "keyword"])
        diffTest(expected: #"""
        if consume_identifier(from: &stream) {
            switch stream.substring {
            case "keyword":
                return .init(kind: .keyword, string: stream.substring)
            default:
                return .init(kind: .identifier, string: stream.substring)
            }
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParseCheck_emitLengthSwitchPhaseInTokenOcclusionSwitch_generateLengthSwitch() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier: ("a"..."z") ("a"..."z" | "0"..."9")* ;
        $foobar1: "foobar1" ;
        $keyword: "keyword" ;
        $keyword1: "keyword1" ;
        $keyword2: "keyword2" ;
        $keyword11: "keyword11" ;
        $keyword12: "keyword12" ;
        """#)
        let dependants: [String] = [
            "foobar1", "keyword", "keyword1", "keyword2", "keyword11", "keyword12",
        ]
        let tokenOcclusionGraph = TokenOcclusionGraph(
            nodes: ["identifier"] + dependants,
            edges: dependants.map({ dependant in
                .init(start: "identifier", end: dependant)
            })
        )
        let sut = makeSut(tokens, tokenOcclusionGraph: tokenOcclusionGraph)
        var emittedTokenNames: Set<String> = []

        let result = try sut.generateTokenParseCheck(
            settings: .default.with(\.emitLengthSwitchPhaseInTokenOcclusionSwitch, value: true),
            tokens[0],
            emittedTokenNames: &emittedTokenNames
        ).emit()

        assertEqual(emittedTokenNames, Set(dependants).union(["identifier"]))
        diffTest(expected: #"""
        if consume_identifier(from: &stream) {
            switch stream.substringLength {
            case 9:
                switch stream.substring {
                case "keyword11":
                    return .init(kind: .keyword11, string: stream.substring)
                case "keyword12":
                    return .init(kind: .keyword12, string: stream.substring)
                default:
                    return .init(kind: .identifier, string: stream.substring)
                }
            case 8:
                switch stream.substring {
                case "keyword1":
                    return .init(kind: .keyword1, string: stream.substring)
                case "keyword2":
                    return .init(kind: .keyword2, string: stream.substring)
                default:
                    return .init(kind: .identifier, string: stream.substring)
                }
            case 7:
                switch stream.substring {
                case "foobar1":
                    return .init(kind: .foobar1, string: stream.substring)
                case "keyword":
                    return .init(kind: .keyword, string: stream.substring)
                default:
                    return .init(kind: .identifier, string: stream.substring)
                }
            default:
                return .init(kind: .identifier, string: stream.substring)
            }
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParseCheck_withDependantTokens_skipEmittedTokens() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier: ("a"..."z")+ ;
        $keyword: "keyword" ;
        """#)
        let tokenOcclusionGraph = TokenOcclusionGraph(
            nodes: ["keyword", "identifier"],
            edges: [.init(start: "identifier", end: "keyword")]
        )
        let sut = makeSut(tokens, tokenOcclusionGraph: tokenOcclusionGraph)
        var emittedTokenNames: Set<String> = ["keyword"]

        let result = try sut.generateTokenParseCheck(
            settings: .default,
            tokens[0],
            emittedTokenNames: &emittedTokenNames
        ).emit()

        assertEqual(emittedTokenNames, ["identifier", "keyword"])
        diffTest(expected: #"""
        if consume_identifier(from: &stream) {
            return .init(kind: .identifier, string: stream.substring)
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_modifiers() throws {
        let tokens = try parseTokenDefinitions(#"""
        $leftSquare: '[' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default.with(\.accessLevel, value: "private"), modifiers: [.static])?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// leftSquare:
        ///     | "["
        ///     ;
        /// ```
        private static func consume_leftSquare<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            stream.advanceIfNext("[")
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_singleCharacter() throws {
        let tokens = try parseTokenDefinitions(#"""
        $leftSquare: '[' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// leftSquare:
        ///     | "["
        ///     ;
        /// ```
        func consume_leftSquare<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            stream.advanceIfNext("[")
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_optionalGroup() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax: '[' ('0'...'9' | '_')? ']' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "[" ("0"..."9" | "_")? "]"
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("[") else {
                    return false
                }

                switch stream.peek() {
                case "0"..."9", "_":
                    stream.advance()
                default:
                    Void()
                }

                guard stream.advanceIfNext("]") else {
                    break alt
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_optionalAtom() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax: '[' !'5' '0'...'9'? ']' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "[" !"5" "0"..."9"? "]"
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("[") else {
                    return false
                }

                if
                    !stream.isNext("5"),
                    !stream.isEof,
                    stream.isNextInRange("0"..."9")
                {
                    stream.advance()
                }

                guard stream.advanceIfNext("]") else {
                    break alt
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }


    @Test
    func generateTokenParser_anyPattern() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | '[' (']' | .)
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "[" ("]" | .)
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            alt:
            do {
                guard stream.advanceIfNext("[") else {
                    return false
                }

                switch stream.peek() {
                case "]":
                    stream.advance()
                default:
                    stream.advance()
                }

                return true
            }
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_identifierToken() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | 'a' (b | 'c')+
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "a" (b | "c")+
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("a") else {
                    return false
                }

                if consume_b(from: &stream) {
                } else if stream.advanceIfNext("c") {
                } else {
                    break alt
                }

                while consume_b(from: &stream) || stream.advanceIfNext("c") {
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_identifierExclusion() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | 'a' (!b 'c' | 'd')+
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "a" (!b "c" | "d")+
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("a") else {
                    return false
                }

                if
                    stream.negativeLookahead(consume_b(from:)),
                    stream.advanceIfNext("c")
                {
                } else if stream.advanceIfNext("d") {
                } else {
                    break alt
                }

                loop:
                while !stream.isEof {
                    if
                        stream.negativeLookahead(consume_b(from:)),
                        stream.advanceIfNext("c")
                    {
                    } else if stream.advanceIfNext("d") {
                    } else {
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_rangeExclusion() throws {
        let tokens = try parseTokenDefinitions(#"""
        $syntax:
            | 'a' (!"b"..."d" e | f)+
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// syntax:
        ///     | "a" (!"b"..."d" e | f)+
        ///     ;
        /// ```
        func consume_syntax<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("a") else {
                    return false
                }

                if
                    !stream.isNextInRange("b"..."d"),
                    consume_e(from: &stream)
                {
                } else if consume_f(from: &stream) {
                } else {
                    break alt
                }

                loop:
                while !stream.isEof {
                    if
                        !stream.isNextInRange("b"..."d"),
                        consume_e(from: &stream)
                    {
                    } else if consume_f(from: &stream) {
                    } else {
                        break loop
                    }
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_mergeAtoms() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | ('a'...'j' | '_' | 'j'...'p' | 'p' | 'p'...'z')
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | ("a"..."j" | "_" | "j"..."p" | "p" | "p"..."z")
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            alt:
            do {
                switch stream.peek() {
                case "a"..."j", "_", "j"..."z":
                    stream.advance()
                default:
                    return false
                }

                return true
            }
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_characterRanges() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | ('A'...'Z' | 'a'...'z' | '_') ('0'...'9' | 'A'...'Z' | 'a'...'z' | '_')*
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | ("A"..."Z" | "a"..."z" | "_") ("0"..."9" | "A"..."Z" | "a"..."z" | "_")*
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            alt:
            do {
                switch stream.peek() {
                case "A"..."Z", "a"..."z", "_":
                    stream.advance()
                default:
                    return false
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
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_characterActionOnly() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | c { c.isLetter || c == "_" } (c { c.isLetter || c.isWholeNumber || c == "_" })*
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | c { c.isLetter || c == "_" } (c { c.isLetter || c.isWholeNumber || c == "_" })*
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            alt:
            do {
                guard
                    let c = stream.safePeek(),
                    c.isLetter || c == "_"
                else {
                    return false
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
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_mixedLiteralAndCharacterActions() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | c { c.isLetter || c == "_" } ("_" | "A"..."Z" | c { c.isLetter || c.isWholeNumber || c == "_" } | "0"..."9")*
            ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// identifier:
        ///     | c { c.isLetter || c == "_" } ("_" | "A"..."Z" | c { c.isLetter || c.isWholeNumber || c == "_" } | "0"..."9")*
        ///     ;
        /// ```
        func consume_identifier<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            alt:
            do {
                guard
                    let c = stream.safePeek(),
                    c.isLetter || c == "_"
                else {
                    return false
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
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_trailExclusions() throws {
        let tokens = try parseTokenDefinitions(#"""
        $for: 'for' !letter !digit ;
        $letter: 'a'...'z' ;
        $digit: '0'...'9' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// for:
        ///     | "for" !letter !digit
        ///     ;
        /// ```
        func consume_for<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("for") else {
                    return false
                }

                guard
                    stream.negativeLookahead(consume_letter(from:)),
                    stream.negativeLookahead(consume_digit(from:))
                else {
                    break alt
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }

    @Test
    func generateTokenParser_trailOptionals() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ('b' | 'c' | 'd' | 'ee')? ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_a(from: &stream) {
                    return .init(kind: .a, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"a" ("b" | "c" | "d" | "ee")?`
                case a

                var description: String {
                    switch self {
                    case .a:
                        "a"
                    }
                }
            }

            /// ```
            /// a:
            ///     | "a" ("b" | "c" | "d" | "ee")?
            ///     ;
            /// ```
            static func consume_a<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

                alt:
                do {
                    guard stream.advanceIfNext("a") else {
                        return false
                    }

                    _ = stream.advanceIfNext("b") || stream.advanceIfNext("c") || stream.advanceIfNext("d") || stream.advanceIfNext("ee")

                    return true
                }
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenParser_stringSyntax() throws {
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

        let result = try sut.generateTokenParser(tokens[0], settings: .default)?.emit() ?? ""

        diffTest(expected: #"""
        /// ```
        /// stringLiteral:
        ///     | "\"\"\"" (!"\"\"\"" . | "\\\"\"\"")* "\"\"\""
        ///     | "'" (!"'" !"\n" . | "\\'")* "'"
        ///     | "\"" (!"\"" !"\n" . | "\\\"")* "\""
        ///     ;
        /// ```
        func consume_stringLiteral<StringType>(from stream: inout StringStream<StringType>) -> Bool {
            guard !stream.isEof else {
                return false
            }

            let state: StringStream<StringType>.State = stream.save()

            alt:
            do {
                guard stream.advanceIfNext("\"\"\"") else {
                    break alt
                }

                loop:
                while !stream.isEof {
                    if
                        !stream.isNext("\"\"\""),
                        !stream.isEof
                    {
                        stream.advance()
                    } else if stream.advanceIfNext("\\\"\"\"") {
                    } else {
                        break loop
                    }
                }

                guard stream.advanceIfNext("\"\"\"") else {
                    break alt
                }

                return true
            }

            stream.restore(state)

            alt:
            do {
                guard stream.advanceIfNext("\'") else {
                    break alt
                }

                loop:
                while !stream.isEof {
                    if
                        !stream.isNext("\'"),
                        !stream.isNext("\n"),
                        !stream.isEof
                    {
                        stream.advance()
                    } else if stream.advanceIfNext("\\\'") {
                    } else {
                        break loop
                    }
                }

                guard stream.advanceIfNext("\'") else {
                    break alt
                }

                return true
            }

            stream.restore(state)

            alt:
            do {
                guard stream.advanceIfNext("\"") else {
                    return false
                }

                loop:
                while !stream.isEof {
                    if
                        !stream.isNext("\""),
                        !stream.isNext("\n"),
                        !stream.isEof
                    {
                        stream.advance()
                    } else if stream.advanceIfNext("\\\"") {
                    } else {
                        break loop
                    }
                }

                guard stream.advanceIfNext("\"") else {
                    break alt
                }

                return true
            }

            stream.restore(state)

            return false
        }
        """#).diff(result)
    }

    @Test
    func generateTokenType_tokenTypeHeader() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(
            grammar: .init(metas: [
                .init(
                    name: SwiftCodeGen.tokenTypeHeader,
                    value: .string("// Token header content\nimport SwiftPEG\n")
                )
            ], rules: []),
            tokens
        )

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        // Token header content
        import SwiftPEG

        struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"abc"`
                case tok

                var description: String {
                    switch self {
                    case .tok:
                        "abc"
                    }
                }
            }

            /// ```
            /// tok:
            ///     | "abc"
            ///     ;
            /// ```
            static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("abc")
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenType_tokenTypeName() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(
            grammar: .init(metas: [
                .init(
                    name: SwiftCodeGen.tokenTypeName,
                    value: .identifier("CustomTypeName")
                )
            ], rules: []),
            tokens
        )

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        struct CustomTypeName: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"abc"`
                case tok

                var description: String {
                    switch self {
                    case .tok:
                        "abc"
                    }
                }
            }

            /// ```
            /// tok:
            ///     | "abc"
            ///     ;
            /// ```
            static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("abc")
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenType_ignoreTokenFragmentsInTokenKindAndLexerFunction() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'a' frag ;
        %frag: 'bcd' ;
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            enum TokenKind: TokenKindType {
                /// `"a" frag`
                case tok

                var description: String {
                    switch self {
                    case .tok:
                        "tok"
                    }
                }
            }

            /// ```
            /// frag:
            ///     | "bcd"
            ///     ;
            /// ```
            static func consume_frag<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                stream.advanceIfNext("bcd")
            }

            /// ```
            /// tok:
            ///     | "a" frag
            ///     ;
            /// ```
            static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

                let state: StringStream<StringType>.State = stream.save()

                alt:
                do {
                    guard stream.advanceIfNext("a") else {
                        return false
                    }

                    guard consume_frag(from: &stream) else {
                        break alt
                    }

                    return true
                }

                stream.restore(state)

                return false
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenType_usesStaticTokenDefinition() throws {
        let tokens = try parseTokenDefinitions(#"""
        $TOKEN[".tokenName"]: 'a' ;
        $TOKEN2["A.tokenName2"]: 'b';
        $TOKEN3["tokenName3"]: 'c';
        """#)
        let sut = makeSut(tokens)

        let result = try sut.generateTokenType()

        diffTest(expected: #"""
        struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

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
                    case .tokenName:
                        "a"
                    case .TOKEN2:
                        "b"
                    case .tokenName3:
                        "c"
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

    @Test
    func generateTokenType_emitInlinable() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

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
                    case .tok:
                        "abc"
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

    @Test
    func generateTokenType_accessLevel() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

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
                    case .tok:
                        "abc"
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

    @Test
    func generateTokenType_emitInlinable_accessLevel() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'abc' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")
                .with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

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
                    case .tok:
                        "abc"
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

    @Test
    func generateTokenType_simplifiesOptionalIfChecks() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'a' b? ;
        %b: '0'...'9'+ ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")
                .with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            public enum TokenKind: TokenKindType {
                /// `"a" b?`
                case tok

                @inlinable
                public var description: String {
                    switch self {
                    case .tok:
                        "tok"
                    }
                }
            }

            /// ```
            /// b:
            ///     | ("0"..."9")+
            ///     ;
            /// ```
            @inlinable
            public static func consume_b<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

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
            /// tok:
            ///     | "a" b?
            ///     ;
            /// ```
            @inlinable
            public static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

                alt:
                do {
                    guard stream.advanceIfNext("a") else {
                        return false
                    }

                    _ = consume_b(from: &stream)

                    return true
                }
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenType_simplifiesWhileChecks() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'a' b+ ;
        %b: '0'...'9'+ ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")
                .with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            public enum TokenKind: TokenKindType {
                /// `"a" (b)+`
                case tok

                @inlinable
                public var description: String {
                    switch self {
                    case .tok:
                        "tok"
                    }
                }
            }

            /// ```
            /// b:
            ///     | ("0"..."9")+
            ///     ;
            /// ```
            @inlinable
            public static func consume_b<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

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
            /// tok:
            ///     | "a" (b)+
            ///     ;
            /// ```
            @inlinable
            public static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

                let state: StringStream<StringType>.State = stream.save()

                alt:
                do {
                    guard stream.advanceIfNext("a") else {
                        return false
                    }

                    guard consume_b(from: &stream) else {
                        break alt
                    }

                    while consume_b(from: &stream) {
                    }

                    return true
                }

                stream.restore(state)

                return false
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenType_simplifiesWhileChecks_alts() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: 'a' (b | c)+ | 'a' (b | 'c')+ ;
        %b: '0'...'9'+ ;
        %c: 'a'...'z'+ ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")
                .with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            public enum TokenKind: TokenKindType {
                /// ```
                /// tok:
                ///     | "a" (b | c)+
                ///     | "a" (b | "c")+
                ///     ;
                /// ```
                case tok

                @inlinable
                public var description: String {
                    switch self {
                    case .tok:
                        "tok"
                    }
                }
            }

            /// ```
            /// b:
            ///     | ("0"..."9")+
            ///     ;
            /// ```
            @inlinable
            public static func consume_b<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

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
            /// c:
            ///     | ("a"..."z")+
            ///     ;
            /// ```
            @inlinable
            public static func consume_c<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

                alt:
                do {
                    switch stream.peek() {
                    case "a"..."z":
                        stream.advance()
                    default:
                        return false
                    }

                    loop:
                    while !stream.isEof {
                        switch stream.peek() {
                        case "a"..."z":
                            stream.advance()
                        default:
                            break loop
                        }
                    }

                    return true
                }
            }

            /// ```
            /// tok:
            ///     | "a" (b | c)+
            ///     | "a" (b | "c")+
            ///     ;
            /// ```
            @inlinable
            public static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                guard !stream.isEof else {
                    return false
                }

                let state: StringStream<StringType>.State = stream.save()

                alt:
                do {
                    guard stream.advanceIfNext("a") else {
                        break alt
                    }

                    guard consume_b(from: &stream) || consume_c(from: &stream) else {
                        break alt
                    }

                    while consume_b(from: &stream) || consume_c(from: &stream) {
                    }

                    return true
                }

                stream.restore(state)

                alt:
                do {
                    guard stream.advanceIfNext("a") else {
                        return false
                    }

                    guard consume_b(from: &stream) || stream.advanceIfNext("c") else {
                        break alt
                    }

                    while consume_b(from: &stream) || stream.advanceIfNext("c") {
                    }

                    return true
                }

                stream.restore(state)

                return false
            }
        }

        """#).diff(result)
    }

    @Test
    func generateTokenType_simplifiesTrailGuards() throws {
        let tokens = try parseTokenDefinitions(#"""
        $tok: '+' | '-' ;
        """#)
        let sut = makeSut(tokens)
        let settings: SwiftCodeGen.TokenTypeGenSettings =
            .default.with(\.accessLevel, value: "public")
                .with(\.emitInlinable, value: true)

        let result = try sut.generateTokenType(settings: settings)

        diffTest(expected: #"""
        public struct ParserToken: RawTokenType, CustomStringConvertible {
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
                guard !stream.isEof else {
                    return nil
                }

                stream.markSubstringStart()

                if consume_tok(from: &stream) {
                    return .init(kind: .tok, string: stream.substring)
                }

                return nil
            }

            public enum TokenKind: TokenKindType {
                /// ```
                /// tok:
                ///     | "+"
                ///     | "-"
                ///     ;
                /// ```
                case tok

                @inlinable
                public var description: String {
                    switch self {
                    case .tok:
                        "tok"
                    }
                }
            }

            /// ```
            /// tok:
            ///     | "+"
            ///     | "-"
            ///     ;
            /// ```
            @inlinable
            public static func consume_tok<StringType>(from stream: inout StringStream<StringType>) -> Bool {
                return stream.advanceIfNext("+") || stream.advanceIfNext("-")
            }
        }

        """#).diff(result)
    }
}

// MARK: - Test internals

private func makeSut(
    grammar: InternalGrammar.Grammar = .init(rules: []),
    _ tokens: [InternalGrammar.TokenDefinition],
    tokenOcclusionGraph: TokenOcclusionGraph = .init(nodes: [], edges: [])
) -> SwiftCodeGen {

    SwiftCodeGen(
        grammar: grammar,
        tokenDefinitions: tokens,
        tokenOcclusionGraph: tokenOcclusionGraph
    )
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

    return tokens.compactMap { decl in
        guard let decl = decl as? SwiftPEGGrammar.TokenDefinition else {
            return nil
        }

        return InternalGrammar.TokenDefinition.from(decl)
    }
}

private extension Expression {
    func emit() -> String {
        let emitter = SwiftASTEmitter()
        emitter.emit(self)
        return emitter.finishBuffer()
    }
}

private extension Statement {
    func emit() -> String {
        let emitter = SwiftASTEmitter()
        emitter.emit(self)
        return emitter.finishBuffer()
    }
}

private extension SwiftCodeGen.FunctionMemberDecl {
    func emit() -> String {
        let emitter = SwiftASTEmitter()
        emitter.emit(self)
        return emitter.finishBuffer()
    }
}
