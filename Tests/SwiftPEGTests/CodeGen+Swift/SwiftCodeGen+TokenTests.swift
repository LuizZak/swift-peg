import XCTest

@testable import SwiftPEG

class SwiftCodeGen_TokenTests: XCTestCase {
    func testGenerateTokenParser_characterRanges() throws {
        let tokens = try parseTokenDefinitions(#"""
        $identifier:
            | ('A'...'Z' | 'a'...'z' | '_') ('0'...'9' | 'A'...'Z' | 'a'...'z' | '_')*
            ;
        """#)

        let sut = makeSut(tokens)

        try sut.generateTokenParser(tokens[0])

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
                if !stream.isEof, ("A"..."Z").contains(stream.peek()) {
                    stream.advance()
                } else if !stream.isEof, ("a"..."z").contains(stream.peek()) {
                    stream.advance()
                } else if stream.isNext("_") {
                    stream.advance()
                } else {
                    break alt
                }

                while !stream.isEof {
                    if !stream.isEof, ("0"..."9").contains(stream.peek()) {
                        stream.advance()
                    } else if !stream.isEof, ("A"..."Z").contains(stream.peek()) {
                        stream.advance()
                    } else if !stream.isEof, ("a"..."z").contains(stream.peek()) {
                        stream.advance()
                    } else if stream.isNext("_") {
                        stream.advance()
                    } else {
                        break
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

        try sut.generateTokenParser(tokens[0])

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

                while !stream.isEof {
                    if let c = stream.safePeek(), c.isLetter || c.isWholeNumber || c == "_" {
                        stream.advance()
                    } else {
                        break
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

        try sut.generateTokenParser(tokens[0])

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

                while !stream.isEof {
                    if !stream.isNext("\"\"\""), !stream.isEof {
                        stream.advance()
                    } else if stream.isNext("\\\"\"\"") {
                        stream.advance(4)
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
                guard stream.isNext("'") else {
                    break alt
                }
                stream.advance()

                while !stream.isEof {
                    if !stream.isNext("'"), !stream.isNext("\n"), !stream.isEof {
                        stream.advance()
                    } else if stream.isNext("\\'") {
                        stream.advance(2)
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

            alt:
            do {
                guard stream.isNext("\"") else {
                    break alt
                }
                stream.advance()

                while !stream.isEof {
                    if !stream.isNext("\""), !stream.isNext("\n"), !stream.isEof {
                        stream.advance()
                    } else if stream.isNext("\\\"") {
                        stream.advance(2)
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

            return false
        }
        """#).diff(sut.buffer.finishBuffer())
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
