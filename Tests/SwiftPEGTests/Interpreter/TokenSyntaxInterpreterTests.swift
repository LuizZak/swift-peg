import Testing

@testable import SwiftPEG

struct TokenSyntaxInterpreterTests {
    @Test
    func parseToken_ignoresFragments() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ;
        %b: 'b' ;
        %c: 'c' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "b")

        let result = try sut.parseToken(from: &stream)

        assertNil(result)
    }

    @Test
    func parseToken_success_returnsLongestMatch() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ;
        $b: 'abc' ;
        $c: 'c' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "abcdef")

        let result = try assertUnwrap(try sut.parseToken(from: &stream))

        assertEqual(result.tokenName, "b")
        assertEqual(result.substring, "abc")
        assertEqual(stream.index, stream.source.index(stream.source.startIndex, offsetBy: 3))
    }

    @Test
    func parseToken_failure_resetsStreamPosition() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ;
        $b: 'abc' ;
        $c: 'c' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "abcdef")
        stream.markSubstringStart()
        stream.advance()

        let result = try sut.parseToken(from: &stream)

        assertNil(result)
        assertEqual(stream.substringStartIndex, stream.source.startIndex)
        assertEqual(stream.index, stream.source.index(stream.source.startIndex, offsetBy: 1))
    }

    @Test
    func parse_literal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'abc' ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "abc")
        assertParses(sut, tokens[0], input: "abcd")
        assertDoesNotParse(sut, tokens[0], input: "ab")
    }

    @Test
    func parse_rangeLiteral() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'...'c' ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "b")
        assertParses(sut, tokens[0], input: "c")
        assertDoesNotParse(sut, tokens[0], input: "d")
    }

    @Test
    func parse_identifier() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b ;
        $b: 'b' ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "b")
    }

    @Test
    func parse_identifier_optionalProduction_inOneOrMore() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b+ ;
        $b: 'b'? ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "")
    }

    @Test
    func parse_identifier_optionalProduction_inZeroOrMore() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b* ;
        $b: 'b'? ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "")
    }

    @Test
    func parse_any() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: . ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "b")
        assertParses(sut, tokens[0], input: "c")
        assertParses(sut, tokens[0], input: " ")
        assertParses(sut, tokens[0], input: "\n")
        assertDoesNotParse(sut, tokens[0], input: "")
    }

    @Test
    func parse_exclusion_literal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'b' !'c' 'a'...'d' ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "d")
        assertDoesNotParse(sut, tokens[0], input: "b")
        assertDoesNotParse(sut, tokens[0], input: "c")
    }

    @Test
    func parse_exclusion_range() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'b'...'c' 'a'...'d' ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "d")
        assertDoesNotParse(sut, tokens[0], input: "b")
        assertDoesNotParse(sut, tokens[0], input: "c")
    }

    @Test
    func parse_exclusion_identifier() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !b 'a'...'d' ;
        $b: 'b'...'c' ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "d")
        assertDoesNotParse(sut, tokens[0], input: "b")
        assertDoesNotParse(sut, tokens[0], input: "c")
    }

    @Test
    func parse_zeroOrMore() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' 'b'* ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "ab")
        assertParses(sut, tokens[0], input: "abb")
        assertDoesNotParse(sut, tokens[0], input: "")
        assertDoesNotParse(sut, tokens[0], input: "ba")
    }

    @Test
    func parse_zeroOrMore_preventInfiniteNullableLoops() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' b* ;
        $b: 'b'? ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "ab")
        assertParses(sut, tokens[0], input: "abb")
        assertDoesNotParse(sut, tokens[0], input: "")
        assertDoesNotParse(sut, tokens[0], input: "ba")
    }

    @Test
    func parse_oneOrMore() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' 'b'+ ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "ab")
        assertParses(sut, tokens[0], input: "abb")
        assertDoesNotParse(sut, tokens[0], input: "a")
        assertDoesNotParse(sut, tokens[0], input: "")
        assertDoesNotParse(sut, tokens[0], input: "ba")
    }

    @Test
    func parse_oneOrMore_preventInfiniteNullableLoops() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' b+ ;
        $b: 'b'? ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "ab")
        assertParses(sut, tokens[0], input: "abb")
        assertParses(sut, tokens[0], input: "a")
        assertDoesNotParse(sut, tokens[0], input: "")
        assertDoesNotParse(sut, tokens[0], input: "ba")
    }

    @Test
    func parse_group() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ('b' | 'c' | 'd') ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "ab")
        assertParses(sut, tokens[0], input: "ac")
        assertParses(sut, tokens[0], input: "ad")
        assertDoesNotParse(sut, tokens[0], input: "a")
        assertDoesNotParse(sut, tokens[0], input: "")
        assertDoesNotParse(sut, tokens[0], input: "ba")
    }

    @Test
    func parse_optionalGroup() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ('b' | 'c' | 'd')? ;
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "ab")
        assertParses(sut, tokens[0], input: "ac")
        assertParses(sut, tokens[0], input: "ad")
        assertDoesNotParse(sut, tokens[0], input: "")
        assertDoesNotParse(sut, tokens[0], input: "ba")
    }

    @Test
    func parse_alt_trailExclusions() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' !'b';
        """#)
        let sut = makeSut(tokens)

        assertParses(sut, tokens[0], input: "a")
        assertParses(sut, tokens[0], input: "ac")
        assertParses(sut, tokens[0], input: "ad")
        assertDoesNotParse(sut, tokens[0], input: "ab")
    }

    // MARK: - Diagnostics tests

    @Test
    func parse_error_noTokenSyntax() throws {
        let tokens = [
            InternalGrammar.TokenDefinition(name: "a", isFragment: false)
        ]
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .missingSyntax("a"):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }

    @Test
    func parse_error_unknownIdentifier() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .unknownTokenName("b"):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }

    @Test
    func parse_error_incompatibleRangeLiteral_start() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'aa'...'b' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .incompatibleRangeLiteral("aa"):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }

    @Test
    func parse_error_incompatibleRangeLiteral_end() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'...'bb' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .incompatibleRangeLiteral("bb"):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }

    @Test
    func parse_error_exclusion_incompatibleRangeLiteral_start() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'aa'...'b' 'c' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .incompatibleRangeLiteral("aa"):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }

    @Test
    func parse_error_exclusion_incompatibleRangeLiteral_end() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'a'...'bb' 'c' ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .incompatibleRangeLiteral("bb"):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }

    @Test
    func parse_error_characterPredicateUnsupported() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: a { a == "a" } ;
        """#)
        let sut = makeSut(tokens)
        var stream = StringStream(source: "a")

        let error = assertThrows(errorType: TokenSyntaxInterpreter.Error.self) {
            try sut.parse(tokens[0], from: &stream)
        }

        switch error {
        case .characterPredicateUnsupported(#"a { a == "a" }"#):
            break
        default:
            fail("Expected \(TokenSyntaxInterpreter.Error.self) error to be thrown")
        }
    }
}

// MARK: - Test internals

private typealias Sut = TokenSyntaxInterpreter

private func makeSut(
    _ tokens: [InternalGrammar.TokenDefinition]
) -> Sut {

    Sut(tokenDefinitions: tokens)
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

private func assertParses(
    _ sut: Sut,
    _ token: InternalGrammar.TokenDefinition,
    input: String,
    sourceLocation: SourceLocation = #_sourceLocation
) {
    do {
        var stream = StringStream(source: input)

        let result = try sut.parse(token, from: &stream)

        assertTrue(result, sourceLocation: sourceLocation)
    } catch {
        fail("Unexpected error \(error)", sourceLocation: sourceLocation)
    }
}

private func assertDoesNotParse(
    _ sut: Sut,
    _ token: InternalGrammar.TokenDefinition,
    input: String,
    sourceLocation: SourceLocation = #_sourceLocation
) {
    do {
        var stream = StringStream(source: input)
        let state = stream.save()

        let result = try sut.parse(token, from: &stream)

        assertFalse(result, sourceLocation: sourceLocation)
        assertEqual(
            stream.save(),
            state,
            message: "Expected failed parse to restore stream state back to where it found it",
            sourceLocation: sourceLocation
        )
    } catch {
        fail("Unexpected error \(error)", sourceLocation: sourceLocation)
    }
}
