import XCTest

@testable import SwiftPEG

class TokenSyntaxInterpreter_RegexTests: XCTestCase {
    // MARK: Terminal

    func testRegexConversion_terminal_literal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertNoMatch(resultUnwrap, input: "b")
    }

    func testRegexConversion_terminal_rangeLiteral() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'...'c' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "b")
        assertWholeMatches(resultUnwrap, input: "c")
        assertNoMatch(resultUnwrap, input: "d")
    }

    func testRegexConversion_terminal_identifier() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' b 'a' ;
        $b: 'a' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "aaa")
        assertNoMatch(resultUnwrap, input: "aba")
    }

    func testRegexConversion_terminal_identifier_noTokenSyntax() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' b 'a' ;
        $b ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        assertNil(result)
    }

    func testRegexConversion_terminal_any() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: . ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "0")
        assertWholeMatches(resultUnwrap, input: "É")
        assertWholeMatches(resultUnwrap, input: "{")
        assertNoMatch(resultUnwrap, input: "")
    }

    func testRegexConversion_terminal_characterPredicate() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b { b != "" } ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        assertNil(result)
    }

    // MARK: Exclusion

    func testRegexConversion_exclusion_literal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'b' 'a'...'c' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "c")
        assertNoMatch(resultUnwrap, input: "b")
    }

    func testRegexConversion_exclusion_many_literal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'b' !'d' 'a'...'e' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "c")
        assertWholeMatches(resultUnwrap, input: "e")
        assertNoMatch(resultUnwrap, input: "b")
        assertNoMatch(resultUnwrap, input: "d")
    }

    func testRegexConversion_exclusion_rangeLiteral() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !'b'...'d' 'a'...'e' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "e")
        assertNoMatch(resultUnwrap, input: "b")
        assertNoMatch(resultUnwrap, input: "c")
        assertNoMatch(resultUnwrap, input: "d")
    }

    func testRegexConversion_exclusion_identifier() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: !b 'a'...'e' ;
        $b: 'b'...'d' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "e")
        assertNoMatch(resultUnwrap, input: "b")
        assertNoMatch(resultUnwrap, input: "c")
        assertNoMatch(resultUnwrap, input: "d")
    }

    // MARK: Item

    func testRegexConversion_item_zeroOrMore() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'* ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "")
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "aa")
        assertPrefixMatches(resultUnwrap, input: "e")
    }

    func testRegexConversion_item_oneOrMore() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'+ ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "aa")
        assertPrefixMatches(resultUnwrap, input: "ae")
        assertNoMatch(resultUnwrap, input: "")
        assertNoMatch(resultUnwrap, input: "e")
    }

    func testRegexConversion_item_optionalGroup() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: ('a' | 'b' | 'c')? ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "")
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "b")
        assertWholeMatches(resultUnwrap, input: "c")
        assertPrefixMatches(resultUnwrap, input: "e")
    }

    func testRegexConversion_item_group() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: ('a' | 'b' | 'c') ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "b")
        assertWholeMatches(resultUnwrap, input: "c")
        assertNoMatch(resultUnwrap, input: "e")
        assertNoMatch(resultUnwrap, input: "")
    }

    func testRegexConversion_item_optionalAtom() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a'? ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "")
        assertWholeMatches(resultUnwrap, input: "a")
        assertPrefixMatches(resultUnwrap, input: "e")
    }

    // MARK: Alt

    func testRegexConversion_alt_items() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' 'b' 'c' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "abc")
        assertPrefixMatches(resultUnwrap, input: "abcd")
        assertNoMatch(resultUnwrap, input: "")
        assertNoMatch(resultUnwrap, input: "a")
        assertNoMatch(resultUnwrap, input: "ab")
    }

    func testRegexConversion_alt_trailExclusions() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' 'b' 'c' !'e' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "abc")
        assertPrefixMatches(resultUnwrap, input: "abcd")
        assertNoMatch(resultUnwrap, input: "abce")
        assertNoMatch(resultUnwrap, input: "")
        assertNoMatch(resultUnwrap, input: "a")
        assertNoMatch(resultUnwrap, input: "ab")
    }

    // MARK: Syntax

    func testRegexConversion_syntax_alts() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' | 'b' 'c' | !'e' 'd'...'f' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: "a")
        assertWholeMatches(resultUnwrap, input: "bc")
        assertWholeMatches(resultUnwrap, input: "d")
        assertWholeMatches(resultUnwrap, input: "f")
        assertPrefixMatches(resultUnwrap, input: "abcd")
        assertPrefixMatches(resultUnwrap, input: "bcd")
        assertPrefixMatches(resultUnwrap, input: "de")
        assertNoMatch(resultUnwrap, input: "")
        assertNoMatch(resultUnwrap, input: "b")
        assertNoMatch(resultUnwrap, input: "c")
        assertNoMatch(resultUnwrap, input: "e")
    }

    func testRegexConversion_syntax_characterPredicates_returnsNil() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: 'a' | 'b' 'c' | !'e' 'd'...'f' | g { g == "g" } ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        assertNil(result)
    }

    // MARK: Structured samples

    func testRegexConversion_sample_stringSyntax() throws {
        let tokens = try parseTokenDefinitions(#"""
        $STRING[".string"]:
            | tripleQuote ( '\\"""' | backslashEscape | !tripleQuote . )* tripleQuote
            | doubleQuote ( '\\"' | backslashEscape | !doubleQuote !'\n' . )* doubleQuote
            | singleQuote ( "\\'" | backslashEscape | !singleQuote !'\n' . )* singleQuote
            ;

        %tripleQuote: '"""' ;
        %doubleQuote: '"' ;
        %singleQuote: "'" ;
        %backslashEscape: '\\\\' | '\\' ;
        """#)
        let sut = makeSut(tokens)

        let result = sut.regexConversion(for: tokens[0])

        let resultUnwrap = try assertUnwrap(result)
        assertWholeMatches(resultUnwrap, input: #""abc""#)
        assertWholeMatches(resultUnwrap, input: #"'a\'b\nc'"#)
        assertWholeMatches(resultUnwrap, input: #"""
        """ a
        b c """
        """#)
        assertPrefixMatches(resultUnwrap, input: #"'a'b\nc'"#)
        assertNoMatch(resultUnwrap, input: #""a\"bc"#)
        assertNoMatch(resultUnwrap, input: #""abc"#)
    }
}

// MARK: - Test internals

private func makeSut(_ tokens: [InternalGrammar.TokenDefinition]) -> TokenSyntaxInterpreter {
    return .init(tokenDefinitions: tokens)
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

private func assertWholeMatches(
    _ regex: Regex<Substring>,
    input: String,
    file: StaticString = #file,
    line: UInt = #line
) {

    let result = input.wholeMatch(of: regex)

    assertNotNil(
        result,
        file: file,
        line: line
    )
}

private func assertPrefixMatches(
    _ regex: Regex<Substring>,
    input: String,
    file: StaticString = #file,
    line: UInt = #line
) {

    let result = input.prefixMatch(of: regex)

    assertNotNil(
        result,
        file: file,
        line: line
    )
}

private func assertNoMatch(
    _ regex: Regex<Substring>,
    input: String,
    file: StaticString = #file,
    line: UInt = #line
) {

    let result = input.firstMatch(of: regex)

    assertNil(
        result,
        file: file,
        line: line
    )
}
