import XCTest

@testable import SwiftPEG

class TokenDFA_InliningTests: XCTestCase {
    func testInline_terminal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b ;
        $b: 'c' ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=doublecircle]

            n1 -> n2 [label="\'c\'"]
        }
        """#)
    }

    func testInline_terminal_two() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b ;
        $b: 'c' 'd' ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=doublecircle]
            n3 [label="s3", shape=circle]

            n3 -> n2 [label="\'d\'"]
            n1 -> n3 [label="\'c\'"]
        }
        """#)
    }

    func testInline_terminal_three() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b ;
        $b: 'c' 'd' 'e' ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=doublecircle]
            n3 [label="s3", shape=circle]
            n4 [label="s4", shape=circle]

            n4 -> n2 [label="\'e\'"]
            n1 -> n3 [label="\'c\'"]
            n3 -> n4 [label="\'d\'"]
        }
        """#)
    }

    func testInline_zeroOrMore_terminal() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b* ;
        $b: 'c' ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=doublecircle]

            n1 -> n1 [label="\'c\'"]
        }
        """#)
    }

    func testInline_zeroOrMore_terminal_chained() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b* ;
        $b: 'c' 'd' 'e' ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=doublecircle]
            n2 [label="s2", shape=circle]
            n3 [label="s3", shape=circle]

            n3 -> n1 [label="\'e\'"]
            n1 -> n2 [label="\'c\'"]
            n2 -> n3 [label="\'d\'"]
        }
        """#)
    }

    func testInline_zeroOrMore_optional() throws {
        let tokens = try parseTokenDefinitions(#"""
        $a: b* ;
        $b: 'c'? ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=doublecircle]

            n1 -> n1 [label="\'c\'"]
        }
        """#)
    }

    func testInline_stringSyntax() throws {
        let tokens = try parseTokenDefinitions(#"""
        $STRING:
            | tripleQuote ( '\\"""' | backslashEscape | !tripleQuote . )* tripleQuote
            | doubleQuote ( '\\"' | backslashEscape | !doubleQuote !'\n' . )* doubleQuote
            | singleQuote ( "\\'" | backslashEscape | !singleQuote !'\n' . )* singleQuote
            ;

        %tripleQuote: '"""' ;
        %doubleQuote: '"' ;
        %singleQuote: "'" ;
        %backslashEscape: '\\\\' | '\\' ;
        """#)
        let sut = try makeSut(tokens[0])

        sut.inline(tokens)

        sut.validate()
        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=doublecircle]
            n4 [label="s3", shape=circle]
            n5 [label="s4", shape=doublecircle]
            n6 [label="s5", shape=circle]
            n7 [label="s6", shape=doublecircle]

            n1 -> n2 [label="\'\"\"\"\'"]
            n2 -> n2 [label="\'\\\\\"\"\"\'"]
            n2 -> n2 [label="\'\\\\\'"]
            n2 -> n2 [label="\'\\\\\\\\\'"]
            n2 -> n2 [label="."]
            n2 -> n3 [label="\'\"\"\"\'"]
            n1 -> n4 [label="\'\"\'"]
            n4 -> n4 [label="\'\\\\\"\'"]
            n4 -> n4 [label="\'\\\\\'"]
            n4 -> n4 [label="\'\\\\\\\\\'"]
            n4 -> n4 [label="."]
            n4 -> n5 [label="\'\"\'"]
            n1 -> n6 [label="\"\'\""]
            n6 -> n6 [label="\"\\\\\'\""]
            n6 -> n6 [label="\'\\\\\'"]
            n6 -> n6 [label="\'\\\\\\\\\'"]
            n6 -> n6 [label="."]
            n6 -> n7 [label="\"\'\""]
        }
        """#)
    }
}

// MARK: - Test internals

private func makeSut(
    _ tokenDefinition: InternalGrammar.TokenDefinition,
    file: StaticString = #file,
    line: UInt = #line
) throws -> TokenDFA {

    try assertUnwrap(TokenDFA.from(tokenDefinition), file: file, line: line)
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
