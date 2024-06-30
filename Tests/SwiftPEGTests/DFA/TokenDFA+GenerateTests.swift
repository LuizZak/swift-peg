import XCTest

@testable import SwiftPEG

class TokenDFA_GenerateTests: XCTestCase {
    func testFrom_singleTerminal() throws {
        let syntax = try parseTokenSyntax("""
        'a'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
        }
        """#)
    }

    func testFrom_sequential_terminals() throws {
        let syntax = try parseTokenSyntax("""
        'a' 'b'...'d' 'e'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=circle]
            n4 [label="s3", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n3 [label="\'b\'...\'d\'"]
            n3 -> n4 [label="\'e\'"]
        }
        """#)
    }

    func testFrom_optional_atom() throws {
        let syntax = try parseTokenSyntax("""
        'a' 'b'? 'c'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=circle]
            n4 [label="s3", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n3 [label="\'b\'"]
            n2 -> n4 [label="\'c\'"]
            n3 -> n4 [label="\'c\'"]
        }
        """#)
    }

    func testFrom_group_atoms() throws {
        let syntax = try parseTokenSyntax("""
        'a' ('b' | 'c' | 'd') 'e'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=circle]
            n4 [label="s3", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n3 [label="\'b\'"]
            n2 -> n3 [label="\'c\'"]
            n2 -> n3 [label="\'d\'"]
            n3 -> n4 [label="\'e\'"]
        }
        """#)
    }

    func testFrom_zeroOrMore_atoms() throws {
        let syntax = try parseTokenSyntax("""
        'a' ('b' | 'c' | 'd')* 'e'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n2 [label="\'b\'"]
            n2 -> n2 [label="\'c\'"]
            n2 -> n2 [label="\'d\'"]
            n2 -> n3 [label="\'e\'"]
        }
        """#)
    }

    func testFrom_zeroOrMore_terminal_singleElement() throws {
        let syntax = try parseTokenSyntax("""
        'a'*
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=doublecircle]

            n1 -> n1 [label="\'a\'"]
        }
        """#)
    }

    func testFrom_terminal_zeroOrMore_oneOrMoreEquivalent() throws {
        let syntax = try parseTokenSyntax("""
        'a' ('b' | 'c' | 'd') ('b' | 'c' | 'd')* 'e'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=circle]
            n4 [label="s3", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n3 [label="\'b\'"]
            n2 -> n3 [label="\'c\'"]
            n2 -> n3 [label="\'d\'"]
            n3 -> n3 [label="\'b\'"]
            n3 -> n3 [label="\'c\'"]
            n3 -> n3 [label="\'d\'"]
            n3 -> n4 [label="\'e\'"]
        }
        """#)
    }

    func testFrom_oneOrMore_atoms() throws {
        let syntax = try parseTokenSyntax("""
        'a' ('b' | 'c' | 'd')+ 'e'
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=circle]
            n3 [label="s2", shape=circle]
            n4 [label="s3", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n3 [label="\'b\'"]
            n2 -> n3 [label="\'c\'"]
            n2 -> n3 [label="\'d\'"]
            n3 -> n3 [label="\'b\'"]
            n3 -> n3 [label="\'c\'"]
            n3 -> n3 [label="\'d\'"]
            n3 -> n4 [label="\'e\'"]
        }
        """#)
    }

    func testFrom_oneOrMore_terminal_singleElement() throws {
        let syntax = try parseTokenSyntax("""
        'a'+
        """)

        let sut = makeSut(syntax)

        assertGraphviz(dfa: sut, matches: #"""
        digraph {
            graph [rankdir=LR]

            n1 [label="s0", shape=circle]
            n2 [label="s1", shape=doublecircle]

            n1 -> n2 [label="\'a\'"]
            n2 -> n2 [label="\'a\'"]
        }
        """#)
    }

    func testFrom_alts() throws {
        let syntax = try parseTokenSyntax(#"""
        | tripleQuote ( '\\"""' | backslashEscape | !tripleQuote . )* tripleQuote
        | doubleQuote ( '\\"' | backslashEscape | !doubleQuote !'\n' . )* doubleQuote
        | singleQuote ( "\\'" | backslashEscape | !singleQuote !'\n' . )* singleQuote
        """#)

        let sut = makeSut(syntax)

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

            n1 -> n2 [label="tripleQuote"]
            n2 -> n2 [label="\'\\\\\"\"\"\'"]
            n2 -> n2 [label="."]
            n2 -> n2 [label="backslashEscape"]
            n2 -> n3 [label="tripleQuote"]
            n1 -> n4 [label="doubleQuote"]
            n4 -> n4 [label="\'\\\\\"\'"]
            n4 -> n4 [label="."]
            n4 -> n4 [label="backslashEscape"]
            n4 -> n5 [label="doubleQuote"]
            n1 -> n6 [label="singleQuote"]
            n6 -> n6 [label="\"\\\\\'\""]
            n6 -> n6 [label="."]
            n6 -> n6 [label="backslashEscape"]
            n6 -> n7 [label="singleQuote"]
        }
        """#)
    }
}

// MARK: - Test internals

private func makeSut(
    _ tokenSyntax: CommonAbstract.TokenSyntax
) -> TokenDFA {

    TokenDFA.from(tokenSyntax)
}

private func parseTokenSyntax(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> CommonAbstract.TokenSyntax {

    let tokenizer = GrammarRawTokenizer(source: grammar)
    let parser = GrammarParser(raw: tokenizer)

    guard let tokenSyntax = try parser.tokenSyntax(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return tokenSyntax
}
