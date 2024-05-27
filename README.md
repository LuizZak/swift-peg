# SwiftPEG

An implementation of a [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) in Swift for generating memoized [Packrat parsers](https://en.wikipedia.org/wiki/Packrat_parser) with support for left-recursive rules.

Contains a [self-describing grammar implementation](Sources/SwiftPEG/Grammar/metagrammar.gram) which can be used to describe grammars and later generate parsers for those grammars.

This library assumes that an appropriate [raw tokenizer](Sources/SwiftPEG/RawTokenizerType.swift) for the matched grammar has been implemented beforehand, defining the result of `parser.expect("<some token literal>")` and `parser.expect(kind: .someTokenKind)`, and that an appropriate set of types describing the desired AST representation also exists, to be used in `{ actions }` for generating the desired AST structured during parsing.

Inspired after a reading of Guido van Rossum's (of Python fame) [PEG Parsing](https://medium.com/@gvanrossum_83706/peg-parsing-series-de5d41b2ed60) series of blog posts.

## Example

Starting from a grammar file of a left-recursive mathematical expression:

```
# Define that the parser being generated is called 'TestGrammarParser'. Assumes it already exists as a subclass of PEGParser<RawTokenizer>
@parserName "TestGrammarParser" ; # @meta-properties can be used to signal grammar processors and code generators of certain properties, and must precede all rules in a grammar

@token NAME ;     # Identifiers must resolve to rules/tokens; @token meta-property forward-declares token identifiers and is detected by GrammarProcessor
@token NUMBER ;
@token NEWLINE ;

start[TestGrammarAST.Expression]:  # Rules can optionally specify their return value within [ square brackets ]
    | expr _=NEWLINE? { expr }     # Optional code with { curly braces } to be executed and returned when alternative matches
    ;

expr[TestGrammarAST.Expr]:
          # v Tokens can be specified as string literals- no need to forward declare
    | expr '+' term { .add(expr, term) }
    | expr '-' term { .sub(expr, term) }
    | term { .term(term) }
    ;

term[TestGrammarAST.Term]:
    | term '*' factor { .mul(term, factor) }
    | term '/' factor { .div(term, factor) }
    | factor { .factor(factor) }
    ;

factor[TestGrammarAST.Factor]:
    | '(' expr ')' { .expr(expr) }
    | atom { .atom(atom) }
    ;

atom[TestGrammarAST.Atom]:
    | NAME { .name(name) }
    | NUMBER { .number(number) }
    ;
```

And with a custom syntax tree defined for the grammar:

```swift
indirect enum Expr: CustomStringConvertible {
    case add(Self, Term)
    case sub(Self, Term)
    case term(Term)

    var description: String { ... }
}

indirect enum Term: CustomStringConvertible {
    case mul(Self, Factor)
    case div(Self, Factor)
    case factor(Factor)

    var description: String { ... }
}

indirect enum Factor: CustomStringConvertible {
    case expr(Expr)
    case atom(Atom)

    var description: String { ... }
}

enum Atom: CustomStringConvertible {
    case name(Substring)
    case number(Double)

    var description: String { ... }
}
```

A parser may be generated with the following code:

```swift
let tokenizer = GrammarRawTokenizer(source: grammarString)
let parser = GrammarParser(raw: tokenizer)

guard let grammar = try parser.start(), tokenizer.isEOF else {
    throw parser.makeSyntaxError()
}

let processor = GrammarProcessor(delegate: nil)
// Process and validate grammar
let processedGrammar = try processor.process(grammar)

let codeGen = SwiftCodeGen(from: processedGrammar)
print(try codeGen.generateParser())
```

A type definition extending a parser is generated like so:

```swift
extension TestGrammarParser {
    /// ```
    /// start[TestGrammarAST.Expr]:
    ///     | expr _=NEWLINE? { expr }
    ///     ;
    /// ```
    @memoized("start")
    @inlinable
    public func __start() throws -> TestGrammarAST.Expr? {
        let mark = self.mark()

        if
            let expr = try self.expr(),
            let _ = try self.optional({
                try self.NEWLINE()
            })
        {
            return expr
        }

        self.restore(mark)
        return nil
    }

    ...
}
```

And after pairing with an appropriately implemented tokenizer, can be used to parse syntax trees from the original grammar:

```swift
let tokenizer = TestGrammarRawTokenizer(source: """
a + b + (c / d) * 10
""")
let parser = TestGrammarParser(raw: tokenizer)

guard let expression = try parser.start(), tokenizer.isEOF else {
    throw parser.makeSyntaxError()
}

dump(expression)
```

Prints:

```
▿ Optional(a + b + (c / d) * 10.0)
  ▿ some: a + b + (c / d) * 10.0
    ▿ add: (2 elements)
      ▿ .0: a + b
        ▿ add: (2 elements)
          ▿ .0: a
            ▿ term: a
              ▿ factor: a
                ▿ atom: a
                  - name: "a"
          ▿ .1: b
            ▿ factor: b
              ▿ atom: b
                - name: "b"
      ▿ .1: (c / d) * 10.0
        ▿ mul: (2 elements)
          ▿ .0: (c / d)
            ▿ factor: (c / d)
              ▿ expr: c / d
                ▿ term: c / d
                  ▿ div: (2 elements)
                    ▿ .0: c
                      ▿ factor: c
                        ▿ atom: c
                          - name: "c"
                    ▿ .1: d
                      ▿ atom: d
                        - name: "d"
          ▿ .1: 10.0
            ▿ atom: 10.0
              - number: 10.0
```

A full implementation of this example can be found in [TestGrammarParser.swift](Tests/SwiftPEGTests/TestGrammarParser.swift) which is bootstrapped in unit test `testGenerateParser_fullGrammar` within [SwiftCodeGenTests.swift](Tests/SwiftPEGTests/CodeGen+Swift/SwiftCodeGenTests.swift).
