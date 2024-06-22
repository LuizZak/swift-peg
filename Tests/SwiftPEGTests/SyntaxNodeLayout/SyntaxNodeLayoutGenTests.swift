import XCTest

@testable import SwiftPEG

class SyntaxNodeLayoutGenTests: XCTestCase {
    func testGenerateSyntaxNodes() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        start: a ;
        a: 'a' | b 'a' ;
        b: 'b' | c+ ;
        c: 'c' ;
        """#)
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "start", layout: .makeFixed([
                "a": .rule("a"),
            ])),
            .init(name: "a", layout: .makeFixed([
                "A": .token("A"),
                "b": .optional(.rule("b")),
            ])),
            .init(name: "b", layout: .oneOf([
                .makeFixed(["B": .token("B")]),
                .makeFixed(["c": .collectionOf(.rule("c"))]),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorTogglingElements() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ; $D: 'd' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: A b c d | A b d | A c d | A d ;
        b: 'b' ;
        c: 'c' ;
        d: 'd' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "A": .token("A"),
                "b": .optional(.rule("b")),
                "c": .optional(.rule("c")),
                "d": .rule("d"),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
            .init(name: "d", layout: .makeFixed([
                "D": .token("D"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorTogglingElements_factorOptionalVariations() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ; $D: 'd' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: A b? c d | A b d | A c d | A d ;
        b: 'b' ;
        c: 'c' ;
        d: 'd' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "A": .token("A"),
                "b": .optional(.rule("b")),
                "c": .optional(.rule("c")),
                "d": .rule("d"),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
            .init(name: "d", layout: .makeFixed([
                "D": .token("D"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorTogglingElements_abortOnUnmatchedElement() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ; $D: 'd' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: A b c d | A b b | A c d | A d ;
        b: 'b' ;
        c: 'c' ;
        d: 'd' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .oneOf([
                .makeFixed([
                    "A": .token("A"),
                    "b": .rule("b"),
                    "c": .rule("c"),
                    "d": .rule("d"),
                ]),
                .makeFixed([
                    "A": .token("A"),
                    "b": .rule("b"),
                    "b1": .rule("b"),
                ]),
                .makeFixed([
                    "A": .token("A"),
                    "c": .rule("c"),
                    "d": .rule("d"),
                ]),
                .makeFixed([
                    "A": .token("A"),
                    "d": .rule("d"),
                ]),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
            .init(name: "d", layout: .makeFixed([
                "D": .token("D"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorTogglingElements_abortOnMultipleLongestElements() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ; $D: 'd' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: A b c | A b d | A c;
        b: 'b' ;
        c: 'c' ;
        d: 'd' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "A": .token("A"),
                "b": .optional(.rule("b")),
                "c": .optional(.rule("c")),
                "b1": .optional(.rule("b")),
                "d": .optional(.rule("d")),
                "c1": .optional(.rule("c")),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
            .init(name: "d", layout: .makeFixed([
                "D": .token("D"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorCommonElements() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: A b | A c | A ;
        b: 'b' ;
        c: 'c' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "A": .token("A"),
                "b": .optional(.rule("b")),
                "c": .optional(.rule("c")),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorCommonElements_duplicated() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: b A | c A | A A ;
        b: 'b' ;
        c: 'c' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "b": .optional(.rule("b")),
                "A": .token("A"),
                "c": .optional(.rule("c")),
                "A1": .optional(.token("A")),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_factorCommonElements_suffix() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: b A B | c A B ;
        b: 'b' ;
        c: 'c' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "b": .optional(.rule("b")),
                "A": .token("A"),
                "B": .token("B"),
                "c": .optional(.rule("c")),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_gather() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: b.c+ ;
        b: 'b' ;
        c: 'c' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "c": .collectionOf(.makeFixed([
                    "c": .rule("c"),
                    "b": .optional(.rule("b")),
                ])),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_gather_groupNode() throws {
        let processed = try processGrammar(tokens: #"""
        $A: 'a' ; $B: 'b' ; $C: 'c' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        a: b.(c?)+ ;
        b: 'b' ;
        c: 'c' ;
        """#, entryRuleName: "a")
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "a", layout: .makeFixed([
                "c": .collectionOf(.makeFixed([
                    "c": .optional(.rule("c")),
                    "b": .optional(.rule("b")),
                ])),
            ])),
            .init(name: "b", layout: .makeFixed([
                "B": .token("B"),
            ])),
            .init(name: "c", layout: .makeFixed([
                "C": .token("C"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_sample_tokenSyntaxAtom() throws {
        let processed = try processGrammar(tokens: #"""
        $IDENTIFIER: 'a'...'z'+ ;
        $STRING: '"' (!'"' .)+ '"' ;
        $BANG: '!' ;
        $ELLIPSIS: '...' ;
        $DOT: '.' ;
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        start: tokenSyntaxAtom+ ;

        tokenSyntaxAtom:
            | tokenSyntaxExclusion* tokenSyntaxTerminal
            ;

        tokenSyntaxExclusion:
            | '!' start=string '...' end=string
            | '!' string
            | '!' IDENTIFIER
            ;

        tokenSyntaxTerminal:
            | IDENTIFIER action
            | start=string '...' end=string
            | string
            | IDENTIFIER
            | '.'
            ;

        string:
            | token=STRING
            ;
        
        action:
            | string
            ;

        """#)
        let sut = makeSut(processed)

        let result = try sut.generateSyntaxNodes()

        assertSyntaxNodesEqual(result, [
            .init(name: "start", layout: .makeFixed([
                "tokenSyntaxAtom": .collectionOf(.rule("tokenSyntaxAtom")),
            ])),
            .init(name: "tokenSyntaxAtom", layout: .makeFixed([
                "tokenSyntaxExclusion": .collectionOf(.rule("tokenSyntaxExclusion")),
                "tokenSyntaxTerminal": .rule("tokenSyntaxTerminal"),
            ])),
            .init(name: "tokenSyntaxExclusion", layout: .makeFixed([
                "BANG": .token("BANG"),
                "start": .optional(.rule("string")),
                "ELLIPSIS": .optional(.token("ELLIPSIS")),
                "end": .optional(.rule("string")),
                "string": .optional(.rule("string")),
                "IDENTIFIER": .optional(.token("IDENTIFIER")),
            ])),
            .init(name: "tokenSyntaxTerminal", layout: .oneOf([
                .makeFixed([
                    "IDENTIFIER": .token("IDENTIFIER"),
                    "action": .rule("action"),
                ]),
                .makeFixed([
                    "start": .rule("string"),
                    "ELLIPSIS": .token("ELLIPSIS"),
                    "end": .rule("string"),
                ]),
                .makeFixed([
                    "string": .rule("string"),
                ]),
                .makeFixed([
                    "IDENTIFIER": .token("IDENTIFIER"),
                ]),
                .makeFixed([
                    "DOT": .token("DOT"),
                ]),
            ])),
            .init(name: "string", layout: .makeFixed([
                "token": .token("STRING"),
            ])),
            .init(name: "action", layout: .makeFixed([
                "string": .rule("string"),
            ])),
        ])
    }

    func testGenerateSyntaxNodes_error_unrecognizedTokenLiteral() throws {
        let processed = try processGrammar(tokens: #"""
        """#, grammar: #"""
        @tokensFile "tokens.tokens" ;

        start: a ;
        a: 'a' ;
        """#)
        let sut = makeSut(processed)

        let error = assertThrows(errorType: SyntaxNodeLayoutGen.Error.self) {
            try sut.generateSyntaxNodes()
        }

        switch error {
        case .unknownTokenLiteral(trimmedLiteral: "a"):
            success()
        default:
            fail("Expected to throw error")
        }
    }
}

// MARK: - Test internals

private func makeSut(_ processed: ProcessedGrammar) -> SyntaxNodeLayoutGen {
    return SyntaxNodeLayoutGen(processedGrammar: processed)
}

private func processGrammar(
    tokens: String,
    grammar: String,
    entryRuleName: String = "start",
    file: StaticString = #file,
    line: UInt = #line
) throws -> ProcessedGrammar {

    let delegate = stubDelegate(tokensFile: tokens)
    let rawGrammar = try parseGrammar(grammar)

    let processor = GrammarProcessor(delegate: delegate)

    return try processor.process(rawGrammar, entryRuleName: entryRuleName)
}

private func stubDelegate(tokensFile: String) -> TestGrammarProcessorDelegate {
    let delegate = TestGrammarProcessorDelegate()
    delegate.grammarProcessor_loadTokensFileNamed_stub = { (_, _, _) in
        return tokensFile
    }

    return delegate
}

private func assertSyntaxNodesEqual(
    _ actual: [SyntaxNode],
    _ expected: [SyntaxNode],
    file: StaticString = #file,
    line: UInt = #line
) {
    guard actual != expected else { return }

    var unmatched: [SyntaxNode] = []
    var remainingMap: [SyntaxNode?] = actual
    var expectedIndexOnActual: [Int] = []

    for entry in expected {
        if let index = remainingMap.firstIndex(of: entry) {
            expectedIndexOnActual.append(index)
            remainingMap[index] = nil
        } else {
            unmatched.append(entry)
        }
    }

    let remaining = remainingMap.compactMap { $0 }

    // If the arrays match content-wise, they must mismatch order-wise.
    if remaining.isEmpty && unmatched.isEmpty {
        fail("Nodes are in a different order:", file: file, line: line)

        for (expectedIndex, actualIndex) in zip(expectedIndexOnActual.indices, expectedIndexOnActual) {
            guard expectedIndex != actualIndex else {
                continue
            }

            let expNode = expected[expectedIndex]

            print("Expected node '\(expNode.name)' to be at index \(expectedIndex) but found at index \(actualIndex)")
        }

        return
    }

    fail("Node sequences aren't equal", file: file, line: line)

    // Attempt to pair remaining/unmatched by name
    let byName = Dictionary(grouping: (unmatched + remaining), by: {
        $0.name
    })

    for (name, values) in byName {
        if values.count == 1 {
            print("Found unmatched node '\(values[0].name)':")
            debugPrint(values[0])
        } else if values.count == 2 {
            print("Node '\(name)' is mismatched:")

            let expected = debugDescription(values[0])
            let actual = debugDescription(values[1])

            let stdout = StandardOutputDiffTestCaseFailureReporter()
            stdout.diffTest(
                expected: expected,
                highlightLineInEditor: false,
                file: file,
                line: line
            ).diff(actual, file: file, line: line)
        } else {
            print("Found multiple mismatched nodes named '\(name)':")

            for node in values {
                debugPrint(node)
            }
        }
    }
}

private func debugDescription(_ node: SyntaxNode) -> String {
    node.debugDescription
}
