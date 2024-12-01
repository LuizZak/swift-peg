import Testing

@testable import SwiftPEG

struct AltReducerTests {
    @Test
    func reduce_staticAlt() throws {
        let alt = try parseAlt("""
        a b 'c'
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(result, alt)
    }

    @Test
    func reduce_optionalItem() throws {
        let alt = try parseAlt("""
        a b? (c d)? e
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a e
            """)
        )
    }

    @Test
    func reduce_optionalItems() throws {
        let alt = try parseAlt("""
        a b ['c'] d [e [f] g.h+]
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a b d
            """)
        )
    }

    @Test
    func reduce_zeroOrMore() throws {
        let alt = try parseAlt("""
        a b* c
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a c
            """)
        )
    }

    @Test
    func reduce_gatherSeparator_returnsOneOrMoreNode() throws {
        let alt = try parseAlt("""
        a (b?).c+
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a c+
            """)
        )
    }

    @Test
    func reduce_gatherNode_elidesGather() throws {
        let alt = try parseAlt("""
        a b.(c?)+
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(
            result,
            try parseAlt("""
            a
            """)
        )
    }

    @Test
    func permute_zeroOrMore() throws {
        let alt = try parseAlt("""
        a b*
        """)
        let sut = makeSut(alt)

        let result = sut.permuted(depthLimit: 4)

        assertEqual(result, [
            try parseAlt("a b+"),
            try parseAlt("a"),
        ])
    }

    @Test
    func permute_leadingOptional() throws {
        let alt = try parseAlt("""
        a? b? c? d? e
        """)
        let sut = makeSut(alt)

        let result = sut.permuted(depthLimit: 4)

        assertEqual(result, [
            try parseAlt("a b c d e"),
            try parseAlt("a b c e"),
            try parseAlt("a b d e"),
            try parseAlt("a b e"),
            try parseAlt("a c d e"),
            try parseAlt("a c e"),
            try parseAlt("a d e"),
            try parseAlt("a e"),
            try parseAlt("b c d e"),
            try parseAlt("b c e"),
            try parseAlt("b d e"),
            try parseAlt("b e"),
            try parseAlt("c d e"),
            try parseAlt("c e"),
            try parseAlt("d e"),
            try parseAlt("e"),
        ])
    }

    @Test
    func permute_optionalsInGroup() throws {
        let alt = try parseAlt("""
        e (a b? | c? d)
        """)
        let sut = makeSut(alt)

        let result = sut.permuted(depthLimit: 4)

        assertEqual(result, [
            try parseAlt("e (a b | c d)"),
            try parseAlt("e (a b | d)"),
            try parseAlt("e (a | c d)"),
            try parseAlt("e (a | d)"),
        ])
    }

    @Test
    func permute_optionalsInGroup_flattensEmptyGroups() throws {
        let alt = try parseAlt("""
        e (a? b? | c? d?)
        """)
        let sut = makeSut(alt)

        let result = sut.permuted(depthLimit: 4)

        assertEqual(result, [
            try parseAlt("e (a b | c d)"),
            try parseAlt("e (a b | c)"),
            try parseAlt("e (a b | d)"),
            try parseAlt("e (a b)"),
            try parseAlt("e (a | c d)"),
            try parseAlt("e (a | c)"),
            try parseAlt("e (a | d)"),
            try parseAlt("e (a)"),
            try parseAlt("e (b | c d)"),
            try parseAlt("e (b | c)"),
            try parseAlt("e (b | d)"),
            try parseAlt("e (b)"),
            try parseAlt("e (c d)"),
            try parseAlt("e (c)"),
            try parseAlt("e (d)"),
            try parseAlt("e"),
        ])
    }

    @Test
    func permute_depthLimit() throws {
        let alt = try parseAlt("""
        e (a? b? | c? d?)
        """)
        let sut = makeSut(alt)

        let result = sut.permuted(depthLimit: 2)

        assertEqual(result, [
            try parseAlt("e (a b | c? d?)"),
            try parseAlt("e (a | c? d?)"),
            try parseAlt("e (b | c? d?)"),
            try parseAlt("e (c? d?)"),
        ])
    }
}

// MARK: - Test internals

private func makeSut(_ alt: InternalGrammar.Alt) -> AltReducer {
    AltReducer(alt)
}

private func assertEqual(
    _ lhs: [InternalGrammar.Alt],
    _ rhs: [InternalGrammar.Alt],
    sourceLocation: SourceLocation = #_sourceLocation
) {
    guard lhs != rhs else {
        return success()
    }

    let diffAt =
        zip(lhs, rhs)
            .enumerated()
            .first {
                $0.element.0 != $0.element.1
            }?.offset

    let diffAtMessage =
        diffAt.map({ "First mismatch at index \($0)." })
            ?? "Lengths are unequal."

    fail(
        """
        Sequences of alts do not match. \(diffAtMessage)
        
        \(makeBuffer(lhs))

        is not equal to 

        \(makeBuffer(rhs))
        """,
        sourceLocation: sourceLocation
    )
}

private func makeBuffer(_ alts: [InternalGrammar.Alt]) -> String {
    "  " + alts.map(\.description).joined(separator: "\n  ")
}

private func parseAlt(
    _ alt: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> InternalGrammar.Alt {

    let tokenizer = GrammarRawTokenizer(source: alt)
    let parser = GrammarParser(raw: tokenizer)

    guard let alt = try parser.alt() else {
        throw parser.makeSyntaxError()
    }

    return InternalGrammar.Alt.from(alt)
}
