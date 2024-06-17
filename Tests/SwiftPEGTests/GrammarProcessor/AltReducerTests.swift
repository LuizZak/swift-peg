import XCTest

@testable import SwiftPEG

class AltReducerTests: XCTestCase {
    func testReduce_staticAlt() throws {
        let alt = try parseAlt("""
        a b 'c'
        """)
        let sut = makeSut(alt)

        let result = sut.reduced()

        assertEqual(result, alt)
    }

    func testReduce_optionalItem() throws {
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

    func testReduce_optionalItems() throws {
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

    func testReduce_zeroOrMore() throws {
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

    func testReduce_gatherSeparator_returnsOneOrMoreNode() throws {
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

    func testReduce_gatherNode_elidesGather() throws {
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

    func testPermute_zeroOrMore() throws {
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

    func testPermute_leadingOptional() throws {
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

    func testPermute_optionalsInGroup() throws {
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

    func testPermute_optionalsInGroup_flattensEmptyGroups() throws {
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

    func testPermute_depthLimit() throws {
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
    file: StaticString = #file,
    line: UInt = #line
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
        file: file,
        line: line
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
