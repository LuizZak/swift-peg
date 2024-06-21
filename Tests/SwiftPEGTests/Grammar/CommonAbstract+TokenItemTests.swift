import XCTest

@testable import SwiftPEG

class CommonAbstract_TokenItemTests: XCTestCase {
    // Ensure all atoms are prefixes of the 'any' terminal
    func testIsPrefix_any() throws {
        let sut = makeSut(zeroOrMore: [makeAtom(.any)])

        assertIsPrefix(makeSut(terminal: .any), sut)
        assertIsPrefix(makeSut(terminal: .rangeLiteral("a", "z")), sut)
        assertIsPrefix(makeSut(terminal: .literal("a")), sut)
        try assertIsPrefix(parsing: #"("a"..."z")+"#, sut)
        try assertIsPrefix(parsing: #"("_" | !"k" "a"..."z")*"#, sut)
    }

    func testIsPrefix_multiCharacterLiterals() throws {
        try assertIsPrefix(parsing: #""a""#, parsing: #""abc""#)
        try assertIsPrefix(parsing: #""ab""#, parsing: #"("a" | "abc")"#)
        try assertIsPrefix(parsing: #"("a" | "ab")"#, parsing: #""abc""#)
        try assertIsPrefix(parsing: #"("ab" | "bc")"#, parsing: #"("abc" | "bcd")"#)
        try assertIsNotPrefix(parsing: #"("ab" | "bc")"#, parsing: #"("abc")"#)
        try assertIsNotPrefix(parsing: #"("ab" | "bc")"#, parsing: #""abc""#)
    }

    func testIsPrefix_zeroOrMore() throws {
        let sut = makeSut(zeroOrMore: ["a", "b", "c"])

        assertIsPrefix(sut, makeSut(zeroOrMore: ["a", "b", "c"]))
        assertIsPrefix(sut, makeSut(oneOrMore: ["a", "b", "c"]))
        assertIsNotPrefix(sut, makeSut(group: ["a", "b", "c"]))
        assertIsNotPrefix(sut, makeSut(atom: "a"))
        assertIsNotPrefix(sut, makeSut(atom: "abc"))
    }

    func testIsPrefix_oneOrMore() throws {
        let sut = makeSut(oneOrMore: ["a", "b", "c"])

        assertIsPrefix(sut, makeSut(zeroOrMore: ["a", "b", "c"]))
        assertIsPrefix(sut, makeSut(oneOrMore: ["a", "b", "c"]))
        assertIsNotPrefix(sut, makeSut(group: ["a", "b", "c"]))
        assertIsNotPrefix(sut, makeSut(atom: "a"))
        assertIsNotPrefix(sut, makeSut(atom: "abc"))
    }

    func testIsPrefix_optionalGroup() throws {
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"'abc'"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abc')"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abc')+"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abc')*"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abc')?"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"'abcd'"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abcd')"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abcd')+"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abcd')*"#)
        try assertIsPrefix(parsing: #"('abc')?"#, parsing: #"('abcd')?"#)
        try assertIsPrefix(parsing: #"('ab' | 'bc')?"#, parsing: #"('abcd' | 'bcde')"#)
        try assertIsPrefix(parsing: #"('ab' | 'bc')?"#, parsing: #"('abcd' | 'bcde')+"#)
        try assertIsPrefix(parsing: #"('ab' | 'bc')?"#, parsing: #"('abcd' | 'bcde')*"#)
        try assertIsPrefix(parsing: #"('ab' | 'bc')?"#, parsing: #"('abcd' | 'bcde')?"#)
        try assertIsNotPrefix(parsing: #"('abcd')?"#, parsing: #"'abc'"#)
        try assertIsNotPrefix(parsing: #"('abcd')?"#, parsing: #"'abc'?"#)
        try assertIsNotPrefix(parsing: #"('abcd')?"#, parsing: #"('abc')?"#)
        try assertIsNotPrefix(parsing: #"('abcd')?"#, parsing: #"('abc')*"#)
        try assertIsNotPrefix(parsing: #"('abcd')?"#, parsing: #"('abc')+"#)
        try assertIsNotPrefix(parsing: #"('ab' | 'bc')?"#, parsing: #"'abcd'"#)
    }

    func testIsPrefix_group() throws {
        let sut = makeSut(group: ["a", "b", "c"])

        assertIsPrefix(sut, makeSut(group: ["a", "b", "c", "d"]))
        assertIsPrefix(sut, makeSut(terminal: .any))
        assertIsPrefix(sut, makeSut(oneOrMore: ["a", "b", "c"]))
        assertIsPrefix(sut, makeSut(oneOrMore: ["a"..."c"]))
        assertIsPrefix(sut, makeSut(zeroOrMore: ["a", "b", "c"]))
        try assertIsPrefix(parsing: #"("a"..."d")"#, makeSut(oneOrMore: ["a"..."d"]))
        assertIsNotPrefix(sut, makeSut(atom: "abc"))
        assertIsNotPrefix(sut, makeSut(oneOrMore: [.init(excluded: [.literal("b")], terminal: "a"..."c")]))
    }

    func testIsPrefix_optionalAtom() throws {
        try assertIsPrefix(parsing: #"'a'?"#, parsing: #"'a'"#)
        try assertIsPrefix(parsing: #"'a'?"#, parsing: #"('a')"#)
        try assertIsPrefix(parsing: #"'abc'?"#, parsing: #"'abcd'"#)
        try assertIsPrefix(parsing: #"'abc'?"#, parsing: #"('abcd')"#)
        try assertIsNotPrefix(parsing: #"'abc'?"#, parsing: #"'ab'"#)
        try assertIsNotPrefix(parsing: #"'abc'?"#, parsing: #"('ab')"#)
    }

    func testIsPrefix_atom() throws {
        let sut = makeSut(atom: "abc")

        assertIsPrefix(sut, makeSut(atom: "abcd"))
        assertIsPrefix(sut, makeSut(group: ["abcd", "bcde"]))
        assertIsPrefix(sut, makeSut(zeroOrMore: ["abcd"]))
        assertIsPrefix(sut, makeSut(oneOrMore: ["abcd"]))
        assertIsPrefix(sut, makeSut(oneOrMore: [.init(terminal: .any)]))
        assertIsPrefix(sut, makeSut(oneOrMore: [.init(excluded: [.literal("abcd")], terminal: .any)]))
        try assertIsPrefix(sut, parsing: #"'abcd'?"#)
        assertIsNotPrefix(sut, makeSut(oneOrMore: [.init(excluded: [.literal("a")], terminal: .any)]))
        assertIsNotPrefix(sut, makeSut(oneOrMore: [.init(excluded: [.literal("abc")], terminal: .any)]))
        assertIsNotPrefix(sut, makeSut(group: ["a", "b", "c", "d"]))
        assertIsNotPrefix(sut, makeSut(terminal: .any))
        assertIsNotPrefix(sut, makeSut(oneOrMore: ["a", "b", "c"]))
        assertIsNotPrefix(sut, makeSut(oneOrMore: ["a"..."c"]))
        assertIsNotPrefix(sut, makeSut(zeroOrMore: ["a", "b", "c"]))
        assertIsNotPrefix(sut, makeSut(oneOrMore: [.init(excluded: [.literal("b")], terminal: "a"..."c")]))
    }
}

// MARK: - Test internals
private typealias Sut = CommonAbstract.TokenItem

private func makeSut(zeroOrMore items: [CommonAbstract.TokenAtom]) -> CommonAbstract.TokenItem {
    .zeroOrMore(items)
}

private func makeSut(oneOrMore items: [CommonAbstract.TokenAtom]) -> CommonAbstract.TokenItem {
    .oneOrMore(items)
}

private func makeSut(group items: [CommonAbstract.TokenAtom]) -> CommonAbstract.TokenItem {
    .group(items)
}

private func makeSut(atom: CommonAbstract.TokenAtom) -> CommonAbstract.TokenItem {
    .atom(atom)
}

private func makeSut(terminal: CommonAbstract.TokenTerminal) -> CommonAbstract.TokenItem {
    .atom(makeAtom(terminal))
}

private func makeAtom(_ terminal: CommonAbstract.TokenTerminal) -> CommonAbstract.TokenAtom {
    .init(terminal: terminal)
}

private func assertIsPrefix(
    _ lhs: Sut,
    _ rhs: Sut,
    file: StaticString = #file,
    line: UInt = #line
) {
    if lhs.isPrefix(of: rhs) {
        return success()
    }

    fail("Expected \(lhs) to be prefix of \(rhs)", file: file, line: line)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    _ rhs: Sut,
    file: StaticString = #file,
    line: UInt = #line
) {
    if !lhs.isPrefix(of: rhs) {
        return success()
    }

    fail("Expected \(lhs) to not be prefix of \(rhs)", file: file, line: line)
}

private func assertIsPrefix(
    parsing lhs: String,
    _ rhs: Sut,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenItem(lhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let rhs = try parseTokenItem(rhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsPrefix(
    parsing lhs: String,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenItem(lhs)
    let rhs = try parseTokenItem(rhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let rhs = try parseTokenItem(rhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenItem(lhs)
    let rhs = try parseTokenItem(rhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func parseTokenItem(_ string: String) throws -> Sut {
    let tokenizer = GrammarRawTokenizer(source: string)
    let parser = GrammarParser(raw: tokenizer)

    guard let item = try parser.tokenSyntaxItem(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return item
}
