import XCTest

@testable import SwiftPEG

class CommonAbstract_TokenAltTests: XCTestCase {
    func testIsPrefix() throws {
        let sut = try parseTokenAlt(#"""
        'a' ('b')+
        """#)

        try assertIsPrefix(parsing: "'a'", sut)
        try assertIsPrefix(parsing: "'a' 'b'", sut)
        try assertIsPrefix(parsing: "'a' ('b')", sut)
        try assertIsPrefix(parsing: "'a' ('b')+", sut)
        try assertIsPrefix(parsing: "'a' ('b')*", sut)
        try assertIsNotPrefix(parsing: "'a' ('b')+ 'c'", sut)
    }
}

// MARK: - Test internals
private typealias Sut = CommonAbstract.TokenAlt

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

    let lhs = try parseTokenAlt(lhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let rhs = try parseTokenAlt(rhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsPrefix(
    parsing lhs: String,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenAlt(lhs)
    let rhs = try parseTokenAlt(rhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    _ rhs: Sut,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenAlt(lhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let rhs = try parseTokenAlt(rhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenAlt(lhs)
    let rhs = try parseTokenAlt(rhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func parseTokenAlt(_ string: String) throws -> Sut {
    let tokenizer = GrammarRawTokenizer(source: string)
    let parser = GrammarParser(raw: tokenizer)

    guard let alt = try parser.tokenSyntaxAlt(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return alt
}
