import XCTest

@testable import SwiftPEG

class CommonAbstract_TokenSyntaxTests: XCTestCase {
    func testIsPrefix() throws {
        let sut = try parseTokenSyntax(#"""
        | 'a' ('b')+
        | 'a' 'c'
        """#)

        try assertIsPrefix(parsing: "'a'", sut)
    }
}

// MARK: - Test internals
private typealias Sut = CommonAbstract.TokenSyntax

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

    let lhs = try parseTokenSyntax(lhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let rhs = try parseTokenSyntax(rhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsPrefix(
    parsing lhs: String,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenSyntax(lhs)
    let rhs = try parseTokenSyntax(rhs)

    assertIsPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let rhs = try parseTokenSyntax(rhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    parsing rhs: String,
    file: StaticString = #file,
    line: UInt = #line
) throws {

    let lhs = try parseTokenSyntax(lhs)
    let rhs = try parseTokenSyntax(rhs)

    assertIsNotPrefix(lhs, rhs, file: file, line: line)
}

private func parseTokenSyntax(_ string: String) throws -> Sut {
    let tokenizer = GrammarRawTokenizer(source: string)
    let parser = GrammarParser(raw: tokenizer)

    guard let syntax = try parser.tokenSyntax(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return syntax
}
