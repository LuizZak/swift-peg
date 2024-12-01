import Testing

@testable import SwiftPEG

struct CommonAbstract_TokenSyntaxTests {
    @Test
    func isPrefix() throws {
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
    sourceLocation: SourceLocation = #_sourceLocation
) {
    if lhs.isPrefix(of: rhs) {
        return success()
    }

    fail("Expected \(lhs) to be prefix of \(rhs)", sourceLocation: sourceLocation)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    _ rhs: Sut,
    sourceLocation: SourceLocation = #_sourceLocation
) {
    if !lhs.isPrefix(of: rhs) {
        return success()
    }

    fail("Expected \(lhs) to not be prefix of \(rhs)", sourceLocation: sourceLocation)
}

private func assertIsPrefix(
    parsing lhs: String,
    _ rhs: Sut,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let lhs = try parseTokenSyntax(lhs)

    assertIsPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let rhs = try parseTokenSyntax(rhs)

    assertIsPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsPrefix(
    parsing lhs: String,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let lhs = try parseTokenSyntax(lhs)
    let rhs = try parseTokenSyntax(rhs)

    assertIsPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let rhs = try parseTokenSyntax(rhs)

    assertIsNotPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let lhs = try parseTokenSyntax(lhs)
    let rhs = try parseTokenSyntax(rhs)

    assertIsNotPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func parseTokenSyntax(_ string: String) throws -> Sut {
    let tokenizer = GrammarRawTokenizer(source: string)
    let parser = GrammarParser(raw: tokenizer)

    guard let syntax = try parser.tokenSyntax(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return syntax
}
