import Testing

@testable import SwiftPEG

struct CommonAbstract_TokenAltTests {
    @Test
    func isPrefix() throws {
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

    let lhs = try parseTokenAlt(lhs)

    assertIsPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let rhs = try parseTokenAlt(rhs)

    assertIsPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsPrefix(
    parsing lhs: String,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let lhs = try parseTokenAlt(lhs)
    let rhs = try parseTokenAlt(rhs)

    assertIsPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    _ rhs: Sut,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let lhs = try parseTokenAlt(lhs)

    assertIsNotPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsNotPrefix(
    _ lhs: Sut,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let rhs = try parseTokenAlt(rhs)

    assertIsNotPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func assertIsNotPrefix(
    parsing lhs: String,
    parsing rhs: String,
    sourceLocation: SourceLocation = #_sourceLocation
) throws {

    let lhs = try parseTokenAlt(lhs)
    let rhs = try parseTokenAlt(rhs)

    assertIsNotPrefix(lhs, rhs, sourceLocation: sourceLocation)
}

private func parseTokenAlt(_ string: String) throws -> Sut {
    let tokenizer = GrammarRawTokenizer(source: string)
    let parser = GrammarParser(raw: tokenizer)

    guard let alt = try parser.tokenSyntaxAlt(), tokenizer.isEOF else {
        throw parser.makeSyntaxError()
    }

    return alt
}
