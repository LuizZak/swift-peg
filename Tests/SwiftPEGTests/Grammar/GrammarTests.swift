import XCTest

@testable import SwiftPEG

class GrammarTests: XCTestCase {
    func testOperatorTokens() throws {
        try runTokenTest(literal: "(", kind: .leftParen)
        try runTokenTest(literal: ")", kind: .rightParen)
        try runTokenTest(literal: "{", kind: .leftBrace)
        try runTokenTest(literal: "}", kind: .rightBrace)
        try runTokenTest(literal: "[", kind: .leftSquare)
        try runTokenTest(literal: "]", kind: .rightSquare)
        try runTokenTest(literal: "<", kind: .leftAngle)
        try runTokenTest(literal: ">", kind: .rightAngle)
        try runTokenTest(literal: ":", kind: .colon)
        try runTokenTest(literal: ";", kind: .semicolon)
        try runTokenTest(literal: "|", kind: .bar)
        try runTokenTest(literal: "=", kind: .equals)
        try runTokenTest(literal: "~", kind: .tilde)
        try runTokenTest(literal: "*", kind: .star)
        try runTokenTest(literal: "+", kind: .plus)
        try runTokenTest(literal: "-", kind: .minus)
        try runTokenTest(literal: "?", kind: .questionMark)
        try runTokenTest(literal: "!", kind: .exclamationMark)
        try runTokenTest(literal: "&", kind: .ampersand)
        try runTokenTest(literal: ",", kind: .comma)
        try runTokenTest(literal: ".", kind: .period)
        try runTokenTest(literal: "@", kind: .at)
        try runTokenTest(literal: "$", kind: .dollarSign)
        try runTokenTest(literal: "/", kind: .forwardSlash)
        try runTokenTest(literal: "\\", kind: .backslash)
    }

    func testWhitespaceToken() throws {
        try runTokenTest(literal: " ", kind: .whitespace)
        try runTokenTest(literal: "\n", kind: .whitespace)
        try runTokenTest(literal: "\t", kind: .whitespace)
        try runTokenTest(literal: "\r", kind: .whitespace)
        try runTokenTest(literal: " \n", kind: .whitespace)
        try runTokenTest(literal: "\r \r\n", kind: .whitespace)
        try runTokenTest(literal: "\r \r\na", kind: .whitespace, expectedLength: 3)
        try runTokenTest(literal: "    +", kind: .whitespace, expectedLength: 4)
    }

    func testIdentifierToken() throws {
        try runTokenTest(literal: "anIdent", kind: .identifier)
        try runTokenTest(literal: "_anIdent", kind: .identifier)
        try runTokenTest(literal: "AnIdent", kind: .identifier)
        try runTokenTest(literal: "a123", kind: .identifier)
        try runTokenTest(literal: "a123_", kind: .identifier)
        try runTokenTest(literal: "a123_ ", kind: .identifier, expectedLength: 5)
    }

    func testDigitsToken() throws {
        try runTokenTest(literal: "0123", kind: .digits)
        try runTokenTest(literal: "0123a", kind: .digits, expectedLength: 4)
    }

    func testStringLiteralToken() throws {
        try runTokenTest(literal: "'t'", kind: .string)
        try runTokenTest(literal: #""a""#, kind: .string)
        try runTokenTest(literal: #"'\' b' c"#, kind: .string, expectedLength: 6)
        try runTokenTest(literal: #"'\\' b' c"#, kind: .string, expectedLength: 4)
    }

    func testStringLiteralToken_tripleQuote() throws {
        let terminator = "\"\"\""

        try runTokenTest(literal: "\(terminator)a\(terminator)", kind: .string)
        try runTokenTest(literal: "\(terminator)a\nb\(terminator)", kind: .string)
        try runTokenTest(literal: "\(terminator)\na\nb\(terminator)", kind: .string)

        var stream = StringStream(source: "\(terminator)\na\nb\n\(terminator)")
        let tok = try assertUnwrap(SwiftPEGGrammar.GrammarToken
            .from(stream: &stream)
        )

        assertEqual(tok.processedString, "\(terminator)\na\nb\n\(terminator)")
    }

    func testStringLiteralToken_interpolatedString() throws {
        try runTokenTest(literal: #""ab\(some code)cd""#, kind: .string)
        try runTokenTest(literal: #""ab\(some "co\(d)e")cd""#, kind: .string, expectedLength: 11)
        try runTokenTest(
            literal: #""ab\(some "co\\(d)e")cd""#,
            kind: .string,
            expectedLength: 11
        )
        try runTokenTest(
            literal: #""ab\(some "co\\(de")cd""#,
            kind: .string,
            expectedLength: 11
        )
    }
}

// MARK: - Test internals

private func runTokenTest(
    literal: String,
    kind: SwiftPEGGrammar.GrammarTokenKind,
    expectedLength: Int? = nil,
    file: StaticString = #file,
    line: UInt = #line
) throws {
    var stream = StringStream(source: literal)
    guard let token = try SwiftPEGGrammar.GrammarToken.from(stream: &stream) else {
        fail("Token literal '\(literal)' was not recognized by \(SwiftPEGGrammar.GrammarToken.self)", file: file, line: line)
        return
    }

    assertEqual(token.kind, kind, file: file, line: line)
    if expectedLength == nil {
        assertEqual(
            String(token.string),
            literal,
            message: "Token string does not match provided literal",
            file: file,
            line: line
        )
    }
    assertEqual(
        token.length,
        expectedLength ?? literal.count,
        message: "Token length does not match expected length",
        file: file,
        line: line
    )
}
