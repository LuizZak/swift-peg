import XCTest

@testable import SwiftPEG

class MetagrammarTests: XCTestCase {
    func testOperatorTokens() {
        runTokenTest(literal: "(", kind: .leftParen)
        runTokenTest(literal: ")", kind: .rightParen)
        runTokenTest(literal: "{", kind: .leftBrace)
        runTokenTest(literal: "}", kind: .rightBrace)
        runTokenTest(literal: "[", kind: .leftSquare)
        runTokenTest(literal: "]", kind: .rightSquare)
        runTokenTest(literal: "<", kind: .leftAngle)
        runTokenTest(literal: ">", kind: .rightAngle)
        runTokenTest(literal: ":", kind: .colon)
        runTokenTest(literal: ";", kind: .semicolon)
        runTokenTest(literal: "|", kind: .bar)
        runTokenTest(literal: "=", kind: .equals)
        runTokenTest(literal: "~", kind: .tilde)
        runTokenTest(literal: "*", kind: .star)
        runTokenTest(literal: "+", kind: .plus)
        runTokenTest(literal: "-", kind: .minus)
        runTokenTest(literal: "?", kind: .questionMark)
        runTokenTest(literal: "!", kind: .exclamationMark)
        runTokenTest(literal: "&", kind: .ampersand)
        runTokenTest(literal: ",", kind: .comma)
        runTokenTest(literal: ".", kind: .period)
        runTokenTest(literal: "@", kind: .at)
        runTokenTest(literal: "$", kind: .dollarSign)
        runTokenTest(literal: "/", kind: .forwardSlash)
        runTokenTest(literal: "\\", kind: .backslash)
    }

    func testWhitespaceToken() {
        runTokenTest(literal: " ", kind: .whitespace)
        runTokenTest(literal: "\n", kind: .whitespace)
        runTokenTest(literal: "\t", kind: .whitespace)
        runTokenTest(literal: "\r", kind: .whitespace)
        runTokenTest(literal: " \n", kind: .whitespace)
        runTokenTest(literal: "\r \r\n", kind: .whitespace)
        runTokenTest(literal: "\r \r\na", kind: .whitespace, expectedLength: 3)
        runTokenTest(literal: "    +", kind: .whitespace, expectedLength: 4)
    }

    func testIdentifierToken() {
        runTokenTest(literal: "anIdent", kind: .identifier)
        runTokenTest(literal: "_anIdent", kind: .identifier)
        runTokenTest(literal: "AnIdent", kind: .identifier)
        runTokenTest(literal: "a123", kind: .identifier)
        runTokenTest(literal: "a123_", kind: .identifier)
        runTokenTest(literal: "a123_ ", kind: .identifier, expectedLength: 5)
    }

    func testDigitsToken() {
        runTokenTest(literal: "0123", kind: .digits)
        runTokenTest(literal: "0123a", kind: .digits, expectedLength: 4)
    }

    func testStringLiteralToken() {
        runTokenTest(literal: "'t'", kind: .string)
        runTokenTest(literal: #""a""#, kind: .string)
        runTokenTest(literal: #"'\' b' c"#, kind: .string, expectedLength: 6)
        runTokenTest(literal: #"'\\' b' c"#, kind: .string, expectedLength: 4)
    }

    func testStringLiteralToken_tripleQuote() throws {
        let terminator = "\"\"\""

        runTokenTest(literal: "\(terminator)a\(terminator)", kind: .string)
        runTokenTest(literal: "\(terminator)a\nb\(terminator)", kind: .string)
        runTokenTest(literal: "\(terminator)\na\nb\(terminator)", kind: .string)
        
        let tok = try assertUnwrap(Metagrammar.MetagrammarToken
            .from(string: "\(terminator)\na\nb\n\(terminator)")
        )

        assertEqual(tok.processedString, "a\nb\n")
    }
}

// MARK: - Test internals

private func runTokenTest(
    literal: String,
    kind: Metagrammar.MetagrammarTokenKind,
    expectedLength: Int? = nil,
    file: StaticString = #file,
    line: UInt = #line
) {
    guard let token = Metagrammar.MetagrammarToken.from(string: literal[...]) else {
        fail("Token literal '\(literal)' was not recognized by \(Metagrammar.MetagrammarToken.self)", file: file, line: line)
        return
    }

    assertEqual(token.kind, kind, file: file, line: line)
    if expectedLength == nil {
        assertEqual(
            String(token.string),
            literal,
            "Token string does not match provided literal",
            file: file,
            line: line
        )
    }
    assertEqual(
        token.length,
        expectedLength ?? literal.count,
        "Token length does not match expected length",
        file: file,
        line: line
    )
}
