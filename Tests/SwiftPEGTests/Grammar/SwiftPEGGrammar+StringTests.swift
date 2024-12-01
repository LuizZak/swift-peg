import Testing

@testable import SwiftPEG

struct SwiftPEGGrammar_StringTests {
    @Test
    func grammarString_fromStringToken_tripleQuote_ignoresLeadingNewline() throws {
        let token = makeStringToken("\"\"\"\na\n\"\"\"")

        let result = try makeSut(fromStringToken: token)

        assertEqual(result.rawContents(), "a\n")
    }

    @Test
    func grammarString_fromStringToken_interpolatedEscapeSequence() throws {
        let token = makeStringToken(#""a\(bc)""#)

        let result = try makeSut(fromStringToken: token)

        assertEqual(result.rawContents(), #"a\(bc)"#)
    }
}

// MARK: - Test internals

private func makeSut(
    fromStringToken token: SwiftPEGGrammar.Token
) throws -> SwiftPEGGrammar.GrammarString {
    try .fromStringToken(token, 0)
}

private func makeStringToken(
    _ string: some StringProtocol
) -> SwiftPEGGrammar.Token {
    .string(Substring(string))
}
