import XCTest

@testable import SwiftPEG

class InternalGrammarTests: XCTestCase {
    func testAction_description() {
        let tokens = makeTokenSequence([
            .whitespace(" "), .identifier("a"), .period, .identifier("b"), .whitespace(" "),
        ])
        let action = makeAction(tokens)

        let result = InternalGrammar.Action.from(action)

        assertEqual(result.description, "{ a.b }")
    }

    func testAlt_description() {
        let tokens = makeTokenSequence([
            .whitespace(" "), .identifier("a"), .period, .identifier("b"), .whitespace(" "),
        ])
        let action = makeAction(tokens)
        let alt = makeAlt(namedItems: [], action: action)

        let result = InternalGrammar.Alt.from(alt)

        assertEqual(result.description, " { a.b }")
    }
}

// MARK: - Test internals

private func makeAlt(
    namedItems: [SwiftPEGGrammar.NamedItem],
    action: SwiftPEGGrammar.Action? = nil,
    failAction: SwiftPEGGrammar.Action? = nil
) -> SwiftPEGGrammar.Alt {
    .init(namedItems: namedItems, action: action, failAction: failAction)
}

private func makeAction(
    _ tokens: SwiftPEGGrammar.TokenSequence
) -> SwiftPEGGrammar.Action {
    .init(balancedTokens: tokens)
}

private func makeTokenSequence(
    _ tokens: [SwiftPEGGrammar.Token]
) -> SwiftPEGGrammar.TokenSequence {
    .from(tokens.map(SwiftPEGGrammar.TokenSequence.from))
}
