import Testing

@testable import SwiftPEG

struct InternalGrammarTests {
    @Test
    func action_description() {
        let tokens = makeTokenSequence([
            .whitespace(" "), .identifier("a"), .period, .identifier("b"), .whitespace(" "),
        ])
        let action = makeAction(tokens)

        let result = InternalGrammar.Action.from(action)

        assertEqual(result.description, "{ a.b }")
    }

    @Test
    func alt_description() {
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
    attributes: [SwiftPEGGrammar.ActionAttribute] = [],
    _ tokens: SwiftPEGGrammar.TokenSequence
) -> SwiftPEGGrammar.Action {
    .init(attributes: attributes, balancedTokens: tokens)
}

private func makeTokenSequence(
    _ tokens: [SwiftPEGGrammar.Token]
) -> SwiftPEGGrammar.TokenSequence {
    .from(tokens.map(SwiftPEGGrammar.TokenSequence.from))
}
