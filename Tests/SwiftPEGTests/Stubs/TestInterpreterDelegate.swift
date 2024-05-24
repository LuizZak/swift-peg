@testable import SwiftPEG

class TestInterpreterDelegate {
    var tokenMatchers: [TokenProduction] = []
    var altMatchers: [AltProduction] = []

    @discardableResult
    func addToken(_ token: String, named name: String) -> Self {
        var token = TokenProduction(stringLiteral: token)
        token.name = name
        tokenMatchers.append(token)
        return self
    }

    @discardableResult
    func addToken(_ token: String) -> Self {
        let token = TokenProduction(stringLiteral: token)
        tokenMatchers.append(token)
        return self
    }

    @discardableResult
    func addTokens(_ tokens: String...) -> Self {
        for token in tokens {
            _=addToken(token)
        }
        return self
    }

    @discardableResult
    func addAlt(action: StringMatcher, _ closure: @escaping (GrammarInterpreter.AltContext) -> Any) -> Self {
        let alt = AltProduction(action: action, closure: closure)
        altMatchers.append(alt)
        return self
    }

    @discardableResult
    func addAlt(action: StringMatcher, value: Any) -> Self {
        return addAlt(action: action, { _ in value })
    }

    struct TokenProduction: ExpressibleByStringLiteral {
        var pattern: StringMatcher
        var value: String
        var literal: String
        var name: String

        init(stringLiteral value: String) {
            self.pattern = .prefix(.string(value))
            self.value = value
            self.literal = value
            self.name = value
        }
    }

    struct AltProduction {
        var action: StringMatcher
        var closure: (GrammarInterpreter.AltContext) -> Any
    }

    enum Error: Swift.Error {
        case unknownAlt
        case unknownToken
    }
}

extension TestInterpreterDelegate: GrammarInterpreter.Delegate {
    func produceResult(
        for alt: InternalGrammar.Alt,
        context: GrammarInterpreter.AltContext
    ) throws -> Any {

        guard let action = alt.action else {
            throw Error.unknownAlt
        }

        for matcher in altMatchers {
            if matcher.action.matches(action.string) {
                return matcher.closure(context)
            }
        }
        
        throw Error.unknownAlt
    }

    func produceToken(
        string: Substring
    ) throws -> GrammarInterpreter.TokenResult? {

        if string.isEmpty { return nil }

        for token in tokenMatchers {
            if token.pattern.matches(string) {
                return (token.value, token.value.count)
            }
        }

        throw Error.unknownToken
    }

    func tokenResult(
        _ tokenResult: GrammarInterpreter.TokenResult,
        matchesTokenName tokenName: String
    ) -> Bool {

        guard let tokenString = tokenResult.0 as? String else {
            return false
        }

        for token in tokenMatchers {
            if token.value == tokenString && token.name == tokenName {
                return true
            }
        }

        return false
    }

    func tokenResult(
        _ tokenResult: GrammarInterpreter.TokenResult,
        matchesTokenLiteral tokenLiteral: String
    ) -> Bool {

        guard let tokenString = tokenResult.0 as? String else {
            return false
        }

        for token in tokenMatchers {
            if token.literal == tokenString {
                return true
            }
        }

        return false
    }
}
