@testable import SwiftPEG

class TestInterpreterDelegate: GrammarInterpreter.Delegate {
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
    func addAlt(action: StringMatcher, _ closure: @escaping (GrammarInterpreter.AltContext) throws -> Any) -> Self {
        let alt = AltProduction(action: action, closure: closure)
        altMatchers.append(alt)
        return self
    }

    @discardableResult
    func addAlt(action: StringMatcher, value: Any) -> Self {
        return addAlt(action: action, { _ in value })
    }

    // MARK: Assertion helpers

    func countOfAlts(actionName: String) -> Int {
        var total = 0
        for call in produceResult_calls {
            if call.0.action?.string == actionName {
                total += 1
            }
        }

        return total
    }

    // MARK: Structures

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
        var closure: (GrammarInterpreter.AltContext) throws -> Any
    }

    enum Error: Swift.Error {
        case unknownAlt(InternalGrammar.Alt)
        case unknownToken
    }

    // MARK: GrammarInterpreter.Delegate

    var produceResult_callCount: Int = 0
    var produceResult_calls: [(InternalGrammar.Alt, GrammarInterpreter.AltContext)] = []
    func produceResult(
        for alt: InternalGrammar.Alt,
        context: GrammarInterpreter.AltContext
    ) throws -> Any {

        produceResult_callCount += 1
        produceResult_calls.append((alt, context))

        guard let action = alt.action else {
            throw Error.unknownAlt(alt)
        }

        for matcher in altMatchers {
            if matcher.action.matches(action.string) {
                return try matcher.closure(context)
            }
        }
        
        throw Error.unknownAlt(alt)
    }

    var produceToken_callCount: Int = 0
    var produceToken_calls: [(Substring)] = []
    func produceToken(
        string: Substring
    ) throws -> GrammarInterpreter.TokenResult? {

        produceToken_callCount += 1
        produceToken_calls.append((string))

        if string.isEmpty { return nil }

        for token in tokenMatchers {
            if token.pattern.matches(string) {
                return (token.value, token.value.count)
            }
        }

        throw Error.unknownToken
    }

    var tokenResult_matchesTokenName_callCount: Int = 0
    var tokenResult_matchesTokenName_calls: [(GrammarInterpreter.TokenResult, String)] = []
    func tokenResult(
        _ tokenResult: GrammarInterpreter.TokenResult,
        matchesTokenName tokenName: String
    ) -> Bool {

        tokenResult_matchesTokenName_callCount += 1
        tokenResult_matchesTokenName_calls.append((tokenResult, tokenName))

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

    var tokenResult_matchesTokenLiteral_callCount: Int = 0
    var tokenResult_matchesTokenLiteral_calls: [(GrammarInterpreter.TokenResult, String)] = []
    func tokenResult(
        _ tokenResult: GrammarInterpreter.TokenResult,
        matchesTokenLiteral tokenLiteral: String
    ) -> Bool {

        tokenResult_matchesTokenLiteral_callCount += 1
        tokenResult_matchesTokenLiteral_calls.append((tokenResult, tokenLiteral))

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
