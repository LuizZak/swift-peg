@testable import SwiftPEG

class TestInterpreterDelegate: GrammarInterpreter.Delegate {
    var tokenMatchers: [TokenProduction] = []
    var altMatchers: [AltProduction] = []

    @discardableResult
    func addToken(_ token: StringMatcher, named name: String) -> Self {
        let token = TokenProduction(pattern: token, value: name, literal: name, name: name)
        tokenMatchers.append(token)
        return self
    }

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
    func addAlt(action: StringMatcher, _ closure: @escaping (GrammarInterpreter.AltContext) throws -> Any?) -> Self {
        let alt = AltProduction(action: action, closure: closure)
        altMatchers.append(alt)
        return self
    }

    @discardableResult
    func addAlt(action: StringMatcher, value: Any?) -> Self {
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
            self.init(
                pattern: .prefix(.string(value)),
                value: value,
                literal: value,
                name: value
            )
        }

        init(pattern: StringMatcher, value: String, literal: String, name: String) {
            self.pattern = pattern
            self.value = value
            self.literal = literal
            self.name = name
        }
    }

    struct AltProduction {
        var action: StringMatcher
        var closure: (GrammarInterpreter.AltContext) throws -> Any?
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
    ) throws -> Any? {

        produceResult_callCount += 1
        produceResult_calls.append((alt, context))

        let matchTarget = alt.action?.string ?? alt.description

        for matcher in altMatchers {
            if matcher.action.matches(matchTarget) {
                return try matcher.closure(context)
            }
        }

        throw Error.unknownAlt(alt)
    }

    var produceToken_callCount: Int = 0
    var produceToken_calls: [(Substring)] = []
    func produceToken(
        string: Substring
    ) throws -> GrammarInterpreter.Token? {

        produceToken_callCount += 1
        produceToken_calls.append((string))

        if string.isEmpty { return nil }

        for token in tokenMatchers {
            if let match = token.pattern.match(in: string) {
                return (match, match.count)
            }
        }

        throw Error.unknownToken
    }

    var tokenResult_matchesTokenName_callCount: Int = 0
    var tokenResult_matchesTokenName_calls: [(GrammarInterpreter.Token, String)] = []
    func tokenResult(
        _ tokenResult: GrammarInterpreter.Token,
        matchesTokenName tokenName: String
    ) -> Bool {

        tokenResult_matchesTokenName_callCount += 1
        tokenResult_matchesTokenName_calls.append((tokenResult, tokenName))

        guard let tokenString = _string(from: tokenResult.token) else {
            return false
        }

        for token in tokenMatchers {
            if token.value == tokenString && token.name == tokenName {
                return true
            }
        }
        for token in tokenMatchers {
            if token.pattern.matches(tokenString), token.name == tokenName {
                return true
            }
        }

        return false
    }

    var tokenResult_matchesTokenLiteral_callCount: Int = 0
    var tokenResult_matchesTokenLiteral_calls: [(GrammarInterpreter.Token, String)] = []
    func tokenResult(
        _ tokenResult: GrammarInterpreter.Token,
        matchesTokenLiteral tokenLiteral: String
    ) -> Bool {

        tokenResult_matchesTokenLiteral_callCount += 1
        tokenResult_matchesTokenLiteral_calls.append((tokenResult, tokenLiteral))

        guard let tokenString = _string(from: tokenResult.token) else {
            return false
        }

        for token in tokenMatchers {
            if token.literal == tokenString {
                return true
            }
        }

        return false
    }

    private func _string(from tokenValue: Any) -> String? {
        if let value = tokenValue as? String {
            return value
        }
        if let value = tokenValue as? Substring {
            return String(value)
        }
        return nil
    }
}
