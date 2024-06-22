extension TokenSyntaxInterpreter: GrammarInterpreter.TokenDelegate {
    public func produceToken(
        string: Substring
    ) throws -> GrammarInterpreter.Token? {
        var stream = StringStream(source: string)

        guard let token = try self.parseToken(from: &stream) else {
            return nil
        }

        return (token: token.substring, length: token.substring.count)
    }

    public func tokenResult(
        _ tokenResult: GrammarInterpreter.Token,
        matchesTokenName tokenName: String
    ) -> Bool {
        guard let token = token(named: tokenName) else {
            return false
        }

        switch tokenResult.token {
        case let tokenValue as Substring:
            return tokenValue.count == tokenResult.length
                && tokenFullyParses(token, input: tokenValue)

        case let tokenValue as String:
            return tokenValue.count == tokenResult.length
                && tokenFullyParses(token, input: tokenValue)

        default:
            return false
        }
    }

    public func tokenResult(
        _ tokenResult: GrammarInterpreter.Token,
        matchesTokenLiteral tokenLiteral: String
    ) -> Bool {
        guard let token = token(forLiteral: tokenLiteral) else {
            return false
        }

        switch tokenResult.token {
        case let tokenValue as Substring:
            return tokenValue.count == tokenResult.length
                && tokenFullyParses(token, input: tokenValue)

        case let tokenValue as String:
            return tokenValue.count == tokenResult.length
                && tokenFullyParses(token, input: tokenValue)

        default:
            return false
        }
    }
}
