import RegexBuilder

extension TokenSyntaxInterpreter {
    /// Attempts to reduce the syntax of a token with a given name into a regular
    /// expression. If the token is not found, `nil` is returned.
    ///
    /// If the conversion was not possible, `nil` is returned, instead.
    func regexConversion(forTokenNamed name: String) -> Regex<Substring>? {
        if let token = self.token(named: name) {
            return regexConversion(for: token)
        }

        return nil
    }

    /// Attempts to reduce a given token's syntax into a regular expression.
    ///
    /// If the conversion was not possible, `nil` is returned, instead.
    func regexConversion(for token: InternalGrammar.TokenDefinition) -> Regex<Substring>? {
        cacheRegex(for: token)

        if let cached = self.cachedRegex(for: token) {
            return cached?.regex
        }

        return nil
    }

    /// If the given token definition can be converted into a regular expression
    /// object, this method stores that regular expression into `self.regexCache`,
    /// or stores `nil`, if the token is not regex-convertible.
    func cacheRegex(for token: InternalGrammar.TokenDefinition) {
        guard !regexCache.keys.contains(token.name) else {
            return
        }

        if let tokenSyntax = token.tokenSyntax {
            let result = regexConversion(for: tokenSyntax)
            regexCache[token.name] = result.map(RegexCacheEntry.init(regex:))
        } else {
            regexCache[token.name] = nil
        }
    }

    private func regexConversion(for tokenSyntax: CommonAbstract.TokenSyntax) -> Regex<Substring>? {
        let converter = RegexConverter(interpreter: self)

        guard let regex = converter.convert(tokenSyntax) else {
            return nil
        }

        return regex
            .repetitionBehavior(.possessive)
            .matchingSemantics(.graphemeCluster)
    }

    private class RegexConverter {
        let interpreter: TokenSyntaxInterpreter

        init(interpreter: TokenSyntaxInterpreter) {
            self.interpreter = interpreter
        }

        func escapeRegex(_ component: String) -> String {
            component
        }

        func convert(_ syntax: CommonAbstract.TokenSyntax) -> Regex<Substring>? {
            return convert(syntax.alts)
        }

        func convert(_ alts: [CommonAbstract.TokenAlt]) -> Regex<Substring>? {
            var result: [Regex<Substring>] = []
            for alt in alts {
                guard let alt = convert(alt) else {
                    return nil
                }

                result.append(alt)
            }

            return makeChoiceOf(result)
        }

        func convert(_ alt: CommonAbstract.TokenAlt) -> Regex<Substring>? {
            guard let items = convert(alt.items) else {
                return nil
            }

            if let trailExclusions = convert(exclusions: alt.trailExclusions) {
                return Regex {
                    items
                    trailExclusions
                }
            }

            return items
        }

        func convert(_ items: [CommonAbstract.TokenItem]) -> Regex<Substring>? {
            guard let first = items.first.flatMap(convert(_:)) else {
                return nil
            }

            var acc = first

            for item in items.dropFirst() {
                guard let next = convert(item) else {
                    return nil
                }

                acc = Regex {
                    acc
                    next
                }
            }

            return acc
        }

        func convert(_ item: CommonAbstract.TokenItem) -> Regex<Substring>? {
            switch item {
            case .zeroOrMore(let atoms):
                return parseZeroOrMore(atoms)?.regex

            case .oneOrMore(let atoms):
                return parseOneOrMore(atoms)?.regex

            case .optionalGroup(let atoms):
                return parseOptionalGroup(atoms)?.regex

            case .group(let atoms):
                return convertAnyOf(atoms)

            case .optionalAtom(let atom):
                if let atom = convert(atom) {
                    return RegexBuilder.Optionally(atom).regex
                }

                return nil

            case .atom(let atom):
                return convert(atom)
            }
        }

        func parseZeroOrMore(_ atoms: [CommonAbstract.TokenAtom]) -> RegexBuilder.ZeroOrMore<Substring>? {
            guard let atom = convertAnyOf(atoms) else {
                return nil
            }

            return RegexBuilder.ZeroOrMore(atom)
        }

        func parseOneOrMore(_ atoms: [CommonAbstract.TokenAtom]) -> RegexBuilder.OneOrMore<Substring>? {
            guard let atom = convertAnyOf(atoms) else {
                return nil
            }

            return RegexBuilder.OneOrMore(atom)
        }

        func parseOptionalGroup(_ atoms: [CommonAbstract.TokenAtom]) -> RegexBuilder.Optionally<Substring>? {
            guard let atom = convertAnyOf(atoms) else {
                return nil
            }

            return RegexBuilder.Optionally(atom)
        }

        func convertAnyOf(_ atoms: [CommonAbstract.TokenAtom]) -> Regex<Substring>? {
            var result: [Regex<Substring>] = []
            for atom in atoms {
                guard let converted = convert(atom) else {
                    return nil
                }

                result.append(converted)
            }

            guard var acc: Regex<Substring> = result.first else {
                return nil
            }

            for next in result.dropFirst() {
                acc = RegexBuilder.ChoiceOf {
                    acc
                    next
                }.regex
            }

            return acc
        }

        func convert(_ atom: CommonAbstract.TokenAtom) -> Regex<Substring>? {
            guard let terminal = convert(atom.terminal) else {
                return nil
            }

            guard !atom.excluded.isEmpty else {
                return terminal
            }

            if let exclusion = convert(exclusions: atom.excluded) {
                return Regex {
                    exclusion
                    terminal
                }
            }

            return nil
        }

        func convert(exclusions: some Collection<CommonAbstract.TokenExclusion>) -> Regex<Substring>? {
            var result: [RegexBuilder.NegativeLookahead<Substring>] = []
            for exclusion in exclusions {
                guard let exclusion = convert(exclusion) else {
                    return nil
                }

                result.append(exclusion)
            }

            if let first = result.first {
                var exclusion: Regex<Substring> = first.regex
                for next in result.dropFirst() {
                    exclusion = Regex {
                        exclusion
                        next
                    }
                }

                return exclusion
            }

            return nil
        }

        func convert(_ exclusion: CommonAbstract.TokenExclusion) -> RegexBuilder.NegativeLookahead<Substring>? {
            switch exclusion {
            case .identifier(let ident):
                if let conversion = interpreter.regexConversion(forTokenNamed: ident) {
                    return RegexBuilder.NegativeLookahead(conversion)
                }

                return nil

            case .literal(let literal):
                return RegexBuilder.NegativeLookahead(literal.contents)

            case .rangeLiteral(let low, let high):
                if let range = makeRange(start: low, end: high) {
                    return RegexBuilder.NegativeLookahead(range)
                }

                return nil
            }
        }

        func convert(_ terminal: CommonAbstract.TokenTerminal) -> Regex<Substring>? {
            switch terminal {
            case .characterPredicate:
                return nil

            case .identifier(let ident):
                if let conversion = interpreter.regexConversion(forTokenNamed: ident) {
                    return conversion
                }

                return nil

            case .literal(let literal):
                return literal.contents.regex

            case .rangeLiteral(let low, let high):
                return makeRange(start: low, end: high)?.regex

            case .any:
                return CharacterClass.anyGraphemeCluster.regex
            }
        }

        func makeChoiceOf(_ items: [Regex<Substring>]) -> Regex<Substring>? {
            guard var acc: Regex<Substring> = items.first else {
                return nil
            }

            for next in items.dropFirst() {
                acc = RegexBuilder.ChoiceOf<Substring> {
                    acc
                    next
                }.regex
            }

            return acc
        }

        func makeRange(start: CommonAbstract.DualString, end: CommonAbstract.DualString) -> CharacterClass? {
            let start = start.contents
            let end = end.contents

            guard let startChar = start.first, start.count == 1 else {
                return nil
            }
            guard let endChar = end.first, end.count == 1 else {
                return nil
            }

            return startChar...endChar
        }

        enum Error: Swift.Error {
            case unsupported
        }
    }
}
