/// Provides interpreting capabilities to `InternalGrammar.TokenDefinition` objects.
public class TokenSyntaxInterpreter {
    let tokenDefinitions: [InternalGrammar.TokenDefinition]

    public init(tokenDefinitions: [InternalGrammar.TokenDefinition]) {
        self.tokenDefinitions = tokenDefinitions
    }

    /// Attempts to parse the contents of the given stream with one of the known
    /// token definitions, returning the token parsing that consumed the most
    /// characters from the stream.
    public func parseToken<StringType: StringProtocol>(
        from stream: inout StringStream<StringType>
    ) throws -> StringType.SubSequence? {

        let initialState = stream.save()

        var longest: (StringStream<StringType>.State, StringType.SubSequence)?
        stream.markSubstringStart()

        let state = stream.save()

        for token in tokenDefinitions {
            guard try parse(token, from: &stream) else {
                continue
            }

            let result = stream.substring
            if let l = longest {
                if result.count > l.1.count {
                    longest = (stream.save(), result)
                }
            } else {
                longest = (stream.save(), result)
            }

            stream.restore(state)
        }

        if let longest {
            stream.restore(longest.0)
            return longest.1
        }

        stream.restore(initialState)
        return nil
    }

    /// Attempts to parse a token with a given name from the given input stream's
    /// position, returning `true` if the parsing succeeded, otherwise, returns
    /// `false` and restores the stream back to the state it was provided in.
    ///
    /// - throws: `Error.unknownTokenName` if `name` is not a token or fragment
    /// name in the list of tokens this interpreter was initialized with.
    /// - throws: `Error.missingSyntax` if the token has a nil `tokenSyntax`.
    public func parse<StringType: StringProtocol>(
        tokenNamed name: String,
        from stream: inout StringStream<StringType>
    ) throws -> Bool {
        guard let token = token(named: name) else {
            throw Error.unknownTokenName(name)
        }

        return try parse(token, from: &stream)
    }

    /// Attempts to parse a given token definition's syntax from a given input
    /// stream's position, returning `true` if the parsing succeeded, otherwise,
    /// returns `false` and restores the stream back to the state it was provided
    /// in.
    ///
    /// - throws: `Error.missingSyntax` if the token has a nil `tokenSyntax`.
    func parse<StringType: StringProtocol>(
        _ token: InternalGrammar.TokenDefinition,
        from stream: inout StringStream<StringType>
    ) throws -> Bool {
        guard let syntax = token.tokenSyntax else {
            throw Error.missingSyntax(tokenName: token.name)
        }

        return try parse(syntax, from: &stream)
    }

    /// Attempts to parse a given token syntax from a given input stream's position,
    /// returning `true` if the parsing succeeded, otherwise, returns `false`
    /// and restores the stream back to the state it was provided in.
    func parse<StringType: StringProtocol>(
        _ syntax: CommonAbstract.TokenSyntax,
        from stream: inout StringStream<StringType>
    ) throws -> Bool {
        let parser = InternalParser(interpreter: self, stream: stream)

        if try parser.parse(syntax) {
            stream = parser.stream
            return true
        }

        return false
    }

    private func token(named name: String) -> InternalGrammar.TokenDefinition? {
        tokenDefinitions.first(where: { $0.name == name })
    }

    private class InternalParser<StringType: StringProtocol> {
        typealias Stream = StringStream<StringType>

        let interpreter: TokenSyntaxInterpreter
        let startState: Stream.State
        var stream: Stream

        init(interpreter: TokenSyntaxInterpreter, stream: Stream) {
            self.interpreter = interpreter
            self.stream = stream
            self.startState = stream.save()
        }

        func parse(_ syntax: CommonAbstract.TokenSyntax) throws -> Bool {
            guard try parse(syntax.alts) else {
                return false
            }

            return true
        }

        func parse(_ alts: [CommonAbstract.TokenAlt]) throws -> Bool {
            let start = stream.save()
            for alt in alts {
                guard try parse(alt) else {
                    stream.restore(start)
                    return false
                }
            }

            return true
        }

        func parse(_ alt: CommonAbstract.TokenAlt) throws -> Bool {
            let start = stream.save()

            guard try parse(alt.items) else {
                return false
            }

            for exclusion in alt.trailExclusions {
                guard try passes(exclusion) else {
                    stream.restore(start)
                    return false
                }
            }

            return true
        }

        func parse(_ items: [CommonAbstract.TokenItem]) throws -> Bool {
            let start = stream.save()
            for item in items {
                guard try parse(item) else {
                    stream.restore(start)
                    return false
                }
            }

            return true
        }

        func parse(_ item: CommonAbstract.TokenItem) throws -> Bool {
            switch item {
            case .zeroOrMore(let atoms):
                return try parseZeroOrMore(atoms)

            case .oneOrMore(let atoms):
                return try parseOneOrMore(atoms)

            case .optionalGroup(let atoms):
                _=try parseAnyOf(atoms)
                return true

            case .group(let atoms):
                return try parseAnyOf(atoms)

            case .optionalAtom(let atom):
                _=try parse(atom)
                return true

            case .atom(let atom):
                return try parse(atom)
            }
        }

        func parseZeroOrMore(_ atoms: [CommonAbstract.TokenAtom]) throws -> Bool {
            var lastState = stream.save()
            while try parseAnyOf(atoms) {
                // Prevent infinite loops resulting from nullable productions that
                // are wrapped in repetitions.
                guard stream.save() != lastState else {
                    break
                }

                lastState = stream.save()
            }

            return true
        }

        func parseOneOrMore(_ atoms: [CommonAbstract.TokenAtom]) throws -> Bool {
            guard try parseAnyOf(atoms) else {
                return false
            }

            var lastState = stream.save()
            while try parseAnyOf(atoms) {
                // Prevent infinite loops resulting from nullable productions that
                // are wrapped in repetitions.
                guard stream.save() != lastState else {
                    break
                }

                lastState = stream.save()
            }

            return true
        }

        func parseAnyOf(_ atoms: [CommonAbstract.TokenAtom]) throws -> Bool {
            let state = stream.save()

            for atom in atoms {
                if try parse(atom) {
                    return true
                }

                stream.restore(state)
            }

            return false
        }

        func parse(_ atom: CommonAbstract.TokenAtom) throws -> Bool {
            guard try atom.excluded.allSatisfy(passes(_:)) else {
                return false
            }

            return try parse(atom.terminal)
        }

        func passes(_ exclusion: CommonAbstract.TokenExclusion) throws -> Bool {
            if stream.isEof {
                return true
            }

            switch exclusion {
            case .identifier(let ident):
                return try stream.negativeLookahead { stream in
                    try interpreter.parse(tokenNamed: ident, from: &stream)
                }

            case .string(let literal):
                return !stream.isNext(literal.contents)

            case .rangeLiteral(let low, let high):
                let range = try makeRange(start: low, end: high)

                return !stream.isNextInRange(range)
            }
        }

        func parse(_ terminal: CommonAbstract.TokenTerminal) throws -> Bool {
            guard !stream.isEof else {
                return false
            }

            switch terminal {
            case .characterPredicate(let c, let predicate):
                throw Error.characterPredicateUnsupported("\(c) {\(predicate)}")

            case .identifier(let ident):
                return try interpreter.parse(tokenNamed: ident, from: &stream)

            case .literal(let literal):
                return stream.advanceIfNext(literal.contents)

            case .rangeLiteral(let low, let high):
                let range = try makeRange(start: low, end: high)

                if stream.isNextInRange(range) {
                    stream.advance()
                    return true
                }

                return false

            case .any:
                stream.advance()
                return true
            }
        }

        func makeRange(start: CommonAbstract.DualString, end: CommonAbstract.DualString) throws -> ClosedRange<Character> {
            let start = start.contents
            let end = end.contents

            guard let startChar = start.first, start.count == 1 else {
                throw Error.incompatibleRangeLiteral(start)
            }
            guard let endChar = end.first, end.count == 1 else {
                throw Error.incompatibleRangeLiteral(end)
            }

            return startChar...endChar
        }
    }

    enum Error: Swift.Error, CustomStringConvertible {
        case characterPredicateUnsupported(String)
        case incompatibleRangeLiteral(String)
        case unknownTokenName(String)
        case missingSyntax(tokenName: String)

        var description: String {
            switch self {
            case .characterPredicateUnsupported(let msg):
                return """
                Found reference to a character predicate terminal ('\(msg)'). Character
                predicate terminals are not supported by the interpreter.
                """

            case .incompatibleRangeLiteral(let literal):
                return """
                Found literal '\(literal)' in a range terminal that cannot be
                converted into a single Swift.Character.
                """

            case .unknownTokenName(let name):
                return "Reference to unknown token name '\(name)'."

            case .missingSyntax(let tokenName):
                return "Token '\(tokenName)' does not have a syntax to parse with."
            }
        }
    }
}
