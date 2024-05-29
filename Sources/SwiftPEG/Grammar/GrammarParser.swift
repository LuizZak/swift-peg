// HEADS UP! This is a generated file

/// A parser for SwiftPEG grammar files.
public final class GrammarParser<RawTokenizer: RawTokenizerType>: PEGParser<RawTokenizer>
    where RawTokenizer.Token == SwiftPEGGrammar.GrammarToken, RawTokenizer.Location == FileSourceLocation
{ }

extension GrammarParser {
    /// ```
    /// start[SwiftPEGGrammar.Grammar]:
    ///     | grammar { grammar }
    ///     ;
    /// ```
    @memoized("start")
    @inlinable
    public func __start() throws -> SwiftPEGGrammar.Grammar? {
        let mark = self.mark()

        if
            let grammar = try self.grammar()
        {
            return grammar
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// grammar[SwiftPEGGrammar.Grammar]:
    ///     | metas=meta* rules=rule+ { self.setLocation(.init(metas: metas, rules: rules), at: mark) }
    ///     ;
    /// ```
    @memoized("grammar")
    @inlinable
    public func __grammar() throws -> SwiftPEGGrammar.Grammar? {
        let mark = self.mark()

        if
            let metas = try self.repeatZeroOrMore({
                try self.meta()
            }),
            let rules = try self.repeatOneOrMore({
                try self.rule()
            })
        {
            return self.setLocation(.init(metas: metas, rules: rules), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// meta[SwiftPEGGrammar.Meta]:
    ///     | "@" name=IDENTIFIER value=metaValue? ';' { self.setLocation(.init(name: name.token, value: value), at: mark) }
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func __meta() throws -> SwiftPEGGrammar.Meta? {
        let mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let name = try self.expect(kind: .identifier),
            let value = try self.optional({
                try self.metaValue()
            }),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, value: value), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// metaValue[SwiftPEGGrammar.MetaValue]:
    ///     | ident=IDENTIFIER { self.setLocation(SwiftPEGGrammar.MetaIdentifierValue(identifier: ident.token), at: mark) }
    ///     | string=STRING { self.setLocation(SwiftPEGGrammar.MetaStringValue(string: string.token), at: mark) }
    ///     ;
    /// ```
    @memoized("metaValue")
    @inlinable
    public func __metaValue() throws -> SwiftPEGGrammar.MetaValue? {
        let mark = self.mark()

        if
            let ident = try self.expect(kind: .identifier)
        {
            return self.setLocation(SwiftPEGGrammar.MetaIdentifierValue(identifier: ident.token), at: mark)
        }

        self.restore(mark)

        if
            let string = try self.expect(kind: .string)
        {
            return self.setLocation(SwiftPEGGrammar.MetaStringValue(string: string.token), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// rule[SwiftPEGGrammar.Rule]:
    ///     | ruleName ":" '|'? alts ';' { self.setLocation(.init(name: ruleName, alts: alts), at: mark) }
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func __rule() throws -> SwiftPEGGrammar.Rule? {
        let mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(kind: .colon),
            let _ = try self.optional({
                try self.expect(kind: .bar)
            }),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: ruleName, alts: alts), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// ruleName[SwiftPEGGrammar.RuleName]:
    ///     | name=IDENTIFIER '[' ~ type=swiftType ']' { self.setLocation(.init(name: name.token, type: type), at: mark) }
    ///     | name=IDENTIFIER { self.setLocation(.init(name: name.token, type: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("ruleName")
    @inlinable
    public func __ruleName() throws -> SwiftPEGGrammar.RuleName? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            cut.toggleOn(),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(name: name.token, type: type), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier)
        {
            return self.setLocation(.init(name: name.token, type: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// alts[[SwiftPEGGrammar.Alt]]:
    ///     | "|".alt+
    ///     ;
    /// ```
    @memoized("alts")
    @inlinable
    public func __alts() throws -> [SwiftPEGGrammar.Alt]? {
        let mark = self.mark()

        if
            let alt = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.alt()
            })
        {
            return alt
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// alt[SwiftPEGGrammar.Alt]:
    ///     | namedItems action? failAction? { self.setLocation(.init(namedItems: namedItems, action: action, failAction: failAction), at: mark) }
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func __alt() throws -> SwiftPEGGrammar.Alt? {
        let mark = self.mark()

        if
            let namedItems = try self.namedItems(),
            let action = try self.optional({
                try self.action()
            }),
            let failAction = try self.optional({
                try self.failAction()
            })
        {
            return self.setLocation(.init(namedItems: namedItems, action: action, failAction: failAction), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// namedItems[[SwiftPEGGrammar.NamedItem]]:
    ///     | namedItem+
    ///     ;
    /// ```
    @memoized("namedItems")
    @inlinable
    public func __namedItems() throws -> [SwiftPEGGrammar.NamedItem]? {
        let mark = self.mark()

        if
            let namedItem = try self.repeatOneOrMore({
                try self.namedItem()
            })
        {
            return namedItem
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// namedItem[SwiftPEGGrammar.NamedItem]:
    ///     | name=IDENTIFIER '[' type=swiftType ']' '=' ~ item { self.setLocation(.init(name: name.token, item: item, type: type, lookahead: nil), at: mark) }
    ///     | name=IDENTIFIER '=' ~ item { self.setLocation(.init(name: name.token, item: item, type: nil, lookahead: nil), at: mark) }
    ///     | item { self.setLocation(.init(name: nil, item: item, type: nil, lookahead: nil), at: mark) }
    ///     | lookahead { self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: mark) }
    ///     ;
    /// ```
    @memoized("namedItem")
    @inlinable
    public func __namedItem() throws -> SwiftPEGGrammar.NamedItem? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .equals),
            cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name: name.token, item: item, type: type, lookahead: nil), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .equals),
            cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name: name.token, item: item, type: nil, lookahead: nil), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let item = try self.item()
        {
            return self.setLocation(.init(name: nil, item: item, type: nil, lookahead: nil), at: mark)
        }

        self.restore(mark)

        if
            let lookahead = try self.lookahead()
        {
            return self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// lookahead[SwiftPEGGrammar.LookaheadOrCut]:
    ///     | '&' ~ atom { self.setLocation(SwiftPEGGrammar.PositiveLookahead(atom: atom), at: mark) }
    ///     | '!' ~ atom { self.setLocation(SwiftPEGGrammar.NegativeLookahead(atom: atom), at: mark) }
    ///     | '~' { self.setLocation(SwiftPEGGrammar.Cut(), at: mark) }
    ///     ;
    /// ```
    @memoized("lookahead")
    @inlinable
    public func __lookahead() throws -> SwiftPEGGrammar.LookaheadOrCut? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .ampersand),
            cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.PositiveLookahead(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .exclamationMark),
            cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.NegativeLookahead(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .tilde)
        {
            return self.setLocation(SwiftPEGGrammar.Cut(), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// item[SwiftPEGGrammar.Item]:
    ///     | '[' ~ alts ']' { self.setLocation(SwiftPEGGrammar.OptionalItems(alts: alts), at: mark) }
    ///     | atom '?' { self.setLocation(SwiftPEGGrammar.OptionalItem(atom: atom), at: mark) }
    ///     | atom '*' { self.setLocation(SwiftPEGGrammar.ZeroOrMoreItem(atom: atom), at: mark) }
    ///     | atom '+' { self.setLocation(SwiftPEGGrammar.OneOrMoreItem(atom: atom), at: mark) }
    ///     | sep=atom '.' node=atom '+' { self.setLocation(SwiftPEGGrammar.GatherItem(sep: sep, item: node), at: mark) }
    ///     | atom { self.setLocation(SwiftPEGGrammar.AtomItem(atom: atom), at: mark) }
    ///     ;
    /// ```
    @memoized("item")
    @inlinable
    public func __item() throws -> SwiftPEGGrammar.Item? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftSquare),
            cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(SwiftPEGGrammar.OptionalItems(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(SwiftPEGGrammar.OptionalItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .star)
        {
            return self.setLocation(SwiftPEGGrammar.ZeroOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .plus)
        {
            return self.setLocation(SwiftPEGGrammar.OneOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            let sep = try self.atom(),
            let _ = try self.expect(kind: .period),
            let node = try self.atom(),
            let _ = try self.expect(kind: .plus)
        {
            return self.setLocation(SwiftPEGGrammar.GatherItem(sep: sep, item: node), at: mark)
        }

        self.restore(mark)

        if
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.AtomItem(atom: atom), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// atom[SwiftPEGGrammar.Atom]:
    ///     | '(' ~ alts ')' { self.setLocation(SwiftPEGGrammar.GroupAtom(alts: alts), at: mark) }
    ///     | IDENTIFIER { self.setLocation(SwiftPEGGrammar.IdentAtom(identifier: identifier.token, identity: .unresolved), at: mark) }
    ///     | STRING { self.setLocation(SwiftPEGGrammar.StringAtom(string: string.token), at: mark) }
    ///     ;
    /// ```
    @memoized("atom")
    @inlinable
    public func __atom() throws -> SwiftPEGGrammar.Atom? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftParen),
            cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .rightParen)
        {
            return self.setLocation(SwiftPEGGrammar.GroupAtom(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let identifier = try self.expect(kind: .identifier)
        {
            return self.setLocation(SwiftPEGGrammar.IdentAtom(identifier: identifier.token, identity: .unresolved), at: mark)
        }

        self.restore(mark)

        if
            let string = try self.expect(kind: .string)
        {
            return self.setLocation(SwiftPEGGrammar.StringAtom(string: string.token), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// swiftType[CommonAbstract.SwiftType]:
    ///     | '[' key=swiftType ':' ~ value=swiftType ']' { .dictionary(key: key, value: value) }
    ///     | '[' ~ swiftType ']' { .array(swiftType) }
    ///     | swiftType '?' { .optional(swiftType) }
    ///     | swiftType '.' IDENTIFIER '<' swiftTypeList '>' { .nested(swiftType, .init(identifier: identifier.token.string, genericArguments: swiftTypeList)) }
    ///     | swiftType '.' IDENTIFIER { .nested(swiftType, .init(identifier: identifier.token.string)) }
    ///     | IDENTIFIER '<' swiftTypeList '>' { .nominal(.init(identifier: identifier.token.string, genericArguments: swiftTypeList)) }
    ///     | IDENTIFIER { .nominal(.init(identifier: identifier.token.string)) }
    ///     ;
    /// ```
    @memoizedLeftRecursive("swiftType")
    @inlinable
    public func __swiftType() throws -> CommonAbstract.SwiftType? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftSquare),
            let key = try self.swiftType(),
            let _ = try self.expect(kind: .colon),
            cut.toggleOn(),
            let value = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return .dictionary(key: key, value: value)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .leftSquare),
            cut.toggleOn(),
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return .array(swiftType)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .questionMark)
        {
            return .optional(swiftType)
        }

        self.restore(mark)

        if
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .period),
            let identifier = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftAngle),
            let swiftTypeList = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle)
        {
            return .nested(swiftType, .init(identifier: identifier.token.string, genericArguments: swiftTypeList))
        }

        self.restore(mark)

        if
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .period),
            let identifier = try self.expect(kind: .identifier)
        {
            return .nested(swiftType, .init(identifier: identifier.token.string))
        }

        self.restore(mark)

        if
            let identifier = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftAngle),
            let swiftTypeList = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle)
        {
            return .nominal(.init(identifier: identifier.token.string, genericArguments: swiftTypeList))
        }

        self.restore(mark)

        if
            let identifier = try self.expect(kind: .identifier)
        {
            return .nominal(.init(identifier: identifier.token.string))
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// swiftTypeList[[CommonAbstract.SwiftType]]:
    ///     | ','.swiftType+
    ///     ;
    /// ```
    @memoized("swiftTypeList")
    @inlinable
    public func __swiftTypeList() throws -> [CommonAbstract.SwiftType]? {
        let mark = self.mark()

        if
            let swiftType = try self.gather(separator: {
                try self.expect(kind: .comma)
            }, item: {
                try self.swiftType()
            })
        {
            return swiftType
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// action[SwiftPEGGrammar.Action]:
    ///     | "{" ~ balancedTokens "}" { self.setLocation(.init(balancedTokens: balancedTokens), at: mark) }
    ///     ;
    /// ```
    @memoized("action")
    @inlinable
    public func __action() throws -> SwiftPEGGrammar.Action? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftBrace),
            cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(balancedTokens: balancedTokens), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// failAction[SwiftPEGGrammar.Action]:
    ///     | "!!" "{" ~ balancedTokens "}" { self.setLocation(.init(balancedTokens: balancedTokens), at: mark) }
    ///     ;
    /// ```
    @memoized("failAction")
    @inlinable
    public func __failAction() throws -> SwiftPEGGrammar.Action? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .doubleExclamationMark),
            let _ = try self.expect(kind: .leftBrace),
            cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(balancedTokens: balancedTokens), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// balancedTokens[SwiftPEGGrammar.BalancedTokens]:
    ///     | balancedToken balancedTokens { self.setLocation(.init(tokens: balancedToken.tokens + balancedTokens.tokens), at: mark) }
    ///     | balancedToken { balancedToken }
    ///     ;
    /// ```
    @memoized("balancedTokens")
    @inlinable
    public func __balancedTokens() throws -> SwiftPEGGrammar.BalancedTokens? {
        let mark = self.mark()

        if
            let balancedToken = try self.balancedToken(),
            let balancedTokens = try self.balancedTokens()
        {
            return self.setLocation(.init(tokens: balancedToken.tokens + balancedTokens.tokens), at: mark)
        }

        self.restore(mark)

        if
            let balancedToken = try self.balancedToken()
        {
            return balancedToken
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// balancedToken[SwiftPEGGrammar.BalancedTokens]:
    ///     | token=WHITESPACE { self.setLocation(.init(tokens: [.init(token)]), at: mark) }
    ///     | l='{' balancedTokens r='}' { self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark) }
    ///     | l='[' balancedTokens r=']' { self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark) }
    ///     | l='<' balancedTokens r='>' { self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark) }
    ///     | l='(' balancedTokens r=')' { self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark) }
    ///     | l='[' ~ r=']' { self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark) }
    ///     | l='{' ~ r='}' { self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark) }
    ///     | l='<' ~ r='>' { self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark) }
    ///     | l='(' ~ r=')' { self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark) }
    ///     | token=balancedTokenAtom { .init(tokens: [token]) }
    ///     ;
    /// ```
    @memoized("balancedToken")
    @inlinable
    public func __balancedToken() throws -> SwiftPEGGrammar.BalancedTokens? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let token = try self.expect(kind: .whitespace)
        {
            return self.setLocation(.init(tokens: [.init(token)]), at: mark)
        }

        self.restore(mark)

        if
            let l = try self.expect(kind: .leftBrace),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if
            let l = try self.expect(kind: .leftSquare),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if
            let l = try self.expect(kind: .leftAngle),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightAngle)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if
            let l = try self.expect(kind: .leftParen),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightParen)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if
            let l = try self.expect(kind: .leftSquare),
            cut.toggleOn(),
            let r = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftBrace),
            cut.toggleOn(),
            let r = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftAngle),
            cut.toggleOn(),
            let r = try self.expect(kind: .rightAngle)
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftParen),
            cut.toggleOn(),
            let r = try self.expect(kind: .rightParen)
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.balancedTokenAtom()
        {
            return .init(tokens: [token])
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// balancedTokenAtom[TokenNode<RawTokenizer.Token, RawTokenizer.Location>]:
    ///     | !"[" !"]" !"{" !"}" !"(" !")" token=ANY { .init(token) }
    ///     ;
    /// ```
    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> TokenNode<RawTokenizer.Token, RawTokenizer.Location>? {
        let mark = self.mark()

        if
            try self.negativeLookahead({
                try self.expect(kind: .leftSquare)
            }),
            try self.negativeLookahead({
                try self.expect(kind: .rightSquare)
            }),
            try self.negativeLookahead({
                try self.expect(kind: .leftBrace)
            }),
            try self.negativeLookahead({
                try self.expect(kind: .rightBrace)
            }),
            try self.negativeLookahead({
                try self.expect(kind: .leftParen)
            }),
            try self.negativeLookahead({
                try self.expect(kind: .rightParen)
            }),
            let token = try self.nextToken()
        {
            return .init(token)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokensFile[[SwiftPEGGrammar.TokenDefinition]]:
    ///     | tokens=tokenDefinition*
    ///     ;
    /// ```
    @memoized("tokensFile")
    @inlinable
    public func __tokensFile() throws -> [SwiftPEGGrammar.TokenDefinition]? {
        let mark = self.mark()

        if
            let tokens = try self.repeatZeroOrMore({
                try self.tokenDefinition()
            })
        {
            return tokens
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokenDefinition[SwiftPEGGrammar.TokenDefinition]:
    ///     | '$' name=IDENTIFIER '[' staticToken=STRING ']' ':' ~ tokenSyntax ';' { self.setLocation(.init(name: name.token, staticToken: staticToken.token, tokenSyntax: tokenSyntax), at: mark) }
    ///     | '$' name=IDENTIFIER '[' staticToken=STRING ']' ';' { self.setLocation(.init(name: name.token, staticToken: staticToken.token, tokenSyntax: nil), at: mark) }
    ///     | '$' name=IDENTIFIER ':' ~ tokenSyntax ';' { self.setLocation(.init(name: name.token, staticToken: nil, tokenSyntax: tokenSyntax), at: mark) }
    ///     | '$' name=IDENTIFIER ';' { self.setLocation(.init(name: name.token, staticToken: nil, tokenSyntax: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("tokenDefinition")
    @inlinable
    public func __tokenDefinition() throws -> SwiftPEGGrammar.TokenDefinition? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .dollarSign),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let staticToken = try self.expect(kind: .string),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .colon),
            cut.toggleOn(),
            let tokenSyntax = try self.tokenSyntax(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, staticToken: staticToken.token, tokenSyntax: tokenSyntax), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .dollarSign),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let staticToken = try self.expect(kind: .string),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, staticToken: staticToken.token, tokenSyntax: nil), at: mark)
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .dollarSign),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            cut.toggleOn(),
            let tokenSyntax = try self.tokenSyntax(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, staticToken: nil, tokenSyntax: tokenSyntax), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .dollarSign),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, staticToken: nil, tokenSyntax: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokenSyntax[CommonAbstract.TokenSyntax]:
    ///     | '|'? '|'.tokenSyntaxAlt+ { .init(alts: tokenSyntaxAlt) }
    ///     ;
    /// ```
    @memoized("tokenSyntax")
    @inlinable
    public func __tokenSyntax() throws -> CommonAbstract.TokenSyntax? {
        let mark = self.mark()

        if
            let _ = try self.optional({
                try self.expect(kind: .bar)
            }),
            let tokenSyntaxAlt = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxAlt()
            })
        {
            return .init(alts: tokenSyntaxAlt)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokenSyntaxAlt[CommonAbstract.TokenAlt]:
    ///     | tokenSyntaxAtom+ { .init(atoms: tokenSyntaxAtom) }
    ///     ;
    /// ```
    @memoized("tokenSyntaxAlt")
    @inlinable
    public func __tokenSyntaxAlt() throws -> CommonAbstract.TokenAlt? {
        let mark = self.mark()

        if
            let tokenSyntaxAtom = try self.repeatOneOrMore({
                try self.tokenSyntaxAtom()
            })
        {
            return .init(atoms: tokenSyntaxAtom)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokenSyntaxAtom[CommonAbstract.TokenAtom]:
    ///     | '(' '|'.tokenSyntaxTerminal+ ')' '*' { .zeroOrMore(tokenSyntaxTerminal) }
    ///     | '(' '|'.tokenSyntaxTerminal+ ')' '+' { .oneOrMore(tokenSyntaxTerminal) }
    ///     | '(' '|'.tokenSyntaxTerminal+ ')' { .group(tokenSyntaxTerminal) }
    ///     | tokenSyntaxTerminal { .terminal(tokenSyntaxTerminal) }
    ///     ;
    /// ```
    @memoized("tokenSyntaxAtom")
    @inlinable
    public func __tokenSyntaxAtom() throws -> CommonAbstract.TokenAtom? {
        let mark = self.mark()

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxTerminal = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxTerminal()
            }),
            let _ = try self.expect(kind: .rightParen),
            let _ = try self.expect(kind: .star)
        {
            return .zeroOrMore(tokenSyntaxTerminal)
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxTerminal = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxTerminal()
            }),
            let _ = try self.expect(kind: .rightParen),
            let _ = try self.expect(kind: .plus)
        {
            return .oneOrMore(tokenSyntaxTerminal)
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxTerminal = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxTerminal()
            }),
            let _ = try self.expect(kind: .rightParen)
        {
            return .group(tokenSyntaxTerminal)
        }

        self.restore(mark)

        if
            let tokenSyntaxTerminal = try self.tokenSyntaxTerminal()
        {
            return .terminal(tokenSyntaxTerminal)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokenSyntaxTerminal[CommonAbstract.TokenTerminal]:
    ///     | IDENTIFIER action { .characterPredicate("\(identifier)", "\(action.description)") }
    ///     | '!' STRING tokenSyntaxTerminal { .excludingLiteral("\(string)", tokenSyntaxTerminal) }
    ///     | '!' IDENTIFIER tokenSyntaxTerminal { .excludingIdentifier("\(identifier)", tokenSyntaxTerminal) }
    ///     | start=STRING '...' end=STRING { .rangeLiteral("\(start)", "\(end)") }
    ///     | STRING { .literal("\(string)") }
    ///     | IDENTIFIER { .identifier("\(identifier)") }
    ///     | '.' { .any }
    ///     ;
    /// ```
    @memoized("tokenSyntaxTerminal")
    @inlinable
    public func __tokenSyntaxTerminal() throws -> CommonAbstract.TokenTerminal? {
        let mark = self.mark()

        if
            let identifier = try self.expect(kind: .identifier),
            let action = try self.action()
        {
            return .characterPredicate("\(identifier)", "\(action.description)")
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .exclamationMark),
            let string = try self.expect(kind: .string),
            let tokenSyntaxTerminal = try self.tokenSyntaxTerminal()
        {
            return .excludingLiteral("\(string)", tokenSyntaxTerminal)
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .exclamationMark),
            let identifier = try self.expect(kind: .identifier),
            let tokenSyntaxTerminal = try self.tokenSyntaxTerminal()
        {
            return .excludingIdentifier("\(identifier)", tokenSyntaxTerminal)
        }

        self.restore(mark)

        if
            let start = try self.expect(kind: .string),
            let _ = try self.expect(kind: .ellipsis),
            let end = try self.expect(kind: .string)
        {
            return .rangeLiteral("\(start)", "\(end)")
        }

        self.restore(mark)

        if
            let string = try self.expect(kind: .string)
        {
            return .literal("\(string)")
        }

        self.restore(mark)

        if
            let identifier = try self.expect(kind: .identifier)
        {
            return .identifier("\(identifier)")
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .period)
        {
            return .any
        }

        self.restore(mark)
        return nil
    }
}
