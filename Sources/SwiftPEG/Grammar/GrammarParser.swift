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
    ///     | metas rules { self.setLocation(.init(metas: metas, rules: rules), at: mark) }
    ///     | rules { self.setLocation(.init(metas: [], rules: rules), at: mark) }
    ///     ;
    /// ```
    @memoized("grammar")
    @inlinable
    public func __grammar() throws -> SwiftPEGGrammar.Grammar? {
        let mark = self.mark()

        if
            let metas = try self.metas(),
            let rules = try self.rules()
        {
            return self.setLocation(.init(metas: metas, rules: rules), at: mark)
        }

        self.restore(mark)

        if
            let rules = try self.rules()
        {
            return self.setLocation(.init(metas: [], rules: rules), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// metas[[SwiftPEGGrammar.Meta]]:
    ///     | metas=meta+ { metas }
    ///     ;
    /// ```
    @memoized("metas")
    @inlinable
    public func __metas() throws -> [SwiftPEGGrammar.Meta]? {
        let mark = self.mark()

        if
            let metas = try self.repeatOneOrMore({
                try self.meta()
            })
        {
            return metas
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// meta[SwiftPEGGrammar.Meta]:
    ///     | "@" name=IDENTIFIER value=metaValue ';' { self.setLocation(.init(name: name.token, value: value), at: mark) }
    ///     | "@" name=IDENTIFIER ';' { self.setLocation(.init(name: name.token, value: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func __meta() throws -> SwiftPEGGrammar.Meta? {
        let mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let name = try self.expect(kind: .identifier),
            let value = try self.metaValue(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, value: value), at: mark)
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .at),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, value: nil), at: mark)
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
    /// rules[[SwiftPEGGrammar.Rule]]:
    ///     | rules=rule+ { rules }
    ///     ;
    /// ```
    @memoized("rules")
    @inlinable
    public func __rules() throws -> [SwiftPEGGrammar.Rule]? {
        let mark = self.mark()

        if
            let rules = try self.repeatOneOrMore({
                try self.rule()
            })
        {
            return rules
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// rule[SwiftPEGGrammar.Rule]:
    ///     | ruleName ":" '|' alts ';' { self.setLocation(.init(name: ruleName, alts: alts), at: mark) }
    ///     | ruleName ":" alts ';' { self.setLocation(.init(name: ruleName, alts: alts), at: mark) }
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func __rule() throws -> SwiftPEGGrammar.Rule? {
        let mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(kind: .colon),
            let _ = try self.expect(kind: .bar),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: ruleName, alts: alts), at: mark)
        }

        self.restore(mark)

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(kind: .colon),
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
    ///     | name=IDENTIFIER '[' type=swiftType ']' { self.setLocation(.init(name: name.token, type: type), at: mark) }
    ///     | name=IDENTIFIER { self.setLocation(.init(name: name.token, type: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("ruleName")
    @inlinable
    public func __ruleName() throws -> SwiftPEGGrammar.RuleName? {
        let mark = self.mark()

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(name: name.token, type: type), at: mark)
        }

        self.restore(mark)

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
    ///     | alt "|" alts { [alt] + alts }
    ///     | alt { [alt] }
    ///     ;
    /// ```
    @memoized("alts")
    @inlinable
    public func __alts() throws -> [SwiftPEGGrammar.Alt]? {
        let mark = self.mark()

        if
            let alt = try self.alt(),
            let _ = try self.expect(kind: .bar),
            let alts = try self.alts()
        {
            return [alt] + alts
        }

        self.restore(mark)

        if
            let alt = try self.alt()
        {
            return [alt]
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// alt[SwiftPEGGrammar.Alt]:
    ///     | namedItems action { self.setLocation(.init(namedItems: namedItems, action: action), at: mark) }
    ///     | namedItems { self.setLocation(.init(namedItems: namedItems, action: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func __alt() throws -> SwiftPEGGrammar.Alt? {
        let mark = self.mark()

        if
            let namedItems = try self.namedItems(),
            let action = try self.action()
        {
            return self.setLocation(.init(namedItems: namedItems, action: action), at: mark)
        }

        self.restore(mark)

        if
            let namedItems = try self.namedItems()
        {
            return self.setLocation(.init(namedItems: namedItems, action: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// namedItems[[SwiftPEGGrammar.NamedItem]]:
    ///     | namedItem namedItems { [namedItem] + namedItems }
    ///     | namedItem { [namedItem] }
    ///     ;
    /// ```
    @memoized("namedItems")
    @inlinable
    public func __namedItems() throws -> [SwiftPEGGrammar.NamedItem]? {
        let mark = self.mark()

        if
            let namedItem = try self.namedItem(),
            let namedItems = try self.namedItems()
        {
            return [namedItem] + namedItems
        }

        self.restore(mark)

        if
            let namedItem = try self.namedItem()
        {
            return [namedItem]
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

        if cut.isOn {
            return nil
        }

        if
            let lookahead = try self.lookahead()
        {
            return self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
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

        if cut.isOn {
            return nil
        }
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

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .star)
        {
            return self.setLocation(SwiftPEGGrammar.ZeroOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .plus)
        {
            return self.setLocation(SwiftPEGGrammar.OneOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let sep = try self.atom(),
            let _ = try self.expect(kind: .period),
            let node = try self.atom(),
            let _ = try self.expect(kind: .plus)
        {
            return self.setLocation(SwiftPEGGrammar.GatherItem(sep: sep, item: node), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.AtomItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
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

        if cut.isOn {
            return nil
        }

        if
            let string = try self.expect(kind: .string)
        {
            return self.setLocation(SwiftPEGGrammar.StringAtom(string: string.token), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// swiftType[SwiftPEGGrammar.SwiftType]:
    ///     | raw=STRING { self.setLocation(.init(name: raw.token.processedString), at: mark) }
    ///     | '[' ~ type=swiftType ']' { self.setLocation(.init(name: "[" + type.name + "]"), at: mark) }
    ///     | '(' ~ types=swiftTypeList ')' { self.setLocation(.init(name: "(" + types.map(\.name).joined(separator: ", ") + ")"), at: mark) }
    ///     | name=IDENTIFIER '<' ~ types=swiftTypeList '>' '?' { self.setLocation(.init(name: name.token.string + "<" + types.map(\.name).joined(separator: ", ") + ">?"), at: mark) }
    ///     | name=IDENTIFIER '<' ~ types=swiftTypeList '>' { self.setLocation(.init(name: name.token.string + "<" + types.map(\.name).joined(separator: ", ") + ">"), at: mark) }
    ///     | name=IDENTIFIER '.' ~ inner=swiftType { self.setLocation(.init(name: name.token.string + "." + inner.name), at: mark) }
    ///     | name=IDENTIFIER '?' { self.setLocation(.init(name: name.token.string + "?"), at: mark) }
    ///     | name=IDENTIFIER { self.setLocation(.init(name: name.token.string), at: mark) }
    ///     ;
    /// ```
    @memoized("swiftType")
    @inlinable
    public func __swiftType() throws -> SwiftPEGGrammar.SwiftType? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let raw = try self.expect(kind: .string)
        {
            return self.setLocation(.init(name: raw.token.processedString), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .leftSquare),
            cut.toggleOn(),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(name: "[" + type.name + "]"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .leftParen),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightParen)
        {
            return self.setLocation(.init(name: "(" + types.map(\.name).joined(separator: ", ") + ")"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftAngle),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(.init(name: name.token.string + "<" + types.map(\.name).joined(separator: ", ") + ">?"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftAngle),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle)
        {
            return self.setLocation(.init(name: name.token.string + "<" + types.map(\.name).joined(separator: ", ") + ">"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .period),
            cut.toggleOn(),
            let inner = try self.swiftType()
        {
            return self.setLocation(.init(name: name.token.string + "." + inner.name), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(.init(name: name.token.string + "?"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier)
        {
            return self.setLocation(.init(name: name.token.string), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// swiftTypeList[[SwiftPEGGrammar.SwiftType]]:
    ///     | type=swiftType ',' types=swiftTypeList { [type] + types }
    ///     | type=swiftType { [type] }
    ///     ;
    /// ```
    @memoized("swiftTypeList")
    @inlinable
    public func __swiftTypeList() throws -> [SwiftPEGGrammar.SwiftType]? {
        let mark = self.mark()

        if
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .comma),
            let types = try self.swiftTypeList()
        {
            return [type] + types
        }

        self.restore(mark)

        if
            let type = try self.swiftType()
        {
            return [type]
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

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftBrace),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftSquare),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftAngle),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightAngle)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftParen),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(kind: .rightParen)
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

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

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// balancedTokenAtom[TokenNode<RawTokenizer.Token, RawTokenizer.Location>]:
    ///     | token=IDENTIFIER { .init(token) }
    ///     | token=DIGITS { .init(token) }
    ///     | token=STRING { .init(token) }
    ///     | token=':' { .init(token) }
    ///     | token=';' { .init(token) }
    ///     | token='|' { .init(token) }
    ///     | token='=' { .init(token) }
    ///     | token='~' { .init(token) }
    ///     | token='*' { .init(token) }
    ///     | token='+' { .init(token) }
    ///     | token='?' { .init(token) }
    ///     | token=',' { .init(token) }
    ///     | token='.' { .init(token) }
    ///     | token='@' { .init(token) }
    ///     | token='$' { .init(token) }
    ///     | token='/' { .init(token) }
    ///     | token='\\' { .init(token) }
    ///     ;
    /// ```
    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> TokenNode<RawTokenizer.Token, RawTokenizer.Location>? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .identifier)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .digits)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .string)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .colon)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .semicolon)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .bar)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .equals)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .tilde)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .star)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .plus)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .questionMark)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .comma)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .period)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .at)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .dollarSign)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .forwardSlash)
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(kind: .backslash)
        {
            return .init(token)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// tokensFile[[SwiftPEGGrammar.TokenDefinition]]:
    ///     | tokens=tokenDefinition* { tokens }
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
    ///     | '$' name=IDENTIFIER '[' expectArgs=STRING ']' ':' ~ literal=STRING ';' { self.setLocation(.init(name: name.token, expectArgs: expectArgs.token, literal: literal.token), at: mark) }
    ///     | '$' name=IDENTIFIER '[' expectArgs=STRING ']' ';' { self.setLocation(.init(name: name.token, expectArgs: expectArgs.token, literal: nil), at: mark) }
    ///     | '$' name=IDENTIFIER ':' ~ literal=STRING ';' { self.setLocation(.init(name: name.token, expectArgs: nil, literal: literal.token), at: mark) }
    ///     | '$' name=IDENTIFIER ';' { self.setLocation(.init(name: name.token, expectArgs: nil, literal: nil), at: mark) }
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
            let expectArgs = try self.expect(kind: .string),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .colon),
            cut.toggleOn(),
            let literal = try self.expect(kind: .string),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, expectArgs: expectArgs.token, literal: literal.token), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .dollarSign),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let expectArgs = try self.expect(kind: .string),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, expectArgs: expectArgs.token, literal: nil), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .dollarSign),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            cut.toggleOn(),
            let literal = try self.expect(kind: .string),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.token, expectArgs: nil, literal: literal.token), at: mark)
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
            return self.setLocation(.init(name: name.token, expectArgs: nil, literal: nil), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }
}
