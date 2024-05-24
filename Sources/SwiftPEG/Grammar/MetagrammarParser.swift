// HEADS UP! This is a generated file

/// A parser for SwiftPEG grammar files.
public final class MetagrammarParser<RawTokenizer: RawTokenizerType>: PEGParser<RawTokenizer>
    where RawTokenizer.Token == Metagrammar.MetagrammarToken, RawTokenizer.Location == FileSourceLocation
{
    /// ```
    /// WHITESPACE ;
    /// ```
    @memoized("WHITESPACE")
    @inlinable
    public func _WHITESPACE() throws -> TokenNode<Metagrammar.MetagrammarToken, RawTokenizer.Location>? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .whitespace)
        {
            return self.setLocation(.init(token), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// IDENTIFIER ;
    /// ```
    @memoized("IDENTIFIER")
    @inlinable
    public func _IDENT() throws -> Metagrammar.IdentifierToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .identifier)
        {
            return self.setLocation(.init(token), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// IDENTIFIER ;
    /// ```
    @memoized("DIGITS")
    @inlinable
    public func _DIGITS() throws -> Metagrammar.IdentifierToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .digits)
        {
            return self.setLocation(.init(token), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// STRING ;
    /// ```
    @memoized("STRING")
    @inlinable
    public func _STRING() throws -> Metagrammar.StringToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .string)
        {
            return self.setLocation(.init(token), at: mark)
        }

        self.restore(mark)
        return nil
    }
}

extension MetagrammarParser {
    /// ```
    /// start[Metagrammar.Grammar]:
    ///     | grammar { grammar }
    ///     ;
    /// ```
    @memoized("start")
    @inlinable
    public func __start() throws -> Metagrammar.Grammar? {
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
    /// grammar[Metagrammar.Grammar]:
    ///     | metas rules { self.setLocation(.init(metas: metas, rules: rules), at: mark) }
    ///     | rules { self.setLocation(.init(metas: [], rules: rules), at: mark) }
    ///     ;
    /// ```
    @memoized("grammar")
    @inlinable
    public func __grammar() throws -> Metagrammar.Grammar? {
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
    /// metas[[Metagrammar.Meta]]:
    ///     | metas=meta+ { metas }
    ///     ;
    /// ```
    @memoized("metas")
    @inlinable
    public func __metas() throws -> [Metagrammar.Meta]? {
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
    /// meta[Metagrammar.Meta]:
    ///     | "@" name=IDENTIFIER value=metaValue ';' { self.setLocation(.init(name: name, value: value), at: mark) }
    ///     | "@" name=IDENTIFIER ';' { self.setLocation(.init(name: name, value: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func __meta() throws -> Metagrammar.Meta? {
        let mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let name = try self.IDENTIFIER(),
            let value = try self.metaValue(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name, value: value), at: mark)
        }

        self.restore(mark)

        if
            let _ = try self.expect(kind: .at),
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name, value: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// metaValue[Metagrammar.MetaValue]:
    ///     | ident=IDENTIFIER { self.setLocation(Metagrammar.MetaIdentifierValue(identifier: ident), at: mark) }
    ///     | string=STRING { self.setLocation(Metagrammar.MetaStringValue(string: string), at: mark) }
    ///     ;
    /// ```
    @memoized("metaValue")
    @inlinable
    public func __metaValue() throws -> Metagrammar.MetaValue? {
        let mark = self.mark()

        if
            let ident = try self.IDENTIFIER()
        {
            return self.setLocation(Metagrammar.MetaIdentifierValue(identifier: ident), at: mark)
        }

        self.restore(mark)

        if
            let string = try self.STRING()
        {
            return self.setLocation(Metagrammar.MetaStringValue(string: string), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// rules[[Metagrammar.Rule]]:
    ///     | rules=rule+ { rules }
    ///     ;
    /// ```
    @memoized("rules")
    @inlinable
    public func __rules() throws -> [Metagrammar.Rule]? {
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
    /// rule[Metagrammar.Rule]:
    ///     | ruleName ":" '|' alts ';' { self.setLocation(.init(name: ruleName, alts: alts), at: mark) }
    ///     | ruleName ":" alts ';' { self.setLocation(.init(name: ruleName, alts: alts), at: mark) }
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func __rule() throws -> Metagrammar.Rule? {
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
    /// ruleName[Metagrammar.RuleName]:
    ///     | name=IDENTIFIER '[' type=swiftType ']' { self.setLocation(.init(name: name, type: type), at: mark) }
    ///     | name=IDENTIFIER { self.setLocation(.init(name: name, type: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("ruleName")
    @inlinable
    public func __ruleName() throws -> Metagrammar.RuleName? {
        let mark = self.mark()

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .leftSquare),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(name: name, type: type), at: mark)
        }

        self.restore(mark)

        if
            let name = try self.IDENTIFIER()
        {
            return self.setLocation(.init(name: name, type: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// alts[[Metagrammar.Alt]]:
    ///     | alt "|" alts { [alt] + alts }
    ///     | alt { [alt] }
    ///     ;
    /// ```
    @memoized("alts")
    @inlinable
    public func __alts() throws -> [Metagrammar.Alt]? {
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
    /// alt[Metagrammar.Alt]:
    ///     | namedItems action { self.setLocation(.init(namedItems: namedItems, action: action), at: mark) }
    ///     | namedItems { self.setLocation(.init(namedItems: namedItems, action: nil), at: mark) }
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func __alt() throws -> Metagrammar.Alt? {
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
    /// namedItems[[Metagrammar.NamedItem]]:
    ///     | namedItem namedItems { [namedItem] + namedItems }
    ///     | namedItem { [namedItem] }
    ///     ;
    /// ```
    @memoized("namedItems")
    @inlinable
    public func __namedItems() throws -> [Metagrammar.NamedItem]? {
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
    /// namedItem[Metagrammar.NamedItem]:
    ///     | name=IDENTIFIER '[' type=swiftType ']' '=' ~ item { self.setLocation(.init(name: name, item: item, type: type, lookahead: nil), at: mark) }
    ///     | name=IDENTIFIER '=' ~ item { self.setLocation(.init(name: name, item: item, type: nil, lookahead: nil), at: mark) }
    ///     | item { self.setLocation(.init(name: nil, item: item, type: nil, lookahead: nil), at: mark) }
    ///     | lookahead { self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: mark) }
    ///     ;
    /// ```
    @memoized("namedItem")
    @inlinable
    public func __namedItem() throws -> Metagrammar.NamedItem? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .leftSquare),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .equals),
            cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name: name, item: item, type: type, lookahead: nil), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .equals),
            cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name: name, item: item, type: nil, lookahead: nil), at: mark)
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
    /// lookahead[Metagrammar.LookaheadOrCut]:
    ///     | '&' ~ atom { self.setLocation(Metagrammar.PositiveLookahead(atom: atom), at: mark) }
    ///     | '!' ~ atom { self.setLocation(Metagrammar.NegativeLookahead(atom: atom), at: mark) }
    ///     | '~' { self.setLocation(Metagrammar.Cut(), at: mark) }
    ///     ;
    /// ```
    @memoized("lookahead")
    @inlinable
    public func __lookahead() throws -> Metagrammar.LookaheadOrCut? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .ampersand),
            cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.PositiveLookahead(atom: atom), at: mark)
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
            return self.setLocation(Metagrammar.NegativeLookahead(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .tilde)
        {
            return self.setLocation(Metagrammar.Cut(), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// item[Metagrammar.Item]:
    ///     | '[' ~ alts ']' { self.setLocation(Metagrammar.OptionalItems(alts: alts), at: mark) }
    ///     | atom '?' { self.setLocation(Metagrammar.OptionalItem(atom: atom), at: mark) }
    ///     | atom '*' { self.setLocation(Metagrammar.ZeroOrMoreItem(atom: atom), at: mark) }
    ///     | atom '+' { self.setLocation(Metagrammar.OneOrMoreItem(atom: atom), at: mark) }
    ///     | sep=atom '.' node=atom '+' { self.setLocation(Metagrammar.GatherItem(sep: sep, item: node), at: mark) }
    ///     | atom { self.setLocation(Metagrammar.AtomItem(atom: atom), at: mark) }
    ///     ;
    /// ```
    @memoized("item")
    @inlinable
    public func __item() throws -> Metagrammar.Item? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftSquare),
            cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(Metagrammar.OptionalItems(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(Metagrammar.OptionalItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .star)
        {
            return self.setLocation(Metagrammar.ZeroOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .plus)
        {
            return self.setLocation(Metagrammar.OneOrMoreItem(atom: atom), at: mark)
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
            return self.setLocation(Metagrammar.GatherItem(sep: sep, item: node), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.AtomItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// atom[Metagrammar.Atom]:
    ///     | '(' ~ alts ')' { self.setLocation(Metagrammar.GroupAtom(alts: alts), at: mark) }
    ///     | IDENTIFIER { self.setLocation(Metagrammar.IdentAtom(identifier: identifier, identity: .unresolved), at: mark) }
    ///     | STRING { self.setLocation(Metagrammar.StringAtom(string: string), at: mark) }
    ///     ;
    /// ```
    @memoized("atom")
    @inlinable
    public func __atom() throws -> Metagrammar.Atom? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftParen),
            cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .rightParen)
        {
            return self.setLocation(Metagrammar.GroupAtom(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let identifier = try self.IDENTIFIER()
        {
            return self.setLocation(Metagrammar.IdentAtom(identifier: identifier, identity: .unresolved), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let string = try self.STRING()
        {
            return self.setLocation(Metagrammar.StringAtom(string: string), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// swiftType[Metagrammar.SwiftType]:
    ///     | raw=STRING { self.setLocation(.init(name: raw.valueTrimmingQuotes), at: mark) }
    ///     | '[' ~ type=swiftType ']' { self.setLocation(.init(name: "[" + type.name + "]"), at: mark) }
    ///     | '(' ~ types=swiftTypeList ')' { self.setLocation(.init(name: "(" + types.map(\.name).joined(separator: ", ") + ")"), at: mark) }
    ///     | name=IDENTIFIER '<' ~ types=swiftTypeList '>' '?' { self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">?"), at: mark) }
    ///     | name=IDENTIFIER '<' ~ types=swiftTypeList '>' { self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">"), at: mark) }
    ///     | name=IDENTIFIER '.' ~ inner=swiftType { self.setLocation(.init(name: name.identifier + "." + inner.name), at: mark) }
    ///     | name=IDENTIFIER '?' { self.setLocation(.init(name: name.identifier + "?"), at: mark) }
    ///     | name=IDENTIFIER { self.setLocation(.init(name: name.identifier), at: mark) }
    ///     ;
    /// ```
    @memoized("swiftType")
    @inlinable
    public func __swiftType() throws -> Metagrammar.SwiftType? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let raw = try self.STRING()
        {
            return self.setLocation(.init(name: raw.valueTrimmingQuotes), at: mark)
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
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .leftAngle),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">?"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .leftAngle),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle)
        {
            return self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .period),
            cut.toggleOn(),
            let inner = try self.swiftType()
        {
            return self.setLocation(.init(name: name.identifier + "." + inner.name), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(.init(name: name.identifier + "?"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENTIFIER()
        {
            return self.setLocation(.init(name: name.identifier), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// swiftTypeList[[Metagrammar.SwiftType]]:
    ///     | type=swiftType ',' types=swiftTypeList { [type] + types }
    ///     | type=swiftType { [type] }
    ///     ;
    /// ```
    @memoized("swiftTypeList")
    @inlinable
    public func __swiftTypeList() throws -> [Metagrammar.SwiftType]? {
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
    /// action[Metagrammar.Action]:
    ///     | "{" ~ balancedTokens "}" { self.setLocation(.init(balancedTokens: balancedTokens), at: mark) }
    ///     ;
    /// ```
    @memoized("action")
    @inlinable
    public func __action() throws -> Metagrammar.Action? {
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
    /// balancedTokens[Metagrammar.BalancedTokens]:
    ///     | balancedToken balancedTokens { self.setLocation(.init(tokens: balancedToken.tokens + balancedTokens.tokens), at: mark) }
    ///     | balancedToken { balancedToken }
    ///     ;
    /// ```
    @memoized("balancedTokens")
    @inlinable
    public func __balancedTokens() throws -> Metagrammar.BalancedTokens? {
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
    /// balancedToken[Metagrammar.BalancedTokens]:
    ///     | token=WHITESPACE { self.setLocation(.init(tokens: [token]), at: mark) }
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
    public func __balancedToken() throws -> Metagrammar.BalancedTokens? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let token = try self.WHITESPACE()
        {
            return self.setLocation(.init(tokens: [token]), at: mark)
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
    ///     | token=IDENTIFIER { token }
    ///     | token=DIGITS { token }
    ///     | token=STRING { token }
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
    ///     | token='/' { .init(token) }
    ///     | token='\' { .init(token) }
    ///     ;
    /// ```
    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> TokenNode<RawTokenizer.Token, RawTokenizer.Location>? {
        let mark = self.mark()

        if
            let token = try self.IDENTIFIER()
        {
            return token
        }

        self.restore(mark)

        if
            let token = try self.DIGITS()
        {
            return token
        }

        self.restore(mark)

        if
            let token = try self.STRING()
        {
            return token
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
    /// tokensFile[[Metagrammar.TokenDefinition]]:
    ///     | tokens=tokenDefinition* { tokens }
    ///     ;
    /// ```
    @memoized("tokensFile")
    @inlinable
    public func __tokensFile() throws -> [Metagrammar.TokenDefinition]? {
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
    /// tokenDefinition[Metagrammar.TokenDefinition]:
    ///     | name=IDENTIFIER '[' expectArgs=STRING ']' ':' literal=STRING ';' { self.setLocation(.init(name: name, expectArgs: expectArgs, literal: literal), at: mark) }
    ///     | name=IDENTIFIER ':' literal=STRING ';' { self.setLocation(.init(name: name, expectArgs: nil, literal: literal), at: mark) }
    ///     ;
    /// ```
    @memoized("tokenDefinition")
    @inlinable
    public func __tokenDefinition() throws -> Metagrammar.TokenDefinition? {
        let mark = self.mark()

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .leftSquare),
            let expectArgs = try self.STRING(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .colon),
            let literal = try self.STRING(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name, expectArgs: expectArgs, literal: literal), at: mark)
        }

        self.restore(mark)

        if
            let name = try self.IDENTIFIER(),
            let _ = try self.expect(kind: .colon),
            let literal = try self.STRING(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name, expectArgs: nil, literal: literal), at: mark)
        }

        self.restore(mark)
        return nil
    }
}
