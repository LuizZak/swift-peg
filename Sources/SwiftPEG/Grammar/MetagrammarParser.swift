/// A parser for SwiftPEG grammar files.
public final class MetagrammarParser<RawTokenizer: RawTokenizerType>
    : PEGParser<RawTokenizer> where RawTokenizer.Token == Metagrammar.MetagrammarToken
{
    /// ```
    /// start: grammar ;
    /// ```
    @memoized("start")
    @inlinable
    public func _start() throws -> Metagrammar.Grammar? {
        try grammar()
    }

    /// ```
    /// grammar: metas? rules ;
    /// 
    /// metas: meta+ ;
    /// rules: rule+ ;
    /// ```
    @memoized("grammar")
    @inlinable
    public func _grammar() throws -> Metagrammar.Grammar? {
        let mark = self.mark()
        let metas = try self.metas() ?? []
        if let rules = try self.rules() {
            return .init(metas: metas, rules: rules)
        }
        self.restore(mark)
        return nil
    }

    /// ```
    /// metas: meta+ ;
    /// ```
    @memoized("metas")
    @inlinable
    public func _metas() throws -> [Metagrammar.Meta]? {
        let mark = self.mark()
        guard let meta = try self.meta() else {
            self.restore(mark)
            return nil
        }

        var result: [Metagrammar.Meta] = [meta]
        while let meta = try self.meta() {
            result.append(meta)
        }
        return result
    }

    /// ```
    /// meta:
    ///     | '@' name=IDENT metaValue? ';'
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func _meta() throws -> Metagrammar.Meta? {
        let mark = self.mark()

        if
            try self.expect(kind: .at) != nil,
            let name = try self.identToken(),
            let metaValue = try self.metaValue(),
            try self.expect(kind: .semicolon) != nil
        {
            return .init(name: name, value: metaValue)
        }

        self.restore(mark)

        if
            try self.expect(kind: .at) != nil,
            let name = try self.identToken(),
            try self.expect(kind: .semicolon) != nil
        {
            return .init(name: name, value: nil)
        }

        self.restore(mark)

        return nil
    }

    /// ```
    /// metaValue:
    ///     | metaValueIdent
    ///     | metaValueString
    ///     ;
    /// ```
    @memoized("metaValue")
    @inlinable
    public func _metaValue() throws -> Metagrammar.MetaValue? {
        let mark = self.mark()

        if
            let metaValueIdent = try self.metaValueIdent()
        {
            return metaValueIdent
        }

        self.restore(mark)

        if
            let metaValueString = try self.metaValueString()
        {
            return metaValueString
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// metaValueIdent: IDENT ;
    /// ```
    @memoized("metaValueIdent")
    @inlinable
    public func _metaValueIdent() throws -> Metagrammar.MetaIdentifierValue? {
        let mark = self.mark()

        if
            let ident = try self.identToken()
        {
            return .init(identifier: ident)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// metaValueString: STRING ;
    /// ```
    @memoized("metaValueString")
    @inlinable
    public func _metaValueString() throws -> Metagrammar.MetaStringValue? {
        let mark = self.mark()

        if
            let string = try self.stringToken()
        {
            return .init(string: string)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// rules: rule+ ;
    /// ```
    @memoized("rules")
    @inlinable
    public func _rules() throws -> [Metagrammar.Rule]? {
        let mark = self.mark()
        guard let rule = try self.rule() else {
            self.restore(mark)
            return nil
        }

        var result: [Metagrammar.Rule] = [rule]
        while let rule = try self.rule() {
            result.append(rule)
        }
        return result
    }

    /// ```
    /// rule:
    ///     | ruleName ':' '|'? alts ';'
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func _rule() throws -> Metagrammar.Rule? {
        let mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            try self.expect(kind: .colon) != nil,
            (try self.maybe(kind: .bar) || true),
            let alts = try self.alts(),
            try self.expect(kind: .semicolon) != nil
        {
            return .init(name: ruleName, alts: alts)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// ruleName:
    ///     | name=IDENT ('[' type=IDENT ']')?
    ///     ;
    /// ```
    @memoized("ruleName")
    @inlinable
    public func _ruleName() throws -> Metagrammar.RuleName? {
        let mark = self.mark()

        if
            let name = try self.identToken(),
            try self.expect(kind: .leftSquare) != nil,
            let type = try self.identToken(),
            try self.expect(kind: .rightSquare) != nil
        {
            return .init(name: name, type: type)
        }

        self.restore(mark)

        if
            let name = try self.identToken()
        {
            return .init(name: name, type: nil)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// alts: '|'.alt+;
    /// ```
    @memoized("alts")
    @inlinable
    public func _alts() throws -> [Metagrammar.Alt]? {
        var mark = self.mark()

        var result: [Metagrammar.Alt] = []
        repeat {
            guard let alt = try self.alt() else {
                break
            }

            result.append(alt)
            mark = self.mark()
        } while try self.maybe(kind: .bar)

        self.restore(mark)
        return result.isEmpty ? nil : result
    }

    /// ```
    /// alt:
    ///     | namedItems action?
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func _alt() throws -> Metagrammar.Alt? {
        let mark = self.mark()

        if
            let namedItems = try self.namedItems()
        {
            let action = try? self.action()
            return .init(namedItems: namedItems, action: action)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// namedItems: namedItem+ ;
    /// ```
    @memoized("namedItems")
    @inlinable
    public func _namedItems() throws -> [Metagrammar.NamedItem]? {
        let mark = self.mark()
        guard let namedItem = try self.namedItem() else {
            self.restore(mark)
            return nil
        }

        var result: [Metagrammar.NamedItem] = [namedItem]
        while let namedItem = try self.namedItem() {
            result.append(namedItem)
        }
        return result
    }

    /// ```
    /// namedItem:
    ///     | name=IDENT '[' type=IDENT ']' '=' ~ item
    ///     | name=IDENT '=' ~ item
    ///     | item
    ///     | lookahead
    ///     ;
    /// ```
    @memoized("namedItem")
    @inlinable
    public func _namedItem() throws -> Metagrammar.NamedItem? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let name = try self.identToken(),
            try self.expect(kind: .leftSquare) != nil,
            try self.identToken() != nil,
            try self.expect(kind: .rightSquare) != nil,
            try self.expect(kind: .equals) != nil,
            cut.toggleOn(),
            let item = try self.item()
        {
            return .init(name: name, item: item, lookahead: nil)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.identToken(),
            try self.expect(kind: .equals) != nil,
            cut.toggleOn(),
            let item = try self.item()
        {
            return .init(name: name, item: item, lookahead: nil)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let item = try self.item()
        {
            return .init(name: nil, item: item, lookahead: nil)
        }

        self.restore(mark)

        if
            let lookahead = try self.lookahead()
        {
            return .init(name: nil, item: nil, lookahead: lookahead)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// lookahead:
    ///     | '&' atom
    ///     | '!' atom
    ///     | '~'
    ///     ;
    /// ```
    @memoized("lookahead")
    @inlinable
    public func _lookahead() throws -> Metagrammar.LookaheadOrCut? {
        let mark = self.mark()

        if
            let positiveLookahead = try self.positiveLookahead()
        {
            return positiveLookahead
        }

        self.restore(mark)

        if
            let negativeLookahead = try self.negativeLookahead()
        {
            return negativeLookahead
        }

        self.restore(mark)

        if
            let cut = try self.cut()
        {
            return cut
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// '&' atom ;
    /// ```
    @memoized("positiveLookahead")
    @inlinable
    public func _positiveLookahead() throws -> Metagrammar.PositiveLookahead? {
        let mark = self.mark()

        if
            try self.expect(kind: .ampersand) != nil,
            let atom = try self.atom()
        {
            return .init(atom: atom)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// '!' atom ;
    /// ```
    @memoized("negativeLookahead")
    @inlinable
    public func _negativeLookahead() throws -> Metagrammar.NegativeLookahead? {
        let mark = self.mark()

        if
            try self.expect(kind: .exclamationMark) != nil,
            let atom = try self.atom()
        {
            return .init(atom: atom)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// '~' ;
    /// ```
    @memoized("cut")
    @inlinable
    public func _cut() throws -> Metagrammar.Cut? {
        let mark = self.mark()

        if
            try self.expect(kind: .tilde) != nil
        {
            return .init()
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// item:
    ///     | '[' ~ alts ']'
    ///     | atom '?'
    ///     | atom '*'
    ///     | atom '+'
    ///     | sep=atom '.' node=atom '+'
    ///     | atom
    ///     ;
    /// ```
    @memoized("item")
    @inlinable
    public func _item() throws -> Metagrammar.Item? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect(kind: .leftSquare) != nil,
            cut.toggleOn(),
            let alts = try self.alts(),
            try self.expect(kind: .rightSquare) != nil
        {
            return Metagrammar.OptionalItems(alts: alts)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            try self.expect(kind: .questionMark) != nil
        {
            return Metagrammar.OptionalItem(atom: atom)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            try self.expect(kind: .star) != nil
        {
            return Metagrammar.ZeroOrMoreItem(atom: atom)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            try self.expect(kind: .plus) != nil
        {
            return Metagrammar.OneOrMoreItem(atom: atom)
        }

        self.restore(mark)

        if
            let sep = try self.atom(),
            try self.expect(kind: .period) != nil,
            let node = try self.atom(),
            try self.expect(kind: .plus) != nil
        {
            return Metagrammar.GatherItem(sep: sep, item: node)
        }

        self.restore(mark)

        if
            let atom = try self.atom()
        {
            return Metagrammar.AtomItem(atom: atom)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// atom:
    ///     | '(' ~ alts ')'
    ///     | IDENT
    ///     | STRING
    ///     ;
    /// ```
    @memoized("atom")
    @inlinable
    public func _atom() throws -> Metagrammar.Atom? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect(kind: .leftParen) != nil,
            cut.toggleOn(),
            let alts = try self.alts(),
            try self.expect(kind: .rightParen) != nil
        {
            return Metagrammar.GroupAtom(alts: alts)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let ident = try self.identToken()
        {
            return Metagrammar.IdentAtom(identifier: ident)
        }

        self.restore(mark)

        if
            let string = try self.stringToken()
        {
            return Metagrammar.StringAtom(string: string)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// action: '{' balancedTokens? '}' ;
    /// ```
    @memoized("action")
    @inlinable
    public func _action() throws -> Metagrammar.Action? {
        let mark = self.mark()

        if
            try self.expect(kind: .leftBrace) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightBrace) != nil
        {
            return .init(balancedTokens: balancedTokens)
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftBrace) != nil,
            try self.expect(kind: .rightBrace) != nil
        {
            return .init(balancedTokens: nil)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// balancedTokens:
    ///     | balancedToken balancedTokens
    ///     | balancedToken
    ///     ;
    /// ```
    @memoized("balancedTokens")
    @inlinable
    public func _balancedTokens() throws -> Metagrammar.BalancedTokens? {
        let mark = self.mark()

        if
            let balancedToken = try self.balancedToken(),
            let balancedTokens = try self.balancedTokens()
        {
            return .init(tokens: balancedToken.tokens + balancedTokens.tokens)
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
    /// balancedToken:
    ///     | '[' balancedTokens ']'
    ///     | '{' balancedTokens '}'
    ///     | '(' balancedTokens ')'
    ///     | '[' ~ ']'
    ///     | '{' ~ '}'
    ///     | '(' ~ ')'
    ///     | IDENT
    ///     | DIGITS
    ///     | STRING
    ///     | ':'
    ///     | ';'
    ///     | '|'
    ///     | '='
    ///     | '~'
    ///     | '*'
    ///     | '+'
    ///     | '?'
    ///     | ','
    ///     | '.'
    ///     | '@'
    ///     | '/'
    ///     | '\'
    ///     ;
    /// ```
    @memoized("balancedToken")
    @inlinable
    public func _balancedToken() throws -> Metagrammar.BalancedTokens? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect(kind: .leftBrace) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightBrace) != nil
        {
            return .init(tokens: ["{"] + balancedTokens.tokens + ["}"])
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftSquare) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightSquare) != nil
        {
            return .init(tokens: ["["] + balancedTokens.tokens + ["]"])
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftAngle) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightAngle) != nil
        {
            return .init(tokens: ["<"] + balancedTokens.tokens + [">"])
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftParen) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightParen) != nil
        {
            return .init(tokens: ["("] + balancedTokens.tokens + [")"])
        }

        if
            try self.expect(kind: .leftBrace) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightBrace) != nil
        {
            return .init(tokens: ["{"] + balancedTokens.tokens + ["}"])
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftSquare) != nil,
            cut.toggleOn(),
            try self.expect(kind: .rightSquare) != nil
        {
            return .init(tokens: ["[", "]"])
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            try self.expect(kind: .leftAngle) != nil,
            cut.toggleOn(),
            try self.expect(kind: .rightAngle) != nil
        {
            return .init(tokens: ["<", ">"])
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            try self.expect(kind: .leftParen) != nil,
            cut.toggleOn(),
            try self.expect(kind: .rightParen) != nil
        {
            return .init(tokens: ["(", ")"])
        }

        self.restore(mark)
        
        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect(oneOfKind: [
                .identifier, .digits, .string, .colon, .semicolon, .bar, .equals,
                .tilde, .star, .plus, .questionMark, .comma, .period, .at,
                .forwardSlash, .backslash,
            ])
        {
            return .init(tokens: [token.string])
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// IDENT ;
    /// ```
    @memoized("identToken")
    @inlinable
    public func _identToken() throws -> Metagrammar.IdentifierToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .identifier)
        {
            return .init(token: token.string)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// STRING ;
    /// ```
    @memoized("stringToken")
    @inlinable
    public func _stringToken() throws -> Metagrammar.StringToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .string)
        {
            return .init(token: token.string)
        }

        self.restore(mark)
        return nil
    }
}
