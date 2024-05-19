/// A parser for SwiftPEG grammar files.
public final class MetagrammarParser<RawTokenizer: RawTokenizerType>
    : PEGParser<RawTokenizer> where RawTokenizer.Token == Metagrammar.MetagrammarToken
{
    /// ```
    /// start[Grammar]: grammar { grammar };
    /// ```
    @memoized("start")
    @inlinable
    public func _start() throws -> Metagrammar.Grammar? {
        let mark = self.mark()
        
        if
            let grammar = try grammar()
        {
            return grammar
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// grammar[Grammar]:
    ///     | metas rules { Metagrammar.Grammar(metas: metas, rules: rules) }
    ///     | rules { Metagrammar.Grammar(metas: [], rules: rules) }
    ///     ;
    /// ```
    @memoized("grammar")
    @inlinable
    public func _grammar() throws -> Metagrammar.Grammar? {
        let mark = self.mark()

        if
            let metas = try self.metas(),
            let rules = try self.rules()
        {
            return .init(metas: metas, rules: rules)
        }

        self.restore(mark)

        if
            let rules = try self.rules()
        {
            return .init(metas: [], rules: rules)
        }

        self.restore(mark)

        return nil
    }

    /// ```
    /// metas[[Metagrammar.Meta]]:
    ///     | meta metas { [meta] + metas }
    ///     | meta { [meta] }
    ///     ;
    /// ```
    @memoized("metas")
    @inlinable
    public func _metas() throws -> [Metagrammar.Meta]? {
        let mark = self.mark()

        if
            let meta = try self.meta(),
            let metas = try self.metas()
        {
            return [meta] + metas
        }

        self.restore(mark)

        if
            let meta = try self.meta()
        {
            return [meta]
        }

        self.restore(mark)

        return nil
    }

    /// ```
    /// meta:
    ///     | "@" name=IDENT value=metaValue ';' { .init(name: name, value: value) }
    ///     | "@" name=IDENT ';' { .init(name: name, value: nil) }
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func _meta() throws -> Metagrammar.Meta? {
        let mark = self.mark()

        if
            try self.expect(kind: .at) != nil,
            let name = try self.identToken(),
            let value = try self.metaValue(),
            try self.expect(kind: .semicolon) != nil
        {
            return .init(name: name, value: value)
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
    ///     | ident=IDENT { Metagrammar.MetaIdentifierValue(identifier: ident) }
    ///     | string=STRING { Metagrammar.MetaStringValue(string: string) }
    ///     ;
    /// ```
    @memoized("metaValue")
    @inlinable
    public func _metaValue() throws -> Metagrammar.MetaValue? {
        let mark = self.mark()

        if
            let ident = try self.identToken()
        {
            return Metagrammar.MetaIdentifierValue(identifier: ident)
        }

        self.restore(mark)

        if
            let string = try self.stringToken()
        {
            return Metagrammar.MetaStringValue(string: string)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// rules[[Metagrammar.Rule]]:
    ///     | rule rules { [rule] + rules }
    ///     | rule { [rule] }
    ///     ;
    /// ```
    @memoized("rules")
    @inlinable
    public func _rules() throws -> [Metagrammar.Rule]? {
        let mark = self.mark()

        if
            let rule = try self.rule(),
            let rules = try self.rules()
        {
            return [rule] + rules
        }

        self.restore(mark)

        if
            let rule = try self.rule()
        {
            return [rule]
        }

        self.restore(mark)

        return nil
    }

    /// ```
    /// rule:
    ///     | ruleName ':' '|' alts ';'
    ///     | ruleName ':' alts ';'
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func _rule() throws -> Metagrammar.Rule? {
        let mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            try self.expect(kind: .colon) != nil,
            try self.expect(kind: .bar) != nil,
            let alts = try self.alts(),
            try self.expect(kind: .semicolon) != nil
        {
            return .init(name: ruleName, alts: alts)
        }

        self.restore(mark)

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(kind: .colon),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .semicolon)
        {
            return .init(name: ruleName, alts: alts)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// ruleName:
    ///     | name=IDENT '[' type=swiftType ']' { .init(name: name, type: type) }
    ///     | name=IDENT { .init(name: name, type: nil) }
    ///     ;
    /// ```
    @memoized("ruleName")
    @inlinable
    public func _ruleName() throws -> Metagrammar.RuleName? {
        let mark = self.mark()

        if
            let name = try self.identToken(),
            try self.expect(kind: .leftSquare) != nil,
            let type = try self.swiftType(),
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

    /// alts[[Metagrammar.Alt]]:
    ///     | alt "|" alts { Rhs([alt] + alts.alts) }
    ///     | alt { Rhs([alt]) }
    ///     ;
    @memoized("alts")
    @inlinable
    public func _alts() throws -> [Metagrammar.Alt]? {
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
    /// alt:
    ///     | namedItems action { .init(namedItems: namedItems, action: action) }
    ///     | namedItems { .init(namedItems: namedItems, action: nil) }
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func _alt() throws -> Metagrammar.Alt? {
        let mark = self.mark()

        if
            let namedItems = try self.namedItems(),
            let action = try self.action()
        {
            return .init(namedItems: namedItems, action: action)
        }

        self.restore(mark)

        if
            let namedItems = try self.namedItems()
        {
            return .init(namedItems: namedItems, action: nil)
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
    public func _namedItems() throws -> [Metagrammar.NamedItem]? {
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
    /// namedItem:
    ///     | name=IDENT '[' type=swiftType ']' '=' ~ item
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
            let swiftType = try self.swiftType(),
            try self.expect(kind: .rightSquare) != nil,
            try self.expect(kind: .equals) != nil,
            cut.toggleOn(),
            let item = try self.item()
        {
            return .init(name: name, item: item, type: swiftType, lookahead: nil)
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
            return .init(name: name, item: item, type: nil, lookahead: nil)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let item = try self.item()
        {
            return .init(name: nil, item: item, type: nil, lookahead: nil)
        }

        self.restore(mark)

        if
            let lookahead = try self.lookahead()
        {
            return .init(name: nil, item: nil, type: nil, lookahead: lookahead)
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
            try self.expect(kind: .ampersand) != nil,
            let atom = try self.atom()
        {
            return Metagrammar.PositiveLookahead(atom: atom)
        }

        self.restore(mark)

        if
            try self.expect(kind: .exclamationMark) != nil,
            let atom = try self.atom()
        {
            return Metagrammar.NegativeLookahead(atom: atom)
        }

        self.restore(mark)

        if
            try self.expect(kind: .exclamationMark) != nil,
            let atom = try self.atom()
        {
            return Metagrammar.NegativeLookahead(atom: atom)
        }

        if
            try self.expect(kind: .tilde) != nil
        {
            return Metagrammar.Cut()
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
    ///     | '(' ~ alts ')' { Metagrammar.GroupAtom(alts: alts) }
    ///     | IDENT { Metagrammar.IdentAtom(identifier: ident) }
    ///     | STRING { Metagrammar.StringAtom(string: string) }
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
    /// swiftType[SwiftType]:
    ///     | '[' ~ type=swiftType ']' { .init(name: "[" + type.name + "]") }
    ///     | '(' ~ types=swiftTypeList ')' { .init(name: "(" + types.map(\.name).joined(separator: ", ") + ")") }
    ///     | name=IDENT '<' ~ types=swiftTypeList '>' { .init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">") }
    ///     | name=IDENT '.' inner=swiftType { .init(name: name.identifier + "." + inner.name) }
    ///     | name=IDENT '?' { .init(name: name.identifier + "?") }
    ///     | name=IDENT { .init(name: name.identifier) }
    ///     ;
    /// ```
    @memoized("swiftType")
    @inlinable
    public func _swiftType() throws -> Metagrammar.SwiftType? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect(kind: .leftSquare) != nil,
            cut.toggleOn(),
            let type = try self.swiftType(),
            try self.expect(kind: .rightSquare) != nil
        {
            return .init(name: "[" + type.name + "]")
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            try self.expect(kind: .leftParen) != nil,
            cut.toggleOn(),
            let types = try self.swiftTypes(),
            try self.expect(kind: .rightParen) != nil
        {
            return .init(name: "(" + types.map(\.name).joined(separator: ", ") + ")")
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.identToken(),
            try self.expect(kind: .leftAngle) != nil,
            cut.toggleOn(),
            let types = try self.swiftTypes(),
            try self.expect(kind: .rightAngle) != nil,
            try self.expect(kind: .questionMark) != nil
        {
            return .init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">?")
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.identToken(),
            try self.expect(kind: .leftAngle) != nil,
            cut.toggleOn(),
            let types = try self.swiftTypes(),
            try self.expect(kind: .rightAngle) != nil
        {
            return .init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">")
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        
        if
            let name = try self.identToken(),
            try self.expect(kind: .period) != nil,
            cut.toggleOn(),
            let inner = try self.swiftType()
        {
            return .init(name: name.identifier + "." + inner.name)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        
        if
            let name = try self.identToken(),
            try self.expect(kind: .questionMark) != nil
        {
            return .init(name: name.identifier + "?")
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        
        if
            let name = try self.identToken()
        {
            return .init(name: name.identifier)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// swiftTypeList[[SwiftType]]:
    ///     | type=swiftType ',' types=swiftTypeList { [type] + types }
    ///     | type=swiftType { [type] }
    ///     ;
    /// ```
    @memoized("swiftTypes")
    @inlinable
    public func _swiftTypes() throws -> [Metagrammar.SwiftType]? {
        return []
    }

    /// ```
    /// action: '{' ~ balancedTokens '}' ;
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
        return nil
    }

    /// ```
    /// balancedTokens:
    ///     | balancedToken balancedTokens { .init(tokens: balancedToken.tokens + balancedTokens.tokens) }
    ///     | balancedToken { balancedToken }
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
    ///     | '{' balancedTokens '}' { .init(tokens: ["{"] + balancedTokens.tokens + ["}"]) }
    ///     | '[' balancedTokens ']' { .init(tokens: ["["] + balancedTokens.tokens + ["]"]) }
    ///     | '<' balancedTokens '>' { .init(tokens: ["<"] + balancedTokens.tokens + [">"]) }
    ///     | '(' balancedTokens ')' { .init(tokens: ["("] + balancedTokens.tokens + [")"]) }
    ///     | '[' ~ ']' { .init(tokens: ["(", ")"]) }
    ///     | '{' ~ '}' { .init(tokens: ["{", "}"]) }
    ///     | '<' ~ '>' { .init(tokens: ["<", ">"]) }
    ///     | '(' ~ ')' { .init(tokens: ["(", ")"]) }
    ///     | token=balancedTokenAtom { .init(tokens: [token.string]) }
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
            cut.toggleOn(),
            try self.expect(kind: .rightBrace) != nil
        {
            return .init(tokens: ["{", "}"])
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
            let token = try self.balancedTokenAtom()
        {
            return .init(tokens: [token.string])
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// balancedTokenAtom[Token]:
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
    @memoized("balancedTokenAtom")
    @inlinable
    public func _balancedTokenAtom() throws -> Token? {
        let mark = self.mark()

        if
            let token = try self.expect(oneOfKind: [
                .identifier, .digits, .string, .colon, .semicolon, .bar, .equals,
                .tilde, .star, .plus, .questionMark, .comma, .period, .at,
                .forwardSlash, .backslash,
            ])
        {
            return token
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
