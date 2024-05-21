/// A parser for SwiftPEG grammar files.
public final class MetagrammarParser<RawTokenizer: RawTokenizerType>
    : PEGParser<RawTokenizer> where RawTokenizer.Token == Metagrammar.MetagrammarToken
{
    /// ```
    /// IDENT ;
    /// ```
    @memoized("IDENT")
    @inlinable
    public func _IDENT() throws -> Metagrammar.IdentifierToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .identifier)
        {
            return self.setLocation(.init(token: token.token.string, location: token.location), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// IDENT ;
    /// ```
    @memoized("DIGITS")
    @inlinable
    public func _DIGITS() throws -> Metagrammar.IdentifierToken? {
        let mark = self.mark()

        if
            let token = try self.expect(kind: .digits)
        {
            return self.setLocation(.init(token: token.token.string, location: token.location), at: mark)
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
            return self.setLocation(.init(token: token.token.string, location: token.location), at: mark)
        }

        self.restore(mark)
        return nil
    }
}

#if false

extension MetagrammarParser {
    @memoized("start")
    @inlinable
    public func __start() throws -> Metagrammar.Grammar? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let grammar = try self.grammar()
        {
            return grammar
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("grammar")
    @inlinable
    public func __grammar() throws -> Metagrammar.Grammar? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let metas = try self.metas(),
            let rules = try self.rules()
        {
            return self.setLocation(.init(metas:metas,rules:rules),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let rules = try self.rules()
        {
            return self.setLocation(.init(metas:[],rules:rules),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("metas")
    @inlinable
    public func __metas() throws -> [Metagrammar.Meta]? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let metas = try self.repeatOneOrMore({
                try self.meta()
            })
        {
            return metas
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("meta")
    @inlinable
    public func __meta() throws -> Metagrammar.Meta? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("@"),
            let name = try self.IDENT(),
            let value = try self.metaValue(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name:name,value:value),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("@"),
            let name = try self.IDENT(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name:name,value:nil),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("metaValue")
    @inlinable
    public func __metaValue() throws -> Metagrammar.MetaValue? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let ident = try self.IDENT()
        {
            return self.setLocation(Metagrammar.MetaIdentifierValue(identifier:ident),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let string = try self.STRING()
        {
            return self.setLocation(Metagrammar.MetaStringValue(string:string),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("rules")
    @inlinable
    public func __rules() throws -> [Metagrammar.Rule]? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let rules = try self.repeatOneOrMore({
                try self.rule()
            })
        {
            return rules
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("rule")
    @inlinable
    public func __rule() throws -> Metagrammar.Rule? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(":"),
            let _ = try self.expect("|"),
            let alts = try self.alts(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name:ruleName,alts:alts),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(":"),
            let alts = try self.alts(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name:ruleName,alts:alts),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("ruleName")
    @inlinable
    public func __ruleName() throws -> Metagrammar.RuleName? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let a = try self.IDENT(),
            let _ = try self.expect("["),
            let type = try self.swiftType(),
            let _ = try self.expect("]")
        {
            return self.setLocation(.init(name:name,type:type),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let a = try self.IDENT()
        {
            return self.setLocation(.init(name:name,type:nil),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("alts")
    @inlinable
    public func __alts() throws -> [Metagrammar.Alt]? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let alt = try self.alt(),
            let _ = try self.expect("|"),
            let alts = try self.alts()
        {
            return [alt]+alts
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let alt = try self.alt()
        {
            return [alt]
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("alt")
    @inlinable
    public func __alt() throws -> Metagrammar.Alt? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let namedItems = try self.namedItems(),
            let action = try self.action()
        {
            return self.setLocation(.init(namedItems:namedItems,action:action),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let namedItems = try self.namedItems()
        {
            return self.setLocation(.init(namedItems:namedItems,action:nil),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("namedItems")
    @inlinable
    public func __namedItems() throws -> [Metagrammar.NamedItem]? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let namedItem = try self.namedItem(),
            let namedItems = try self.namedItems()
        {
            return [namedItem]+namedItems
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let namedItem = try self.namedItem()
        {
            return [namedItem]
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("namedItem")
    @inlinable
    public func __namedItem() throws -> Metagrammar.NamedItem? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let name = try self.IDENT(),
            let _ = try self.expect("["),
            let type = try self.swiftType(),
            let _ = try self.expect("]"),
            let _ = try self.expect("="),
            cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name:name,item:item,type:swiftType,lookahead:nil),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT(),
            let _ = try self.expect("="),
            cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name:name,item:item,type:nil,lookahead:nil),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let item = try self.item()
        {
            return self.setLocation(.init(name:nil,item:item,type:nil,lookahead:nil),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let lookahead = try self.lookahead()
        {
            return self.setLocation(.init(name:nil,item:nil,type:nil,lookahead:lookahead),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("lookahead")
    @inlinable
    public func __lookahead() throws -> Metagrammar.LookaheadOrCut? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("&"),
            cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.PositiveLookahead(atom:atom),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("!"),
            cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.NegativeLookahead(atom:atom),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("~")
        {
            return self.setLocation(Metagrammar.Cut(),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("item")
    @inlinable
    public func __item() throws -> Metagrammar.Item? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("["),
            cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect("]")
        {
            return self.setLocation(Metagrammar.OptionalItems(alts:alts),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect("?")
        {
            return self.setLocation(Metagrammar.OptionalItem(atom:atom),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect("*")
        {
            return self.setLocation(Metagrammar.ZeroOrMoreItem(atom:atom),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect("+")
        {
            return self.setLocation(Metagrammar.OneOrMoreItem(atom:atom),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let sep = try self.atom(),
            let _ = try self.expect("."),
            let node = try self.atom(),
            let _ = try self.expect("+")
        {
            return self.setLocation(Metagrammar.GatherItem(sep:sep,item:node),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.AtomItem(atom:atom),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("atom")
    @inlinable
    public func __atom() throws -> Metagrammar.Atom? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("("),
            cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(")")
        {
            return self.setLocation(Metagrammar.GroupAtom(alts:alts),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let a = try self.IDENT()
        {
            return self.setLocation(Metagrammar.IdentAtom(identifier:ident,identity:.unresolved),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let a = try self.STRING()
        {
            return self.setLocation(Metagrammar.StringAtom(string:string),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("swiftType")
    @inlinable
    public func __swiftType() throws -> Metagrammar.SwiftType? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("["),
            cut.toggleOn(),
            let type = try self.swiftType(),
            let _ = try self.expect("]")
        {
            return self.setLocation(.init(name:"["+type.name+"]"),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("("),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(")")
        {
            return self.setLocation(.init(name:"("+types.map(\.name).joined(separator:", ")+")"),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT(),
            let _ = try self.expect("<"),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(">"),
            let _ = try self.expect("?")
        {
            return self.setLocation(.init(name:name.identifier+"<"+types.map(\.name).joined(separator:", ")+">?"),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT(),
            let _ = try self.expect("<"),
            cut.toggleOn(),
            let types = try self.swiftTypeList(),
            let _ = try self.expect(">")
        {
            return self.setLocation(.init(name:name.identifier+"<"+types.map(\.name).joined(separator:", ")+">"),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT(),
            let _ = try self.expect("."),
            cut.toggleOn(),
            let inner = try self.swiftType()
        {
            return self.setLocation(.init(name:name.identifier+"."+inner.name),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT(),
            let _ = try self.expect("?")
        {
            return self.setLocation(.init(name:name.identifier+"?"),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT()
        {
            return self.setLocation(.init(name:name.identifier),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("swiftTypeList")
    @inlinable
    public func __swiftTypeList() throws -> [Metagrammar.SwiftType]? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let type = try self.swiftType(),
            let _ = try self.expect(","),
            let types = try self.swiftTypeList()
        {
            return [type]+types
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let type = try self.swiftType()
        {
            return [type]
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("action")
    @inlinable
    public func __action() throws -> Metagrammar.Action? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("{"),
            cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect("}")
        {
            return self.setLocation(.init(balancedTokens:balancedTokens),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("balancedTokens")
    @inlinable
    public func __balancedTokens() throws -> Metagrammar.BalancedTokens? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let balancedToken = try self.balancedToken(),
            let balancedTokens = try self.balancedTokens()
        {
            return self.setLocation(.init(tokens:balancedToken.tokens+balancedTokens.tokens),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let balancedToken = try self.balancedToken()
        {
            return balancedToken
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("balancedToken")
    @inlinable
    public func __balancedToken() throws -> Metagrammar.BalancedTokens? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let _ = try self.expect("{"),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect("}")
        {
            return self.setLocation(.init(tokens:["{"]+balancedTokens.tokens+["}"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("["),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect("]")
        {
            return self.setLocation(.init(tokens:["["]+balancedTokens.tokens+["]"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("<"),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(">")
        {
            return self.setLocation(.init(tokens:["<"]+balancedTokens.tokens+[">"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("("),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(")")
        {
            return self.setLocation(.init(tokens:["("]+balancedTokens.tokens+[")"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("["),
            cut.toggleOn(),
            let _ = try self.expect("]")
        {
            return self.setLocation(.init(tokens:["(",")"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("{"),
            cut.toggleOn(),
            let _ = try self.expect("}")
        {
            return self.setLocation(.init(tokens:["{","}"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("<"),
            cut.toggleOn(),
            let _ = try self.expect(">")
        {
            return self.setLocation(.init(tokens:["<",">"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("("),
            cut.toggleOn(),
            let _ = try self.expect(")")
        {
            return self.setLocation(.init(tokens:["(",")"]),at:mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.balancedTokenAtom()
        {
            return .init(tokens:[token.string])
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }

    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> Metagrammar.MetagrammarToken? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let token = try self.IDENT()
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.DIGITS()
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.STRING()
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect(":")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect(";")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("|")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("=")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("~")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("*")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("+")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("?")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect(",")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect(".")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("@")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("/")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let token = try self.expect("\\")
        {
            return token.token
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        return nil
    }
}

#else

// MARK: - Current Parser

extension MetagrammarParser {
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
            return setLocation(grammar, at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// grammar[Grammar]:
    ///     | metas rules { setLocation(.init(metas: metas, rules: rules), at: mark) }
    ///     | rules { setLocation(.init(metas: [], rules: rules), at: mark) }
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
    public func _metas() throws -> [Metagrammar.Meta]? {
        let mark = self.mark()

        if
            let metas = try self.repeatOneOrMore(self.meta)
        {
            return metas
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
            return self.setLocation(.init(name: name, value: value), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .at) != nil,
            let name = try self.identToken(),
            try self.expect(kind: .semicolon) != nil
        {
            return self.setLocation(.init(name: name, value: nil), at: mark)
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
            return self.setLocation(Metagrammar.MetaIdentifierValue(identifier: ident), at: mark)
        }

        self.restore(mark)

        if
            let string = try self.stringToken()
        {
            return self.setLocation(Metagrammar.MetaStringValue(string: string), at: mark)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// rules[[Metagrammar.Rule]]:
    ///     | rules=rule+ { rules }
    ///     | rule { [rule] }
    ///     ;
    /// ```
    @memoized("rules")
    @inlinable
    public func _rules() throws -> [Metagrammar.Rule]? {
        let mark = self.mark()

        if
            let rules = try self.repeatOneOrMore(self.rule)
        {
            return rules
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
            return self.setLocation(.init(name: name, type: type), at: mark)
        }

        self.restore(mark)

        if
            let name = try self.identToken()
        {
            return self.setLocation(.init(name: name, type: nil), at: mark)
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
            return self.setLocation(.init(name: name, item: item, type: swiftType, lookahead: nil), at: mark)
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

        if
            let lookahead = try self.lookahead()
        {
            return self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: mark)
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
            return self.setLocation(Metagrammar.PositiveLookahead(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .exclamationMark) != nil,
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.NegativeLookahead(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .exclamationMark) != nil,
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.NegativeLookahead(atom: atom), at: mark)
        }

        if
            try self.expect(kind: .tilde) != nil
        {
            return self.setLocation(Metagrammar.Cut(), at: mark)
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
            return self.setLocation(Metagrammar.OptionalItems(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            try self.expect(kind: .questionMark) != nil
        {
            return self.setLocation(Metagrammar.OptionalItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            try self.expect(kind: .star) != nil
        {
            return self.setLocation(Metagrammar.ZeroOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            try self.expect(kind: .plus) != nil
        {
            return self.setLocation(Metagrammar.OneOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if
            let sep = try self.atom(),
            try self.expect(kind: .period) != nil,
            let node = try self.atom(),
            try self.expect(kind: .plus) != nil
        {
            return self.setLocation(Metagrammar.GatherItem(sep: sep, item: node), at: mark)
        }

        self.restore(mark)

        if
            let atom = try self.atom()
        {
            return self.setLocation(Metagrammar.AtomItem(atom: atom), at: mark)
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
            return self.setLocation(Metagrammar.GroupAtom(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let ident = try self.identToken()
        {
            return self.setLocation(Metagrammar.IdentAtom(identifier: ident, identity: .unresolved), at: mark)
        }

        self.restore(mark)

        if
            let string = try self.stringToken()
        {
            return self.setLocation(Metagrammar.StringAtom(string: string), at: mark)
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
            return self.setLocation(.init(name: "[" + type.name + "]"), at: mark)
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
            return self.setLocation(.init(name: "(" + types.map(\.name).joined(separator: ", ") + ")"), at: mark)
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
            return self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">?"), at: mark)
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
            return self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">"), at: mark)
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
            return self.setLocation(.init(name: name.identifier + "." + inner.name), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        
        if
            let name = try self.identToken(),
            try self.expect(kind: .questionMark) != nil
        {
            return self.setLocation(.init(name: name.identifier + "?"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        
        if
            let name = try self.identToken()
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
            return self.setLocation(.init(balancedTokens: balancedTokens), at: mark)
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
            return self.setLocation(.init(tokens: ["{"] + balancedTokens.tokens + ["}"]), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftSquare) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightSquare) != nil
        {
            return self.setLocation(.init(tokens: ["["] + balancedTokens.tokens + ["]"]), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftAngle) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightAngle) != nil
        {
            return self.setLocation(.init(tokens: ["<"] + balancedTokens.tokens + [">"]), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftParen) != nil,
            let balancedTokens = try self.balancedTokens(),
            try self.expect(kind: .rightParen) != nil
        {
            return self.setLocation(.init(tokens: ["("] + balancedTokens.tokens + [")"]), at: mark)
        }

        if
            try self.expect(kind: .leftBrace) != nil,
            cut.toggleOn(),
            try self.expect(kind: .rightBrace) != nil
        {
            return self.setLocation(.init(tokens: ["{", "}"]), at: mark)
        }

        self.restore(mark)

        if
            try self.expect(kind: .leftSquare) != nil,
            cut.toggleOn(),
            try self.expect(kind: .rightSquare) != nil
        {
            return self.setLocation(.init(tokens: ["[", "]"]), at: mark)
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
            return self.setLocation(.init(tokens: ["<", ">"]), at: mark)
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
            return self.setLocation(.init(tokens: ["(", ")"]), at: mark)
        }

        self.restore(mark)
        
        if cut.isOn {
            return nil
        }

        if
            let token = try self.balancedTokenAtom()
        {
            return self.setLocation(.init(tokens: [token.string]), at: mark)
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
            return token.token
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
            return self.setLocation(.init(token: token.token.string, location: token.location), at: mark)
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
            return self.setLocation(.init(token: token.token.string, location: token.location), at: mark)
        }

        self.restore(mark)
        return nil
    }
}

#endif
