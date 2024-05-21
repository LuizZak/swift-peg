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
    /// IDENT ;
    /// ```
    @memoized("IDENT")
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
    /// IDENT ;
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

    @memoized("meta")
    @inlinable
    public func __meta() throws -> Metagrammar.Meta? {
        let mark = self.mark()

        if
            let _ = try self.expect("@"),
            let name = try self.IDENT(),
            let value = try self.metaValue(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name: name, value: value), at: mark)
        }

        self.restore(mark)

        if
            let _ = try self.expect("@"),
            let name = try self.IDENT(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name: name, value: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    @memoized("metaValue")
    @inlinable
    public func __metaValue() throws -> Metagrammar.MetaValue? {
        let mark = self.mark()

        if
            let ident = try self.IDENT()
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

    @memoized("rule")
    @inlinable
    public func __rule() throws -> Metagrammar.Rule? {
        let mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(":"),
            let _ = try self.expect("|"),
            let alts = try self.alts(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name: ruleName, alts: alts), at: mark)
        }

        self.restore(mark)

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(":"),
            let alts = try self.alts(),
            let _ = try self.expect(";")
        {
            return self.setLocation(.init(name: ruleName, alts: alts), at: mark)
        }

        self.restore(mark)
        return nil
    }

    @memoized("ruleName")
    @inlinable
    public func __ruleName() throws -> Metagrammar.RuleName? {
        let mark = self.mark()

        if
            let name = try self.IDENT(),
            let _ = try self.expect("["),
            let type = try self.swiftType(),
            let _ = try self.expect("]")
        {
            return self.setLocation(.init(name: name, type: type), at: mark)
        }

        self.restore(mark)

        if
            let name = try self.IDENT()
        {
            return self.setLocation(.init(name: name, type: nil), at: mark)
        }

        self.restore(mark)
        return nil
    }

    @memoized("alts")
    @inlinable
    public func __alts() throws -> [Metagrammar.Alt]? {
        let mark = self.mark()

        if
            let alt = try self.alt(),
            let _ = try self.expect("|"),
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
            return self.setLocation(.init(name: name, item: item, type: type, lookahead: nil), at: mark)
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
            return self.setLocation(Metagrammar.PositiveLookahead(atom: atom), at: mark)
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
            return self.setLocation(Metagrammar.NegativeLookahead(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let _ = try self.expect("~")
        {
            return self.setLocation(Metagrammar.Cut(), at: mark)
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
            return self.setLocation(Metagrammar.OptionalItems(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect("?")
        {
            return self.setLocation(Metagrammar.OptionalItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect("*")
        {
            return self.setLocation(Metagrammar.ZeroOrMoreItem(atom: atom), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect("+")
        {
            return self.setLocation(Metagrammar.OneOrMoreItem(atom: atom), at: mark)
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
            return self.setLocation(Metagrammar.GroupAtom(alts: alts), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let ident = try self.IDENT()
        {
            return self.setLocation(Metagrammar.IdentAtom(identifier: ident, identity: .unresolved), at: mark)
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
            let _ = try self.expect("["),
            cut.toggleOn(),
            let type = try self.swiftType(),
            let _ = try self.expect("]")
        {
            return self.setLocation(.init(name: "[" + type.name + "]"), at: mark)
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
            return self.setLocation(.init(name: "(" + types.map(\.name).joined(separator: ", ") + ")"), at: mark)
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
            return self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">?"), at: mark)
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
            return self.setLocation(.init(name: name.identifier + "<" + types.map(\.name).joined(separator: ", ") + ">"), at: mark)
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
            return self.setLocation(.init(name: name.identifier + "." + inner.name), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT(),
            let _ = try self.expect("?")
        {
            return self.setLocation(.init(name: name.identifier + "?"), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let name = try self.IDENT()
        {
            return self.setLocation(.init(name: name.identifier), at: mark)
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

        if
            let type = try self.swiftType(),
            let _ = try self.expect(","),
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
            return self.setLocation(.init(balancedTokens: balancedTokens), at: mark)
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
            let l = try self.expect("{"),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect("}")
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("["),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect("]")
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("<"),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(">")
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("("),
            let balancedTokens = try self.balancedTokens(),
            let r = try self.expect(")")
        {
            return self.setLocation(.init(tokens: [.init(l)] + balancedTokens.tokens + [.init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("["),
            cut.toggleOn(),
            let r = try self.expect("]")
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("{"),
            cut.toggleOn(),
            let r = try self.expect("}")
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("<"),
            cut.toggleOn(),
            let r = try self.expect(">")
        {
            return self.setLocation(.init(tokens: [.init(l), .init(r)]), at: mark)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let l = try self.expect("("),
            cut.toggleOn(),
            let r = try self.expect(")")
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

    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> TokenNode<RawTokenizer.Token, RawTokenizer.Location>? {
        let mark = self.mark()

        if
            let token = try self.IDENT()
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
            let token = try self.expect(":")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(";")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("|")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("=")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("~")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("*")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("+")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("?")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(",")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect(".")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("@")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("/")
        {
            return .init(token)
        }

        self.restore(mark)

        if
            let token = try self.expect("\\")
        {
            return .init(token)
        }

        self.restore(mark)
        return nil
    }
}
