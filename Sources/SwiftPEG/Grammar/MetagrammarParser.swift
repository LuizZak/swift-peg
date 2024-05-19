/// A parser for SwiftPEG grammar files.
public class MetagrammarParser<RawTokenizer: RawTokenizerType>
    : PEGParser<RawTokenizer> where RawTokenizer.Token == Metagrammar.MetagrammarToken
{
    /// Fetches the next token's contents as a string.
    @memoized("peekAsString")
    func _peekAsString() throws -> String? {
        return try tokenizer.peekToken()?.kind.description
    }

    /// Fetches the next token in the stream and compares its kind against `kind`,
    /// returning the token if it matches. If the method fails, `nil` is returned
    /// and the tokenizer position is reset.
    @memoized("expect")
    public func _expect(kind: String) throws -> Token? {
        if let next = try peekAsString(), next == kind {
            return try tokenizer.next()
        }
        return nil
    }

    /// ```
    /// start: grammar ;
    /// ```
    @memoized("start")
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
    public func _meta() throws -> Metagrammar.Meta? {
        let mark = self.mark()

        if
            try self.expect("@") != nil,
            let name = try self.identToken(),
            let metaValue = try self.metaValue(),
            try self.expect(";") != nil
        {
            return .init(name: name, value: metaValue)
        }

        self.restore(mark)

        if
            try self.expect("@") != nil,
            let name = try self.identToken(),
            try self.expect(";") != nil
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
    public func _rule() throws -> Metagrammar.Rule? {
        let mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            try self.expect(":") != nil,
            (try self.maybe("|") || true),
            let alts = try self.alts(),
            try self.expect(";") != nil
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
    public func _ruleName() throws -> Metagrammar.RuleName? {
        let mark = self.mark()

        if
            let name = try self.identToken(),
            try self.expect("[") != nil,
            let type = try self.identToken(),
            try self.expect("]") != nil
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
    public func _alts() throws -> [Metagrammar.Alt]? {
        var mark = self.mark()

        var result: [Metagrammar.Alt] = []
        repeat {
            guard let alt = try self.alt() else {
                break
            }

            result.append(alt)
            mark = self.mark()
        } while try self.maybe("|")

        self.restore(mark)
        return result.isEmpty ? nil : result
    }

    /// ```
    /// alt:
    ///     | namedItems action?
    ///     ;
    /// ```
    @memoized("alt")
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
    public func _namedItem() throws -> Metagrammar.NamedItem? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            let name = try self.identToken(),
            try self.expect("=") != nil,
            cut.toggleOn(),
            try self.expect("[") != nil,
            try self.identToken() != nil,
            try self.expect("]") != nil,
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
            try self.expect("=") != nil,
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
    public func _positiveLookahead() throws -> Metagrammar.PositiveLookahead? {
        let mark = self.mark()

        if
            try self.expect("&") != nil,
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
    public func _negativeLookahead() throws -> Metagrammar.NegativeLookahead? {
        let mark = self.mark()

        if
            try self.expect("!") != nil,
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
    public func _cut() throws -> Metagrammar.Cut? {
        let mark = self.mark()

        if
            try self.expect("~") != nil
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
    public func _item() throws -> Metagrammar.Item? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect("[") != nil,
            cut.toggleOn(),
            let alts = try self.alts(),
            try self.expect("]") != nil
        {
            return Metagrammar.OptionalItems(alts: alts)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            try self.expect("?") != nil
        {
            return Metagrammar.OptionalItem(atom: atom)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            try self.expect("*") != nil
        {
            return Metagrammar.ZeroOrMoreItem(atom: atom)
        }

        self.restore(mark)

        if
            let atom = try self.atom(),
            try self.expect("+") != nil
        {
            return Metagrammar.OneOrMoreItem(atom: atom)
        }

        self.restore(mark)

        if
            let sep = try self.atom(),
            try self.expect(".") != nil,
            let node = try self.atom(),
            try self.expect("+") != nil
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

    /*
    /// ```
    /// '[' ~ alts ']' ;
    /// ```
    @memoized("optionalItems")
    public func _optionalItems() throws -> Metagrammar.OptionalItems? {
        return nil
    }

    /// ```
    /// atom '?' ;
    /// ```
    @memoized("optionalItem")
    public func _optionalItem() throws -> Metagrammar.OptionalItem? {
        return nil
    }

    /// ```
    /// atom '*' ;
    /// ```
    @memoized("zeroOrMoreItem")
    public func _zeroOrMoreItem() throws -> Metagrammar.ZeroOrMoreItem? {
        return nil
    }

    /// ```
    /// atom '+' ;
    /// ```
    @memoized("oneOrMoreItem")
    public func _oneOrMoreItem() throws -> Metagrammar.OneOrMoreItem? {
        return nil
    }

    /// ```
    /// sep=atom '.' node=atom '+' ;
    /// ```
    @memoized("gatherItem")
    public func _gatherItem() throws -> Metagrammar.GatherItem? {
        return nil
    }

    /// ```
    /// atom ;
    /// ```
    @memoized("atomItem")
    public func _atomItem() throws -> Metagrammar.AtomItem? {
        return nil
    }
    */

    /// ```
    /// atom:
    ///     | '(' ~ alts ')'
    ///     | IDENT
    ///     | STRING
    ///     ;
    /// ```
    @memoized("atom")
    public func _atom() throws -> Metagrammar.Atom? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect("(") != nil,
            cut.toggleOn(),
            let alts = try self.alts(),
            try self.expect(")") != nil
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

    /*
    /// ```
    /// '(' ~ alts ')' ;
    /// ```
    @memoized("groupAtom")
    public func _groupAtom() throws -> Metagrammar.GroupAtom? {
        return nil
    }

    /// ```
    /// STRING ;
    /// ```
    @memoized("stringAtom")
    public func _stringAtom() throws -> Metagrammar.StringAtom? {
        return nil
    }

    /// ```
    /// IDENT ;
    /// ```
    @memoized("identAtom")
    public func _identAtom() throws -> Metagrammar.IdentAtom? {
        return nil
    }
    */

    /// ```
    /// action: '{' ~ balancedTokens? '}' ;
    /// ```
    @memoized("action")
    public func _action() throws -> Metagrammar.Action? {
        let mark = self.mark()
        var cut = CutFlag()

        if
            try self.expect("{") != nil,
            cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            try self.expect("}") != nil
        {
            return .init(balancedTokens: balancedTokens)
        }

        self.restore(mark)

        if cut.isOn {
            return nil
        }
        
        return nil
    }

    /// ```
    /// balancedTokens:
    ///     | '[' ~ balancedTokens* ']'
    ///     | '{' ~ balancedTokens* '}'
    ///     | '(' ~ balancedTokens* ')'
    ///     | balancedToken+
    ///     ;
    ///
    /// balancedToken:
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
    @memoized("balancedTokens")
    public func _balancedTokens() throws -> Metagrammar.BalancedTokens? {
        return nil
    }

    /// ```
    /// IDENT ;
    /// ```
    @memoized("identToken")
    public func _identToken() throws -> Metagrammar.IdentifierToken? {
        let mark = self.mark()

        if
            let next = try self.tokenizer.next(),
            case .identifier = next
        {
            return .init(token: next.string)
        }

        self.restore(mark)
        return nil
    }

    /// ```
    /// STRING ;
    /// ```
    @memoized("stringToken")
    public func _stringToken() throws -> Metagrammar.StringToken? {
        let mark = self.mark()

        if
            let next = try self.tokenizer.next(),
            case .string = next
        {
            return .init(token: next.string)
        }

        self.restore(mark)
        return nil
    }
}
