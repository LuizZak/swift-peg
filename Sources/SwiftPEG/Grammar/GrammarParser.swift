// HEADS UP! This is a generated file

/// A parser for SwiftPEG grammar files.
public final class GrammarParser<RawTokenizer: RawTokenizerType>: PEGParser<RawTokenizer>
    where RawTokenizer.RawToken == SwiftPEGGrammar.Token, RawTokenizer.Location == FileSourceLocation
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
        let _mark = self.mark()

        if
            let grammar = try self.grammar()
        {
            return grammar
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// grammar[SwiftPEGGrammar.Grammar]:
    ///     | metas=meta* rules=rule+ { self.setLocation(.init(metas: metas, rules: rules), at: _mark) }
    ///     ;
    /// ```
    @memoized("grammar")
    @inlinable
    public func __grammar() throws -> SwiftPEGGrammar.Grammar? {
        let _mark = self.mark()

        if
            let metas = try self.repeatZeroOrMore({
                try self.meta()
            }),
            let rules = try self.repeatOneOrMore({
                try self.rule()
            })
        {
            return self.setLocation(.init(metas: metas, rules: rules), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// meta[SwiftPEGGrammar.Meta]:
    ///     | "@" name=IDENTIFIER value=metaValue? ';' { self.setLocation(.init(name: name.rawToken, value: value), at: _mark) }
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func __meta() throws -> SwiftPEGGrammar.Meta? {
        let _mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let name = try self.expect(kind: .identifier),
            case let value = try self.metaValue(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, value: value), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// metaValue[SwiftPEGGrammar.MetaValue]:
    ///     | ident=IDENTIFIER { self.setLocation(SwiftPEGGrammar.MetaIdentifierValue(identifier: ident.rawToken), at: _mark) }
    ///     | string { self.setLocation(SwiftPEGGrammar.MetaStringValue(string: string), at: _mark) }
    ///     ;
    /// ```
    @memoized("metaValue")
    @inlinable
    public func __metaValue() throws -> SwiftPEGGrammar.MetaValue? {
        let _mark = self.mark()

        if
            let ident = try self.expect(kind: .identifier)
        {
            return self.setLocation(SwiftPEGGrammar.MetaIdentifierValue(identifier: ident.rawToken), at: _mark)
        }

        self.restore(_mark)

        if
            let string = try self.string()
        {
            return self.setLocation(SwiftPEGGrammar.MetaStringValue(string: string), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// rule[SwiftPEGGrammar.Rule]:
    ///     | ruleName ":" action? failAction? '|'? alts ';' { self.setLocation(.init(name: ruleName, action: action, failAction: failAction, alts: alts), at: _mark) }
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func __rule() throws -> SwiftPEGGrammar.Rule? {
        let _mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            let _ = try self.expect(kind: .colon),
            case let action = try self.action(),
            case let failAction = try self.failAction(),
            case _ = try self.expect(kind: .bar),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: ruleName, action: action, failAction: failAction, alts: alts), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// ruleName[SwiftPEGGrammar.RuleName]:
    ///     | name=IDENTIFIER '[' ~ type=swiftType ']' { self.setLocation(.init(name: name.rawToken, type: type), at: _mark) }
    ///     | name=IDENTIFIER { self.setLocation(.init(name: name.rawToken, type: nil), at: _mark) }
    ///     ;
    /// ```
    @memoized("ruleName")
    @inlinable
    public func __ruleName() throws -> SwiftPEGGrammar.RuleName? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            _cut.toggleOn(),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(.init(name: name.rawToken, type: type), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier)
        {
            return self.setLocation(.init(name: name.rawToken, type: nil), at: _mark)
        }

        self.restore(_mark)
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
        let _mark = self.mark()

        if
            let alt = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.alt()
            })
        {
            return alt
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// alt[SwiftPEGGrammar.Alt]:
    ///     | namedItems action? failAction? { self.setLocation(.init(namedItems: namedItems, action: action, failAction: failAction), at: _mark) }
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func __alt() throws -> SwiftPEGGrammar.Alt? {
        let _mark = self.mark()

        if
            let namedItems = try self.namedItems(),
            case let action = try self.action(),
            case let failAction = try self.failAction()
        {
            return self.setLocation(.init(namedItems: namedItems, action: action, failAction: failAction), at: _mark)
        }

        self.restore(_mark)
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
        let _mark = self.mark()

        if
            let namedItem = try self.repeatOneOrMore({
                try self.namedItem()
            })
        {
            return namedItem
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// namedItem[SwiftPEGGrammar.NamedItem]:
    ///     | name=IDENTIFIER '[' type=swiftType ']' '=' ~ item { self.setLocation(.init(name: name.rawToken, item: item, type: type, lookahead: nil), at: _mark) }
    ///     | name=IDENTIFIER '=' ~ item { self.setLocation(.init(name: name.rawToken, item: item, type: nil, lookahead: nil), at: _mark) }
    ///     | item { self.setLocation(.init(name: nil, item: item, type: nil, lookahead: nil), at: _mark) }
    ///     | lookahead { self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: _mark) }
    ///     ;
    /// ```
    @memoized("namedItem")
    @inlinable
    public func __namedItem() throws -> SwiftPEGGrammar.NamedItem? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let type = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .equals),
            _cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name: name.rawToken, item: item, type: type, lookahead: nil), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .equals),
            _cut.toggleOn(),
            let item = try self.item()
        {
            return self.setLocation(.init(name: name.rawToken, item: item, type: nil, lookahead: nil), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let item = try self.item()
        {
            return self.setLocation(.init(name: nil, item: item, type: nil, lookahead: nil), at: _mark)
        }

        self.restore(_mark)

        if
            let lookahead = try self.lookahead()
        {
            return self.setLocation(.init(name: nil, item: nil, type: nil, lookahead: lookahead), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// lookahead[SwiftPEGGrammar.LookaheadOrCut]:
    ///     | '&' '&' ~ atom { self.setLocation(SwiftPEGGrammar.Forced(atom: atom), at: _mark) }
    ///     | '&' ~ atom { self.setLocation(SwiftPEGGrammar.PositiveLookahead(atom: atom), at: _mark) }
    ///     | '!' ~ atom { self.setLocation(SwiftPEGGrammar.NegativeLookahead(atom: atom), at: _mark) }
    ///     | '~' { self.setLocation(SwiftPEGGrammar.Cut(), at: _mark) }
    ///     ;
    /// ```
    @memoized("lookahead")
    @inlinable
    public func __lookahead() throws -> SwiftPEGGrammar.LookaheadOrCut? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let _ = try self.expect(kind: .ampersand),
            let _ = try self.expect(kind: .ampersand),
            _cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.Forced(atom: atom), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .ampersand),
            _cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.PositiveLookahead(atom: atom), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .exclamationMark),
            _cut.toggleOn(),
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.NegativeLookahead(atom: atom), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .tilde)
        {
            return self.setLocation(SwiftPEGGrammar.Cut(), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// item[SwiftPEGGrammar.Item]:
    ///     | '[' ~ alts ']' { self.setLocation(SwiftPEGGrammar.OptionalItems(alts: alts), at: _mark) }
    ///     | atom '?' { self.setLocation(SwiftPEGGrammar.OptionalItem(atom: atom), at: _mark) }
    ///     | atom '*' repetitionMode? { self.setLocation(SwiftPEGGrammar.ZeroOrMoreItem(atom: atom, repetitionMode: repetitionMode ?? .standard), at: _mark) }
    ///     | atom '+' repetitionMode? { self.setLocation(SwiftPEGGrammar.OneOrMoreItem(atom: atom, repetitionMode: repetitionMode ?? .standard), at: _mark) }
    ///     | sep=atom '.' node=atom '+' repetitionMode? { self.setLocation(SwiftPEGGrammar.GatherItem(sep: sep, item: node, repetitionMode: repetitionMode ?? .standard), at: _mark) }
    ///     | atom { self.setLocation(SwiftPEGGrammar.AtomItem(atom: atom), at: _mark) }
    ///     ;
    /// ```
    @memoized("item")
    @inlinable
    public func __item() throws -> SwiftPEGGrammar.Item? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftSquare),
            _cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return self.setLocation(SwiftPEGGrammar.OptionalItems(alts: alts), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .questionMark)
        {
            return self.setLocation(SwiftPEGGrammar.OptionalItem(atom: atom), at: _mark)
        }

        self.restore(_mark)

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .star),
            case let repetitionMode = try self.repetitionMode()
        {
            return self.setLocation(SwiftPEGGrammar.ZeroOrMoreItem(atom: atom, repetitionMode: repetitionMode ?? .standard), at: _mark)
        }

        self.restore(_mark)

        if
            let atom = try self.atom(),
            let _ = try self.expect(kind: .plus),
            case let repetitionMode = try self.repetitionMode()
        {
            return self.setLocation(SwiftPEGGrammar.OneOrMoreItem(atom: atom, repetitionMode: repetitionMode ?? .standard), at: _mark)
        }

        self.restore(_mark)

        if
            let sep = try self.atom(),
            let _ = try self.expect(kind: .period),
            let node = try self.atom(),
            let _ = try self.expect(kind: .plus),
            case let repetitionMode = try self.repetitionMode()
        {
            return self.setLocation(SwiftPEGGrammar.GatherItem(sep: sep, item: node, repetitionMode: repetitionMode ?? .standard), at: _mark)
        }

        self.restore(_mark)

        if
            let atom = try self.atom()
        {
            return self.setLocation(SwiftPEGGrammar.AtomItem(atom: atom), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// repetitionMode[CommonAbstract.RepetitionMode]:
    ///     | '<' { .minimal }
    ///     | '>' { .maximal }
    ///     ;
    /// ```
    @memoized("repetitionMode")
    @inlinable
    public func __repetitionMode() throws -> CommonAbstract.RepetitionMode? {
        let _mark = self.mark()

        if
            let _ = try self.expect(kind: .leftAngle)
        {
            return .minimal
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .rightAngle)
        {
            return .maximal
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// atom[SwiftPEGGrammar.Atom]:
    ///     | '(' ~ alts ')' { self.setLocation(SwiftPEGGrammar.GroupAtom(alts: alts), at: _mark) }
    ///     | IDENTIFIER { self.setLocation(SwiftPEGGrammar.IdentAtom(identifier: identifier.rawToken, identity: .unresolved), at: _mark) }
    ///     | string { self.setLocation(SwiftPEGGrammar.StringAtom(string: string), at: _mark) }
    ///     ;
    /// ```
    @memoized("atom")
    @inlinable
    public func __atom() throws -> SwiftPEGGrammar.Atom? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftParen),
            _cut.toggleOn(),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .rightParen)
        {
            return self.setLocation(SwiftPEGGrammar.GroupAtom(alts: alts), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let identifier = try self.expect(kind: .identifier)
        {
            return self.setLocation(SwiftPEGGrammar.IdentAtom(identifier: identifier.rawToken, identity: .unresolved), at: _mark)
        }

        self.restore(_mark)

        if
            let string = try self.string()
        {
            return self.setLocation(SwiftPEGGrammar.StringAtom(string: string), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// swiftType[CommonAbstract.SwiftType]:
    ///     | '(' swiftTupleTypeList? ')' { .tuple(swiftTupleTypeList ?? []) }
    ///     | '[' key=swiftType ':' ~ value=swiftType ']' { .dictionary(key: key, value: value) }
    ///     | '[' ~ swiftType ']' { .array(swiftType) }
    ///     | swiftType '?' { .optional(swiftType) }
    ///     | swiftType '.' IDENTIFIER '<' swiftTypeList '>' { .nested(swiftType, .init(identifier: "\(identifier)", genericArguments: swiftTypeList)) }
    ///     | swiftType '.' IDENTIFIER { .nested(swiftType, .init(identifier: "\(identifier)")) }
    ///     | IDENTIFIER '<' swiftTypeList '>' { .nominal(.init(identifier: "\(identifier)", genericArguments: swiftTypeList)) }
    ///     | IDENTIFIER { .nominal(.init(identifier: "\(identifier)")) }
    ///     ;
    /// ```
    @memoizedLeftRecursive("swiftType")
    @inlinable
    public func __swiftType() throws -> CommonAbstract.SwiftType? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftParen),
            case let swiftTupleTypeList = try self.swiftTupleTypeList(),
            let _ = try self.expect(kind: .rightParen)
        {
            return .tuple(swiftTupleTypeList ?? [])
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .leftSquare),
            let key = try self.swiftType(),
            let _ = try self.expect(kind: .colon),
            _cut.toggleOn(),
            let value = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return .dictionary(key: key, value: value)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let _ = try self.expect(kind: .leftSquare),
            _cut.toggleOn(),
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .rightSquare)
        {
            return .array(swiftType)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .questionMark)
        {
            return .optional(swiftType)
        }

        self.restore(_mark)

        if
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .period),
            let identifier = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftAngle),
            let swiftTypeList = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle)
        {
            return .nested(swiftType, .init(identifier: "\(identifier)", genericArguments: swiftTypeList))
        }

        self.restore(_mark)

        if
            let swiftType = try self.swiftType(),
            let _ = try self.expect(kind: .period),
            let identifier = try self.expect(kind: .identifier)
        {
            return .nested(swiftType, .init(identifier: "\(identifier)"))
        }

        self.restore(_mark)

        if
            let identifier = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftAngle),
            let swiftTypeList = try self.swiftTypeList(),
            let _ = try self.expect(kind: .rightAngle)
        {
            return .nominal(.init(identifier: "\(identifier)", genericArguments: swiftTypeList))
        }

        self.restore(_mark)

        if
            let identifier = try self.expect(kind: .identifier)
        {
            return .nominal(.init(identifier: "\(identifier)"))
        }

        self.restore(_mark)
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
        let _mark = self.mark()

        if
            let swiftType = try self.gather(separator: {
                try self.expect(kind: .comma)
            }, item: {
                try self.swiftType()
            })
        {
            return swiftType
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// swiftTupleTypeList[[CommonAbstract.TupleTypeElement]]:
    ///     | ','.swiftTupleTypeElement+
    ///     ;
    /// ```
    @memoized("swiftTupleTypeList")
    @inlinable
    public func __swiftTupleTypeList() throws -> [CommonAbstract.TupleTypeElement]? {
        let _mark = self.mark()

        if
            let swiftTupleTypeElement = try self.gather(separator: {
                try self.expect(kind: .comma)
            }, item: {
                try self.swiftTupleTypeElement()
            })
        {
            return swiftTupleTypeElement
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// swiftTupleTypeElement[CommonAbstract.TupleTypeElement]:
    ///     | label=IDENTIFIER ':' swiftType { .labeled(label: "\(label)", swiftType) }
    ///     | swiftType { .unlabeled(swiftType) }
    ///     ;
    /// ```
    @memoized("swiftTupleTypeElement")
    @inlinable
    public func __swiftTupleTypeElement() throws -> CommonAbstract.TupleTypeElement? {
        let _mark = self.mark()

        if
            let label = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            let swiftType = try self.swiftType()
        {
            return .labeled(label: "\(label)", swiftType)
        }

        self.restore(_mark)

        if
            let swiftType = try self.swiftType()
        {
            return .unlabeled(swiftType)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// action[SwiftPEGGrammar.Action]:
    ///     | "{" ~ balancedTokens "}" { self.setLocation(.init(balancedTokens: balancedTokens), at: _mark) }
    ///     ;
    /// ```
    @memoized("action")
    @inlinable
    public func __action() throws -> SwiftPEGGrammar.Action? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let _ = try self.expect(kind: .leftBrace),
            _cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(balancedTokens: balancedTokens), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// failAction[SwiftPEGGrammar.Action]:
    ///     | "!!" "{" ~ balancedTokens "}" { self.setLocation(.init(balancedTokens: balancedTokens), at: _mark) }
    ///     ;
    /// ```
    @memoized("failAction")
    @inlinable
    public func __failAction() throws -> SwiftPEGGrammar.Action? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let _ = try self.expect(kind: .doubleExclamationMark),
            let _ = try self.expect(kind: .leftBrace),
            _cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(balancedTokens: balancedTokens), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }
        return nil
    }

    /// ```
    /// balancedTokens[SwiftPEGGrammar.TokenSequence]:
    ///     | balancedToken+ { .from(balancedToken) }
    ///     ;
    /// ```
    @memoized("balancedTokens")
    @inlinable
    public func __balancedTokens() throws -> SwiftPEGGrammar.TokenSequence? {
        let _mark = self.mark()

        if
            let balancedToken = try self.repeatOneOrMore({
                try self.balancedToken()
            })
        {
            return .from(balancedToken)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// balancedToken[SwiftPEGGrammar.TokenSequence]:
    ///     | token=WHITESPACE { .from(token) }
    ///     | l='{' ~ balancedToken* r='}' { .from(l).appending(contentsOf: balancedToken).appending(.from(r)) }
    ///     | l='[' ~ balancedToken* r=']' { .from(l).appending(contentsOf: balancedToken).appending(.from(r)) }
    ///     | l='<' ~ balancedToken* r='>' { .from(l).appending(contentsOf: balancedToken).appending(.from(r)) }
    ///     | l='(' ~ balancedToken* r=')' { .from(l).appending(contentsOf: balancedToken).appending(.from(r)) }
    ///     | token=balancedTokenAtom { token }
    ///     ;
    /// ```
    @memoized("balancedToken")
    @inlinable
    public func __balancedToken() throws -> SwiftPEGGrammar.TokenSequence? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let token = try self.expect(kind: .whitespace)
        {
            return .from(token)
        }

        self.restore(_mark)

        if
            let l = try self.expect(kind: .leftBrace),
            _cut.toggleOn(),
            let balancedToken = try self.repeatZeroOrMore({
                try self.balancedToken()
            }),
            let r = try self.expect(kind: .rightBrace)
        {
            return .from(l).appending(contentsOf: balancedToken).appending(.from(r))
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftSquare),
            _cut.toggleOn(),
            let balancedToken = try self.repeatZeroOrMore({
                try self.balancedToken()
            }),
            let r = try self.expect(kind: .rightSquare)
        {
            return .from(l).appending(contentsOf: balancedToken).appending(.from(r))
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftAngle),
            _cut.toggleOn(),
            let balancedToken = try self.repeatZeroOrMore({
                try self.balancedToken()
            }),
            let r = try self.expect(kind: .rightAngle)
        {
            return .from(l).appending(contentsOf: balancedToken).appending(.from(r))
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let l = try self.expect(kind: .leftParen),
            _cut.toggleOn(),
            let balancedToken = try self.repeatZeroOrMore({
                try self.balancedToken()
            }),
            let r = try self.expect(kind: .rightParen)
        {
            return .from(l).appending(contentsOf: balancedToken).appending(.from(r))
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let token = try self.balancedTokenAtom()
        {
            return token
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// balancedTokenAtom[SwiftPEGGrammar.TokenSequence]:
    ///     | string { .from(string) }
    ///     | !"[" !"]" !"{" !"}" !"(" !")" token=ANY { .from(token) }
    ///     ;
    /// ```
    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> SwiftPEGGrammar.TokenSequence? {
        let _mark = self.mark()

        if
            let string = try self.string()
        {
            return .from(string)
        }

        self.restore(_mark)

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
            return .from(token)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// string[SwiftPEGGrammar.GrammarString]:
    ///     | token=STRING { try .fromStringToken(token) }
    ///     ;
    /// ```
    @memoized("string")
    @inlinable
    public func __string() throws -> SwiftPEGGrammar.GrammarString? {
        let _mark = self.mark()

        if
            let token = try self.expect(kind: .string)
        {
            return try .fromStringToken(token)
        }

        self.restore(_mark)
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
        let _mark = self.mark()

        if
            let tokens = try self.repeatZeroOrMore({
                try self.tokenDefinition()
            })
        {
            return tokens
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenDefinition[SwiftPEGGrammar.TokenDefinition]:
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER '[' staticToken=string ']' ':' ~ tokenSyntax ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: staticToken, tokenSyntax: tokenSyntax), at: _mark) }
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER '[' staticToken=string ']' ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: staticToken, tokenSyntax: nil), at: _mark) }
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER ':' ~ tokenSyntax ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: nil, tokenSyntax: tokenSyntax), at: _mark) }
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: nil, tokenSyntax: nil), at: _mark) }
    ///     ;
    /// ```
    @memoized("tokenDefinition")
    @inlinable
    public func __tokenDefinition() throws -> SwiftPEGGrammar.TokenDefinition? {
        let _mark = self.mark()
        var _cut = CutFlag()

        if
            let spec = try self.tokenOrFragmentSpecifier(),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let staticToken = try self.string(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .colon),
            _cut.toggleOn(),
            let tokenSyntax = try self.tokenSyntax(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: staticToken, tokenSyntax: tokenSyntax), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let spec = try self.tokenOrFragmentSpecifier(),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let staticToken = try self.string(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: staticToken, tokenSyntax: nil), at: _mark)
        }

        self.restore(_mark)

        if
            let spec = try self.tokenOrFragmentSpecifier(),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            _cut.toggleOn(),
            let tokenSyntax = try self.tokenSyntax(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: nil, tokenSyntax: tokenSyntax), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let spec = try self.tokenOrFragmentSpecifier(),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, staticToken: nil, tokenSyntax: nil), at: _mark)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenOrFragmentSpecifier[RawToken]:
    ///     | token='$' { token.rawToken }
    ///     | token='%' { token.rawToken }
    ///     ;
    /// ```
    @memoized("tokenOrFragmentSpecifier")
    @inlinable
    public func __tokenOrFragmentSpecifier() throws -> RawToken? {
        let _mark = self.mark()

        if
            let token = try self.expect(kind: .dollarSign)
        {
            return token.rawToken
        }

        self.restore(_mark)

        if
            let token = try self.expect(kind: .percent)
        {
            return token.rawToken
        }

        self.restore(_mark)
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
        let _mark = self.mark()

        if
            case _ = try self.expect(kind: .bar),
            let tokenSyntaxAlt = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxAlt()
            })
        {
            return .init(alts: tokenSyntaxAlt)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenSyntaxAlt[CommonAbstract.TokenAlt]:
    ///     | tokenSyntaxItem+ tokenSyntaxExclusion* { .init(items: tokenSyntaxItem, trailExclusions: tokenSyntaxExclusion) }
    ///     ;
    /// ```
    @memoized("tokenSyntaxAlt")
    @inlinable
    public func __tokenSyntaxAlt() throws -> CommonAbstract.TokenAlt? {
        let _mark = self.mark()

        if
            let tokenSyntaxItem = try self.repeatOneOrMore({
                try self.tokenSyntaxItem()
            }),
            let tokenSyntaxExclusion = try self.repeatZeroOrMore({
                try self.tokenSyntaxExclusion()
            })
        {
            return .init(items: tokenSyntaxItem, trailExclusions: tokenSyntaxExclusion)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenSyntaxItem[CommonAbstract.TokenItem]:
    ///     | '(' '|'.tokenSyntaxAtom+ ')' '*' { .zeroOrMore(tokenSyntaxAtom) }
    ///     | '(' '|'.tokenSyntaxAtom+ ')' '+' { .oneOrMore(tokenSyntaxAtom) }
    ///     | '(' '|'.tokenSyntaxAtom+ ')' '?' { .optionalGroup(tokenSyntaxAtom) }
    ///     | '(' '|'.tokenSyntaxAtom+ ')' { .group(tokenSyntaxAtom) }
    ///     | tokenSyntaxAtom '*' { .zeroOrMore([tokenSyntaxAtom]) }
    ///     | tokenSyntaxAtom '+' { .oneOrMore([tokenSyntaxAtom]) }
    ///     | tokenSyntaxAtom '?' { .optionalAtom(tokenSyntaxAtom) }
    ///     | tokenSyntaxAtom { .atom(tokenSyntaxAtom) }
    ///     ;
    /// ```
    @memoized("tokenSyntaxItem")
    @inlinable
    public func __tokenSyntaxItem() throws -> CommonAbstract.TokenItem? {
        let _mark = self.mark()

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxAtom = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxAtom()
            }),
            let _ = try self.expect(kind: .rightParen),
            let _ = try self.expect(kind: .star)
        {
            return .zeroOrMore(tokenSyntaxAtom)
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxAtom = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxAtom()
            }),
            let _ = try self.expect(kind: .rightParen),
            let _ = try self.expect(kind: .plus)
        {
            return .oneOrMore(tokenSyntaxAtom)
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxAtom = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxAtom()
            }),
            let _ = try self.expect(kind: .rightParen),
            let _ = try self.expect(kind: .questionMark)
        {
            return .optionalGroup(tokenSyntaxAtom)
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .leftParen),
            let tokenSyntaxAtom = try self.gather(separator: {
                try self.expect(kind: .bar)
            }, item: {
                try self.tokenSyntaxAtom()
            }),
            let _ = try self.expect(kind: .rightParen)
        {
            return .group(tokenSyntaxAtom)
        }

        self.restore(_mark)

        if
            let tokenSyntaxAtom = try self.tokenSyntaxAtom(),
            let _ = try self.expect(kind: .star)
        {
            return .zeroOrMore([tokenSyntaxAtom])
        }

        self.restore(_mark)

        if
            let tokenSyntaxAtom = try self.tokenSyntaxAtom(),
            let _ = try self.expect(kind: .plus)
        {
            return .oneOrMore([tokenSyntaxAtom])
        }

        self.restore(_mark)

        if
            let tokenSyntaxAtom = try self.tokenSyntaxAtom(),
            let _ = try self.expect(kind: .questionMark)
        {
            return .optionalAtom(tokenSyntaxAtom)
        }

        self.restore(_mark)

        if
            let tokenSyntaxAtom = try self.tokenSyntaxAtom()
        {
            return .atom(tokenSyntaxAtom)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenSyntaxAtom[CommonAbstract.TokenAtom]:
    ///     | tokenSyntaxExclusion* tokenSyntaxTerminal { .init(excluded: tokenSyntaxExclusion, terminal: tokenSyntaxTerminal) }
    ///     ;
    /// ```
    @memoized("tokenSyntaxAtom")
    @inlinable
    public func __tokenSyntaxAtom() throws -> CommonAbstract.TokenAtom? {
        let _mark = self.mark()

        if
            let tokenSyntaxExclusion = try self.repeatZeroOrMore({
                try self.tokenSyntaxExclusion()
            }),
            let tokenSyntaxTerminal = try self.tokenSyntaxTerminal()
        {
            return .init(excluded: tokenSyntaxExclusion, terminal: tokenSyntaxTerminal)
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenSyntaxExclusion[CommonAbstract.TokenExclusion]:
    ///     | '!' start=string '...' end=string { .rangeLiteral(.from(start), .from(end)) }
    ///     | '!' string { .string(.from(string)) }
    ///     | '!' IDENTIFIER { .identifier("\(identifier)") }
    ///     ;
    /// ```
    @memoized("tokenSyntaxExclusion")
    @inlinable
    public func __tokenSyntaxExclusion() throws -> CommonAbstract.TokenExclusion? {
        let _mark = self.mark()

        if
            let _ = try self.expect(kind: .exclamationMark),
            let start = try self.string(),
            let _ = try self.expect(kind: .ellipsis),
            let end = try self.string()
        {
            return .rangeLiteral(.from(start), .from(end))
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .exclamationMark),
            let string = try self.string()
        {
            return .string(.from(string))
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .exclamationMark),
            let identifier = try self.expect(kind: .identifier)
        {
            return .identifier("\(identifier)")
        }

        self.restore(_mark)
        return nil
    }

    /// ```
    /// tokenSyntaxTerminal[CommonAbstract.TokenTerminal]:
    ///     | IDENTIFIER action { .characterPredicate("\(identifier)", action.rawAction) }
    ///     | start=string '...' end=string { .rangeLiteral(.from(start), .from(end)) }
    ///     | string { .literal(.from(string)) }
    ///     | IDENTIFIER { .identifier("\(identifier)") }
    ///     | '.' { .any }
    ///     ;
    /// ```
    @memoized("tokenSyntaxTerminal")
    @inlinable
    public func __tokenSyntaxTerminal() throws -> CommonAbstract.TokenTerminal? {
        let _mark = self.mark()

        if
            let identifier = try self.expect(kind: .identifier),
            let action = try self.action()
        {
            return .characterPredicate("\(identifier)", action.rawAction)
        }

        self.restore(_mark)

        if
            let start = try self.string(),
            let _ = try self.expect(kind: .ellipsis),
            let end = try self.string()
        {
            return .rangeLiteral(.from(start), .from(end))
        }

        self.restore(_mark)

        if
            let string = try self.string()
        {
            return .literal(.from(string))
        }

        self.restore(_mark)

        if
            let identifier = try self.expect(kind: .identifier)
        {
            return .identifier("\(identifier)")
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .period)
        {
            return .any
        }

        self.restore(_mark)
        return nil
    }
}
