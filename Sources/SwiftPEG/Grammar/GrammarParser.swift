// HEADS UP! This is a generated file

/// A parser for SwiftPEG grammar files.

public class GrammarParser<RawTokenizer: RawTokenizerType>: PEGParser<RawTokenizer> where RawTokenizer.RawToken == GrammarParserToken, RawTokenizer.Location == FileSourceLocation {
    public override func skipChannelSkipTokens(_ except: Set<RawToken.TokenKind>) throws -> Void {
        repeat {
            let next: Token? = try tokenizer.peekToken()

            guard
                let kind = next?.rawToken.kind,
                kind == .comment || kind == .whitespace
            else {
                break
            }

            if except.contains(kind) {
                break
            }

            try tokenizer.skip()
        } while !tokenizer.isEOF
    }

    /// ```
    /// start[SwiftPEGGrammar.Grammar]:
    ///     | grammar { grammar }
    ///     ;
    /// ```
    @memoized("start")
    @inlinable
    public func __start() throws -> SwiftPEGGrammar.Grammar? {
        let _mark: Mark = self.mark()

        if let grammar = try self.grammar() {
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
        let _mark: Mark = self.mark()

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
    ///     | "@" name=IDENTIFIER values=metaValue* ';' { self.setLocation(.init(name: name.rawToken, values: values), at: _mark) }
    ///     ;
    /// ```
    @memoized("meta")
    @inlinable
    public func __meta() throws -> SwiftPEGGrammar.Meta? {
        let _mark: Mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let name = try self.expect(kind: .identifier),
            let values = try self.repeatZeroOrMore({
                try self.metaValue()
            }),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, values: values), at: _mark)
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
        let _mark: Mark = self.mark()

        if let ident = try self.expect(kind: .identifier) {
            return self.setLocation(SwiftPEGGrammar.MetaIdentifierValue(identifier: ident.rawToken), at: _mark)
        }

        self.restore(_mark)

        if let string = try self.string() {
            return self.setLocation(SwiftPEGGrammar.MetaStringValue(string: string), at: _mark)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// rule[SwiftPEGGrammar.Rule]:
    ///     | ruleName ruleParameters? ":" action? failAction? '|'? alts ';' { self.setLocation(.init(name: ruleName, parameters: ruleParameters, action: action, failAction: failAction, alts: alts), at: _mark) }
    ///     ;
    /// ```
    @memoized("rule")
    @inlinable
    public func __rule() throws -> SwiftPEGGrammar.Rule? {
        let _mark: Mark = self.mark()

        if
            let ruleName = try self.ruleName(),
            case let ruleParameters = try self.ruleParameters(),
            let _ = try self.expect(kind: .colon),
            case let action = try self.action(),
            case let failAction = try self.failAction(),
            case _ = try self.expect(kind: .bar),
            let alts = try self.alts(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: ruleName, parameters: ruleParameters, action: action, failAction: failAction, alts: alts), at: _mark)
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
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

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

        if let name = try self.expect(kind: .identifier) {
            return self.setLocation(.init(name: name.rawToken, type: nil), at: _mark)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// ruleParameters[SwiftPEGGrammar.RuleParameters]:
    ///     | '(' ','.ruleParameter+ ')' { self.setLocation(.init(parameters: ruleParameter), at: _mark) }
    ///     ;
    /// ```
    @memoized("ruleParameters")
    @inlinable
    public func __ruleParameters() throws -> SwiftPEGGrammar.RuleParameters? {
        let _mark: Mark = self.mark()

        if
            let _ = try self.expect(kind: .leftParen),
            let ruleParameter = try self.gather(separator: {
                try self.expect(kind: .comma)
            }, item: {
                try self.ruleParameter()
            }),
            let _ = try self.expect(kind: .rightParen)
        {
            return self.setLocation(.init(parameters: ruleParameter), at: _mark)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// ruleParameter[SwiftPEGGrammar.RuleParameter]:
    ///     | IDENTIFIER ':' swiftType { self.setLocation(.init(name: identifier.rawToken, type: swiftType), at: _mark) }
    ///     ;
    /// ```
    @memoized("ruleParameter")
    @inlinable
    public func __ruleParameter() throws -> SwiftPEGGrammar.RuleParameter? {
        let _mark: Mark = self.mark()

        if
            let identifier = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            let swiftType = try self.swiftType()
        {
            return self.setLocation(.init(name: identifier.rawToken, type: swiftType), at: _mark)
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
        let _mark: Mark = self.mark()

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
    ///     | altLabel? namedItems action? failAction? { self.setLocation(.init(altLabel: altLabel, namedItems: namedItems, action: action, failAction: failAction), at: _mark) }
    ///     ;
    /// ```
    @memoized("alt")
    @inlinable
    public func __alt() throws -> SwiftPEGGrammar.Alt? {
        let _mark: Mark = self.mark()

        if
            case let altLabel = try self.altLabel(),
            let namedItems = try self.namedItems(),
            case let action = try self.action(),
            case let failAction = try self.failAction()
        {
            return self.setLocation(.init(altLabel: altLabel, namedItems: namedItems, action: action, failAction: failAction), at: _mark)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// altLabel[SwiftPEGGrammar.AltLabel]:
    ///     | label=IDENTIFIER ':' { self.setLocation(.init(name: label.rawToken), at: _mark) }
    ///     ;
    /// ```
    @memoized("altLabel")
    @inlinable
    public func __altLabel() throws -> SwiftPEGGrammar.AltLabel? {
        let _mark: Mark = self.mark()

        if
            let label = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon)
        {
            return self.setLocation(.init(name: label.rawToken), at: _mark)
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
        let _mark: Mark = self.mark()

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
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

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

        if let item = try self.item() {
            return self.setLocation(.init(name: nil, item: item, type: nil, lookahead: nil), at: _mark)
        }

        self.restore(_mark)

        if let lookahead = try self.lookahead() {
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
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

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

        if let _ = try self.expect(kind: .tilde) {
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
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

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

        if let atom = try self.atom() {
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
        let _mark: Mark = self.mark()

        if let _ = try self.expect(kind: .leftAngle) {
            return .minimal
        }

        self.restore(_mark)

        if let _ = try self.expect(kind: .rightAngle) {
            return .maximal
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// atom[SwiftPEGGrammar.Atom]:
    ///     | '(' ~ alts ')' { self.setLocation(SwiftPEGGrammar.GroupAtom(alts: alts), at: _mark) }
    ///     | IDENTIFIER atomParameters? { self.setLocation(SwiftPEGGrammar.IdentAtom(identifier: identifier.rawToken, parameters: atomParameters, identity: .unresolved), at: _mark) }
    ///     | string { self.setLocation(SwiftPEGGrammar.StringAtom(string: string), at: _mark) }
    ///     ;
    /// ```
    @memoized("atom")
    @inlinable
    public func __atom() throws -> SwiftPEGGrammar.Atom? {
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

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
            let identifier = try self.expect(kind: .identifier),
            case let atomParameters = try self.atomParameters()
        {
            return self.setLocation(SwiftPEGGrammar.IdentAtom(identifier: identifier.rawToken, parameters: atomParameters, identity: .unresolved), at: _mark)
        }

        self.restore(_mark)

        if let string = try self.string() {
            return self.setLocation(SwiftPEGGrammar.StringAtom(string: string), at: _mark)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// atomParameters[SwiftPEGGrammar.AtomParameters]:
    ///     | '(' ','.atomParameter+ ')' { self.setLocation(.init(parameters: atomParameter), at: _mark) }
    ///     ;
    /// ```
    @memoized("atomParameters")
    @inlinable
    public func __atomParameters() throws -> SwiftPEGGrammar.AtomParameters? {
        let _mark: Mark = self.mark()

        if
            let _ = try self.expect(kind: .leftParen),
            let atomParameter = try self.gather(separator: {
                try self.expect(kind: .comma)
            }, item: {
                try self.atomParameter()
            }),
            let _ = try self.expect(kind: .rightParen)
        {
            return self.setLocation(.init(parameters: atomParameter), at: _mark)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// atomParameter[SwiftPEGGrammar.AtomParameter]:
    ///     | IDENTIFIER ':' action { self.setLocation(.init(label: identifier.rawToken, action: action), at: _mark) }
    ///     ;
    /// ```
    @memoized("atomParameter")
    @inlinable
    public func __atomParameter() throws -> SwiftPEGGrammar.AtomParameter? {
        let _mark: Mark = self.mark()

        if
            let identifier = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            let action = try self.action()
        {
            return self.setLocation(.init(label: identifier.rawToken, action: action), at: _mark)
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
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

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

        if let identifier = try self.expect(kind: .identifier) {
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
        let _mark: Mark = self.mark()

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
        let _mark: Mark = self.mark()

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
        let _mark: Mark = self.mark()

        if
            let label = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .colon),
            let swiftType = try self.swiftType()
        {
            return .labeled(label: "\(label)", swiftType)
        }

        self.restore(_mark)

        if let swiftType = try self.swiftType() {
            return .unlabeled(swiftType)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// action[SwiftPEGGrammar.Action]:
    ///     | actionAttribute* "{" ~ balancedTokens "}" { self.setLocation(.init(attributes: actionAttribute, balancedTokens: balancedTokens), at: _mark) }
    ///     ;
    /// ```
    @memoized("action")
    @inlinable
    public func __action() throws -> SwiftPEGGrammar.Action? {
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

        if
            let actionAttribute = try self.repeatZeroOrMore({
                try self.actionAttribute()
            }),
            let _ = try self.expect(kind: .leftBrace),
            _cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(attributes: actionAttribute, balancedTokens: balancedTokens), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        return nil
    }

    /// ```
    /// failAction[SwiftPEGGrammar.Action]:
    ///     | actionAttribute* "!!" "{" ~ balancedTokens "}" { self.setLocation(.init(attributes: actionAttribute, balancedTokens: balancedTokens), at: _mark) }
    ///     ;
    /// ```
    @memoized("failAction")
    @inlinable
    public func __failAction() throws -> SwiftPEGGrammar.Action? {
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

        if
            let actionAttribute = try self.repeatZeroOrMore({
                try self.actionAttribute()
            }),
            let _ = try self.expect(kind: .doubleExclamationMark),
            let _ = try self.expect(kind: .leftBrace),
            _cut.toggleOn(),
            let balancedTokens = try self.balancedTokens(),
            let _ = try self.expect(kind: .rightBrace)
        {
            return self.setLocation(.init(attributes: actionAttribute, balancedTokens: balancedTokens), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        return nil
    }

    /// ```
    /// actionAttribute[SwiftPEGGrammar.ActionAttribute]:
    ///     | '@' IDENTIFIER { self.setLocation(.init(name: identifier.rawToken), at: _mark) }
    ///     ;
    /// ```
    @memoized("actionAttribute")
    @inlinable
    public func __actionAttribute() throws -> SwiftPEGGrammar.ActionAttribute? {
        let _mark: Mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let identifier = try self.expect(kind: .identifier)
        {
            return self.setLocation(.init(name: identifier.rawToken), at: _mark)
        }

        self.restore(_mark)

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
        let _mark: Mark = self.mark()

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
    ///     | token=balancedTokenAtom { token }
    ///     ;
    /// ```
    @memoized("balancedToken")
    @inlinable
    public func __balancedToken() throws -> SwiftPEGGrammar.TokenSequence? {
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

        if let token = try self.expect(kind: .whitespace) {
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

        if let token = try self.balancedTokenAtom() {
            return token
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// balancedTokenAtom[SwiftPEGGrammar.TokenSequence]:
    ///     | string { .from(string) }
    ///     | !"{" !"}" token=ANY { .from(token) }
    ///     ;
    /// ```
    @memoized("balancedTokenAtom")
    @inlinable
    public func __balancedTokenAtom() throws -> SwiftPEGGrammar.TokenSequence? {
        let _mark: Mark = self.mark()

        if let string = try self.string() {
            return .from(string)
        }

        self.restore(_mark)

        if
            try self.negativeLookahead({
                try self.expect(kind: .leftBrace)
            }),
            try self.negativeLookahead({
                try self.expect(kind: .rightBrace)
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
        let _mark: Mark = self.mark()

        if let token = try self.expect(kind: .string) {
            return try .fromStringToken(token)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// tokensFile[[SwiftPEGGrammar.TokenFileDeclaration]]:
    ///     | tokensFileDeclaration*
    ///     ;
    /// ```
    @memoized("tokensFile")
    @inlinable
    public func __tokensFile() throws -> [SwiftPEGGrammar.TokenFileDeclaration]? {
        let _mark: Mark = self.mark()

        if
            let tokensFileDeclaration = try self.repeatZeroOrMore({
                try self.tokensFileDeclaration()
            })
        {
            return tokensFileDeclaration
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// tokensFileDeclaration[SwiftPEGGrammar.TokenFileDeclaration]:
    ///     | tokenDefinition
    ///     | tokenChannelDeclaration
    ///     ;
    /// ```
    @memoized("tokensFileDeclaration")
    @inlinable
    public func __tokensFileDeclaration() throws -> SwiftPEGGrammar.TokenFileDeclaration? {
        let _mark: Mark = self.mark()

        if let tokenDefinition = try self.tokenDefinition() {
            return tokenDefinition
        }

        self.restore(_mark)

        if let tokenChannelDeclaration = try self.tokenChannelDeclaration() {
            return tokenChannelDeclaration
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// tokenChannelDeclaration[SwiftPEGGrammar.TokenChannelDeclaration]:
    ///     | '@' IDENTIFIER name=IDENTIFIER '~>' tokenChannelTarget ';' { "\(identifier)" == "channel" ? self.setLocation(.init(name: name.rawToken, target: tokenChannelTarget), at: _mark) : nil }
    ///     | '@' IDENTIFIER name=IDENTIFIER? ';' { "\(identifier)" == "channel" ? self.setLocation(.init(name: name?.rawToken, target: nil), at: _mark) : nil }
    ///     ;
    /// ```
    @memoized("tokenChannelDeclaration")
    @inlinable
    public func __tokenChannelDeclaration() throws -> SwiftPEGGrammar.TokenChannelDeclaration? {
        let _mark: Mark = self.mark()

        if
            let _ = try self.expect(kind: .at),
            let identifier = try self.expect(kind: .identifier),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .tildeArrow),
            let tokenChannelTarget = try self.tokenChannelTarget(),
            let _ = try self.expect(kind: .semicolon)
        {
            return "\(identifier)" == "channel" ? self.setLocation(.init(name: name.rawToken, target: tokenChannelTarget), at: _mark) : nil
        }

        self.restore(_mark)

        if
            let _ = try self.expect(kind: .at),
            let identifier = try self.expect(kind: .identifier),
            case let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .semicolon)
        {
            return "\(identifier)" == "channel" ? self.setLocation(.init(name: name?.rawToken, target: nil), at: _mark) : nil
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// tokenChannelTarget[SwiftPEGGrammar.TokenChannelTarget]:
    ///     | IDENTIFIER { .init(identifier: identifier.rawToken) }
    ///     ;
    /// ```
    @memoized("tokenChannelTarget")
    @inlinable
    public func __tokenChannelTarget() throws -> SwiftPEGGrammar.TokenChannelTarget? {
        let _mark: Mark = self.mark()

        if let identifier = try self.expect(kind: .identifier) {
            return .init(identifier: identifier.rawToken)
        }

        self.restore(_mark)

        return nil
    }

    /// ```
    /// tokenDefinition[SwiftPEGGrammar.TokenDefinition]:
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER '[' tokenCodeReference=string ']' ':' ~ tokenSyntax ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: tokenCodeReference, tokenSyntax: tokenSyntax), at: _mark) }
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER '[' tokenCodeReference=string ']' ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: tokenCodeReference, tokenSyntax: nil), at: _mark) }
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER ':' ~ tokenSyntax ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: nil, tokenSyntax: tokenSyntax), at: _mark) }
    ///     | spec=tokenOrFragmentSpecifier name=IDENTIFIER ';' { self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: nil, tokenSyntax: nil), at: _mark) }
    ///     ;
    /// ```
    @memoized("tokenDefinition")
    @inlinable
    public func __tokenDefinition() throws -> SwiftPEGGrammar.TokenDefinition? {
        let _mark: Mark = self.mark()

        var _cut: CutFlag = CutFlag()

        if
            let spec = try self.tokenOrFragmentSpecifier(),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let tokenCodeReference = try self.string(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .colon),
            _cut.toggleOn(),
            let tokenSyntax = try self.tokenSyntax(),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: tokenCodeReference, tokenSyntax: tokenSyntax), at: _mark)
        }

        self.restore(_mark)

        if _cut.isOn {
            return nil
        }

        if
            let spec = try self.tokenOrFragmentSpecifier(),
            let name = try self.expect(kind: .identifier),
            let _ = try self.expect(kind: .leftSquare),
            let tokenCodeReference = try self.string(),
            let _ = try self.expect(kind: .rightSquare),
            let _ = try self.expect(kind: .semicolon)
        {
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: tokenCodeReference, tokenSyntax: nil), at: _mark)
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
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: nil, tokenSyntax: tokenSyntax), at: _mark)
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
            return self.setLocation(.init(name: name.rawToken, isFragment: spec.kind == .percent, tokenCodeReference: nil, tokenSyntax: nil), at: _mark)
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
        let _mark: Mark = self.mark()

        if let token = try self.expect(kind: .dollarSign) {
            return token.rawToken
        }

        self.restore(_mark)

        if let token = try self.expect(kind: .percent) {
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
        let _mark: Mark = self.mark()

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
        let _mark: Mark = self.mark()

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
    ///     | '(' _=tokenSyntaxAtom _=tokenSyntaxAtom+ ')' @noReturn { throw syntaxError("Token atom sequences cannot be nested. Consider splitting the token syntax into sub-tokens or fragments instead.") }
    ///     | tokenSyntaxAtom '*' { .zeroOrMore([tokenSyntaxAtom]) }
    ///     | tokenSyntaxAtom '+' { .oneOrMore([tokenSyntaxAtom]) }
    ///     | tokenSyntaxAtom '?' { .optionalAtom(tokenSyntaxAtom) }
    ///     | tokenSyntaxAtom { .atom(tokenSyntaxAtom) }
    ///     ;
    /// ```
    @memoized("tokenSyntaxItem")
    @inlinable
    public func __tokenSyntaxItem() throws -> CommonAbstract.TokenItem? {
        let _mark: Mark = self.mark()

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
            let _ = try self.expect(kind: .leftParen),
            let _ = try self.tokenSyntaxAtom(),
            let _ = try self.repeatOneOrMore({
                try self.tokenSyntaxAtom()
            }),
            let _ = try self.expect(kind: .rightParen)
        {
            throw syntaxError("Token atom sequences cannot be nested. Consider splitting the token syntax into sub-tokens or fragments instead.")
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

        if let tokenSyntaxAtom = try self.tokenSyntaxAtom() {
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
        let _mark: Mark = self.mark()

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
    ///     | '!' string { .literal(.from(string)) }
    ///     | '!' IDENTIFIER { .identifier("\(identifier)") }
    ///     ;
    /// ```
    @memoized("tokenSyntaxExclusion")
    @inlinable
    public func __tokenSyntaxExclusion() throws -> CommonAbstract.TokenExclusion? {
        let _mark: Mark = self.mark()

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
            return .literal(.from(string))
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
        let _mark: Mark = self.mark()

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

        if let string = try self.string() {
            return .literal(.from(string))
        }

        self.restore(_mark)

        if let identifier = try self.expect(kind: .identifier) {
            return .identifier("\(identifier)")
        }

        self.restore(_mark)

        if let _ = try self.expect(kind: .period) {
            return .any
        }

        self.restore(_mark)

        return nil
    }
}
