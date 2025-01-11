import SwiftAST

// MARK: Generation
extension SwiftCodeGen {

    /// Generates Swift parser code.
    public func _generateParser(
        settings: ParserGenSettings = .default
    ) throws -> [TopLevelDecl] {

        self.latestSettings = settings

        var result: [TopLevelDecl] = []
        buffer.resetState()

        // @parserHeader
        if let header = grammar.parserHeader() {
            result.append(.unknown(header))
        }
        // @implicitReturns
        if let value = grammar.implicitReturns() {
            switch value {
            case "true":
                implicitReturns = true
            case "false":
                implicitReturns = false
            default:
                // TODO: Issue diagnostic
                break
            }
        }
        // @implicitBindings
        if let value = grammar.implicitBindings() {
            switch value {
            case "true":
                implicitBindings = true
            case "false":
                implicitBindings = false
            default:
                // TODO: Issue diagnostic
                break
            }
        }
        // @bindTokenLiterals
        if let value = grammar.bindTokenLiterals() {
            switch value {
            case "true":
                bindTokenLiterals = true
            case "false":
                bindTokenLiterals = false
            default:
                // TODO: Issue diagnostic
                break
            }
        }

        bindingEngine.clearState()
        bindingEngine.implicitBindings = self.implicitBindings
        bindingEngine.bindTokenLiterals = self.bindTokenLiterals
        for rule in grammar.rules {
            bindingEngine.registerRule(rule)
        }
        for token in tokenDefinitions {
            bindingEngine.registerToken(token)
        }

        declContext = DeclarationsContext()
        declContext.push() // No need to pop as the context is meant to be replaced in new generate calls

        if settings.emitProducerProtocol {
            let protocolInfo = producerProtocolInfo()
            result.append(.protocol(try generateProducerProtocol(protocolInfo)))

            let ruleReturnAliases: [String: CommonAbstract.SwiftType] =
                protocolInfo
                .ruleReturnType.mapValues { identifier in
                    .nested(.typeName(self.producerGenericParameterName()), identifier)
                }

            let info = generateDefaultProducerImplementationInfo()
            result.append(.class(try generateDefaultProducerProtocol(protocolInfo, info)))

            if settings.emitVoidProducer {
                let info = generateVoidProducerImplementationInfo()
                result.append(.class(try generateDefaultProducerProtocol(protocolInfo, info)))
            }

            self.producerProtocol = protocolInfo
            bindingEngine = bindingEngine.withCustomRuleResultMappings(ruleReturnAliases)
        }

        self.remaining = grammar.rules.map(RemainingProduction.rule)

        let generationKind = grammar.generationKind()

        switch generationKind {
        case nil, "extension":
            result.append(
                .extension(try generateParser_extension())
            )

        case "class":
            result.append(
                .class(try generateParser_class())
            )

        case let ident?:
            throw Error.unknownGenerationKind(ident)
        }

        return result
    }

    /// Generates a parser as an extension of a type.
    func generateParser_extension() throws -> ExtensionDecl {
        let members = try generateRemainingProductions()

        return .init(
            leadingComments: [],
            baseType: .typeName(parserName),
            accessLevel: .internal,
            inheritances: [],
            genericWhereClause: nil,
            members: members
        )
    }

    /// Generates a parser as class type.
    func generateParser_class() throws -> ClassDecl {
        guard let tokenTypeName = grammar.tokenTypeName() else {
            throw Error.missingTokenTypeName
        }

        var members = try generateRemainingProductions()
        members.insert(.function(try generateSkipChannelSkipTokens()), at: 0)

        return .init(
            leadingComments: [],
            accessLevel: .public,
            name: parserName,
            genericArguments: [
                .init(name: "RawTokenizer", type: "RawTokenizerType"),
            ],
            inheritances: [.generic("PEGParser", parameters: ["RawTokenizer"])],
            genericWhereClause: [
                .sameTypeRequirement(.nested(.init(base: "RawTokenizer", nested: "RawToken")), .typeName(tokenTypeName)),
                .sameTypeRequirement(.nested(.init(base: "RawTokenizer", nested: "Location")), "FileSourceLocation"),
            ],
            members: members
        )
    }

    // MARK: - Token channel manager

    /// ```
    /// public override func skipChannelSkipTokens(
    ///     _ except: Set<RawToken.TokenKind> = []
    /// ) throws {
    ///     repeat {
    ///         let next = try tokenizer.peekToken()
    ///         guard
    ///             let kind = next?.rawToken.kind,
    ///             kind == .<skip token kind1> || kind == .<skip token kind2> || ...
    ///         else {
    ///             break
    ///         }
    ///         if except.contains(kind) {
    ///             break
    ///         }
    ///         try tokenizer.skip()
    ///     } while !tokenizer.isEOF
    /// }
    /// ```
    func generateSkipChannelSkipTokens() throws -> FunctionMemberDecl {
        let signature = FunctionSignature(
            attributes: [],
            genericParameters: nil,
            name: "skipChannelSkipTokens",
            parameters: [
                .init(
                    label: nil,
                    name: "except",
                    type: .generic("Set", parameters: [.nested(.init(base: "RawToken", nested: "TokenKind"))]),
                    defaultValue: .arrayLiteral([])
                ),
            ],
            returnType: .void,
            genericWhereClause: nil,
            traits: [.throwing]
        )

        let skipChannels = processedGrammar.tokenChannels.filter { tokenChannel in
            tokenChannel.target == .skip
        }.compactMap { tokenChannel in
            tokenChannel.name
        }
        let skipTokens: [Expression] = processedGrammar.tokens.filter { token in
            guard let tokenChannel = token.tokenChannel else {
                return false
            }
            guard !token.isFragment else {
                return false
            }

            return skipChannels.contains(tokenChannel)
        }.map({ Expression.implicitMember(caseName(for: $0)) })

        let body: CompoundStatement
        if !skipTokens.isEmpty {
            let conditionals: [Expression] = skipTokens.map { exp in
                .identifier("kind").binary(op: .equals, rhs: exp)
            }

            body = [
                .repeatWhile(.unary(op: .negate, .identifier("tokenizer").dot("isEOF")), body: [
                    .variableDeclaration(
                        identifier: "next",
                        type: .optional("Token"),
                        isConstant: true,
                        initialization: .try(.identifier("tokenizer").dot("peekToken").call())
                    ),
                    .guard(
                        clauses: [
                            .init(
                                pattern: .valueBindingPattern(constant: true, .identifier("kind")),
                                expression: .identifier("next").optional().dot("rawToken").dot("kind")
                            ),
                            .init(expression: conditionals.dropFirst().reduce(conditionals[0]) { $0.binary(op: .or, rhs: $1) }),
                        ],
                        else: [.break()]
                    ),
                    .if(.identifier("except").dot("contains").call([.identifier("kind")]), body: [
                        .break(),
                    ]),
                    .expression(.try(.identifier("tokenizer").dot("skip").call()))
                ]),
            ]
        } else {
            body = []
        }

        return .init(
            leadingComments: [],
            accessLevel: .public,
            isOverride: true,
            signature: signature,
            body: body
        )
    }

    // MARK: - Main productions

    func generateRemainingProductions() throws -> [MemberDecl] {
        var result: [MemberDecl] = []

        while !remaining.isEmpty {
            let next = remaining.removeFirst()

            result.append(contentsOf: try generateRemainingProduction(next))

            generated.append(next)
        }

        return result
    }

    func generateRemainingProduction(_ production: RemainingProduction) throws -> [MemberDecl] {
        switch production {
        case .rule(let rule):
            if let rule = try generateRule(rule, for: production) {
                return [.function(rule)]
            }

        case .auxiliary(let rule, _):
            if let rule = try generateRule(rule, for: production) {
                return [.function(rule)]
            }

        case .nonStandardRepetition(let auxInfo, let repetition):
            let rule = try generateNonStandardRepetition(
                auxInfo,
                repetition,
                for: production
            )

            return [.function(rule)]
        }

        return []
    }

    // MARK: Production: Rule/Auxiliary rule

    fileprivate func generateRule(
        _ rule: InternalGrammar.Rule,
        for production: RemainingProduction
    ) throws -> FunctionMemberDecl? {
        if latestSettings.omitUnreachable && !rule.isReachable {
            return nil
        }

        let comments = generateRuleDocComment(rule)

        // @memoized/@memoizedLeftRecursive
        // @inlinable
        let name = bindingEngine.alias(for: rule)
        let memoizationMode = memoizationMode(for: production)

        let attributes = generateRuleMethodAttributes(
            memoization: memoizationMode,
            memoizedName: name
        )

        let fName = memoizationMode == .none ? name : "__\(name)"
        let parameters = generateParameters(rule.ruleParameters)

        if let parameters = rule.ruleParameters {
            for parameter in parameters {
                declContext.defineLocal(suggestedName: parameter.name, type: .none)
            }
        }

        let returnType = bindingEngine.returnTypeForRule(rule).asSwiftASTType

        let signature = FunctionSignature(
            attributes: attributes,
            name: fName,
            parameters: parameters,
            returnType: returnType,
            traits: [.throwing]
        )

        let body: CompoundStatement = []

        // let mark = self.mark()
        // var cut = CutFlag()
        //
        // ...if-let alt patterns...
        //
        // return nil
        let failReturnExpression = failReturnExpression(for: rule.type)

        declContext.push()
        defer { declContext.pop() }

        let bailBlock: () -> [Statement] = {
            var result: [Statement] = []
            if let action = rule.failAction {
                let action = self.generateAction(action)
                result.append(.expression(action))
            }
            result.append(.return(failReturnExpression.copy()))

            return result
        }

        let requiresMarkers = self.requiresMarkers(rule)
        let requiresCutFlag = self.requiresCutFlag(rule)
        var markerBlock: () -> Statement = { .do([]) }
        var cutBlock: () -> Statement = { .do([]) }

        if requiresMarkers {
            let (stmt, backtrackMarkerName) = generateMarkDeclaration()
            body.appendStatement(stmt)
            markerBlock = {
                self.generateMarkRestore(markVarName: backtrackMarkerName)
            }
        }
        if requiresCutFlag {
            let (stmt, cutFlagName) = generateCutFlagDeclaration()
            body.appendStatement(stmt)
            cutBlock = {
                self.generateCutFlagBailStatement(cutVarName: cutFlagName, bailBlock)
            }
        }

        if let action = rule.action {
            let expr = generateAction(action)
            body.appendStatement(.expression(expr))
        }

        for (altIndex, alt) in rule.alts.enumerated() {
            let stmts = try generateAlt(
                alt,
                altIndex,
                in: production,
                backtrackBlock: markerBlock,
                cutFlagBlock: cutBlock
            )

            body.appendStatements(stmts)
        }

        body.appendStatements(bailBlock())

        return .init(
            leadingComments: comments,
            accessLevel: .public,
            signature: signature,
            body: body
        )
    }

    // MARK: Production: Non-standard repetition with trailing cases

    func generateNonStandardRepetition(
        _ info: AuxiliaryRuleInformation,
        _ repetitionInfo: RepetitionInfo,
        for production: RemainingProduction
    ) throws -> FunctionMemberDecl {

        // Production

        let ruleName = info.name
        let ruleType = info.bindings.be_asTupleType().be_optionalWrapped()
        let returnType = ruleType.asSwiftASTType.scg_removingTupleLabels()

        // Synthesize method
        let comments = generateRuleDocComment(ruleName, info.bindings.be_asTupleType(), nil, nil, nil, [
            .init(namedItems: [repetitionInfo.repetition] + repetitionInfo.trail)
        ])

        // @memoized/@memoizedLeftRecursive
        // @inlinable
        let attributes = generateRuleMethodAttributes(
            memoization: info.memoizationMode,
            memoizedName: ruleName
        )

        let fName = info.memoizationMode == .none ? ruleName : "__\(ruleName)"
        let signature = FunctionSignature(
            attributes: attributes,
            name: fName,
            returnType: returnType,
            traits: [.throwing]
        )

        let body: CompoundStatement = []

        func repetitionAtom() -> InternalGrammar.Atom {
            guard let atom = repetitionInfo.repetition.asItem?.atom else {
                fatalError("Repetition request did not reference a repetition item?")
            }

            return atom
        }
        func repetitionType() -> CommonAbstract.SwiftType {
            return bindingEngine.typeForAtom(repetitionAtom())
        }

        let failReturnExpression = failReturnExpression(for: ruleType)
        let info = RepetitionBodyGenInfo(
            production: production,
            ruleInfo: info,
            repetitionAtom: repetitionAtom(),
            repetitionInfo: repetitionInfo,
            failReturnExpression: failReturnExpression.description,
            repetitionAtomType: repetitionType()
        )

        declContext.push()
        defer { declContext.pop() }

        switch repetitionInfo.repetition {
        case .item(_, .zeroOrMore(_, repetitionMode: .minimal), _):
            body.appendStatements(
                try generateZeroOrMoreMinimalBody(info)
            )

        case .item(_, .zeroOrMore(_, repetitionMode: .maximal), _):
            body.appendStatements(
                try generateZeroOrMoreMaximalBody(info)
            )

        case .item(_, .oneOrMore(_, repetitionMode: .minimal), _):
            body.appendStatements(
                try generateOneOrMoreMinimalBody(info)
            )

        case .item(_, .oneOrMore(_, repetitionMode: .maximal), _):
            body.appendStatements(
                try generateOneOrMoreMaximalBody(info)
            )

        case .item(_, .gather(let sep, let node, repetitionMode: .minimal), _):
            body.appendStatements(
                try generateGatherMinimalBody(
                    separator: sep, node: node, info
                )
            )

        case .item(_, .gather(let sep, let node, repetitionMode: .maximal), _):
            body.appendStatements(
                try generateGatherMaximalBody(
                    separator: sep, node: node, info
                )
            )

        default:
            fatalError("Repetition request's Item is not a non-standard repetition item?")
        }

        return .init(
            leadingComments: comments,
            accessLevel: .public,
            signature: signature,
            body: body
        )
    }

    // MARK: - Rule-related productions

    /// ```
    /// /// ```
    /// /// ruleName[ruleType]:
    /// ///     [{ action }] [!!{ failAction }]
    /// ///     | alt1
    /// ///     | alt2
    /// ///     ...
    /// ///     ;
    /// /// ```
    /// ```
    fileprivate func generateRuleDocComment(_ rule: InternalGrammar.Rule) -> [SwiftComment] {
        let ruleName = rule.name
        let ruleType = rule.type
        let ruleParameters = rule.ruleParameters
        let action = rule.action
        let failAction = rule.failAction
        let alts = rule.alts

        return generateRuleDocComment(ruleName, ruleType, ruleParameters, action, failAction, alts)
    }

    /// ```
    /// /// ```
    /// /// ruleName[ruleType]:
    /// ///     [{ action }] [!!{ failAction }]
    /// ///     | alt1
    /// ///     | alt2
    /// ///     ...
    /// ///     ;
    /// /// ```
    /// ```
    fileprivate func generateRuleDocComment(
        _ ruleName: String,
        _ ruleType: CommonAbstract.SwiftType?,
        _ ruleParameters: [InternalGrammar.RuleParameter]?,
        _ action: InternalGrammar.Action?,
        _ failAction: InternalGrammar.Action?,
        _ alts: [InternalGrammar.Alt]
    ) -> [SwiftComment] {
        let buffer = CodeStringBuffer()

        // Derive a doc comment for the generated rule
        let linePrefix = " "
        let indentation = "    "
        func emitIndentation() {
            buffer.emit(linePrefix)
            buffer.emit(indentation)
        }

        buffer.emitLine("\(linePrefix)```")
        buffer.emit("\(linePrefix)\(ruleName)")
        if let ruleType {
            buffer.emit("[\(ruleType.scg_asReturnType())]")
        }
        if let ruleParameters {
            buffer.emit("(")
            buffer.emitWithSeparators(ruleParameters, separator: ", ") { parameter in
                buffer.emit("\(parameter)")
            }
            buffer.emit(")")
        }
        buffer.emitLine(":")
        if action != nil || failAction != nil {
            emitIndentation()
            // { action }
            if let action {
                buffer.emit("\(action)")
            }
            // !!{ failAction }
            if let failAction {
                buffer.ensureSpaceSeparator()
                buffer.emit("!!\(failAction)")
            }
            buffer.emitNewline()
        }
        for alt in alts {
            emitIndentation()
            buffer.emitLine("| \(alt)")
        }
        emitIndentation()
        buffer.emitLine(";")
        buffer.emitLine("\(linePrefix)```")

        return [.docBlock(buffer.finishBuffer(addTrailingNewline: false))]
    }

    /// ```
    /// <none> / @memoized("<name>") / @memoizedLeftRecursive("<name>")
    /// @inlinable
    /// ```
    fileprivate func generateRuleMethodAttributes(
        memoization: MemoizationMode,
        memoizedName: String
    ) -> [DeclarationAttribute] {
        var result: [DeclarationAttribute] = []

        switch memoization {
        case .none:
            break
        case .memoized:
            result.append(
                .init(name: "memoized", arguments: [
                    .unlabeled(.constant(.string(memoizedName)))
                ])
            )
        case .memoizedLeftRecursive:
            result.append(
                .init(name: "memoizedLeftRecursive", arguments: [
                    .unlabeled(.constant(.string(memoizedName)))
                ])
            )
        }

        result.append(.init(name: "inlinable"))

        return result
    }

    fileprivate func generateParameters(
        _ parameters: [InternalGrammar.RuleParameter]?
    ) -> [ParameterSignature] {
        guard let parameters else {
            return []
        }

        return parameters.map(generateParameter)
    }

    fileprivate func generateParameter(
        _ parameter: InternalGrammar.RuleParameter
    ) -> ParameterSignature {
        .init(
            name: escapeIdentifier(parameter.name, isLabel: true),
            type: parameter.type.asSwiftASTType
        )
    }

    fileprivate func generateAlt(
        _ alt: InternalGrammar.Alt,
        _ altIndex: Int,
        in production: RemainingProduction,
        backtrackBlock: () -> Statement,
        cutFlagBlock: () -> Statement
    ) throws -> [Statement] {
        var results: [Statement] = []

        if alt.namedItems.isEmpty { return results }

        declContext.push()

        // if bindings/conditions
        let clauses = try generateNamedItems(alt.namedItems, in: production)

        // if block
        let ifExpr: Expression = .if(clauses: clauses.0, body: [
            // Successful alt match
            generateOnAltMatchBlock(alt, altIndex, in: production)
        ])

        results.append(.expression(ifExpr))

        declContext.pop()

        if requiresMarkers(alt) {
            // Alt failure results in a restore to a previous mark
            results.append(backtrackBlock())
        }

        // Generate fail action, if present
        if let failAction = alt.failAction {
            results.append(.expression(generateAction(failAction)))
        }

        if requiresCutFlag(alt) {
            results.append(cutFlagBlock())
        }

        return results
    }

    /// Generates the block of code that is run when a given alt is matched
    /// successfully.
    ///
    /// If `self.implicitReturns` is `true`, always appends `return` to the start
    /// of the action's resolved string.
    fileprivate func generateOnAltMatchBlock(
        _ alt: InternalGrammar.Alt,
        _ altIndex: Int,
        in production: RemainingProduction
    ) -> Statement {
        if
            let producerProtocol,
            production.isGrammarRule,
            let producer = producerProtocol.ruleProducer(
                forRuleName: production.name,
                altIndex: altIndex
            )
        {
            return .return(
                .try(producer._makeCallExpression(.identifier(producerMemberName()), escapeIdentifier))
            )
        }

        let exp = generateOnAltMatchBlockInterior(
            alt,
            production.productionType
        )
        if implicitReturns {
            return .return(exp)
        }
        return .expression(exp)
    }

    func generateOnAltMatchBlockInterior(
        _ alt: InternalGrammar.Alt,
        _ productionType: CommonAbstract.SwiftType?
    ) -> Expression {
        if let action = alt.action {
            return generateAction(action)
        }

        let expression = defaultReturnExpression(
            for: alt,
            productionType: productionType
        )

        return expression
    }

    /// Generates a given action as an in-line string terminated with a newline.
    ///
    /// Whitespace surrounding the action's string is stripped before emitting
    /// the string.
    fileprivate func generateAction(_ action: InternalGrammar.Action) -> Expression {
        return .unknown(.init(context: action.string.trimmingWhitespace()))
    }

    /// Generates items as a sequence of if-let segments from a given sequence of
    /// named items, separated by commas.
    ///
    /// Returns an array of arrays containing the deduplicated names of all named
    /// bindings created. Return is empty if no named bindings where created.
    @discardableResult
    func generateNamedItems(
        _ namedItems: [InternalGrammar.NamedItem],
        in production: RemainingProduction
    ) throws -> (ConditionalClauses, [[String]]) {

        var resultClauses: [ConditionalClauseElement] = []
        var resultBindings: [[String]] = []
        let nonStandardIndex = Self.nonStandardRepetitionIndex(in: namedItems)

        for (i, namedItem) in namedItems.enumerated() {
            // Switch over to non-standard repetition, if present
            if i == nonStandardIndex {
                let auxInfo = enqueueNonStandardRepetition(
                    for: production,
                    namedItems[i...]
                )

                // Emit a dummy item that binds the repetition's results
                let item: InternalGrammar.Item = .atom(.ruleName(auxInfo.name))

                let (clauses, bindings) = try generateBindingsToItem(
                    item,
                    auxInfo.bindings,
                    in: production
                )
                resultClauses.append(clauses)
                resultBindings.append(bindings)

                // Stop emitting items after the repetition binding is emitted
                break
            } else {
                let (clauses, bindings) = try generateNamedItem(namedItem, in: production)
                resultClauses.append(clauses)
                resultBindings.append(bindings)
            }
        }

        return (.init(clauses: resultClauses), resultBindings)
    }

    /// `let <bind>[: <BindingType>] = <production>`
    /// Or:
    /// `case let (<bind1>?, <bind2?>, ...) = <production>`
    /// Depending on how many bindings exist in the named item.
    ///
    /// Returns an array containing the deduplicated names of all named bindings
    /// created. Return is empty if no named bindings where created.
    @discardableResult
    fileprivate func generateNamedItem(
        _ namedItem: InternalGrammar.NamedItem,
        in production: RemainingProduction
    ) throws -> (ConditionalClauseElement, [String]) {

        switch namedItem {
        case .item(_, let item, _):
            var bindings = bindingEngine.bindings(for: namedItem)
            if bindings.isEmpty {
                bindings = [
                    ("_", bindingEngine.typeForNamedItem(namedItem))
                ]
            }

            return try generateBindingsToItem(item, bindings, in: production)

        case .lookahead(let lookahead):
            let exp = try generateLookahead(lookahead, in: production)
            return (.init(expression: exp), [])
        }
    }

    /// Generates a binding pattern that binds the production described by `item`
    /// with a given set of bindings.
    ///
    /// Returns an array containing the deduplicated names of all named bindings
    /// created. Return is empty if no named bindings where created.
    @discardableResult
    func generateBindingsToItem(
        _ item: InternalGrammar.Item,
        _ bindings: [BindingEngine.Binding],
        in production: RemainingProduction
    ) throws -> (ConditionalClauseElement, [String]) {

        // Emit bindings
        let isOptionalItem = item.isOptional || item.isOptionalItems
        let (clause, bindingNames) = try generatePatternBinds(
            bindings,
            forceCase: isOptionalItem,
            performUnwrap: !isOptionalItem,
            in: production
        )

        let exp = try generateItem(item, in: production)

        // Detect shuffleTuple requirement
        let requiresShuffle = bindingEngine.requiresTupleShuffling(item)
        if requiresShuffle {
            clause.expression = .identifier("self").dot("shuffleTuple").call([exp])
        } else {
            clause.expression = exp
        }

        return (clause, bindingNames)
    }

    fileprivate func generateItem(
        _ item: InternalGrammar.Item,
        in production: RemainingProduction
    ) throws -> Expression {
        switch item {
        case .optional(let atom):
            return try generateAtom(atom, unwrapped: false, in: production)

        case .optionalItems(let alts):
            let aux = enqueueAuxiliaryRule(
                for: production,
                suffix: "_opt",
                unwrapped: true,
                alts: alts
            )

            return .try(.identifier("self").dot(aux).call())

        case .gather(let sep, let item, _):
            return .try(.identifier("self").dot("gather").call([
                .init(label: "separator", expression: .block(signature: nil, body: [
                    .expression(try generateAtom(sep, unwrapped: false, in: production))
                ])),
                .init(label: "item", expression: .block(signature: nil, body: [
                    .expression(try generateAtom(item, unwrapped: false, in: production))
                ])),
            ]))

        case .zeroOrMore(let atom, _):
            return .try(.identifier("self").dot("repeatZeroOrMore").call([
                .block(signature: nil, body: [
                    .expression(try generateAtom(atom, unwrapped: false, in: production))
                ])
            ]))

        case .oneOrMore(let atom, _):
            return .try(.identifier("self").dot("repeatOneOrMore").call([
                .block(signature: nil, body: [
                    .expression(try generateAtom(atom, unwrapped: false, in: production))
                ])
            ]))

        case .atom(let atom):
            return try generateAtom(atom, unwrapped: false, in: production)
        }
    }

    private func generateLookahead(
        _ lookahead: InternalGrammar.Lookahead,
        in production: RemainingProduction
    ) throws -> Expression {
        switch lookahead {
        case .forced(let atom):
            return .try(.identifier("self").dot("expectForced").call([
                .block(signature: nil, body: [
                    .expression(try generateAtom(atom, unwrapped: true, in: production))
                ]),
                .constant(.string(String(atom.description)))
            ]))

        case .positive(let atom):
            return .try(.identifier("self").dot("positiveLookahead").call([
                .block(signature: nil, body: [
                    .expression(try generateAtom(atom, unwrapped: true, in: production))
                ])
            ]))

        case .negative(let atom):
            return .try(.identifier("self").dot("negativeLookahead").call([
                .block(signature: nil, body: [
                    .expression(try generateAtom(atom, unwrapped: true, in: production))
                ])
            ]))

        case .cut:
            return generateCutFlagToggleExpression()
        }
    }

    func generateAtom(
        _ atom: InternalGrammar.Atom,
        unwrapped: Bool,
        in production: RemainingProduction
    ) throws -> Expression {
        switch atom {
        case .group(let group):
            let aux = enqueueAuxiliaryRule(
                for: production,
                suffix: "_group_",
                unwrapped: unwrapped,
                alts: group
            )

            return .try(.identifier("self").dot(aux).call())

        case .ruleName(let ident, let parameters):
            return .try(.identifier("self").dot(escapeIdentifier(ident)).call(generateAtomParameters(parameters)))

        case .token(let ident):
            return .try(expandTokenName(ident))

        case .anyToken:
            return .try(.identifier("self").dot("nextToken").call())

        // Token literal
        case .string(let string, let raw):
            var literal = string

            // Avoid emitting single-quoted string literals
            if string.hasPrefix("'") {
                literal = #""\#(raw)""#
            } else {
                literal = string
            }

            // Escape backslashes contents
            literal = literal.replacing("\\", with: #"\\"#)

            let callArgs = self.expectArguments(forLiteral: literal, raw: raw)

            return .try(.identifier("self").dot("expect").call(callArgs))
        }
    }

    func generateAtomParameters(
        _ parameters: [InternalGrammar.AtomParameter]?
    ) -> [FunctionArgument] {
        guard let parameters else {
            return []
        }

        return parameters.map(generateAtomParameter)
    }

    func generateAtomParameter(
        _ parameters: InternalGrammar.AtomParameter
    ) -> FunctionArgument {
        .init(label: parameters.label, expression: .unknown(.init(context: parameters.action.string.trimmingWhitespace())))
    }

    /// `let <bind>[: <BindingType>]`
    /// Or:
    /// `case let (<bind1>?, <bind2?>, ...)`
    /// Depending on how many bindings exist in `bindings`.
    ///
    /// If `forceCase` is `true`, the pattern always leads with a 'case', which
    /// has the side effect of requiring optional unwrapping to be explicit on
    /// the patterns.
    ///
    /// Returns an array containing the deduplicated names of all named bindings
    /// created. Return is empty if no named bindings where created.
    @discardableResult
    fileprivate func generatePatternBinds(
        _ bindings: [BindingEngine.Binding],
        forceCase: Bool = false,
        performUnwrap: Bool = true,
        in production: RemainingProduction
    ) throws -> (ConditionalClauseElement, [String]) {

        let clause = ConditionalClauseElement(
            isCaseClause: false,
            pattern: nil,
            expression: .voidTuple()
        )
        var isLetPatternBinding = false

        // When emitting more than one optional unwrapping on tuple types,
        // emit a case-let- binding so we can unwrap each identifier within
        // the tuple individually with a '<ident>?' pattern.
        var isCasePatternBind = false
        if bindings.count > 1 {
            isCasePatternBind = true
        }

        if isCasePatternBind || forceCase {
            clause.isCaseClause = true
        }

        // Skip 'let' for 'case' patterns with no named bindings
        let emitLet =
            if (forceCase || isCasePatternBind) && !bindings.be_hasBindings() {
                false
            } else {
                true
            }
        if emitLet {
            isLetPatternBinding = true
        }

        var deduplicatedBindings: [String] = []
        var bindingTuple: [Pattern] = []
        var bindingTupleType: [SwiftType] = []

        for binding in bindings {
            var isOptionalBind = false
            var bindingName = binding.label ?? "_"
            let bindingType = binding.type

            if bindingName != "_" {
                bindingName = declContext.defineLocal(suggestedName: bindingName).name
                deduplicatedBindings.append(bindingName)
            }

            // Emit unwrap pattern
            if isCasePatternBind && performUnwrap {
                isOptionalBind = true
            }

            bindingTupleType.append(bindingType.asSwiftASTType)

            let bindElement: Pattern

            if bindingName == "_" {
                bindElement = .wildcard()
            } else {
                bindElement = .identifier(escapeIdentifier(bindingName))
            }

            if isOptionalBind {
                bindingTuple.append(.optional(bindElement))
            } else {
                bindingTuple.append(bindElement)
            }
        }

        if bindingTuple.count == 0 {
            clause.pattern = .tuple(bindingTuple)
        } else if bindingTuple.count == 1 {
            clause.pattern = bindingTuple[0]
        } else {
            clause.pattern = .tuple(bindingTuple)
        }

        if latestSettings.emitTypesInBindings && !isCasePatternBind {
            if bindingTuple.count == 0 {
                clause.pattern = .tuple(bindingTuple)
            } else if bindingTuple.count == 1 {
                switch bindingTuple[0] {
                case .identifier(let ident, _):
                    clause.pattern = .identifier(ident, .init(type: bindingTupleType[0]))

                case .wildcard(_):
                    clause.pattern = .wildcard(.init(type: bindingTupleType[0]))

                default:
                    clause.pattern = .tuple(bindingTuple, .init(type: bindingTupleType[0]))
                }
            } else {
                clause.pattern = .tuple(bindingTuple, .init(type: .tuple(.types(.fromCollection(bindingTupleType.map { .unlabeled($0) })))))
            }
        }

        if isLetPatternBinding, let pattern = clause.pattern {
            clause.pattern = .valueBindingPattern(constant: true, pattern)
        }

        return (clause, deduplicatedBindings)
    }

    private func failReturnExpression(for type: CommonAbstract.SwiftType?) -> Expression {
        return .constant(.nil)
    }

    private func memoizationMode(for production: RemainingProduction) -> MemoizationMode {
        switch production {
        case .rule(let rule):
            return memoizationMode(for: rule)

        case .auxiliary(_, let info), .nonStandardRepetition(let info, _):
            return info.memoizationMode
        }
    }

    private func memoizationMode(for rule: InternalGrammar.Rule) -> MemoizationMode {
        return
            if rule.isLeftRecursiveLeader {
                .memoizedLeftRecursive
            } else if !rule.isLeftRecursive {
                .memoized
            } else {
                .none
            }
    }
}


// MARK: - Auxiliary method management

extension SwiftCodeGen {
    /// Generates a default expression representing the return of an alternative,
    /// ignoring its associated action.
    func defaultReturnExpression(
        for alt: InternalGrammar.Alt,
        productionType: CommonAbstract.SwiftType?
    ) -> Expression {
        // If no action is specified, attempt to return instead the named
        // item within the alt, if it's the only named item in the alt.
        if alt.namedItems.count == 1 {
            let bindings = bindingEngine.computeBindings(alt)
            if bindings.count == 1, let label = bindings[0].label {
                return .identifier(escapeIdentifier(label))
            }
        }

        // Fallback: Return an initialization of the associated node type, assuming
        // it is not `nil` and is not a known existential type.
        if let type = productionType?.description, type != "Any" {
            if type == "Void" || type == "()" {
                return .voidTuple()
            } else {
                return .identifier(type).call()
            }
        }

        // If no other option was found, default to 'Node()'
        return .identifier("Node").call()
    }

    /// Attempts to compute a default return action for a given set of return
    /// elements of a rule or auxiliary rule.
    func defaultReturnAction(
        for elements: [AuxiliaryRuleInformation.ReturnElement]
    ) -> InternalGrammar.Action {

        return defaultReturnAction(for: elements.map { element in
            (label: element.label ?? "_", identifier: element.label ?? "_")
        })
    }

    /// Attempts to compute a default return action for a given set of
    /// `<label>: <identifier>` pair expressions.
    func defaultReturnAction(
        for labeledExpressions: [(label: String, identifier: String)]
    ) -> InternalGrammar.Action {

        return .init(
            string: " \(defaultReturnExpression(for: labeledExpressions)) "
        )
    }

    /// Attempts to compute a default return expression for a given set of
    /// `<label>: <identifier>` pair expressions.
    func defaultReturnExpression(
        for labeledExpressions: [(label: String?, identifier: String)]
    ) -> Expression {

        // Avoid emitting single-tuple constructions
        if labeledExpressions.count == 1 {
            return .identifier(escapeIdentifier(labeledExpressions[0].identifier))
        } else {
            let tupleElements: [TupleElement] = labeledExpressions.map { expr in
                if let label = expr.label {
                    .init(label: label, exp: .identifier(expr.identifier))
                } else {
                    .init(label: nil, exp: .identifier(expr.identifier))
                }
            }

            return TupleExpression(elements: tupleElements)
        }
    }
}

// MARK: - Rule return type management

fileprivate extension SwiftCodeGen {
    func returnType(for rule: InternalGrammar.Rule) -> SwiftType {
        bindingEngine.returnTypeForRule(rule).asSwiftASTType
    }
}

// MARK: Identifier/token management

fileprivate extension SwiftCodeGen {
    /// Returns the appropriate handling of an identifier that may be a token
    /// identifier
    ///
    /// If the identifier matches a known token definition with explicit
    /// 'tokenCodeReference', returns `self.expect(<tokenCodeReference>)`, otherwise returns
    /// `self.<ident>()`, as a fallback.
    func expandTokenName(_ ident: String) -> Expression {
        if
            let token = bindingEngine.tokenDefinition(named: ident),
            let tokenCodeReference = tokenCodeReference(for: token)
        {
            return
                .identifier("self")
                .dot("expect")
                .call(expectArguments(forResolvedToken: tokenCodeReference))
        }

        return
            .identifier("self")
            .dot(escapeIdentifier(ident))
            .call()
    }

    /// Returns the arguments to invoke a `PEGParser.expect()` call, as a
    /// non-parenthesized labeled expression list separated by commas, in order
    /// to probe the parser about a specific token identifier.
    ///
    /// If no associated token identifier has been defined in a .tokens file,
    /// the result is a default `kind: <identifier>` or `<identifier>`,
    /// depending on the value of '@tokenCall' meta-property, if present.
    func expectArguments(forIdentifier identifier: String) -> [FunctionArgument] {
        // Check for explicit token aliases
        if
            let token = bindingEngine.tokenDefinition(named: identifier),
            let tokenCodeReference = tokenCodeReference(for: token)
        {
            return expectArguments(forResolvedToken: tokenCodeReference)
        }

        return expectArguments(forResolvedToken: .identifier(identifier))
    }

    /// Returns the arguments to invoke a `PEGParser.expect()` call, as a
    /// non-parenthesized labeled expression list separated by commas, in order
    /// to probe the parser about a specific token literal.
    ///
    /// If no associated token literal has been defined in a .tokens file, the
    /// result is a default `kind: "<literal>"` or `"<literal>"`, depending on
    /// the value of '@tokenCall' meta-property, if present.
    func expectArguments(forLiteral literal: String, raw: String) -> [FunctionArgument] {
        // Check for explicit token aliases
        if
            let token = bindingEngine.tokenDefinition(ofRawLiteral: raw),
            let tokenCodeReference = tokenCodeReference(for: token)
        {
            return expectArguments(forResolvedToken: tokenCodeReference)
        }

        return expectArguments(forResolvedToken: .constant(.string(raw)))
    }

    /// Computes the static token name for a given token definition.
    ///
    /// If a custom static token was provided (`['.tokenCodeReference']`), that value
    /// is returned; otherwise, an attempt is made to compute the potential case
    /// name for a generated token type.
    ///
    /// If the token is missing both the static token and token syntax, it is
    /// assumed to be implemented off-lexer and the return is `nil`.
    func tokenCodeReference(for token: InternalGrammar.TokenDefinition) -> Expression? {
        if let tokenCodeReference = token.tokenCodeReference {
            return .unknown(.init(context: tokenCodeReference))
        }
        if token.tokenSyntax == nil {
            return nil
        }

        return .implicitMember(caseName(for: token))
    }

    /// Does final expansion of token `self.expect` call arguments based on the
    /// current configuration of `tokenCallKind`.
    func expectArguments(forResolvedToken resolvedToken: Expression) -> [FunctionArgument] {
        if tokenCallKind == .expectKind {
            return [.labeled("kind", resolvedToken)]
        } else {
            return [.unlabeled(resolvedToken)]
        }
    }
}

internal extension SwiftType {
    /// Returns a copy of this type, deeply removing all tuple labels.
    func scg_removingTupleLabels() -> Self {
        switch self {
        case .array(let element):
            return .array(element.scg_removingTupleLabels())

        case .block(let block):
            return .block(
                .init(
                    returnType: block.returnType.scg_removingTupleLabels(),
                    parameters: block.parameters.map { param in
                        var param = param
                        param.type = param.type.scg_removingTupleLabels()
                        return param
                    },
                    attributes: block.attributes
                )
            )

        case .dictionary(let key, let value):
            return .dictionary(key: key.scg_removingTupleLabels(), value: value.scg_removingTupleLabels())

        case .implicitUnwrappedOptional(let base):
            return .implicitUnwrappedOptional(base.scg_removingTupleLabels())

        case .metatype(let base):
            return .metatype(for: base.scg_removingTupleLabels())

        case .nested(let nested):
            return .nested(
                .init(base: nested.base.scg_removingTupleLabels(), nested: nested.nested)
            )

        case .nominal(.typeName(let name)):
            return .nominal(.typeName(name))

        case .nominal(.generic(let name, parameters: let params)):
            return .nominal(.generic(name, parameters: .fromCollection(params.map { $0.scg_removingTupleLabels() })))

        case .nullabilityUnspecified(let base):
            return .nullabilityUnspecified(base.scg_removingTupleLabels())

        case .optional(let base):
            return .optional(base.scg_removingTupleLabels())

        case .protocolComposition(let types):
            return .protocolComposition(types)

        case .tuple(.empty):
            return .tuple(.empty)

        case .tuple(.types(let elements)):
            let elements: [TupleTypeEntry] = elements.map { element in
                .unlabeled(element.swiftType.scg_removingTupleLabels())
            }

            return .tuple(.types(.fromCollection(elements)))
        }
    }
}

internal extension CommonAbstract.SwiftType {
    /// Returns a string representation of this Swift type that can be used as a
    /// valid Swift type in code.
    func scg_asValidSwiftType() -> String {
        self.description
    }

    /// Returns a string representation of this Swift type that can be used as a
    /// valid return type for production rules.
    func scg_asReturnType() -> String {
        scg_removingTupleLabels().description
    }

    /// Returns a copy of this type, deeply removing all tuple labels.
    func scg_removingTupleLabels() -> Self {
        switch self {
        case .tuple(let elements):
            return .tuple(
                elements.map({ .init(label: nil, $0.swiftType.scg_removingTupleLabels()) })
            )

        case .optional(let inner):
            return .optional(inner.scg_removingTupleLabels())

        case .array(let inner):
            return .array(inner.scg_removingTupleLabels())

        case .dictionary(let key, let value):
            return .dictionary(
                key: key.scg_removingTupleLabels(),
                value: value.scg_removingTupleLabels()
            )

        case .nominal(let identifier):
            return .nominal(identifier)

        case .nested(let base, let identifier):
            return .nested(base.scg_removingTupleLabels(), identifier)
        }
    }
}
