import SwiftAST

extension SwiftCodeGen {
    /// Produces a zero-or-more minimal (`*<`) repetition parsing method.
    func generateZeroOrMoreMinimalBody(_ info: RepetitionBodyGenInfo) throws -> [Statement] {
        try _generateMinimalBody(info) { _ in
            return [.init(expression: .constant(true))]
        } whileTail: { ctx in
            /*
            // Collect an extra item and try again
            if
                let <item>: <Item> = try self.<item>()
            {
                _current.append(<item>)
            } else {
                break
            }
            */
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            let ifExpr = try _generateIfLet(item: .atom(info.repetitionAtom), bindings: bindings, in: info.production) { bindingNames in
                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())

                buffer.emitLine("\(ctx.currentArrayName).append(\(expr))")
                return .expression(
                    .identifier(ctx.currentArrayName).dot("append").call([expr])
                )
            }
            ifExpr.elseBody = .else([
                .break()
            ])

            return
                .expression(ifExpr)
                .withComments([" Collect an extra item and try again"])
        }
    }

    /// Produces a zero-or-more maximal (`*>`) repetition parsing method.
    func generateZeroOrMoreMaximalBody(_ info: RepetitionBodyGenInfo) throws -> [Statement] {
        try _generateMaximalBody(info) { _ in
            /*
            var _current: [(Mark, <Item>)] = try self.repeatZeroOrMore({
                if let <item> = try self.<item>() { return (self.mark(), <item>) }
                return nil
            })
            */
            declContext.push()
            let item = InternalGrammar.Item.atom(info.repetitionAtom)
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            let (clause, bound) = try generateBindingsToItem(item, bindings, in: info.production)
            let mapTupleExpr = (["self.mark()", bound.scgr_asTupleExprString()]).scgr_asTupleExpr()
            let itemStmts: [Statement] = [
                .if(clauses: [clause], body: [
                    .return(defaultReturnExpression(for: mapTupleExpr))
                ]),
                .return(.constant(.nil)),
            ]
            declContext.pop()

            return .try(
                .identifier("self").dot("repeatZeroOrMore").call([
                    FunctionArgument(label: nil, expression: .block(signature: nil, body: .init(statements: itemStmts))),
                ])
            )
        } whileCondition: { ctx in
            return [.init(expression: .constant(true))]
        } whileHead: { ctx in
            // let _endMark = <ctx.currentArrayName>.last?.0 ?? <ctx.markerName>
            let markDecl: Statement =
                .variableDeclaration(
                    identifier: "_endMark",
                    type: "Mark",
                    isConstant: true,
                    initialization:
                        .identifier(ctx.currentArrayName)
                        .dot("last").optional()
                        .dot("0")
                        .binary(op: .nullCoalesce, rhs: .identifier(ctx.markerName))
                )

            return markDecl
        } stopCondition: { ctx in
            buffer.emit(" \(ctx.currentArrayName).isEmpty ")

            return [
                .init(expression: .identifier(ctx.currentArrayName).dot("isEmpty"))
            ]
        }
    }

    /// Produces a one-or-more minimal (`+<`) repetition parsing method.
    func generateOneOrMoreMinimalBody(_ info: RepetitionBodyGenInfo) throws -> [Statement] {
        var nextBindings: [String] = []
        return try _generateMinimalBody(info, whileCondition: { _ -> [ConditionalClauseElement] in
            let item = InternalGrammar.Item.atom(info.repetitionAtom)
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            let clause: ConditionalClauseElement
            (clause, nextBindings) = try generateBindingsToItem(item, bindings, in: info.production)

            return [clause]
        }, whileHead: { ctx in
            let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())

            return .expression(
                .identifier(ctx.currentArrayName).dot("append").call([expr])
            )
        })
    }

    /// Produces a one-or-more maximal (`+>`) repetition parsing method.
    func generateOneOrMoreMaximalBody(_ info: RepetitionBodyGenInfo) throws -> [Statement] {
        return try _generateMaximalBody(info) { _ in
            /*
            var _current: [(Mark, <Item>)] = try self.repeatOneOrMore({
                if let <item> = try self.<item>() { return (self.mark(), <item>) }
                return nil
            })
            */
            declContext.push()
            let item = InternalGrammar.Item.atom(info.repetitionAtom)
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            let (clause, bound) = try generateBindingsToItem(item, bindings, in: info.production)
            let mapTupleExpr = (["self.mark()", bound.scgr_asTupleExprString()]).scgr_asTupleExpr()
            let itemStmts: [Statement] = [
                .if(clauses: [clause], body: [
                    .return(defaultReturnExpression(for: mapTupleExpr))
                ]),
                .return(.constant(.nil)),
            ]
            declContext.pop()

            return .try(
                .identifier("self").dot("repeatOneOrMore").call([
                    FunctionArgument(label: nil, expression: .block(signature: nil, body: .init(statements: itemStmts))),
                ])
            )
        } whileCondition: { ctx in
            // let _endMark = <ctx.currentArrayName>.last?.0
            return [
                .init(
                    pattern: .valueBindingPattern(constant: true, .identifier("_endMark")),
                    expression: .identifier(ctx.currentArrayName).dot("last").optional().dot("0")
                )
            ]
        } stopCondition: { ctx in
            // <ctx.currentArrayName>.count <= 1
            return .init(
                pattern: nil,
                expression: .identifier(ctx.currentArrayName).dot("count").binary(op: .lessThanOrEqual, rhs: .constant(1))
            )
        }
    }

    /// Produces a gather minimal (`<sep>.<node>+<`) repetition parsing method.
    func generateGatherMinimalBody(
        separator: InternalGrammar.Atom,
        node: InternalGrammar.Atom,
        _ info: RepetitionBodyGenInfo
    ) throws -> [Statement] {
        var nextBindings: [String] = []
        return try _generateMinimalBody(info, whileCondition: { _ -> [ConditionalClauseElement] in
            let item = InternalGrammar.Item.atom(info.repetitionAtom)
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            let clause: ConditionalClauseElement
            (clause, nextBindings) = try generateBindingsToItem(item, bindings, in: info.production)

            return [clause]
        }, whileHead: { ctx in
            let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())

            return .expression(
                .identifier(ctx.currentArrayName).dot("append").call([expr])
            )
        }, whileTail: { ctx in
            let bindings = bindingEngine.bindings(for: separator).be_unlabeled()
            let (stmt, _) = try _generateGuardBinding(item: .atom(separator), bindings: bindings, in: info.production) {
                return .break()
            }

            return stmt.withComments([" Try separator before next item"])
        })
    }

    /// Produces a gather maximal (`<sep>.<node>+>`) repetition parsing method.
    func generateGatherMaximalBody(
        separator: InternalGrammar.Atom,
        node: InternalGrammar.Atom,
        _ info: RepetitionBodyGenInfo
    ) throws -> [Statement] {
        try _generateMaximalBody(info) { _ in
            /*
            var _current: [(Mark, <Item>)] = try self.gather(separator: {
                try self.<separator>()
            }, item: {
                if let <item> = try self.<item>() { return (self.mark(), <item>) }
                return nil
            })
            */
            declContext.push()
            let separatorExpr = try generateAtom(separator, unwrapped: true, in: info.production)
            declContext.pop()

            declContext.push()
            let item = InternalGrammar.Item.atom(info.repetitionAtom)
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            let (clause, bound) = try generateBindingsToItem(item, bindings, in: info.production)
            let mapTupleExpr = (["self.mark()", bound.scgr_asTupleExprString()]).scgr_asTupleExpr()
            let itemStmts: [Statement] = [
                .if(clauses: [clause], body: [
                    .return(defaultReturnExpression(for: mapTupleExpr))
                ]),
                .return(.constant(.nil)),
            ]
            declContext.pop()

            return .try(
                .identifier("self").dot("gather").call([
                    FunctionArgument(label: "separator", expression: .block(signature: nil, body: [
                        .expression(separatorExpr)
                    ])),
                    FunctionArgument(label: "item", expression: .block(signature: nil, body: .init(statements: itemStmts))),
                ])
            )
        } whileCondition: { ctx in
            // let _endMark = <ctx.currentArrayName>.last?.0
            return [
                .init(
                    pattern: .valueBindingPattern(constant: true, .identifier("_endMark")),
                    expression: .identifier(ctx.currentArrayName).dot("last").optional().dot("0")
                )
            ]
        } stopCondition: { ctx in
            // <ctx.currentArrayName>.count <= 1
            return .init(
                pattern: nil,
                expression: .identifier(ctx.currentArrayName).dot("count").binary(op: .lessThanOrEqual, rhs: .constant(1))
            )
        }
    }

    /// Main code generation for minimal repetitions.
    ///
    /// - Parameters:
    ///   - info: The information structure for the repetition being generated.
    ///   - whileCondition: A block that generates the condition for the main
    /// while loop. Must emit its own whitespace separating the `while` keyword
    /// and the opening brace of the subsequent block.
    ///   - whileHead: An optional production that gets emitted at the start of
    /// the while loop's body, before the starting `let _mark = self.mark()`
    /// declaration.
    ///   - whileTail: An optional production that gets emitted at the end of the
    /// while loop's body, after the ending `self.restore(_mark)` statement.
    fileprivate func _generateMinimalBody(
        _ info: RepetitionBodyGenInfo,
        whileCondition: (BodyGenContext) throws -> [ConditionalClauseElement],
        whileHead: (BodyGenContext) throws -> Statement? = { _ in nil },
        whileTail: (BodyGenContext) throws -> Statement? = { _ in nil }
    ) throws -> [Statement] {
        var result: [Statement] = []

        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().asSwiftASTType

        // Start of method body
        let (markerStmt, markerName) = generateMarkDeclaration()

        result.append(markerStmt)

        var cutFlagName = "_cut"
        let requiresCut = requiresCutFlag(info.trailItems)
        if requiresCut {
            let cutFlagStmt: Statement

            (cutFlagStmt, cutFlagName) = generateCutFlagDeclaration()

            result.append(cutFlagStmt)
        }

        let ctx = BodyGenContext(
            currentArrayName: currentArray,
            markerName: markerName,
            cutFlagName: cutFlagName
        )

        result.append(
            .variableDeclaration(
                identifier: currentArray,
                type: .array(repetitionItemType),
                isConstant: false,
                initialization: .arrayLiteral([])
            )
        )

        declContext.push()

        var whileClauses: [ConditionalClauseElement] = []
        var whileBody: [Statement] = []

        if requiresCut {
            whileClauses.append(
                .init(expression: .unary(op: .negate, generateCutFlagBailExpression(cutVarName: ctx.cutFlagName)))
            )
        }

        whileClauses.append(contentsOf: try whileCondition(ctx))

        if let whileHead = try whileHead(ctx) {
            whileBody.append(whileHead)
        }
        let (whileMarkerStmt, whileMarker) = generateMarkDeclaration()
        whileBody.append(whileMarkerStmt)

        whileBody.append(
            .expression(try _generateSuccessBlock(info, leadItemExpr: currentArray))
        )

        whileBody.append(
            generateMarkRestore(markVarName: whileMarker)
        )

        if let whileTail = try whileTail(ctx) {
            whileBody.append(whileTail)
        }

        result.append(
            .while(clauses: .init(clauses: whileClauses), body: .init(statements: whileBody))
        )

        declContext.pop()

        result.append(generateMarkRestore(markVarName: markerName))
        result.append(.return(info._failReturnExpression))

        return result
    }

    /// Main code generation for maximal repetitions.
    ///
    /// The method expects that either `whileCondition` or `whileHead` emit code
    /// that includes a declaration of a local variable binding `_endMark: Mark`
    /// to be used by an initial `self.restore(_endMark)` statement that is always
    /// emitted at the top of the loop.
    ///
    /// - Parameters:
    ///   - info: The information structure for the repetition being generated.
    ///   - requiresMarkers: Whether to emit a `let _mark = self.mark()` statement
    /// at the top of the function, before the initial production is emitted.
    ///   - initialProduction: A block that generates the expression that gets
    /// bound to the array of initial productions. Must emit an optional expression
    /// type to be bound to a variable.
    ///   - whileCondition: A block that generates the condition for the main
    /// while loop. Must emit its own whitespace separating the `while` keyword
    /// and the opening brace of the subsequent block.
    ///   - whileHead: An optional production that gets emitted at the start of
    /// the while loop's body, before the starting `self.restore(_endMark)`
    /// statement.
    ///   - stopCondition: A block that generates the stop condition of the main
    /// while loop, and is emitted for an `else-if` statement that proceeds after
    /// the main if-let binding of the while loop. Must emit a boolean expression
    /// or pattern binding that can be used by an `if` statement. Any named
    /// optional bindings will go unused within the `if` statement's body.
    fileprivate func _generateMaximalBody(
        _ info: RepetitionBodyGenInfo,
        initialProduction: (BodyGenContext) throws -> Expression,
        whileCondition: (BodyGenContext) throws -> [ConditionalClauseElement],
        whileHead: (BodyGenContext) throws -> Statement? = { _ in nil },
        stopCondition: (BodyGenContext) throws -> ConditionalClauses
    ) throws -> [Statement] {
        var result: [Statement] = []

        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scgr_flattened()
        let arrayElementType = CommonAbstract.SwiftType.tuple([
            .unlabeled("Mark"),
            .unlabeled(repetitionItemType),
        ]).asSwiftASTType

        let (markerStmt, markerName) = generateMarkDeclaration()

        result.append(markerStmt)

        var cutFlagName = "_cut"
        let requiresCut = requiresCutFlag(info.trailItems)
        if requiresCut {
            let cutStmt: Statement
            (cutStmt, cutFlagName) = generateCutFlagDeclaration()

            result.append(cutStmt)
        }

        let ctx = BodyGenContext(
            currentArrayName: currentArray,
            markerName: markerName,
            cutFlagName: cutFlagName
        )

        let initialGuard: GuardStatement =
            .guard(clauses: [
                .init(
                    isCaseClause: false,
                    pattern: .valueBindingPattern(constant: false, .identifier(currentArray, .init(type: .array(arrayElementType)))),
                    expression: try initialProduction(ctx)
                )
            ], else: [
                .return(info._failReturnExpression)
            ])
            .withComments([" Start by fetching as many productions as possible"])

        result.append(initialGuard)

        declContext.push()

        // Main while loop
        var whileClauses: [ConditionalClauseElement] = []
        var whileBody: [Statement] = []

        if requiresCut {
            whileClauses.append(
                .init(expression: .unary(op: .negate, generateCutFlagBailExpression(cutVarName: ctx.cutFlagName)))
            )
        }

        whileClauses.append(contentsOf: try whileCondition(ctx))

        if let head = try whileHead(ctx) {
            whileBody.append(head)
        }

        whileBody.append(
            .expression(.identifier("self").dot("restore").call([.identifier("_endMark")]))
        )

        // if let <trail> = <trail>()
        //   ...
        let ifExpr = try _generateSuccessBlock(info, leadItemExpr: "\(currentArray).map(\\.1)")
        ifExpr.elseBody = .elseIf(
            .if(clauses: try stopCondition(ctx), body: [
                .break()
            ])
        )

        whileBody.append(.expression(ifExpr))

        whileBody.append(
            .expression(.identifier(currentArray).dot("removeLast").call())
                .withComments([" Drop an item, backtrack the parser, and try again"])
        )

        declContext.pop()

        result.append(
            .while(clauses: .init(clauses: whileClauses), body: .init(statements: whileBody))
        )

        result.append(generateMarkRestore(markVarName: markerName))
        result.append(.return(info._failReturnExpression))

        return result
    }

    /// Generates the main if-let binding that succeeds the repetition parser
    /// method.
    ///
    /// `leadItemExpr` must be an expression that resolves as the first item of
    /// the repetition production's result tuple.
    fileprivate func _generateSuccessBlock(
        _ info: RepetitionBodyGenInfo,
        leadItemExpr: String
    ) throws -> IfExpression {
        try _generateIfLet(namedItems: info.trailItems, in: info.production) { (bindingNames) in
            let bindings = [leadItemExpr] + bindingNames.flatMap({ $0 })

            let expr = defaultReturnExpression(for: bindings.scgr_asTupleExpr())

            return .return(expr)
        }
    }

    /// Generates the if-let binding that succeeds the associated repeated
    /// production.
    ///
    /// ```
    /// if
    ///     let <bindings> = <item>
    /// {
    ///     return (<leftExpression>, <trail bind>)
    /// }
    /// ```
    ///
    /// The provided block receives as argument an array-of-arrays containing
    /// the deduplicated bindings `<bindings>` of the if statement.
    fileprivate func _generateIfLet(
        item: InternalGrammar.Item,
        bindings: [BindingEngine.Binding],
        in production: RemainingProduction,
        block: ([String]) throws -> Statement
    ) throws -> IfExpression {
        let bindingNames = try generateBindingsToItem(
            item,
            bindings,
            in: production
        )

        return .if(clauses: [bindingNames.0], body: [
            try block(bindingNames.1)
        ])
    }

    /// Generates:
    ///
    /// ```
    /// guard
    ///     let <bindings> = <item>
    /// else {
    ///     elseBlock()
    /// }
    /// ```
    ///
    /// - Returns: An array containing the deduplicated bindings `<bindings>`
    /// of the guard statement.
    fileprivate func _generateGuardBinding(
        item: InternalGrammar.Item,
        bindings: [BindingEngine.Binding],
        in production: RemainingProduction,
        elseBlock: () throws -> Statement
    ) throws -> (GuardStatement, [String]) {
        let bindingNames = try generateBindingsToItem(
            item,
            bindings,
            in: production
        )

        let stmt = GuardStatement.guard(clauses: [bindingNames.0], else: [
            try elseBlock()
        ])

        return (stmt, bindingNames.1)
    }

    /// Generates the if-let binding that succeeds the associated repeated
    /// production.
    ///
    /// ```
    /// if
    ///     let <bindings1> = <namedItem1>,
    ///     let <bindings2> = <namedItem2>,
    ///     ...
    /// {
    ///     return (<leftExpression>, <trail bind>)
    /// }
    /// ```
    ///
    /// The provided block receives as argument an array-of-arrays containing
    /// the deduplicated bindings `[<bindings1>, <bindings2>, ...]` of the if
    /// statement.
    fileprivate func _generateIfLet(
        namedItems: [InternalGrammar.NamedItem],
        in production: RemainingProduction,
        block: ([[String]]) throws -> Statement
    ) throws -> IfExpression {
        declContext.push()
        defer { declContext.pop() }

        let bindingNames = try generateNamedItems(namedItems, in: production)

        let body: CompoundStatement = [
            try block(bindingNames.1)
        ]

        let expr = IfExpression.if(clauses: bindingNames.0, body: body)
        return expr
    }

    /// Generates:
    ///
    /// ```
    /// guard
    ///     let <bindings1> = <namedItem1>,
    ///     let <bindings2> = <namedItem2>,
    ///     ...
    /// else {
    ///     elseBlock()
    /// }
    /// ```
    ///
    /// - Returns: An array-of-arrays containing the deduplicated bindings
    /// `[<bindings1>, <bindings2>, ...]` of the guard statement.
    fileprivate func _generateGuardBinding(
        namedItems: [InternalGrammar.NamedItem],
        in production: RemainingProduction,
        elseBlock: () throws -> Statement
    ) throws -> (GuardStatement, [[String]]) {
        let bindingNames = try generateNamedItems(namedItems, in: production)
        let stmt = try GuardStatement.guard(clauses: bindingNames.0, else: [elseBlock()])

        return (stmt, bindingNames.1)
    }

    fileprivate struct BodyGenContext {
        var currentArrayName: String
        var markerName: String
        var cutFlagName: String
    }

    struct RepetitionBodyGenInfo {
        /// The production that triggered this repetition's generation.
        var production: RemainingProduction

        var ruleInfo: AuxiliaryRuleInformation
        /// The atom the repetition is attached to.
        var repetitionAtom: InternalGrammar.Atom
        var repetitionInfo: RepetitionInfo
        var failReturnExpression: String
        var _failReturnExpression: Expression {
            .unknown(.init(context: failReturnExpression))
        }

        /// Storage type for each repetition item produced.
        var repetitionAtomType: CommonAbstract.SwiftType
        /// Convenience for `repetitionInfo.trail`
        var trailItems: [InternalGrammar.NamedItem] {
            repetitionInfo.trail
        }

        func successExpression(
            repetitionVariable: String = "_current",
            codeGen: SwiftCodeGen
        ) -> String {

            var elements = ruleInfo.bindings.map { element in
                (label: element.label ?? "_", identifier: element.label ?? "_")
            }
            elements[0].identifier = repetitionVariable

            return codeGen.defaultReturnAction(for: elements).string.trimmingWhitespace()
        }

        func _successExpression(
            repetitionVariable: String = "_current",
            codeGen: SwiftCodeGen
        ) -> Expression {

            var elements = ruleInfo.bindings.map { element in
                (label: element.label ?? "_", identifier: element.label ?? "_")
            }
            elements[0].identifier = repetitionVariable

            return .unknown(.init(context: codeGen.defaultReturnAction(for: elements).string))
        }
    }
}

private extension Sequence where Element == String {
    /// Returns a tuple expression sequence with this sequence of string elements,
    /// where the labels are all `nil`.
    func scgr_asTupleExpr() -> [(label: String?, identifier: String)] {
        map {
            (label: nil, identifier: $0)
        }
    }

    /// Returns a tuple expression string with this sequence of elements, flattening
    /// the tuple to a single non-parenthesized element if the tuple consists of
    /// a single element.
    func scgr_asTupleExprString() -> String {
        let contents = Array(self)
        if contents.count == 1 {
            return contents[0]
        }

        return "(\(contents.joined(separator: ", ")))"
    }
}

private extension CommonAbstract.SwiftType {
    func scgr_flattened() -> Self {
        switch self {
        case .tuple(let elements) where elements.count == 1:
            return elements[0].swiftType.scgr_flattened()
        default:
            return self
        }
    }
}
