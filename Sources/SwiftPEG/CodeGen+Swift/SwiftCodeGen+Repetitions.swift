extension SwiftCodeGen {
    /// Produces a zero-or-more minimal (`*<`) repetition parsing method.
    func generateZeroOrMoreMinimalBody(_ info: RepetitionBodyGenInfo) throws {
        try _generateMinimalBody(info) { _ in
            buffer.emit(" true")
        } whileTail: { ctx in
            buffer.ensureDoubleNewline()
            buffer.emitLine("// Collect an extra item and try again")
            let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()
            try _generateIfLet(item: .atom(info.repetitionAtom), bindings: bindings, in: info.production) { bindingNames in
                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())

                buffer.emitLine("\(ctx.currentArrayName).append(\(expr))")
            }
            buffer.backtrackWhitespace()
            buffer.emitBlock(" else ") {
                buffer.emit("break")
            }
        }
    }

    /// Produces a zero-or-more maximal (`*>`) repetition parsing method.
    func generateZeroOrMoreMaximalBody(_ info: RepetitionBodyGenInfo) throws {
        try _generateMaximalBody(info) { _ in
            buffer.emit("try self.repeatZeroOrMore(")
            try buffer.emitInlinedBlock {
                declContext.push()
                defer { declContext.pop() }

                let item = InternalGrammar.Item.atom(info.repetitionAtom)
                let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()

                buffer.emit("if ")
                let bound = try generateBindingsToItem(item, bindings, in: info.production)
                let mapTupleExpr = (["self.mark()", bound.scgr_asTupleExprString()]).scgr_asTupleExpr()
                buffer.emitLine(" { return \(self.defaultReturnExpression(for: mapTupleExpr)) }")
                buffer.emitLine("return nil")
            }
            buffer.emitLine(")")
        } whileCondition: { ctx in
            buffer.emit(" true ")
        } whileHead: { ctx in
            buffer.emitLine("let _endMark = \(ctx.currentArrayName).last?.0 ?? \(ctx.markerName)")
        } stopCondition: { ctx in
            buffer.emit(" \(ctx.currentArrayName).isEmpty ")
        }
    }

    /// Produces a one-or-more minimal (`+<`) repetition parsing method.
    func generateOneOrMoreMinimalBody(_ info: RepetitionBodyGenInfo) throws {
        var nextBindings: [String] = []
        try _generateMinimalBody(info) { _ in
            buffer.ensureNewline()
            try buffer.indented {
                let item = InternalGrammar.Item.atom(info.repetitionAtom)
                let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()

                nextBindings.append(contentsOf:
                    try generateBindingsToItem(
                        item,
                        bindings,
                        in: info.production
                    )
                )
            }
            buffer.ensureNewline()
        } whileHead: { ctx in
            let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())
            buffer.emitLine("\(ctx.currentArrayName).append(\(expr))")
        }
    }

    /// Produces a one-or-more maximal (`+>`) repetition parsing method.
    func generateOneOrMoreMaximalBody(_ info: RepetitionBodyGenInfo) throws {
        try _generateMaximalBody(info) { _ in
            buffer.emit("try self.repeatOneOrMore(")
            try buffer.emitInlinedBlock {
                declContext.push()
                defer { declContext.pop() }

                let item = InternalGrammar.Item.atom(info.repetitionAtom)
                let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()

                buffer.emit("if ")
                let bound = try generateBindingsToItem(item, bindings, in: info.production)
                let mapTupleExpr = (["self.mark()", bound.scgr_asTupleExprString()]).scgr_asTupleExpr()
                buffer.emitLine(" { return \(self.defaultReturnExpression(for: mapTupleExpr)) }")
                buffer.emitLine("return nil")
            }
            buffer.emitLine(")")
        } whileCondition: { ctx in
            buffer.emit(" let _endMark = \(ctx.currentArrayName).last?.0 ")
        } stopCondition: { ctx in
            buffer.emit(" \(ctx.currentArrayName).count <= 1 ")
        }
    }

    /// Produces a gather minimal (`<sep>.<node>+<`) repetition parsing method.
    func generateGatherMinimalBody(
        separator: InternalGrammar.Atom,
        node: InternalGrammar.Atom,
        _ info: RepetitionBodyGenInfo
    ) throws {
        var nextBindings: [String] = []
        try _generateMinimalBody(info) { _ in
            buffer.ensureNewline()
            try buffer.indented {
                let item = InternalGrammar.Item.atom(info.repetitionAtom)
                let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()

                nextBindings.append(contentsOf:
                    try generateBindingsToItem(
                        item,
                        bindings,
                        in: info.production
                    )
                )
            }
            buffer.ensureNewline()
        } whileHead: { ctx in
            let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())
            buffer.emitLine("\(ctx.currentArrayName).append(\(expr))")
        } whileTail: { ctx in
            buffer.ensureDoubleNewline()
            buffer.emitLine("// Try separator before next item")
            let bindings = bindingEngine.bindings(for: separator).be_unlabeled()
            _=try _generateGuardBinding(item: .atom(separator), bindings: bindings, in: info.production) {
                buffer.emitLine("break")
            }
        }
    }

    /// Produces a gather maximal (`<sep>.<node>+>`) repetition parsing method.
    func generateGatherMaximalBody(
        separator: InternalGrammar.Atom,
        node: InternalGrammar.Atom,
        _ info: RepetitionBodyGenInfo
    ) throws {
        try _generateMaximalBody(info) { _ in
            buffer.emit("try self.gather(separator: ")
            try buffer.emitInlinedBlock {
                declContext.push()
                defer { declContext.pop() }

                try generateAtom(separator, in: info.production)
            }
            buffer.emit(", item: ")
            try buffer.emitInlinedBlock {
                declContext.push()
                defer { declContext.pop() }

                let item = InternalGrammar.Item.atom(info.repetitionAtom)
                let bindings = bindingEngine.bindings(for: info.repetitionAtom).be_unwrapped()

                buffer.emit("if ")
                let bound = try generateBindingsToItem(item, bindings, in: info.production)
                let mapTupleExpr = (["self.mark()", bound.scgr_asTupleExprString()]).scgr_asTupleExpr()
                buffer.emitLine(" { return \(self.defaultReturnExpression(for: mapTupleExpr)) }")
                buffer.emitLine("return nil")
            }
            buffer.emitLine(")")
        } whileCondition: { ctx in
            buffer.emit(" let _endMark = \(ctx.currentArrayName).last?.0 ")
        } stopCondition: { ctx in
            buffer.emit(" \(ctx.currentArrayName).count <= 1 ")
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
        whileCondition: (BodyGenContext) throws -> Void,
        whileHead: (BodyGenContext) throws -> Void = { _ in },
        whileTail: (BodyGenContext) throws -> Void = { _ in }
    ) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scg_asValidSwiftType()

        // Start of method body
        let markerName = generateMarkDeclaration()

        var cutFlagName = "_cut"
        let requiresCut = requiresCutFlag(info.trailItems)
        if requiresCut {
            cutFlagName = generateCutFlagDeclaration()
        }

        let ctx = BodyGenContext(
            currentArrayName: currentArray,
            markerName: markerName,
            cutFlagName: cutFlagName
        )

        buffer.ensureDoubleNewline()
        buffer.emitLine("var \(currentArray): [\(repetitionItemType)] = []")

        buffer.ensureDoubleNewline()
        buffer.emit("while")
        declContext.push()

        if requiresCut {
            buffer.emit(" !")
            generateCutFlagBailExpression(cutVarName: ctx.cutFlagName)
            buffer.emit(",")
        }

        try whileCondition(ctx)

        buffer.ensureSpaceSeparator()
        try buffer.emitBlock {
            defer { declContext.pop() }
            try whileHead(ctx)

            let whileMarker = generateMarkDeclaration()

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            try _generateSuccessBlock(info, leadItemExpr: currentArray)

            buffer.ensureDoubleNewline()
            generateMarkRestore(markVarName: whileMarker)

            try whileTail(ctx)
        }

        buffer.ensureDoubleNewline()
        generateMarkRestore(markVarName: markerName)
        buffer.emit("return \(info.failReturnExpression)")
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
        initialProduction: (BodyGenContext) throws -> Void,
        whileCondition: (BodyGenContext) throws -> Void,
        whileHead: (BodyGenContext) throws -> Void = { _ in },
        stopCondition: (BodyGenContext) throws -> Void
    ) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scgr_flattened()
        let arrayElementType = CommonAbstract.SwiftType.tuple([
            .unlabeled("Mark"),
            .unlabeled(repetitionItemType),
        ])

        // Start of method body
        let conditional = buffer.startConditionalEmitter()

        let markerName = generateMarkDeclaration()

        var cutFlagName = "_cut"
        let requiresCut = requiresCutFlag(info.trailItems)
        if requiresCut {
            cutFlagName = generateCutFlagDeclaration()
        }

        let ctx = BodyGenContext(
            currentArrayName: currentArray,
            markerName: markerName,
            cutFlagName: cutFlagName
        )

        conditional.conditional { buffer in
            buffer.ensureDoubleNewline()
        }

        // Initial element capturing
        buffer.emitLine("// Start by fetching as many productions as possible")
        buffer.emitLine("guard")
        try buffer.indented {
            buffer.emit("var \(currentArray): [\(arrayElementType)] = ")
            try initialProduction(ctx)
        }
        buffer.emitBlock("else ") {
            buffer.emitLine("return \(info.failReturnExpression)")
        }

        // Main while loop

        buffer.ensureDoubleNewline()
        buffer.emit("while")
        declContext.push()

        if requiresCut {
            buffer.emit(" !")
            generateCutFlagBailExpression(cutVarName: ctx.cutFlagName)
            buffer.emit(",")
        }

        try whileCondition(ctx)

        try buffer.emitBlock {
            defer { declContext.pop() }
            try whileHead(ctx)
            buffer.emitLine("self.restore(_endMark)")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            try _generateSuccessBlock(info, leadItemExpr: "\(currentArray).map(\\.1)")

            buffer.backtrackWhitespace()
            buffer.emit(" else if")
            try stopCondition(ctx)
            buffer.emitBlock {
                buffer.emit("break")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("// Drop an item, backtrack the parser, and try again")
            buffer.emitLine("\(currentArray).removeLast()")
        }

        buffer.ensureDoubleNewline()
        generateMarkRestore(markVarName: markerName)
        buffer.emit("return \(info.failReturnExpression)")
    }

    /// Generates the main if-let binding that succeeds the repetition parser
    /// method.
    ///
    /// `leadItemExpr` must be an expression that resolves as the first item of
    /// the repetition production's result tuple.
    fileprivate func _generateSuccessBlock(
        _ info: RepetitionBodyGenInfo,
        leadItemExpr: String
    ) throws {
        try _generateIfLet(namedItems: info.trailItems, in: info.production) { (bindingNames) in
            let bindings = [leadItemExpr] + bindingNames.flatMap({ $0 })

            let expr = defaultReturnExpression(for: bindings.scgr_asTupleExpr())
            buffer.emitLine("return \(expr)")
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
        block: ([String]) throws -> ()
    ) throws {
        var bindingNames: [String] = []

        buffer.emitLine("if")
        try buffer.indented {
            declContext.push()
            defer { declContext.pop() }

            bindingNames = try generateBindingsToItem(
                item,
                bindings,
                in: production
            )
        }
        buffer.ensureNewline()
        try buffer.emitBlock {
            try block(bindingNames)
        }
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
        elseBlock: () throws -> ()
    ) throws -> [String] {
        var bindingNames: [String] = []

        buffer.emitLine("guard")
        try buffer.indented {
            bindingNames = try generateBindingsToItem(
                item,
                bindings,
                in: production
            )
        }
        buffer.ensureNewline()
        try buffer.emitBlock("else ", elseBlock)

        return bindingNames
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
        block: ([[String]]) throws -> ()
    ) throws {
        var bindingNames: [[String]] = []

        buffer.emitLine("if")
        try buffer.indented {
            declContext.push()
            defer { declContext.pop() }

            bindingNames = try generateNamedItems(namedItems, in: production)
        }
        buffer.ensureNewline()
        try buffer.emitBlock {
            try block(bindingNames)
        }
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
        elseBlock: () throws -> ()
    ) throws -> [[String]] {
        var bindingNames: [[String]] = []

        buffer.emitLine("guard")
        try buffer.indented {
            bindingNames = try generateNamedItems(namedItems, in: production)
        }
        buffer.ensureNewline()
        try buffer.emitBlock("else ", elseBlock)

        return bindingNames
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
