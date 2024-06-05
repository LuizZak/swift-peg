extension SwiftCodeGen {
    /// Produces a zero-or-more minimal (`*<`) repetition parsing method.
    func generateZeroOrMoreMinimalBody(_ info: RepetitionBodyGenInfo) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scg_asValidSwiftType()

        buffer.emitLine("var \(currentArray): [\(repetitionItemType)] = []")

        buffer.ensureDoubleNewline()
        try buffer.emitBlock("while true") {
            declContext.push()
            defer { declContext.pop() }

            buffer.emitLine("let mark = self.mark()")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            let trailItem = info.trailItem
            let trailBindings = info.trailBindings(bindingEngine)
            try _generateIfLet(item: trailItem, bindings: trailBindings, in: info.production) { (bindingNames) in
                let bindingNames = [currentArray] + bindingNames

                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())
                buffer.emitLine("return \(expr)")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("self.restore(mark)")

            // if let <next> = <repetition>()
            //   ...
            buffer.ensureDoubleNewline()
            buffer.emitLine("// Collect an extra item and try again")
            buffer.emitLine("if")
            declContext.push()
            var nextBindings: [String] = []
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
            buffer.emitInlinedBlock {
                let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())

                buffer.emitLine("\(currentArray).append(\(expr))")

                declContext.pop()
            }
            buffer.emitBlock(" else ") {
                buffer.emit("break")
            }
        }

        buffer.ensureDoubleNewline()
        buffer.emit("return \(info.failReturnExpression)")
    }

    /// Produces a zero-or-more maximal (`*>`) repetition parsing method.
    func generateZeroOrMoreMaximalBody(_ info: RepetitionBodyGenInfo) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scgr_flattened()
        let arrayElementType = CommonAbstract.SwiftType.tuple([
            .unlabeled("Mark"),
            .unlabeled(repetitionItemType),
        ])

        // Initial element capturing
        buffer.emitLine("let _mark = self.mark()")
        buffer.ensureDoubleNewline()
        buffer.emitLine("// Start by fetching as many productions as possible")
        buffer.emitLine("guard")
        try buffer.indented {
            buffer.emit("var \(currentArray): [\(arrayElementType)] = ")
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
        }
        buffer.emitBlock("else ") {
            buffer.emitLine("return \(info.failReturnExpression)")
        }

        // Main while loop

        buffer.ensureDoubleNewline()
        try buffer.emitBlock("while true") {
            declContext.push()
            defer { declContext.pop() }

            buffer.emitLine("let _endMark = \(currentArray).last?.0 ?? _mark")
            buffer.emitLine("self.restore(_endMark)")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            let trailItem = info.trailItem
            let trailBindings = info.trailBindings(bindingEngine)
            try _generateIfLet(item: trailItem, bindings: trailBindings, in: info.production) { (bindingNames) in
                let bindingNames = ["\(currentArray).map(\\.1)"] + bindingNames

                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())
                buffer.emitLine("return \(expr)")
            }
            buffer.backtrackWhitespace()
            buffer.emitBlock(" else if \(currentArray).isEmpty ") {
                buffer.emit("return \(info.failReturnExpression)")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("// Drop an item, backtrack the parser, and try again")
            buffer.emitLine("\(currentArray).removeLast()")
        }

        buffer.ensureDoubleNewline()
        buffer.emit("return \(info.failReturnExpression)")
    }

    /// Produces a one-or-more minimal (`+<`) repetition parsing method.
    func generateOneOrMoreMinimalBody(_ info: RepetitionBodyGenInfo) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scg_asValidSwiftType()

        buffer.emitLine("var \(currentArray): [\(repetitionItemType)] = []")

        buffer.ensureDoubleNewline()
        buffer.emitLine("while")
        declContext.push()

        var nextBindings: [String] = []
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
        try buffer.emitBlock {
            defer { declContext.pop() }

            let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())
            buffer.emitLine("\(currentArray).append(\(expr))")
            buffer.emitLine("let _mark = self.mark()")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            let trailItem = info.trailItem
            let trailBindings = info.trailBindings(bindingEngine)
            try _generateIfLet(item: trailItem, bindings: trailBindings, in: info.production) { (bindingNames) in
                let bindingNames = [currentArray] + bindingNames

                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())
                buffer.emitLine("return \(expr)")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("self.restore(_mark)")
        }

        buffer.ensureDoubleNewline()
        buffer.emit("return \(info.failReturnExpression)")
    }

    /// Produces a one-or-more maximal (`+>`) repetition parsing method.
    func generateOneOrMoreMaximalBody(_ info: RepetitionBodyGenInfo) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scgr_flattened()
        let arrayElementType = CommonAbstract.SwiftType.tuple([
            .unlabeled("Mark"),
            .unlabeled(repetitionItemType),
        ])

        // Initial element capturing

        buffer.emitLine("// Start by fetching as many productions as possible")
        buffer.emitLine("guard")
        try buffer.indented {
            buffer.emit("var \(currentArray): [\(arrayElementType)] = ")
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
        }
        buffer.emitBlock("else ") {
            buffer.emitLine("return \(info.failReturnExpression)")
        }

        // Main while loop

        buffer.ensureDoubleNewline()
        try buffer.emitBlock("while let _end = \(currentArray).last") {
            declContext.push()
            defer { declContext.pop() }

            buffer.emitLine("self.restore(_end.0)")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            let trailItem = info.trailItem
            let trailBindings = info.trailBindings(bindingEngine)
            try _generateIfLet(item: trailItem, bindings: trailBindings, in: info.production) { (bindingNames) in
                let bindingNames = ["\(currentArray).map(\\.1)"] + bindingNames

                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())
                buffer.emitLine("return \(expr)")
            }

            buffer.backtrackWhitespace()
            buffer.emitBlock(" else if \(currentArray).count <= 1 ") {
                buffer.emit("return \(info.failReturnExpression)")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("// Drop an item, backtrack the parser, and try again")
            buffer.emitLine("\(currentArray).removeLast()")
        }

        buffer.ensureDoubleNewline()
        buffer.emit("return \(info.failReturnExpression)")
    }

    /// Produces a gather minimal (`<sep>.<node>+<`) repetition parsing method.
    func generateGatherMinimalBody(
        separator: InternalGrammar.Atom,
        node: InternalGrammar.Atom,
        _ info: RepetitionBodyGenInfo
    ) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scg_asValidSwiftType()

        buffer.emitLine("var \(currentArray): [\(repetitionItemType)] = []")

        buffer.ensureDoubleNewline()
        buffer.emitLine("while")
        declContext.push()

        var nextBindings: [String] = []
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
        try buffer.emitBlock {
            defer { declContext.pop() }

            let expr = defaultReturnExpression(for: nextBindings.scgr_asTupleExpr())
            buffer.emitLine("\(currentArray).append(\(expr))")
            buffer.emitLine("let _mark = self.mark()")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            //try _generateSuccessIfLet(info, leftExpression: currentArray)
            let trailItem = info.trailItem
            let trailBindings = info.trailBindings(bindingEngine)
            try _generateIfLet(item: trailItem, bindings: trailBindings, in: info.production) { (bindingNames) in
                let bindingNames = [currentArray] + bindingNames

                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())
                buffer.emitLine("return \(expr)")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("self.restore(_mark)")

            buffer.ensureDoubleNewline()
            buffer.emitLine("// Try separator before next item")
            let bindings = bindingEngine.bindings(for: separator).be_unlabeled()
            _=try _generateGuardBinding(item: .atom(separator), bindings: bindings, in: info.production) {
                buffer.emitLine("break")
            }
        }

        buffer.ensureDoubleNewline()
        buffer.emit("return \(info.failReturnExpression)")
    }

    /// Produces a gather maximal (`<sep>.<node>+>`) repetition parsing method.
    func generateGatherMaximalBody(
        separator: InternalGrammar.Atom,
        node: InternalGrammar.Atom,
        _ info: RepetitionBodyGenInfo
    ) throws {
        let currentArray = "_current"
        let repetitionItemType = info.repetitionAtomType.be_unwrapped().scgr_flattened()
        let arrayElementType = CommonAbstract.SwiftType.tuple([
            .unlabeled("Mark"),
            .unlabeled(repetitionItemType),
        ])

        // Initial element capturing

        buffer.emitLine("// Start by fetching as many productions as possible")
        buffer.emitLine("guard")
        try buffer.indented {
            buffer.emit("var \(currentArray): [\(arrayElementType)] = ")
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
        }
        buffer.emitBlock("else ") {
            buffer.emitLine("return \(info.failReturnExpression)")
        }

        // Main while loop

        buffer.ensureDoubleNewline()
        try buffer.emitBlock("while let _end = \(currentArray).last") {
            declContext.push()
            defer { declContext.pop() }

            buffer.emitLine("self.restore(_end.0)")

            // if let <trail> = <trail>()
            //   ...
            buffer.ensureDoubleNewline()
            let trailItem = info.trailItem
            let trailBindings = info.trailBindings(bindingEngine)
            try _generateIfLet(item: trailItem, bindings: trailBindings, in: info.production) { (bindingNames) in
                let bindingNames = ["\(currentArray).map(\\.1)"] + bindingNames

                let expr = defaultReturnExpression(for: bindingNames.scgr_asTupleExpr())
                buffer.emitLine("return \(expr)")
            }

            buffer.backtrackWhitespace()
            buffer.emitBlock(" else if \(currentArray).count <= 1 ") {
                buffer.emit("return \(info.failReturnExpression)")
            }

            buffer.ensureDoubleNewline()
            buffer.emitLine("// Drop an item, backtrack the parser, and try again")
            buffer.emitLine("\(currentArray).removeLast()")
        }

        buffer.ensureDoubleNewline()
        buffer.emit("return \(info.failReturnExpression)")
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

    struct RepetitionBodyGenInfo {
        /// The production that triggered this repetition's generation.
        var production: RemainingProduction

        var ruleInfo: AuxiliaryRuleInformation
        /// The atom the repetition is attached to.
        var repetitionAtom: InternalGrammar.Atom
        var repetitionInfo: RepetitionInfo
        var trailName: String
        var trailInfo: AuxiliaryRuleInformation
        var failReturnExpression: String

        /// Storage type for each repetition item produced.
        var repetitionAtomType: CommonAbstract.SwiftType
        /// Storage type for the trailing of the repetition.
        var trailType: CommonAbstract.SwiftType
        /// The full type produced when emitting the successful result of a
        /// non-standard repetition operation.
        var fullType: CommonAbstract.SwiftType

        /// Convenience for `repetitionAtomType.be_unwrapped().scg_asValidSwiftType()`
        var repetitionAtomTypeString: String {
            repetitionAtomType.be_unwrapped().scg_asValidSwiftType()
        }
        /// Convenience for `trailType.scg_asValidSwiftType()`
        var trailTypeString: String {
            trailType.be_unwrapped().scg_asValidSwiftType()
        }
        /// Convenience for `fullType.scg_asValidSwiftType()`
        var fullTypeString: String {
            fullType.be_unwrapped().scg_asValidSwiftType()
        }

        /// Convenience for `.atom(.ruleName(trailName))`.
        var trailItem: InternalGrammar.Item {
            .atom(.ruleName(trailName))
        }

        func trailBindings(_ bindingEngine: BindingEngine) -> [BindingEngine.Binding] {
            if trailInfo.bindings.isEmpty {
                return bindingEngine.bindings(for: trailItem)
            }
            if trailInfo.bindings.count == 1 && trailInfo.bindings[0].label == nil {
                return bindingEngine.bindings(for: trailItem)
            }

            return trailInfo.bindings
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

        func trailExpression(codeGen: SwiftCodeGen) -> String {
            codeGen.defaultReturnAction(for: trailInfo.bindings).string.trimmingWhitespace()
        }
    }
}

private extension Sequence where Element == String {
    /// Returns a tuple expression seque ce with this sequence of string elements,
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
