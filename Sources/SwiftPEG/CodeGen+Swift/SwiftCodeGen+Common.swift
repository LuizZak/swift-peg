extension SwiftCodeGen {
    // MARK: Backtrack marker

    /// Generates a marker declaration in the buffer, and defines it in `declContext`.
    ///
    /// Returns the deduplicated name of the declaration for further referencing.
    ///
    /// ```
    /// let markVarName = self.mark()
    /// ```
    func generateMarkDeclaration(markVarName: String = "_mark") -> String {
        let markVar = declContext.defineLocal(
            suggestedName: markVarName,
            type: .marker
        )

        buffer.emitLine("let \(markVar.name) = self.mark()")

        return markVar.name
    }

    /// Generates a marker restore into the buffer.
    ///
    /// Returns the deduplicated name of the declaration for further referencing.
    ///
    /// ```
    /// self.restore(markVarName)
    /// ```
    func generateMarkRestore(markVarName: String) {
        buffer.emitLine("self.restore(\(markVarName))")
    }

    // MARK: Cut flag

    /// Generates a cut flag declaration in the buffer, and defines it in
    /// `declContext`.
    ///
    /// Returns the deduplicated name of the declaration for further referencing.
    ///
    /// ```
    /// var cutVarName = CutFlag()
    /// ```
    func generateCutFlagDeclaration(cutVarName: String = "_cut") -> String {
        let cutVar = declContext.defineLocal(
            suggestedName: cutVarName,
            type: .cutFlag
        )

        buffer.emitLine("var \(cutVar.name) = CutFlag()")
        return cutVar.name
    }

    /// Generates a cut toggle expression in the buffer, using the most recent
    /// cut flag declared in the declaration context.
    ///
    /// - precondition: `declContext.declaration(typed: .cutFlag) != nil`
    func generateCutFlagToggle() {
        guard let cutFlag = declContext.declaration(typed: .cutFlag) else {
            fatalError("\(#function): No cut flag declaration found in declarations context")
        }

        generateCutFlagToggle(cutVarName: cutFlag.name)
    }

    /// Generates a cut toggle expression in the buffer.
    func generateCutFlagToggle(cutVarName: String) {
        buffer.emit("\(cutVarName).toggleOn()")
    }

    /// Generates a cut flag check if- statement in the buffer, calling a given
    /// block for emitting the fail statements within.
    ///
    /// ```
    /// if cutVarName.isOn() {
    ///     failBlock()
    /// }
    /// ```
    func generateCutFlagBailStatement(
        cutVarName: String,
        _ failBlock: () throws -> Void
    ) rethrows {
        buffer.emit("if ")
        generateCutFlagBailExpression(cutVarName: cutVarName)
        buffer.ensureSpaceSeparator()
        try buffer.emitBlock {
            try failBlock()
        }
    }

    /// Generates a cut flag check expression in the buffer.
    ///
    /// `cutVarName.isOn()`
    func generateCutFlagBailExpression(cutVarName: String) {
        buffer.emit("\(cutVarName).isOn")
    }
}
