import SwiftAST

extension SwiftCodeGen {
    // MARK: Backtrack marker

    /// Generates a marker declaration in the buffer, and defines it in `declContext`.
    ///
    /// Returns the deduplicated name of the declaration for further referencing.
    ///
    /// ```
    /// let markVarName = self.mark()
    /// ```
    func generateMarkDeclaration(markVarName: String = "_mark") -> (Statement, String) {
        let markVar = declContext.defineLocal(
            suggestedName: markVarName,
            type: .marker
        )

        let stmt: Statement = .variableDeclaration(
            identifier: markVar.name,
            type: "Mark",
            isConstant: true,
            initialization: .identifier("self").dot("mark").call()
        )

        return (stmt, markVar.name)
    }

    /// Generates a marker restore into the buffer.
    ///
    /// ```
    /// self.restore(markVarName)
    /// ```
    func generateMarkRestore(markVarName: String) -> Statement {
        return .expression(
            .identifier("self").dot("restore").call([.identifier(markVarName)])
        )
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
    func generateCutFlagDeclaration(cutVarName: String = "_cut") -> (Statement, String) {
        let cutVar = declContext.defineLocal(
            suggestedName: cutVarName,
            type: .cutFlag
        )

        let stmt: Statement = .variableDeclaration(
            identifier: cutVar.name,
            type: "CutFlag",
            isConstant: false,
            initialization: .identifier("CutFlag").call()
        )

        return (stmt, cutVar.name)
    }

    /// Generates a cut toggle expression in the buffer, using the most recent
    /// cut flag declared in the declaration context.
    ///
    /// - precondition: `declContext.declaration(typed: .cutFlag) != nil`
    func generateCutFlagToggleExpression() -> Expression {
        guard let cutFlag = declContext.declaration(typed: .cutFlag) else {
            fatalError("\(#function): No cut flag declaration found in declarations context")
        }

        return generateCutFlagToggleExpression(cutVarName: cutFlag.name)
    }

    /// Generates a cut toggle expression in the buffer.
    func generateCutFlagToggleExpression(cutVarName: String) -> Expression {
        .identifier(cutVarName).dot("toggleOn").call()
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
        _ failBlock: () throws -> [Statement]
    ) rethrows -> Statement {
        let bailExpr = generateCutFlagBailExpression(cutVarName: cutVarName)

        return .if(bailExpr, body: .init(statements: try failBlock()))
    }

    /// Generates a cut flag check expression in the buffer.
    ///
    /// `cutVarName.isOn()`
    func generateCutFlagBailExpression(cutVarName: String) -> Expression {
        return .identifier(cutVarName).dot("isOn")
    }
}
