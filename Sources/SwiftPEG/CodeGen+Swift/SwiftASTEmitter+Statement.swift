import SwiftAST

// MARK: Statements

extension SwiftASTEmitter: StatementVisitor {
    typealias StmtResult = Void

    func visitStatement(_ statement: Statement) -> Void {
        emit(statement.comments)
        if let label = statement.label {
            buffer.emitLine("\(label):")
        }

        statement.accept(self)
    }

    func visitCompound(_ stmt: CompoundStatement) -> Void {
        buffer.emitLine("{")
        buffer.indented {
            let conditional = buffer.startConditionalEmitter()
            for stmt in stmt {
                conditional.ensureEmptyLine()
                visitStatement(stmt)
            }
        }
        buffer.emitLine("}")
    }

    func visitConditionalClauses(_ clauses: ConditionalClauses) -> Void {
        let isMultilineClauseVisitor = RequiresMultilineConditionalClauseVisitor()

        if isMultilineClauseVisitor.visitConditionalClauses(clauses) {
            buffer.ensureNewline()
            buffer.indented {
                for (i, clause) in clauses.clauses.enumerated() {
                    visitConditionalClauseElement(clause)
                    if i < clauses.clauses.count - 1 {
                        buffer.emit(",")
                    }
                    buffer.ensureNewline()
                }
            }
        } else {
            buffer.ensureSpaceSeparator()
            buffer.emitWithSeparators(clauses.clauses, separator: ", ") { clause in
                visitConditionalClauseElement(clause)
            }
        }
    }

    func visitConditionalClauseElement(_ clause: ConditionalClauseElement) -> Void {
        if clause.isCaseClause {
            buffer.emit("case ")
        }
        if let pattern = clause.pattern {
            visitPattern(pattern)
            buffer.emit(" = ")
        }
        visitExpression(clause.expression)
    }

    func visitGuard(_ stmt: GuardStatement) -> Void {
        buffer.emit("guard")
        visitConditionalClauses(stmt.conditionalClauses)
        buffer.ensureSpaceSeparator()
        buffer.emit("else")
        buffer.ensureSpaceSeparator()
        visitStatement(stmt.elseBody)
    }

    func visitWhile(_ stmt: WhileStatement) -> Void {
        buffer.emit("while")
        visitConditionalClauses(stmt.conditionalClauses)
        buffer.ensureSpaceSeparator()
        visitStatement(stmt.body)
    }

    func visitRepeatWhile(_ stmt: RepeatWhileStatement) -> Void {
        buffer.emit("repeat ")
        visitStatement(stmt.body)
        buffer.backtrackWhitespace()
        buffer.ensureSpaceSeparator()
        buffer.emit("while")
        buffer.ensureSpaceSeparator()
        visitExpression(stmt.exp)
        buffer.ensureNewline()
    }

    func visitFor(_ stmt: ForStatement) -> Void {
        buffer.emit("for ")
        visitPattern(stmt.pattern)
        buffer.emit(" in ")
        visitExpression(stmt.exp)
        if let whereClause = stmt.whereClause {
            buffer.ensureSpaceSeparator()
            buffer.emit("where")
            buffer.ensureSpaceSeparator()
            visitExpression(whereClause)
        }
        buffer.ensureSpaceSeparator()
        visitStatement(stmt.body)
    }

    func visitDo(_ stmt: DoStatement) -> Void {
        buffer.emit("do ")
        visitStatement(stmt.body)

        for catchBlock in stmt.catchBlocks {
            visitCatchBlock(catchBlock)
        }
    }

    func visitCatchBlock(_ block: CatchBlock) -> Void {
        buffer.backtrackWhitespace()
        buffer.emit(" catch ")
        if let pattern = block.pattern {
            visitPattern(pattern)
        }
        buffer.ensureSpaceSeparator()
        visitStatement(block.body)
    }

    func visitDefer(_ stmt: DeferStatement) -> Void {
        buffer.emit("defer ")
        visitStatement(stmt.body)
    }

    func visitReturn(_ stmt: ReturnStatement) -> Void {
        buffer.emit("return")

        if let exp = stmt.exp {
            buffer.ensureSpaceSeparator()
            visitExpression(exp)
        }

        buffer.ensureNewline()
    }

    func visitBreak(_ stmt: BreakStatement) -> Void {
        buffer.emit("break")
        if let label = stmt.targetLabel {
            buffer.emit(" \(label)")
        }

        buffer.ensureNewline()
    }

    func visitFallthrough(_ stmt: FallthroughStatement) -> Void {
        buffer.emit("fallthrough")
        buffer.ensureNewline()
    }

    func visitContinue(_ stmt: ContinueStatement) -> Void {
        buffer.emit("continue")
        if let label = stmt.targetLabel {
            buffer.emit(" \(label)")
        }

        buffer.ensureNewline()
    }

    func visitExpressions(_ stmt: ExpressionsStatement) -> Void {
        for exp in stmt.expressions {
            visitExpression(exp)
            buffer.ensureNewline()
        }
    }

    func visitVariableDeclarations(_ stmt: VariableDeclarationsStatement) -> Void {
        for decl in stmt.decl {
            visitStatementVariableDeclaration(decl)
            buffer.ensureNewline()
        }
    }

    func visitStatementVariableDeclaration(_ decl: StatementVariableDeclaration) -> Void {
        if decl.isConstant {
            buffer.emit("let ")
        } else {
            buffer.emit("var ")
        }

        buffer.emit(decl.identifier)
        buffer.emit(": ")
        emit(decl.type)

        if let initialization = decl.initialization {
            buffer.emit(" = ")
            visitExpression(initialization)
        }
        buffer.ensureNewline()
    }

    func visitLocalFunction(_ stmt: LocalFunctionStatement) -> Void {
        let function = stmt.function

        buffer.emit("func ")
        buffer.emit(function.identifier)
        buffer.emit("(")
        buffer.emitWithSeparators(function.parameters, separator: ", ") { parameter in
            // TODO: Emit this fully inlined with SwiftASTEmitter instead
            buffer.emit(parameter.description)
        }
        buffer.emit(")")
        buffer.emit(" -> ")
        emit(function.returnType)

        visitStatement(function.body)
    }

    func visitThrow(_ stmt: ThrowStatement) -> Void {
        buffer.emit("throw ")
        visitExpression(stmt.exp)
        buffer.ensureNewline()
    }

    func visitUnknown(_ stmt: UnknownStatement) -> Void {
        buffer.emitLine(stmt.context.description)
    }
}

/// Returns `true` if the given expression requires multiline conditional clauses
/// in `if`, `guard`, and `while` statements.
private class RequiresMultilineConditionalClauseVisitor: ExpressionVisitor {
    typealias ExprResult = Bool

    func visitConditionalClauses(_ clauses: ConditionalClauses) -> Bool {
        if clauses.clauses.count > 1 {
            return true
        }

        for clause in clauses.clauses {
            if visitExpression(clause.expression) {
                return true
            }
        }

        return false
    }

    func visitExpression(_ expression: Expression) -> Bool {
        expression.accept(self)
    }

    func visitAssignment(_ exp: AssignmentExpression) -> Bool {
        visitExpression(exp.lhs) || visitExpression(exp.rhs)
    }

    func visitBinary(_ exp: BinaryExpression) -> Bool {
        visitExpression(exp.lhs) || visitExpression(exp.rhs)
    }

    func visitUnary(_ exp: UnaryExpression) -> Bool {
        visitExpression(exp.exp)
    }

    func visitSizeOf(_ exp: SizeOfExpression) -> Bool {
        switch exp.value {
        case .expression(let exp):
            return visitExpression(exp)

        case .type:
            return false
        }
    }

    func visitPrefix(_ exp: PrefixExpression) -> Bool {
        visitExpression(exp.exp)
    }

    func visitPostfix(_ exp: PostfixExpression) -> Bool {
        if visitExpression(exp.exp) {
            return true
        }

        switch exp.op {
        case is MemberPostfix:
            return false

        case let op as FunctionCallPostfix:
            for arg in op.arguments {
                if visitExpression(arg.expression) {
                    return true
                }
            }

            return false

        case let op as SubscriptPostfix:
            for arg in op.arguments {
                if visitExpression(arg.expression) {
                    return true
                }
            }

            return false

        default:
            return false
        }
    }

    func visitConstant(_ exp: ConstantExpression) -> Bool {
        false
    }

    func visitParens(_ exp: ParensExpression) -> Bool {
        visitExpression(exp.exp)
    }

    func visitIdentifier(_ exp: IdentifierExpression) -> Bool {
        false
    }

    func visitImplicitMember(_ exp: ImplicitMemberExpression) -> Bool {
        false
    }

    func visitCast(_ exp: CastExpression) -> Bool {
        visitExpression(exp.exp)
    }

    func visitTypeCheck(_ exp: TypeCheckExpression) -> Bool {
        visitExpression(exp.exp)
    }

    func visitArray(_ exp: ArrayLiteralExpression) -> Bool {
        for element in exp.items {
            if visitExpression(element) {
                return true
            }
        }

        return false
    }

    func visitDictionary(_ exp: DictionaryLiteralExpression) -> Bool {
        for pair in exp.pairs {
            if visitExpression(pair.key) || visitExpression(pair.value) {
                return true
            }
        }

        return false
    }

    func visitBlock(_ exp: BlockLiteralExpression) -> Bool {
        true
    }

    func visitTernary(_ exp: TernaryExpression) -> Bool {
        visitExpression(exp.exp) || visitExpression(exp.ifTrue) || visitExpression(exp.ifFalse)
    }

    func visitTuple(_ exp: TupleExpression) -> Bool {
        for element in exp.elements {
            if visitExpression(element.exp) {
                return true
            }
        }

        return false
    }

    func visitSelector(_ exp: SelectorExpression) -> Bool {
        false
    }

    func visitTry(_ exp: TryExpression) -> Bool {
        visitExpression(exp.exp)
    }

    func visitIf(_ exp: IfExpression) -> Bool {
        true
    }

    func visitElseBody(_ exp: IfExpression.ElseBody) -> Bool {
        true
    }

    func visitSwitch(_ exp: SwitchExpression) -> Bool {
        true
    }

    func visitSwitchCase(_ switchCase: SwitchCase) -> Bool {
        true
    }

    func visitSwitchCasePattern(_ casePattern: SwitchCase.CasePattern) -> Bool {
        true
    }

    func visitSwitchDefaultCase(_ defaultCase: SwitchDefaultCase) -> Bool {
        true
    }

    func visitUnknown(_ exp: UnknownExpression) -> Bool {
        exp.context.context.trimmingWhitespace().contains("\n")
    }

    func visitPattern(_ pattern: Pattern) -> Bool {
        switch pattern {
        case .asType(let inner, _):
            return visitPattern(inner)

        case .expression(let exp):
            return visitExpression(exp)

        case .identifier(_, _):
            return false

        case .optional(let inner):
            return visitPattern(inner)

        case .tuple(let elements, _):
            for element in elements {
                if visitPattern(element) {
                    return true
                }
            }
            return false

        case .valueBindingPattern(_, let inner):
            return visitPattern(inner)

        case .wildcard:
            return false
        }
    }
}
