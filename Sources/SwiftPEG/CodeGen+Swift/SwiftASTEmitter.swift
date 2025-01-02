import SwiftAST

class SwiftASTEmitter {
    let buffer: CodeStringBuffer = CodeStringBuffer()

    func finishBuffer(addTrailingNewline: Bool = false) -> String {
        buffer.finishBuffer(addTrailingNewline: addTrailingNewline)
    }
}

// MARK: Statements

extension SwiftASTEmitter: StatementVisitor {
    typealias StmtResult = Void

    func visitStatement(_ statement: Statement) -> Void {
        if let label = statement.label {
            buffer.emitLine("\(label):")
        }

        statement.accept(self)
    }

    func visitCompound(_ stmt: CompoundStatement) -> Void {
        buffer.emitLine("{")
        buffer.indented {
            for stmt in stmt {
                visitStatement(stmt)
            }
        }
        buffer.emitLine("}")
    }

    func visitConditionalClauses(_ clauses: ConditionalClauses) -> Void {
        buffer.emitWithSeparators(clauses.clauses, separator: ", ") { clause in
            visitConditionalClauseElement(clause)
        }
    }

    func visitConditionalClauseElement(_ clause: ConditionalClauseElement) -> Void {
        if clause.isCaseClause {
            buffer.emit("case ")
        }
        if let pattern = clause.pattern {
            visitPattern(pattern)
        }
        visitExpression(clause.expression)
    }

    func visitGuard(_ stmt: GuardStatement) -> Void {
        buffer.emit("guard ")
        visitConditionalClauses(stmt.conditionalClauses)
        buffer.emit(" else ")
        visitStatement(stmt.elseBody)
    }

    func visitWhile(_ stmt: WhileStatement) -> Void {
        buffer.emit("while ")
        visitConditionalClauses(stmt.conditionalClauses)
        visitStatement(stmt.body)
    }

    func visitRepeatWhile(_ stmt: RepeatWhileStatement) -> Void {
        buffer.emit("repeat ")
        visitStatement(stmt.body)
        buffer.backtrackWhitespace()
        buffer.emit(" while ")
        visitExpression(stmt.exp)
    }

    func visitFor(_ stmt: ForStatement) -> Void {
        buffer.emit("for ")
        visitPattern(stmt.pattern)
        buffer.emit(" in ")
        visitExpression(stmt.exp)
        if let whereClause = stmt.whereClause {
            buffer.emit(" where ")
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
    }

    func visitBreak(_ stmt: BreakStatement) -> Void {
        buffer.emit("break")

        if let label = stmt.targetLabel {
            buffer.emit(" \(label)")
        }
    }

    func visitFallthrough(_ stmt: FallthroughStatement) -> Void {
        buffer.emit("fallthrough")
    }

    func visitContinue(_ stmt: ContinueStatement) -> Void {
        buffer.emit("continue")

        if let label = stmt.targetLabel {
            buffer.emit(" \(label)")
        }
    }

    func visitExpressions(_ stmt: ExpressionsStatement) -> Void {
        for exp in stmt.expressions {
            visitExpression(exp)
            buffer.emitLine("")
        }
    }

    func visitVariableDeclarations(_ stmt: VariableDeclarationsStatement) -> Void {
        for decl in stmt.decl {
            visitStatementVariableDeclaration(decl)
            buffer.emitLine("")
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
        emitType(decl.type)

        if let initialization = decl.initialization {
            buffer.emit(" = ")
            visitExpression(initialization)
        }
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
        emitType(function.returnType)

        visitStatement(function.body)
    }

    func visitThrow(_ stmt: ThrowStatement) -> Void {
        buffer.emit("throw ")
        visitExpression(stmt.exp)
    }

    func visitUnknown(_ stmt: UnknownStatement) -> Void {
        buffer.emit(stmt.context.description)
    }
}

// MARK: Expressions

extension SwiftASTEmitter: ExpressionVisitor {
    typealias ExprResult = Void

    func visitExpression(_ expression: Expression) -> Void {
        expression.accept(self)
    }

    func visitAssignment(_ exp: AssignmentExpression) -> Void {
        visitExpression(exp.lhs)
        buffer.emit(" \(exp.op) ")
        visitExpression(exp.rhs)
    }

    func visitBinary(_ exp: BinaryExpression) -> Void {
        visitExpression(exp.lhs)
        if exp.op.requiresSpacing {
            buffer.emit(" ")
        }
        buffer.emit("\(exp.op)")
        if exp.op.requiresSpacing {
            buffer.emit(" ")
        }
        visitExpression(exp.rhs)
    }

    func visitUnary(_ exp: UnaryExpression) -> Void {
        buffer.emit(exp.op.description)
        visitExpression(exp.exp)
    }

    func visitSizeOf(_ exp: SizeOfExpression) -> Void {
        switch exp.value {
        case .expression(let exp):
            buffer.emit("MemoryLayout.size(ofValue: ")
            visitExpression(exp)
            buffer.emit(")")

        case .type(let type):
            buffer.emit("MemoryLayout<")
            emitType(type)
            buffer.emit(">.size")
        }
    }

    func visitPrefix(_ exp: PrefixExpression) -> Void {
        buffer.emit(exp.op.description)
        visitExpression(exp.exp)
    }

    func visitPostfix(_ exp: PostfixExpression) -> Void {
        visitExpression(exp.exp)

        switch exp.op.optionalAccessKind {
        case .none:
            break

        case .safeUnwrap:
            buffer.emit("?")

        case .forceUnwrap:
            buffer.emit("!")
        }

        switch exp.op {
        case let op as MemberPostfix:
            buffer.emit(".")
            buffer.emit(op.name)
            if let args = op.argumentNames {
                for arg in args {
                    buffer.emit(arg.description)
                }
            }

        case let op as FunctionCallPostfix:
            buffer.emit("(")
            buffer.emitWithSeparators(op.arguments, separator: ", ") { argument in
                if let label = argument.label {
                    buffer.emit("\(label): ")
                }
                visitExpression(argument.expression)
            }
            buffer.emit(")")

        case let op as SubscriptPostfix:
            buffer.emit("[")
            buffer.emitWithSeparators(op.arguments, separator: ", ") { argument in
                if let label = argument.label {
                    buffer.emit("\(label): ")
                }
                visitExpression(argument.expression)
            }
            buffer.emit("]")

        default:
            break
        }
    }

    func visitConstant(_ exp: ConstantExpression) -> Void {
        buffer.emit(exp.description)
    }

    func visitParens(_ exp: ParensExpression) -> Void {
        buffer.emit("(")
        visitExpression(exp.exp)
        buffer.emit(")")
    }

    func visitIdentifier(_ exp: IdentifierExpression) -> Void {
        buffer.emit(exp.description)
    }

    func visitImplicitMember(_ exp: ImplicitMemberExpression) -> Void {
        buffer.emit(".")
        buffer.emit(exp.identifier)
    }

    func visitCast(_ exp: CastExpression) -> Void {
        visitExpression(exp.exp)

        buffer.emit(" as")
        if exp.isOptionalCast {
            buffer.emit("?")
        }
        buffer.emit(" ")

        emitType(exp.type)
    }

    func visitTypeCheck(_ exp: TypeCheckExpression) -> Void {
        visitExpression(exp)
        buffer.emit(" is ")
        emitType(exp.type)
    }

    func visitArray(_ exp: ArrayLiteralExpression) -> Void {
        buffer.emit("[")
        buffer.emitWithSeparators(exp.items, separator: ", ") { item in
            visitExpression(item)
        }
        buffer.emit("]")
    }

    func visitDictionary(_ exp: DictionaryLiteralExpression) -> Void {
        buffer.emit("[")
        if exp.pairs.isEmpty {
            buffer.emit(":")
        } else {
            buffer.emitWithSeparators(exp.pairs, separator: ", ") { pair in
                visitExpression(pair.key)
                buffer.emit(": ")
                visitExpression(pair.value)
            }
        }
        buffer.emit("]")
    }

    func visitBlock(_ exp: BlockLiteralExpression) -> Void {
        buffer.emitBlock {
            buffer.emit("(")
            buffer.emitWithSeparators(exp.parameters, separator: ", ") { param in
                buffer.emit("\(param.name): \(param.type)")
            }
            buffer.emit(") -> ")
            emitType(exp.returnType)
            buffer.emitLine(" in")

            for stmt in exp.body {
                visitStatement(stmt)
            }
        }
    }

    func visitTernary(_ exp: TernaryExpression) -> Void {
        visitExpression(exp)
        buffer.emit(" ? ")
        visitExpression(exp.ifTrue)
        buffer.emit(" : ")
        visitExpression(exp.ifFalse)
    }

    func visitTuple(_ exp: TupleExpression) -> Void {
        buffer.emit("(")
        buffer.emitWithSeparators(exp.elements, separator: ", ") { element in
            if let label = element.label {
                buffer.emit("\(label): ")
            }

            visitExpression(element.exp)
        }
        buffer.emit(")")
    }

    func visitSelector(_ exp: SelectorExpression) -> Void {
        buffer.emit(exp.description)
    }

    func visitTry(_ exp: TryExpression) -> Void {
        buffer.emit("try ")
        visitExpression(exp)
    }

    func visitIf(_ exp: IfExpression) -> Void {
        buffer.emit("if ")
        visitConditionalClauses(exp.conditionalClauses)
        visitStatement(exp.body)
        if let elseBody = exp.elseBody {
            visitElseBody(elseBody)
        }
    }

    func visitElseBody(_ exp: IfExpression.ElseBody) -> ExprResult {
        buffer.emit("else")
        switch exp {
        case .else(let body):
            visitStatement(body)

        case .elseIf(let expr):
            visitIf(expr)
        }
    }

    func visitSwitch(_ exp: SwitchExpression) -> Void {
        buffer.emit("switch ")
        visitExpression(exp.exp)
        buffer.emitBlock {
            for switchCase in exp.cases {
                visitSwitchCase(switchCase)
            }
            if let defaultCase = exp.defaultCase {
                visitSwitchDefaultCase(defaultCase)
            }
        }
    }

    func visitSwitchCase(_ switchCase: SwitchCase) -> ExprResult {
        buffer.emit("case ")
        buffer.emitWithSeparators(switchCase.casePatterns, separator: ", ") { casePattern in
            visitSwitchCasePattern(casePattern)
        }
        buffer.indented {
            for stmt in switchCase.statements {
                visitStatement(stmt)
            }
        }
    }

    func visitSwitchCasePattern(_ casePattern: SwitchCase.CasePattern) -> ExprResult {
        visitPattern(casePattern.pattern)
        if let whereClause = casePattern.whereClause {
            buffer.emit(" where ")
            visitExpression(whereClause)
        }
    }

    func visitSwitchDefaultCase(_ defaultCase: SwitchDefaultCase) -> ExprResult {
        buffer.emitLine("default:")
        buffer.indented {
            for stmt in defaultCase.statements {
                visitStatement(stmt)
            }
        }
    }

    func visitUnknown(_ exp: UnknownExpression) -> Void {
        buffer.emit(exp.description)
    }

    func visitPattern(_ pattern: Pattern) -> ExprResult {
        buffer.emit(pattern.description)
    }
}

// MARK: SwiftType

extension SwiftASTEmitter {
    func emitType(_ type: SwiftType) {
        buffer.emit(type.description)
    }
}
