import SwiftAST

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
            emit(type)
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
            emit(op.arguments)
            buffer.emit(")")

        case let op as SubscriptPostfix:
            buffer.emit("[")
            emit(op.arguments)
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

        emit(exp.type)
    }

    func visitTypeCheck(_ exp: TypeCheckExpression) -> Void {
        visitExpression(exp)
        buffer.emit(" is ")
        emit(exp.type)
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
            buffer.backtrackWhitespace()
            if let signature = exp.signature {
                buffer.ensureSpaceSeparator()
                buffer.emit("(")
                buffer.emitWithSeparators(signature.parameters, separator: ", ") { param in
                    buffer.emit("\(param.name): \(param.type)")
                }
                buffer.emit(") -> ")
                emit(signature.returnType)
                buffer.emit(" in")
            }

            buffer.ensureNewline()

            let conditional = buffer.startConditionalEmitter()
            for stmt in exp.body {
                conditional.ensureEmptyLine()
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
        visitExpression(exp.exp)
    }

    func visitIf(_ exp: IfExpression) -> Void {
        buffer.emit("if")
        visitConditionalClauses(exp.conditionalClauses)
        buffer.ensureSpaceSeparator()
        visitStatement(exp.body)
        if let elseBody = exp.elseBody {
            visitElseBody(elseBody)
        }
    }

    func visitElseBody(_ exp: IfExpression.ElseBody) -> ExprResult {
        buffer.backtrackWhitespace()
        buffer.ensureSpaceSeparator()
        buffer.emit("else")
        buffer.ensureSpaceSeparator()
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
        buffer.ensureSpaceSeparator()
        buffer.emitLine("{")
        for switchCase in exp.cases {
            visitSwitchCase(switchCase)
        }
        if let defaultCase = exp.defaultCase {
            visitSwitchDefaultCase(defaultCase)
        }
        buffer.ensureNewline()
        buffer.emitLine("}")
    }

    func visitSwitchCase(_ switchCase: SwitchCase) -> ExprResult {
        buffer.emit("case ")
        buffer.emitWithSeparators(switchCase.casePatterns, separator: ", ") { casePattern in
            visitSwitchCasePattern(casePattern)
        }
        buffer.emitLine(":")
        buffer.indented {
            for stmt in switchCase.statements {
                visitStatement(stmt)
            }
        }
    }

    func visitSwitchCasePattern(_ casePattern: SwitchCase.CasePattern) -> ExprResult {
        visitPattern(casePattern.pattern)
        if let whereClause = casePattern.whereClause {
            buffer.ensureSpaceSeparator()
            buffer.emit("where")
            buffer.ensureSpaceSeparator()
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
