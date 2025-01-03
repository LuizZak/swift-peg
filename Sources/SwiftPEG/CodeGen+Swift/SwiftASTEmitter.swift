import SwiftAST

class SwiftASTEmitter {
    let buffer: CodeStringBuffer = CodeStringBuffer()

    func finishBuffer(addTrailingNewline: Bool = false) -> String {
        buffer.finishBuffer(addTrailingNewline: addTrailingNewline)
    }
}

// MARK: Declarations

extension SwiftASTEmitter {
    /// `<type>`
    func emit(_ decl: SwiftCodeGen.TopLevelDecl) {
        switch decl {
        case .class(let decl):
            emit(decl)

        case .struct(let decl):
            emit(decl)

        case .enum(let decl):
            emit(decl)

        case .protocol(let decl):
            emit(decl)

        case .extension(let decl):
            emit(decl)

        case .unknown(let decl):
            buffer.emit(decl)
        }
    }

    /// `<type>`
    func emit(_ decl: SwiftCodeGen.NestedTypeDecl) {
        switch decl {
        case .class(let decl):
            emit(decl)

        case .struct(let decl):
            emit(decl)

        case .enum(let decl):
            emit(decl)
        }
    }

    /// ```
    /// <leadingComments>
    /// <accessLevel> class <name>[<genericParameters>][: <inheritance1>, <inheritance2>, ...] {
    ///     <members>
    /// }
    /// ```
    func emit(_ decl: SwiftCodeGen.ClassDecl) {
        emit(decl.leadingComments)
        emit(decl.accessLevel)
        buffer.ensureSpaceSeparator()
        buffer.emit("class")
        buffer.ensureSpaceSeparator()
        buffer.emit(decl.name)
        emit(decl.genericArguments)
        emit(inheritances: decl.inheritances)
        buffer.ensureSpaceSeparator()
        buffer.emitMembersBlock {
            let separator = TypeMemberSeparator()

            for member in decl.members {
                if separator.willEmit(member) {
                    buffer.ensureDoubleNewline()
                }

                emit(member)
            }
        }
    }

    /// ```
    /// <leadingComments>
    /// <accessLevel> struct <name>[<genericParameters>][: <inheritance1>, <inheritance2>, ...] {
    ///     <members>
    /// }
    /// ```
    func emit(_ decl: SwiftCodeGen.StructDecl) {
        emit(decl.leadingComments)
        emit(decl.accessLevel)
        buffer.ensureSpaceSeparator()
        buffer.emit("struct")
        buffer.ensureSpaceSeparator()
        buffer.emit(decl.name)
        emit(decl.genericArguments)
        emit(inheritances: decl.inheritances)
        buffer.ensureSpaceSeparator()
        buffer.emitMembersBlock {
            let separator = TypeMemberSeparator()

            for member in decl.members {
                if separator.willEmit(member) {
                    buffer.ensureDoubleNewline()
                }

                emit(member)
            }
        }
    }

    /// ```
    /// <leadingComments>
    /// <accessLevel> extension <type> {
    ///     <members>
    /// }
    /// ```
    func emit(_ decl: SwiftCodeGen.ExtensionDecl) {
        emit(decl.leadingComments)
        emit(decl.accessLevel)
        buffer.ensureSpaceSeparator()
        buffer.emit("extension")
        buffer.ensureSpaceSeparator()
        emit(decl.baseType)
        buffer.ensureSpaceSeparator()
        buffer.emitMembersBlock {
            for member in decl.members {
                emit(member)
                buffer.ensureDoubleNewline()
            }
        }
    }

    /// ```
    /// <leadingComments>
    /// <accessLevel> protocol <name> {
    ///     <associatedTypes>
    ///     <members>
    /// }
    /// ```
    func emit(_ decl: SwiftCodeGen.ProtocolDecl) {
        emit(decl.leadingComments)
        emit(decl.accessLevel)
        buffer.ensureSpaceSeparator()
        buffer.emit("protocol \(decl.name)")
        buffer.ensureSpaceSeparator()
        buffer.emitMembersBlock {
            for assocType in decl.associatedTypes {
                emit(assocType)
            }
            buffer.ensureDoubleNewline()
            for member in decl.members {
                emit(member)
            }
        }
    }

    /// ```
    /// <leadingComments>
    /// <accessLevel> enum <name>[: <inheritance1>, <inheritance2>, ...] {
    ///     <cases>
    ///     <members>
    /// }
    /// ```
    func emit(_ decl: SwiftCodeGen.EnumDecl) {
        emit(decl.leadingComments)
        emit(decl.accessLevel)
        buffer.ensureSpaceSeparator()
        buffer.emit("enum \(decl.name)")
        emit(inheritances: decl.inheritances)
        buffer.ensureSpaceSeparator()
        buffer.emitMembersBlock {
            for caseDecl in decl.cases {
                emit(caseDecl)
            }
            buffer.ensureDoubleNewline()
            for member in decl.members {
                emit(member)
                buffer.ensureDoubleNewline()
            }
        }
    }

    /// ```
    /// <leadingComments>
    /// case <name>[(<associatedValues>)][ = <rawValue>]
    /// ```
    func emit(_ decl: SwiftCodeGen.EnumCaseDecl) {
        emit(decl.leadingComments)
        buffer.emit("case \(decl.name)")
        if let associatedValues = decl.associatedValues {
            buffer.emit("(")
            emit(associatedValues)
            buffer.emit(")")
        }
        if let rawValue = decl.rawValue {
            buffer.emit(" = ")
            emit(rawValue)
        }
        buffer.ensureEmptyLine()
    }

    /// `<accessLevel> typealias <alias> = <type>`
    func emit(_ decl: SwiftCodeGen.TypealiasDecl) {
        buffer.emit("\(decl.accessLevel) typealias \(decl.alias) = ")
        emit(decl.type)
        buffer.ensureNewline()
    }

    /// `associatedtype <name>`
    func emit(_ decl: SwiftCodeGen.AssociatedTypeDecl) {
        buffer.emitLine("associatedtype \(decl.name)")
    }

    /// `<member>`
    func emit(_ memberDecl: SwiftCodeGen.MemberDecl) {
        switch memberDecl {
        case .function(let decl):
            emit(decl)

        case .initializer(let decl):
            emit(decl)

        case .subscript(let decl):
            emit(decl)

        case .type(let decl):
            emit(decl)

        case .typealias(let decl):
            emit(decl)

        case .variable(let decl):
            emit(decl)
        }
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// <accessLevel> <var/let> <name>: <type> <storage>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.VariableMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.attributes)
        emit(memberDecl.accessLevel)
        buffer.ensureSpaceSeparator()
        if memberDecl.isConstant {
            buffer.emit("let")
        } else {
            buffer.emit("var")
        }
        buffer.ensureSpaceSeparator()
        buffer.emit(memberDecl.name)
        buffer.emit(": ")
        emit(memberDecl.type)

        switch memberDecl.storage {
        case .stored:
            break

        case .getter(let body):
            buffer.ensureSpaceSeparator()
            visitStatement(body)

        case .getterSetter(let getterBody, (let value, let setterBody)):
            buffer.ensureSpaceSeparator()
            buffer.emitBlock {
                buffer.emit("get")
                buffer.ensureSpaceSeparator()
                emit(getterBody)
                buffer.emit("set(\(value))")
                buffer.ensureSpaceSeparator()
                emit(setterBody)
            }
        }
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// <accessLevel> init(<parameters>) <body>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.InitMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.attributes)
        emit(memberDecl.accessLevel)
        buffer.ensureSpaceSeparator()
        buffer.emit("init")
        buffer.emit("(")
        emit(memberDecl.parameters)
        buffer.emit(")")
        buffer.ensureSpaceSeparator()
        emit(memberDecl.body)
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// <accessLevel> func <signature> <body>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.FunctionMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.signature.attributes)
        emit(memberDecl.accessLevel)
        buffer.ensureSpaceSeparator()
        emit(memberDecl.signature)
        buffer.ensureSpaceSeparator()
        emit(memberDecl.body)
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// <accessLevel> subscript<signature> <body>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.SubscriptMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.attributes)
        emit(memberDecl.accessLevel)
        buffer.ensureSpaceSeparator()
        emit(memberDecl.signature)
        buffer.ensureSpaceSeparator()
        buffer.emitBlock {
            if let setter = memberDecl.setter {
                buffer.emit("get")
                buffer.ensureSpaceSeparator()
                emit(memberDecl.getter)

                buffer.emit("set(\(setter.0))")
                buffer.ensureSpaceSeparator()
                emit(setter.1)
            } else {
                for stmt in memberDecl.getter {
                    emit(stmt)
                }
            }
        }
    }

    /// ```
    /// [static/mutating ]func <name>[<genericArguments>](<parameters>) [throws] -> <returnType>[<genericWhereClause>]
    /// ```
    func emit(_ signature: FunctionSignature) {
        if signature.isStatic {
            buffer.emit("static ")
        }
        if signature.isMutating {
            buffer.emit("mutating ")
        }
        buffer.emit("func \(signature.name)")
        emit(signature.genericParameters)
        buffer.emit("(")
        emit(signature.parameters)
        buffer.emit(")")
        if signature.isThrowing {
            buffer.emit(" throws")
        }
        buffer.emit(" -> ")
        emit(signature.returnType)
        emit(signature.genericWhereClause)
    }

    /// ```
    /// [static ]subscript[<genericArguments>](<parameters>) -> <returnType>[<genericWhereClause>]
    /// ```
    func emit(_ signature: SubscriptSignature) {
        if signature.isStatic {
            buffer.emit("static ")
        }
        buffer.emit("subscript")
        emit(signature.genericParameters)
        buffer.emit("(")
        emit(signature.parameters)
        buffer.emit(")")
        buffer.emit(" -> ")
        emit(signature.returnType)
        emit(signature.genericWhereClause)
    }

    /// `<member>`
    func emit(_ memberDecl: SwiftCodeGen.ProtocolMemberDecl) {
        switch memberDecl {
        case .function(let decl):
            emit(decl)

        case .initializer(let decl):
            emit(decl)

        case .subscript(let decl):
            emit(decl)

        case .typealias(let decl):
            emit(decl)

        case .variable(let decl):
            emit(decl)
        }
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// <var> <name>: <type> <{ get }/{ get set}>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.ProtocolVariableMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.attributes)
        buffer.ensureSpaceSeparator()
        buffer.emit("var")
        buffer.ensureSpaceSeparator()
        buffer.emit(memberDecl.name)
        buffer.emit(": ")
        emit(memberDecl.type)
        if memberDecl.isConstant {
            buffer.emit("{ get }")
        } else {
            buffer.emit("{ get set }")
        }
        buffer.ensureNewline()
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// init(<parameters>)
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.ProtocolInitMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.attributes)
        buffer.ensureSpaceSeparator()
        buffer.emit("init")
        buffer.emit("(")
        emit(memberDecl.parameters)
        buffer.emit(")")
        buffer.ensureNewline()
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// func <signature>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.ProtocolFunctionMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.signature.attributes)
        buffer.ensureSpaceSeparator()
        emit(memberDecl.signature)
        buffer.ensureNewline()
    }

    /// ```
    /// <leadingComments>
    /// @attribute1
    /// @attribute2
    /// ...
    /// subscript<signature> <body>
    /// ```
    func emit(_ memberDecl: SwiftCodeGen.ProtocolSubscriptMemberDecl) {
        emit(memberDecl.leadingComments)
        emit(memberDecl.attributes)
        buffer.ensureSpaceSeparator()
        emit(memberDecl.signature)
        buffer.ensureSpaceSeparator()
        if memberDecl.hasSetter {
            buffer.emit("{ get set }")
        } else {
            buffer.emit("{ get }")
        }
    }

    /// `<<parameter1>, <parameter2>, ...>`
    ///
    /// If `parameters` is nil, nothing is emitted.
    func emit(_ parameters: [SwiftCodeGen.GenericArgumentDecl]) {
        guard !parameters.isEmpty else {
            return
        }

        buffer.emit("<")
        buffer.emitWithSeparators(parameters, separator: ", ") { parameter in
            emit(parameter)
        }
        buffer.emit(">")
    }

    /// `<parameter>[: type]`
    func emit(_ parameter: SwiftCodeGen.GenericArgumentDecl) {
        buffer.emit(parameter.name)
        if let type = parameter.type {
            buffer.emit(": ")
            emit(type)
        }
    }

    /// `<<parameter1>, <parameter2>, ...>`
    ///
    /// If `parameters` is nil, nothing is emitted.
    func emit(_ parameters: GenericParameterClause?) {
        guard let parameters else {
            return
        }

        buffer.emit("<")
        buffer.emitWithSeparators(parameters.genericParameters, separator: ", ") { parameter in
            emit(parameter)
        }
        buffer.emit(">")
    }

    /// `<parameter>`
    func emit(_ parameter: GenericParameterClause.GenericParameter) {
        buffer.emit(parameter.description)
    }

    /// `[where <requirement1>, <requirement2>, ...]`
    ///
    /// If `whereClause` is nil, nothing is emitted.
    func emit(_ whereClause: GenericWhereClause?) {
        guard let whereClause else {
            return
        }

        buffer.ensureSpaceSeparator()
        buffer.emit("where ")
        buffer.emitWithSeparators(whereClause.requirements, separator: ", ") { requirement in
            emit(requirement)
        }
    }

    /// `<requirement>`
    func emit(_ requirement: GenericWhereClause.Requirement) {
        buffer.emit(requirement.description)
    }

    /// ```
    /// @attribute1
    /// @attribute2
    /// ...
    /// ```
    func emit(_ attributes: [DeclarationAttribute]) {
        for attribute in attributes {
            emit(attribute)
        }
    }

    /// `@<attributeName>[(<attributeParameters>)]\n`
    func emit(_ attribute: DeclarationAttribute) {
        buffer.emit("@\(attribute.name)")
        if let parameters = attribute.arguments {
            buffer.emit("(")
            emit(parameters)
            buffer.emit(")")
        }

        buffer.ensureNewline()
    }

    /// `<argument1>, <argument2>, ...`
    func emit(_ arguments: [ParameterSignature]) {
        buffer.emitWithSeparators(arguments, separator: ", ") { argument in
            if let label = argument.label {
                if label != argument.name {
                    buffer.emit("\(label) ")
                }
            } else {
                buffer.emit("_ ")
            }
            buffer.emit("\(argument.name): ")

            if argument.modifier != .none {
                buffer.emit("\(argument.modifier.rawValue) ")
            }

            emit(argument.type)
            if argument.isVariadic {
                buffer.emit("...")
            }
        }
    }

    /// `[<label>: ]<expression>`
    func emit(_ arguments: [FunctionArgument]) {
        buffer.emitWithSeparators(arguments, separator: ", ") { argument in
            if let label = argument.label {
                buffer.emit("\(label): ")
            }

            emit(argument.expression)
            buffer.backtrackWhitespace() // Backtrack newlines produced by block literals
        }
    }

    /// Emits a sequence of comments, one after the other.
    func emit(_ comments: [SwiftComment]) {
        for comment in comments {
            emit(comment)
        }
    }

    /// Emits a comment, with automatic delimiters depending on its type.
    func emit(_ comment: SwiftComment) {
        switch comment {
        case .block(let lines):
            buffer.emitLine("/*")
            for line in lines.split(separator: "\n", omittingEmptySubsequences: false) {
                buffer.emitLine(line)
            }
            buffer.emitLine("*/")

        case .docBlock(let lines):
            for line in lines.split(separator: "\n", omittingEmptySubsequences: false) {
                buffer.emitLine("///\(line)")
            }

        case .docLine(let line):
            buffer.emitLine("///\(line)")

        case .line(let line):
            buffer.emitLine("//\(line)")
        }
    }

    /// `[ ]<accessLevel>`
    func emit(_ accessLevel: AccessLevel) {
        if accessLevel != .internal {
            buffer.ensureSpaceSeparator()
            buffer.emit(accessLevel.rawValue)
        }
    }

    /// `<statement>`
    func emit(_ statement: Statement) {
        visitStatement(statement)
    }

    /// `<expression>`
    func emit(_ expression: Expression) {
        visitExpression(expression)
    }

    /// `[: <type1>, <type2>, ...]`
    ///
    /// If `inheritances` is an empty array, nothing is emitted.
    func emit(inheritances: [SwiftType]) {
        guard !inheritances.isEmpty else {
            return
        }
        buffer.emit(": ")
        buffer.emitWithSeparators(inheritances, separator: ", ") { inheritance in
            emit(inheritance)
        }
    }
}

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

// MARK: SwiftType

extension SwiftASTEmitter {
    func emit(_ type: SwiftType) {
        buffer.emit(type.description)
    }
}

// MARK: Internal helper types

/// Manages separations between type member declarations.
private class TypeMemberSeparator {
    var lastMember: SwiftCodeGen.MemberDecl?

    /// Records the current member to be emitted, and compares it to any previously
    /// recorded member, returning `true` if an empty line should be emitted to
    /// separate the two members.
    func willEmit(_ member: SwiftCodeGen.MemberDecl) -> Bool {
        defer { lastMember = member }
        guard let last = lastMember else {
            return false
        }

        switch (last, member) {
        case (.typealias, .typealias):
            return false

        default:
            return true
        }
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
