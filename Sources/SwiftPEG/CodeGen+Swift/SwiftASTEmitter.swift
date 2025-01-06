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
        emit(decl.genericWhereClause)
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
        emit(memberDecl.signature.attributes)
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
        emit(memberDecl.signature.attributes)
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
