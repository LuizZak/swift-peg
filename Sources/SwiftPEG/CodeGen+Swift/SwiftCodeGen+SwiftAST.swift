import SwiftAST

// MARK: Structures

public extension SwiftCodeGen {
    enum TopLevelDecl {
        case `class`(ClassDecl)
        case `struct`(StructDecl)
        case `enum`(EnumDecl)
        case `protocol`(ProtocolDecl)
        case `extension`(ExtensionDecl)
        case unknown(String)
    }

    enum NestedTypeDecl {
        case `class`(ClassDecl)
        case `struct`(StructDecl)
        case `enum`(EnumDecl)
    }

    struct ClassDecl {
        public var leadingComments: [SwiftComment]
        public var accessLevel: AccessLevel
        public var name: String
        public var genericArguments: [GenericArgumentDecl]
        public var inheritances: [SwiftType]
        public var members: [MemberDecl]
    }

    struct StructDecl {
        public var leadingComments: [SwiftComment]
        public var accessLevel: AccessLevel
        public var name: String
        public var genericArguments: [GenericArgumentDecl]
        public var inheritances: [SwiftType]
        public var members: [MemberDecl]
    }

    struct GenericArgumentDecl {
        public var name: String
        public var type: SwiftType?
    }

    struct ExtensionDecl {
        public var leadingComments: [SwiftComment]
        public var baseType: SwiftType
        public var accessLevel: AccessLevel
        public var members: [MemberDecl]
    }

    struct ProtocolDecl {
        public var leadingComments: [SwiftComment]
        public var accessLevel: AccessLevel
        public var name: String
        public var associatedTypes: [AssociatedTypeDecl]
        public var members: [ProtocolMemberDecl]
    }

    struct EnumDecl {
        public var leadingComments: [SwiftComment]
        public var accessLevel: AccessLevel
        public var name: String
        public var inheritances: [SwiftType]
        public var cases: [EnumCaseDecl]
        public var members: [MemberDecl]
    }

    struct EnumCaseDecl {
        public var leadingComments: [SwiftComment]
        public var name: String
        public var associatedValues: [ParameterSignature]?
        public var rawValue: Expression?
    }

    struct TypealiasDecl {
        public var accessLevel: AccessLevel
        public var alias: String
        public var type: SwiftType
    }

    struct AssociatedTypeDecl {
        public var name: String
    }

    enum MemberDecl {
        case variable(VariableMemberDecl)
        case initializer(InitMemberDecl)
        case function(FunctionMemberDecl)
        case `subscript`(SubscriptMemberDecl)
        case `typealias`(TypealiasDecl)
        case type(NestedTypeDecl)
    }

    struct VariableMemberDecl {
        public var leadingComments: [SwiftComment]
        public var attributes: [DeclarationAttribute]
        public var accessLevel: AccessLevel
        public var isConstant: Bool
        public var name: String
        public var type: SwiftType
        public var storage: VariableStorage

        public enum VariableStorage {
            case stored
            case getter(CompoundStatement)
            case getterSetter(CompoundStatement, (String, CompoundStatement))
        }
    }

    struct InitMemberDecl {
        public var leadingComments: [SwiftComment]
        public var attributes: [DeclarationAttribute]
        public var accessLevel: AccessLevel
        public var parameters: [ParameterSignature]
        public var body: CompoundStatement
    }

    struct FunctionMemberDecl {
        public var leadingComments: [SwiftComment]
        public var accessLevel: AccessLevel
        public var signature: FunctionSignature
        public var body: CompoundStatement
    }

    struct SubscriptMemberDecl {
        public var leadingComments: [SwiftComment]
        public var attributes: [DeclarationAttribute]
        public var accessLevel: AccessLevel
        public var signature: SubscriptSignature
        public var getter: CompoundStatement
        public var setter: (String, CompoundStatement)?
    }

    enum ProtocolMemberDecl {
        case variable(ProtocolVariableMemberDecl)
        case initializer(ProtocolInitMemberDecl)
        case function(ProtocolFunctionMemberDecl)
        case `subscript`(ProtocolSubscriptMemberDecl)
        case `typealias`(TypealiasDecl)
    }

    struct ProtocolVariableMemberDecl {
        public var leadingComments: [SwiftComment]
        public var attributes: [DeclarationAttribute]
        public var isConstant: Bool
        public var name: String
        public var type: SwiftType
    }

    struct ProtocolInitMemberDecl {
        public var leadingComments: [SwiftComment]
        public var attributes: [DeclarationAttribute]
        public var parameters: [ParameterSignature]
    }

    struct ProtocolFunctionMemberDecl {
        public var leadingComments: [SwiftComment]
        public var signature: FunctionSignature
    }

    struct ProtocolSubscriptMemberDecl {
        public var leadingComments: [SwiftComment]
        public var attributes: [DeclarationAttribute]
        public var signature: SubscriptSignature
        public var hasSetter: Bool
    }
}

internal class SyntaxNodeRewriterApplier {
    var topLevelDecls: [SwiftCodeGen.TopLevelDecl]

    init(topLevelDecls: [SwiftCodeGen.TopLevelDecl]) {
        self.topLevelDecls = topLevelDecls
    }

    @discardableResult
    func apply(_ rewriter: SyntaxNodeRewriter) -> [SwiftCodeGen.TopLevelDecl] {
        topLevelDecls = topLevelDecls.map { topLevelDecl in
            return apply(rewriter, decl: topLevelDecl)
        }
        return topLevelDecls
    }

    func apply(_ rewriter: SyntaxNodeRewriter, decl: SwiftCodeGen.TopLevelDecl) -> SwiftCodeGen.TopLevelDecl {
        switch decl {
        case .class(let decl):
            return .class(apply(rewriter, decl: decl))

        case .enum(let decl):
            return .enum(apply(rewriter, decl: decl))

        case .extension(let decl):
            return .extension(apply(rewriter, decl: decl))

        case .protocol(let decl):
            return .protocol(decl) // Protocols do not have syntax node bodies, so are returned as-is.

        case .struct(let decl):
            return .struct(apply(rewriter, decl: decl))

        case .unknown(let decl):
            return .unknown(decl) // Unknown declarations are always kept as-is.
        }
    }

    func apply(_ rewriter: SyntaxNodeRewriter, decl: SwiftCodeGen.ClassDecl) -> SwiftCodeGen.ClassDecl {
        var decl = decl
        decl.members = apply(rewriter, members: decl.members)
        return decl
    }

    func apply(_ rewriter: SyntaxNodeRewriter, decl: SwiftCodeGen.EnumDecl) -> SwiftCodeGen.EnumDecl {
        var decl = decl
        decl.cases = apply(rewriter, switchCases: decl.cases)
        decl.members = apply(rewriter, members: decl.members)
        return decl
    }

    func apply(_ rewriter: SyntaxNodeRewriter, decl: SwiftCodeGen.ExtensionDecl) -> SwiftCodeGen.ExtensionDecl {
        var decl = decl
        decl.members = apply(rewriter, members: decl.members)
        return decl
    }

    func apply(_ rewriter: SyntaxNodeRewriter, decl: SwiftCodeGen.StructDecl) -> SwiftCodeGen.StructDecl {
        var decl = decl
        decl.members = apply(rewriter, members: decl.members)
        return decl
    }

    func apply(_ rewriter: SyntaxNodeRewriter, switchCases: [SwiftCodeGen.EnumCaseDecl]) -> [SwiftCodeGen.EnumCaseDecl] {
        switchCases.map { c in
            apply(rewriter, switchCase: c)
        }
    }

    func apply(_ rewriter: SyntaxNodeRewriter, switchCase: SwiftCodeGen.EnumCaseDecl) -> SwiftCodeGen.EnumCaseDecl {
        var switchCase = switchCase

        if let exp = switchCase.rawValue {
            switchCase.rawValue = apply(rewriter, expression: exp)
        }

        return switchCase
    }

    func apply(_ rewriter: SyntaxNodeRewriter, members: [SwiftCodeGen.MemberDecl]) -> [SwiftCodeGen.MemberDecl] {
        members.map { member in
            apply(rewriter, member: member)
        }
    }

    func apply(_ rewriter: SyntaxNodeRewriter, member: SwiftCodeGen.MemberDecl) -> SwiftCodeGen.MemberDecl {
        switch member {
        case .function(let decl):
            return .function(apply(rewriter, member: decl))

        case .initializer(let decl):
            return .initializer(apply(rewriter, member: decl))

        case .subscript(let decl):
            return .subscript(apply(rewriter, member: decl))

        case .type(let decl):
            return .type(apply(rewriter, member: decl))

        case .typealias(let decl):
            return .typealias(decl)

        case .variable(let decl):
            return .variable(apply(rewriter, member: decl))
        }
    }

    func apply(_ rewriter: SyntaxNodeRewriter, member: SwiftCodeGen.FunctionMemberDecl) -> SwiftCodeGen.FunctionMemberDecl {
        var member = member

        member.body = apply(rewriter, compoundStatement: member.body)

        return member
    }

    func apply(_ rewriter: SyntaxNodeRewriter, member: SwiftCodeGen.InitMemberDecl) -> SwiftCodeGen.InitMemberDecl {
        var member = member

        member.body = apply(rewriter, compoundStatement: member.body)

        return member
    }

    func apply(_ rewriter: SyntaxNodeRewriter, member: SwiftCodeGen.SubscriptMemberDecl) -> SwiftCodeGen.SubscriptMemberDecl {
        var member = member

        member.getter = apply(rewriter, compoundStatement: member.getter)

        if let (setterLabel, setter) = member.setter {
            member.setter = (setterLabel, apply(rewriter, compoundStatement: setter))
        }

        return member
    }

    func apply(_ rewriter: SyntaxNodeRewriter, member: SwiftCodeGen.NestedTypeDecl) -> SwiftCodeGen.NestedTypeDecl {
        switch member {
        case .class(let decl):
            .class(apply(rewriter, decl: decl))

        case .struct(let decl):
            .struct(apply(rewriter, decl: decl))

        case .enum(let decl):
            .enum(apply(rewriter, decl: decl))
        }
    }

    func apply(_ rewriter: SyntaxNodeRewriter, member: SwiftCodeGen.VariableMemberDecl) -> SwiftCodeGen.VariableMemberDecl {
        var member = member

        switch member.storage {
        case .getter(let body):
            member.storage = .getter(apply(rewriter, compoundStatement: body))

        case .getterSetter(let getter, (let setterLabel, let setter)):
            member.storage = .getterSetter(
                apply(rewriter, compoundStatement: getter),
                (setterLabel, apply(rewriter, compoundStatement: setter))
            )

        case .stored:
            break
        }

        return member
    }

    func apply(_ rewriter: SyntaxNodeRewriter, compoundStatement: CompoundStatement) -> CompoundStatement {
        let result = rewriter.visitCompound(compoundStatement)
        if let result = result as? CompoundStatement {
            return result
        }

        return [result]
    }

    func apply(_ rewriter: SyntaxNodeRewriter, expression: Expression) -> Expression {
        rewriter.visitExpression(expression)
    }
}
