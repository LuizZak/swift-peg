import SwiftSyntax
import SwiftSyntaxMacros
import SwiftSyntaxMacroExpansion

/// Macro used to generate boilerplate `var as<Case>: <AssociatedValueType>?`
/// for enumerations that have associated values.
public struct EnumAsGetterGenerator: MemberMacro, PeerMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        conformingTo protocols: [TypeSyntax],
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {

        let impl = EnumAsGetterMemberImplementation(
            node: node,
            declaration: declaration,
            protocols: protocols,
            context: context
        )

        return try runImplementation(impl)
    }

    public static func expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {

        let impl = EnumAsGetterPeerImplementation(
            node: node,
            declaration: declaration,
            context: context
        )

        return try runImplementation(impl)
    }

    private static func runImplementation(
        _ impl: EnumAsGetterGeneratorImplementationBase
    ) throws -> [DeclSyntax] {

        do {
            return try impl.expand()
        } catch MacroError.diagnostic(let diag) {
            impl.context.diagnose(diag)
            return []
        } catch {
            throw error
        }
    }
}

class EnumAsGetterGeneratorImplementationBase {
    var node: AttributeSyntax
    var context: any MacroExpansionContext

    init(
        node: AttributeSyntax,
        context: some MacroExpansionContext
    ) {
        self.node = node
        self.context = context
    }

    func expand() throws -> [DeclSyntax] {
        return []
    }

    func parseArguments() throws -> MacroArguments {
        try MacroArguments.from(node)
    }

    fileprivate func fetchEntries(from decl: EnumCaseDeclSyntax) throws -> [CaseEntry] {
        var result: [CaseEntry] = []
        for element in decl.elements {
            guard
                let parameterClause = element.parameterClause,
                !parameterClause.parameters.isEmpty
            else {
                let name = element.name.description
                throw MacroError.diagnostic(
                    element.ext_errorDiagnostic(
                        message: "Cannot synthesize var as\(name.uppercasedFirstLetter) for case with no parameters"
                    )
                )
            }

            let entry = CaseEntry(
                element: element,
                parameterClause: parameterClause
            )
            result.append(entry)
        }
        return result
    }

    struct MacroArguments: Decodable {
        var accessLevel: String?

        static func from(_ node: AttributeSyntax) throws -> MacroArguments {
            try MacroArgumentParser.parse(self, attributeSyntax: node)
        }
    }
}

/// To be attached to `enum` declarations.
class EnumAsGetterMemberImplementation: EnumAsGetterGeneratorImplementationBase {
    var declaration: any DeclGroupSyntax
    var protocols: [TypeSyntax]

    init(
        node: AttributeSyntax,
        declaration: some DeclGroupSyntax,
        protocols: [TypeSyntax],
        context: some MacroExpansionContext
    ) {
        self.declaration = declaration
        self.protocols = protocols

        super.init(node: node, context: context)
    }

    override func expand() throws -> [DeclSyntax] {
        if declaration.is(EnumCaseDeclSyntax.self) { return [] }

        guard declaration.is(EnumDeclSyntax.self) else {
            throw MacroError.message("Macro can only be attached to enum or enum case declarations.")
        }

        let args = try parseArguments()

        var decls: [DeclSyntax] = []

        let entries = fetchEntries()
        if entries.isEmpty {
            context.diagnose(
                node.ext_warningDiagnostic(message: """
                Enumeration with no cases with associated values generates no members with \(EnumAsGetterGenerator.self)
                """)
            )
        }

        for entry in entries {
            decls.append(entry.asGetterDeclSyntax(arguments: args))
        }

        return decls
    }

    private func fetchEntries() -> [CaseEntry] {
        var result: [CaseEntry] = []

        for member in declaration.memberBlock.members {
            guard let caseDecl = member.decl.as(EnumCaseDeclSyntax.self) else {
                continue
            }

            do {
                result.append(contentsOf: try fetchEntries(from: caseDecl))
            } catch {
                // Silently ignore failed cases so macro is more ergonomic
            }
        }

        return result
    }
}

/// To be attached to individual `case`s of an enum declaration.
class EnumAsGetterPeerImplementation: EnumAsGetterGeneratorImplementationBase {
    var declaration: any DeclSyntaxProtocol

    init(
        node: AttributeSyntax,
        declaration: some DeclSyntaxProtocol,
        context: some MacroExpansionContext
    ) {
        self.declaration = declaration

        super.init(node: node, context: context)
    }

    override func expand() throws -> [DeclSyntax] {
        if declaration.is(EnumDeclSyntax.self) { return [] }

        guard let caseDecl = declaration.as(EnumCaseDeclSyntax.self) else {
            throw MacroError.message("Macro can only be attached to enum or enum case declarations.")
        }

        let entries = try fetchEntries(from: caseDecl)

        let args = try parseArguments()

        var decls: [DeclSyntax] = []

        for entry in entries {
            decls.append(entry.asGetterDeclSyntax(arguments: args))
        }

        return decls
    }
}

fileprivate struct CaseEntry {
    var element: EnumCaseElementSyntax
    var parameterClause: EnumCaseParameterClauseSyntax
    var caseName: TokenSyntax {
        element.name
    }

    /// Synthesize a `var as<CaseName>: <AssociatedValueType>` declaration for the enum case
    /// element associated with this case entry.
    func asGetterDeclSyntax(
        arguments: EnumAsGetterGeneratorImplementationBase.MacroArguments
    ) -> DeclSyntax {
        let varName = TokenSyntax.identifier(
            "as\(caseName.trimmedDescription.uppercasedFirstLetter)"
        )
        let accessLevelToken = arguments.accessLevel.map {
            TokenSyntax.identifier($0).withTrailingSpace()
        }

        let type = synthesizeType()

        return """
        \(accessLevelToken)var \(varName): \(type) {
            switch self {
            case .\(caseName)\(raw: synthesizePattern()):
                return \(synthesizeReturnExpression())
            default:
                return nil
            }
        }
        """
    }

    func synthesizeType() -> OptionalTypeSyntax {
        OptionalTypeSyntax(wrappedType: patternType())
    }

    func patternType() -> TypeSyntax {
        if
            parameterClause.parameters.count == 1,
            let param = parameterClause.parameters.first
        {
            return param.type
        }

        let tupleType = tupleType(from: parameterClause.parameters)
        return TypeSyntax(tupleType)
    }

    func synthesizePattern() -> String {
        let identifiers = synthesizePatternIdentifiers()
        let binds = identifiers.map({ "let \($0)" })

        return "(\(binds.joined(separator: ", ")))"
    }

    func synthesizeReturnExpression() -> ExprSyntax {
        let identifiers = synthesizePatternIdentifiers()
        if identifiers.count == 1 {
            return "\(raw: identifiers[0])"
        }

        return "(\(raw: identifiers.joined(separator: ", ")))"
    }

    func synthesizePatternIdentifiers() -> [String] {
        let count = parameterClause.parameters.count
        guard count > 0 else {
            return []
        }

        let identifiers = (0..<count).map(nameForPattern)
        return identifiers
    }

    func nameForPattern(index: Int) -> String {
        "_\(index)"
    }

    func tupleType(from params: EnumCaseParameterListSyntax) -> TupleTypeSyntax {
        var elements: [TupleTypeElementSyntax] = []
        for param in params {
            let element = TupleTypeElementSyntax(
                firstName: param.firstName,
                secondName: param.secondName,
                colon: param.colon,
                type: param.type,
                trailingComma: param.trailingComma
            )

            elements.append(element)
        }

        return TupleTypeSyntax(
            elements: .init(elements)
        )
    }
}
