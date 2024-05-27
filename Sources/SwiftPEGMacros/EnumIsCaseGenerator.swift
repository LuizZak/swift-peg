import SwiftSyntax
import SwiftSyntaxMacros
import SwiftSyntaxMacroExpansion

/// Macro used to generate boilerplate `var is<Case>: Bool` for enumerations that
/// have associated values.
public struct EnumIsCaseGenerator: MemberMacro, PeerMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        conformingTo protocols: [TypeSyntax],
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {

        let impl = EnumIsCaseMemberImplementation(
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

        let impl = EnumIsCasePeerImplementation(
            node: node,
            declaration: declaration,
            context: context
        )

        return try runImplementation(impl)
    }

    private static func runImplementation(
        _ impl: EnumIsCaseGeneratorImplementationBase
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

class EnumIsCaseGeneratorImplementationBase {
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

    struct MacroArguments {
        var accessLevel: String?
        
        static func from(_ node: AttributeSyntax) throws -> MacroArguments {
            var accessLevel: String? = nil
            if let arguments = node.arguments {
                switch arguments {
                case .argumentList(let list):
                    for expr in list {
                        guard let label = expr.label?.trimmed else {
                            throw MacroError.message("Unexpected argument \(expr).")
                        }

                        if label.trimmedDescription == "accessLevel" {
                            guard let string = expr.expression.as(StringLiteralExprSyntax.self) else {
                                throw MacroError.message(
                                    "Expected 'accessLevel' argument to have a string value of one of Swift's access level modifiers."
                                )
                            }

                            accessLevel = string.segments.description
                        } else {
                            throw MacroError.message(
                                "Unexpected argument \(expr)."
                            )
                        }
                    }
                default:
                    throw MacroError.message("Unsupported argument set \(arguments).")
                }
            }

            return Self(accessLevel: accessLevel)
        }
    }
}

class EnumIsCaseMemberImplementation: EnumIsCaseGeneratorImplementationBase {
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
        for entry in entries {
            decls.append(entry.isCaseDeclSyntax(args.accessLevel))
        }

        return decls
    }

    private func fetchEntries() -> [CaseEntry] {
        var result: [CaseEntry] = []

        for member in declaration.memberBlock.members {
            guard let caseDecl = member.decl.as(EnumCaseDeclSyntax.self) else {
                continue
            }
            
            for element in caseDecl.elements {
                let entry = CaseEntry(element: element)
                result.append(entry)
            }
        }
        
        return result
    }
}

class EnumIsCasePeerImplementation: EnumIsCaseGeneratorImplementationBase {
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

        let args = try parseArguments()

        var decls: [DeclSyntax] = []

        let entries = fetchEntries(from: caseDecl)
        for entry in entries {
            decls.append(entry.isCaseDeclSyntax(args.accessLevel))
        }

        return decls
    }

    private func fetchEntries(from caseDecl: EnumCaseDeclSyntax) -> [CaseEntry] {
        var result: [CaseEntry] = []

        for element in caseDecl.elements {
            let entry = CaseEntry(element: element)
            result.append(entry)
        }
        
        return result
    }
}

fileprivate struct CaseEntry {
    var element: EnumCaseElementSyntax
    var caseName: TokenSyntax {
        element.name
    }

    /// Synthesize a `var is<CaseName>: Bool` declaration for the enum case
    /// element associated with this case entry.
    func isCaseDeclSyntax(_ accessLevel: String? = nil) -> DeclSyntax {
        let varName = TokenSyntax.identifier(
            "is\(caseName.trimmedDescription.uppercasedFirstLetter)"
        )
        let accessLevelToken = accessLevel.map {
            TokenSyntax.identifier($0).withTrailingSpace()
        }

        return """
        \(accessLevelToken)var \(varName): Bool {
            switch self {
            case .\(caseName):
                return true
            default:
                return false
            }
        }
        """
    }
}
