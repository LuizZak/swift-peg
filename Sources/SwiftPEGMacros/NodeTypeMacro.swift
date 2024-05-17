import SwiftSyntax
import SwiftSyntaxMacros

/// Macro used to generate boilerplate grammar node-related glue code.
public struct NodeTypeMacro: MemberMacro {
    private static let _attributeName: String = "NodeProperty"

    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        conformingTo protocols: [TypeSyntax],
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        do {
            return try _expansion(
                of: node,
                providingMembersOf: declaration,
                conformingTo: protocols,
                in: context
            )
        } catch MacroError.diagnostic(let diag) {
            context.diagnose(diag)
            return []
        } catch {
            throw error
        }
    }

    private static func _expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        conformingTo protocols: [TypeSyntax],
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {

        let arguments = try parseArguments(node, in: context)
        
        // Collect members to generate based on private nodes decorated with @NodeProperty
        var nodes = _fetchNodes(declaration: declaration, in: context)

        // For nodes with no explicit access control, inherit the declaration's
        let accessLevel = declaration
            .ext_accessLevel()?
            .name.trimmed
            .with(\.trailingTrivia, .spaces(1))
        nodes = nodes.map { node in
            var node = node
            node.accessLevel = node.accessLevel ?? accessLevel
            return node
        }

        return nodes.map(_synthesizeProperty) + [
            _synthesizeChildrenProperty(
                nodes: nodes,
                accessLevel: accessLevel,
                arguments: arguments
            )
        ]
    }

    private static func _synthesizeProperty(_ node: _NodeEntry) -> DeclSyntax {
        let field = TokenSyntax.identifier(node.memberName)
        let property = TokenSyntax.identifier(node.synthesizedName)

        let body: CodeBlockItemListSyntax
        
        switch node.childNodeType {
        case .simple:
            body = """
                \(field).parent = nil
                \(field) = newValue
                \(field).parent = self
                """
        case .array:
            body = """
                \(field).forEach({
                    $0.parent = nil
                })
                \(field) = newValue
                \(field).forEach({
                    $0.parent = self
                })
                """
        case .optional:
            body = """
                \(field)?.parent = nil
                \(field) = newValue
                \(field)?.parent = self
                """
        }

        return """
        \(node.comments)
        /// Synthesized with `\(self)`.
        \(node.accessLevel)var \(property): \(node.childNodeType.type) {
            get { \(field) }
            set {
                \(body)
            }
        }
        """
    }

    private static func _synthesizeChildrenProperty(
        nodes: [_NodeEntry],
        accessLevel: TokenSyntax?,
        arguments macroArguments: MacroArguments
    ) -> DeclSyntax {

        // Separate the nodes into array / non-array and add them together

        var nodes = nodes
        let nonArrayStart = nodes.partition(by: { !$0.childNodeType.isArray })

        var arguments: LabeledExprListSyntax = LabeledExprListSyntax()

        let arrayNodes = Array(nodes[..<nonArrayStart])
        let nonArrayNodes = Array(nodes[nonArrayStart...])

        if !arrayNodes.isEmpty {
            arguments.append(
                LabeledExprSyntax(label: "lists", expression: arrayNodes[0].synthesizedNameExpr)
            )
            arguments.append(contentsOf: arrayNodes.dropFirst().map { arg in
                LabeledExprSyntax(expression: arg.synthesizedNameExpr)
            })
        }

        if !nonArrayNodes.isEmpty {
            arguments.append(
                LabeledExprSyntax(label: "nodes", expression: nonArrayNodes[0].synthesizedNameExpr)
            )
            arguments.append(contentsOf: nonArrayNodes.dropFirst().map { arg in
                LabeledExprSyntax(expression: arg.synthesizedNameExpr)
            })
        }

        let body: ExprSyntax = "Self.makeNodeList(\(raw: arguments.map(\.description).joined(separator: ", ")))"

        return """
        /// Synthesized with `\(self)`.
        override \(accessLevel)var children: [\(macroArguments.baseNodeType)] { \(body) }
        """
    }

    private static func _fetchNodes(
        declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) -> [_NodeEntry] {
        let members = declaration.memberBlock.members
        guard !members.isEmpty else {
            return []
        }

        return members.flatMap({ _entriesFromMember($0, in: context) })
    }

    private static func _entriesFromMember(
        _ member: MemberBlockItemSyntax,
        in context: some MacroExpansionContext
    ) -> [_NodeEntry] {
        guard let property = member.decl.as(VariableDeclSyntax.self) else {
            return []
        }
        guard let (_, attributeType) = _nodePropertyAttribute(from: property) else {
            return []
        }

        var result: [_NodeEntry] = []
        
        for binding in _bindings(from: property.bindings) {
            let name = binding.name.trimmed
            guard let type = binding.type ?? attributeType else {
                context.diagnose(
                    binding.name.ext_warningDiagnostic(
                        message: "Could not resolve type for @NodeProperty binding"
                    )
                )
                continue
            }
            guard name.description.hasPrefix("_") else {
                context.diagnose(
                    binding.name.ext_warningDiagnostic(
                        message: "Bindings with @NodeProperty need to start with underscored name, e.g. '_\(name)'"
                    )
                )
                continue
            }

            let entry = _NodeEntry(
                accessLevel: nil,
                comments: member.ext_docComments(),
                member: name,
                childNodeType: .fromType(type)
            )
            result.append(entry)
        }

        return result
    }

    private static func _bindings(from bindings: PatternBindingListSyntax) -> [(name: TokenSyntax, type: TypeSyntax?)] {
        var result: [(name: TokenSyntax, type: TypeSyntax?)] = []

        for binding in bindings {
            guard let pattern = binding.pattern.as(IdentifierPatternSyntax.self) else {
                continue
            }

            result.append(
                (name: pattern.identifier, binding.typeAnnotation?.type)
            )
        }

        return result
    }

    private static func _nodePropertyAttribute(from member: VariableDeclSyntax) -> (AttributeSyntax, TypeSyntax?)? {
        for attribute in member.attributes {
            if let result = _nodePropertyAttribute(from: attribute) {
                return result
            }
        }

        return nil
    }

    private static func _nodePropertyAttribute(from attribute: AttributeListSyntax.Element) -> (AttributeSyntax, TypeSyntax?)? {
        switch attribute {
        case .attribute(let attr):
            guard let typeSyntax = attr.attributeName.as(IdentifierTypeSyntax.self) else {
                return nil
            }
            guard typeSyntax.name.trimmed.description == _attributeName else {
                return nil
            }

            if
                let genericArguments = typeSyntax.genericArgumentClause,
                let genericArgument = genericArguments.arguments.first,
                genericArguments.arguments.count == 1
            {
                return (attr, genericArgument.argument)
            }

            return (attr, nil)
        case .ifConfigDecl:
            return nil
        }
    }

    static func parseArguments(
        _ node: AttributeSyntax,
        in context: some MacroExpansionContext
    ) throws -> MacroArguments {
        guard
            let attributeName = node.attributeName.as(IdentifierTypeSyntax.self),
            let genericClause = attributeName.genericArgumentClause,
            let genericArgument = genericClause.arguments.first,
            genericClause.arguments.count == 1
        else {
            throw MacroError.message("Could not extract base class name from generic argument of attribute \(node)")
        }

        return MacroArguments(baseNodeType: genericArgument.argument)
    }

    private struct _NodeEntry {
        var accessLevel: TokenSyntax?
        var comments: Trivia
        var member: TokenSyntax
        var childNodeType: ChildNodeType

        var memberName: String { member.description }
        var synthesizedName: String { String(memberName.trimmingPrefix("_")) }
        var synthesizedNameExpr: DeclReferenceExprSyntax {
            DeclReferenceExprSyntax(baseName: .identifier(synthesizedName))
        }

        enum ChildNodeType {
            case simple(TypeSyntax)
            case array(TypeSyntax, element: TypeSyntax)
            case optional(TypeSyntax, wrapped: TypeSyntax)

            var isArray: Bool {
                switch self {
                case .array:
                    return true
                case .simple, .optional:
                    return false
                }
            }

            var type: TypeSyntax {
                switch self {
                case .simple(let type):
                    return type
                case .array(let type, _):
                    return type
                case .optional(let type, _):
                    return type
                }
            }

            static func fromType(_ type: TypeSyntax) -> Self {
                // Array type
                if let arraySugar = type.as(ArrayTypeSyntax.self) {
                    return .array(type, element: arraySugar.element)
                } else if
                    let identifierType = type.as(IdentifierTypeSyntax.self),
                    let element = identifierType.ext_genericParameter1Exact(name: "Array")
                {
                    return .array(type, element: element)
                }
                // Optional type
                else if let optionalSugar = type.as(OptionalTypeSyntax.self) {
                    return .optional(type, wrapped: optionalSugar.wrappedType)
                } else if
                    let identifierType = type.as(IdentifierTypeSyntax.self),
                    let wrapped = identifierType.ext_genericParameter1Exact(name: "Optional")
                {
                    return .optional(type, wrapped: wrapped)
                } else {
                    return .simple(type)
                }
            }
        }
    }

    struct MacroArguments {
        var baseNodeType: TypeSyntax
    }
}

/// Property wrapper for signaling fields to create node properties with when
/// used with `NodeTypeMacro`.
///
/// By itself does nothing but wrap the value.
@propertyWrapper
public struct NodeProperty<T> {
    public var wrappedValue: T

    @inlinable
    public init(wrappedValue: T) {
        self.wrappedValue = wrappedValue
    }
}
