import SwiftSyntax
import SwiftSyntaxMacros
import SwiftSyntaxMacroExpansion

/// Macro used to generate boilerplate grammar node-related glue code.
public struct NodeTypeMacro: MemberMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        conformingTo protocols: [TypeSyntax],
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        let impl = NodeTypeMacroImplementation(
            node: node,
            declaration: declaration,
            protocols: protocols,
            context: context
        )

        do {
            return try impl.expand()
        } catch MacroError.diagnostic(let diag) {
            context.diagnose(diag)
            return []
        } catch {
            throw error
        }
    }
}

class NodeTypeMacroImplementation {
    private static let _nodePropertyAttributeName: String = "NodeProperty"
    private static let _nodeRequiredAttributeName: String = "NodeRequired"
    private static let _overrideDeepCopyArg: String = "overrideDeepCopyType"
    private static let _knownArgumentLabels: [String] = [
        _overrideDeepCopyArg,
    ]

    var node: AttributeSyntax
    var declaration: any DeclGroupSyntax
    var protocols: [TypeSyntax]
    var context: any MacroExpansionContext

    init(
        node: AttributeSyntax,
        declaration: some DeclGroupSyntax,
        protocols: [TypeSyntax],
        context: some MacroExpansionContext
    ) {
        self.node = node
        self.declaration = declaration
        self.protocols = protocols
        self.context = context
    }

    func expand() throws -> [DeclSyntax] {
        let arguments = try _parseArguments()

        // For nodes with no explicit access control, inherit the declaration's
        let accessLevel = declaration
            .ext_accessLevel()?
            .name.trimmed
            .with(\.trailingTrivia, .spaces(1))

        // Collect members to generate based on private nodes decorated with @NodeProperty
        let evaluatedType = try _collect(arguments)

        var decls: [DeclSyntax] = []

        let properties = evaluatedType.properties.map(_synthesizeProperty)
        let children = _synthesizeChildrenProperty(
            nodes: evaluatedType.properties,
            accessLevel: accessLevel,
            arguments: arguments
        )
        let deepCopy = _synthesizeDeepCopy(evaluatedType: evaluatedType, accessLevel: accessLevel)

        decls.append(contentsOf: properties)
        decls.append(children)
        if let initializer = _synthesizeInit(evaluatedType: evaluatedType, accessLevel: accessLevel) {
            decls.append(initializer)
        }
        decls.append(deepCopy)

        return decls
    }

    private func _collect(_ arguments: MacroArguments) throws -> EvaluatedType {
        // For nodes with no explicit access control, inherit the declaration's
        let accessLevel = declaration
            .ext_accessLevel()?
            .name.trimmed
            .with(\.trailingTrivia, .spaces(1))

        let typeName: TokenSyntax

        if let type = declaration.as(ClassDeclSyntax.self) {
            typeName = type.name
        } else {
            throw MacroError.message("Cannot apply macro to non-class declaration \(declaration.kind)")
        }

        // Collect members to generate based on private nodes decorated with @NodeProperty
        let properties = NodePropertyDecl.fromDeclGroupSyntax(
            declaration,
            accessLevel: accessLevel,
            in: context
        )
        let required = NodeRequiredDecl.fromDeclGroupSyntax(
            declaration,
            accessLevel: accessLevel,
            in: context
        )

        // Ensure no declarations don't overlap
        for property in properties {
            for required in required {
                guard property.member.position == required.member.position else {
                    continue
                }

                throw MacroError.diagnostic(
                    property.diagnosticSyntax.ext_errorDiagnostic(
                        message: "Property cannot be both @\(property.attribute.attributeName) and @\(required.attribute.attributeName)",
                        highlights: [
                            property.attribute,
                            required.attribute
                        ]
                    )
                )
            }
        }

        return EvaluatedType(
            typeName: typeName,
            deepCopyOverride: arguments.overrideDeepCopyType,
            properties: properties,
            required: required
        )
    }

    private func _synthesizedComment() -> Trivia {
        return .docLineComment("/// Synthesized with `\(NodeTypeMacro.self)`.")
    }

    /// Synthesize the property:
    ///
    /// ```
    /// <accessLevel> var <node.property>: <NodeType> {
    ///    get {
    ///        <node.field>
    ///    }
    ///    set {
    ///        <node.field>.parent = nil
    ///        <node.field> = newValue
    ///        <node.field>.parent = self
    ///    }
    /// }
    /// ```
    private func _synthesizeProperty(_ node: NodePropertyDecl) -> DeclSyntax {
        let field = TokenSyntax.identifier(node.memberName)
        let property = TokenSyntax.identifier(node.synthesizedName)

        return """
        \(node.comments)
        \(_synthesizedComment())
        \(node.accessLevel)var \(property): \(node.childNodeType.type) {
            get { \(field) }
            set {
                \(node.synthesizeAssignParent("nil"))
                \(field) = newValue
                \(node.synthesizeAssignParent("self"))
            }
        }
        """
    }

    /// Synthesize the property:
    ///
    /// ```
    /// <accessLevel> var children: [<NodeType>] {
    ///    Self.makeNodeList(lists: <nodes.nodeLists>, nodes: <nodes.nodes>)
    /// }
    /// ```
    private func _synthesizeChildrenProperty(
        nodes: [NodePropertyDecl],
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
        \(_synthesizedComment())
        override \(accessLevel)var children: [\(macroArguments.baseNodeType)] { \(body) }
        """
    }

    /// Synthesize the initializer:
    ///
    /// ```
    /// <accessLevel> init(<args>) {
    ///    self.<properties> = <args.properties>
    ///    self.<required> = <args.required>
    ///
    ///    super.init()
    ///
    ///    self.<properties>.parent = self
    /// }
    /// ```
    ///
    /// If the evaluated type has no fields to initialize, `nil` is returned,
    /// instead.
    private func _synthesizeInit(
        evaluatedType: EvaluatedType,
        accessLevel: TokenSyntax?
    ) -> DeclSyntax? {

        let properties = evaluatedType.properties
        let required = evaluatedType.required

        let params = evaluatedType.synthesizeFunctionParameters()
        guard !params.isEmpty else {
            return nil
        }

        let paramList = FunctionParameterListSyntax(
            params.ext_commaSeparated(comma: .commaToken().withTrailingSpace())
        )

        let preSuperInitBlock = CodeBlockItemListSyntax(
            properties.map { node in
                CodeBlockItemSyntax("self.\(node.member) = \(node.synthesizedNameExpr)").addingTrailingTrivia(.newline)
            } +
            required.map { node in
                CodeBlockItemSyntax("self.\(node.member) = \(node.member)").addingTrailingTrivia(.newline)
            }
        )
        let postSuperInitBlock = CodeBlockItemListSyntax(properties.map { node in
            CodeBlockItemSyntax("self.\(node.synthesizeAssignParent("self"))").addingLeadingTrivia(.newline)
        })

        var accessLevel = accessLevel
        if accessLevel?.tokenKind == .keyword(.open) {
            accessLevel = .keyword(.public).withTrailingSpace()
        }

        // TODO: Figure out better way to attach newline-separated statement lists
        if postSuperInitBlock.isEmpty {
            return """
            \(_synthesizedComment())
            \(accessLevel)init(\(paramList)) {
                \(preSuperInitBlock)
                super.init()
            }
            """
        }

        return """
        \(_synthesizedComment())
        \(accessLevel)init(\(paramList)) {
            \(preSuperInitBlock)
            super.init()
            \(postSuperInitBlock)
        }
        """
    }

    /// Synthesize the deep copy method:
    ///
    /// ```
    /// <accessLevel> func deepCopy() -> <TypeName> {
    ///    return <TypeName>(<properties>.deepCopy()...)
    /// }
    /// ```
    private func _synthesizeDeepCopy(
        evaluatedType: EvaluatedType,
        accessLevel: TokenSyntax?
    ) -> DeclSyntax {

        let methodName: TokenSyntax = "deepCopy"

        let args = evaluatedType.synthesizeDeepCopyArguments()
        let argList = LabeledExprListSyntax(
            args.ext_commaSeparated(comma: .commaToken().withTrailingSpace())
        )

        var accessLevel = accessLevel
        if accessLevel?.tokenKind == .keyword(.open) {
            accessLevel = .keyword(.public).withTrailingSpace()
        }

        let overrideToken: TokenSyntax? =
            evaluatedType
                .deepCopyOverride
                .map { _ in .keyword(.override).withTrailingSpace() }

        let returnType =
            evaluatedType
                .deepCopyOverride
                .map({
                    TokenSyntax.identifier($0)
                }) ?? evaluatedType.typeName

        return """
        \(_synthesizedComment())
        \(accessLevel)\(overrideToken)func \(methodName)() -> \(returnType) {
            return \(evaluatedType.typeName)(\(argList))
        }
        """
    }

    private static func _nodeAttribute(
        named name: String,
        from member: VariableDeclSyntax
    ) -> (AttributeSyntax, TypeSyntax?)? {

        for attribute in member.attributes {
            if let result = _nodeAttribute(
                named: name,
                from: attribute
            ) {
                return result
            }
        }

        return nil
    }

    private static func _nodeAttribute(
        named name: String,
        from attribute: AttributeListSyntax.Element
    ) -> (AttributeSyntax, TypeSyntax?)? {
        switch attribute {
        case .attribute(let attr):
            guard let typeSyntax = attr.attributeName.as(IdentifierTypeSyntax.self) else {
                return nil
            }
            guard typeSyntax.name.trimmed.description == name else {
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

    private func _parseArguments() throws -> MacroArguments {
        guard
            let attributeName = node.attributeName.as(IdentifierTypeSyntax.self),
            let genericClause = attributeName.genericArgumentClause,
            let genericArgument = genericClause.arguments.first,
            genericClause.arguments.count == 1
        else {
            throw MacroError.message(
                "Could not extract base class name from generic argument of attribute \(node)"
            )
        }

        var overrideDeepCopyType: String? = nil
        if let arguments = node.arguments {
            switch arguments {
            case .argumentList(let list):
                for expr in list {
                    guard let label = expr.label?.trimmed else {
                        throw MacroError.message("Unexpected argument \(expr). Known arguments: \(Self._knownArgumentLabels)")
                    }
                    guard Self._knownArgumentLabels.contains(label.description) else {
                        throw MacroError.message("Unexpected argument \(expr). Known arguments: \(Self._knownArgumentLabels)")
                    }

                    if label.description == Self._overrideDeepCopyArg  {
                        guard let string = expr.expression.as(StringLiteralExprSyntax.self) else {
                            throw MacroError.message(
                                "Expected \(Self._overrideDeepCopyArg) argument to have a string value"
                            )
                        }

                        overrideDeepCopyType = string.segments.description
                    } else {
                        throw MacroError.message(
                            "Unexpected argument \(expr). Known arguments: \(Self._knownArgumentLabels)"
                        )
                    }
                }
            default:
                throw MacroError.message("Unsupported argument set \(arguments). Known arguments: \(Self._knownArgumentLabels)")
            }
        }

        return MacroArguments(
            baseNodeType: genericArgument.argument,
            overrideDeepCopyType: overrideDeepCopyType
        )
    }

    /// Collects all properties evaluated within a type declaration.
    private struct EvaluatedType {
        /// The type's name.
        var typeName: TokenSyntax

        /// A deep-copy override string optionally provided for overriding the
        /// return type of generated `deepCopy()` method.
        var deepCopyOverride: String?

        /// Properties tagged with `@NodeProperty`
        var properties: [NodePropertyDecl]

        /// Properties tagged with `@NodeRequired`
        var required: [NodeRequiredDecl]

        /// Generates function parameters that receive all properties and required
        /// nodes for the evaluated type.
        ///
        /// Order of the resulting parameters is significant: it is the same order
        /// that the properties that generated the declarations are laid out within
        /// the type.
        ///
        /// - note: Generated parameters don't include commas.
        func synthesizeFunctionParameters() -> [FunctionParameterSyntax] {
            var result: [(syntax: any SyntaxProtocol, param: FunctionParameterSyntax)] = []

            for property in properties {
                result.append(
                    (property.member, property.synthesizeFunctionParameter())
                )
            }
            for required in required {
                result.append(
                    (required.member, required.synthesizeFunctionParameter())
                )
            }

            return result.sorted {
                $0.syntax.position < $1.syntax.position
            }.map(\.param)
        }

        /// Generates function arguments for a `deepCopy()` call that receive all
        /// properties and required nodes for the evaluated type, calling `deepCopy()`
        /// on each node property, as required. Each argument receives the property
        /// of the same name.
        ///
        /// Order of the resulting arguments is significant: it is the same order
        /// that the properties that generated the declarations are laid out within
        /// the type.
        ///
        /// - note: Generated arguments don't include commas.
        func synthesizeDeepCopyArguments() -> [LabeledExprSyntax] {
            var result: [(syntax: any SyntaxProtocol, param: LabeledExprSyntax)] = []

            for property in properties {
                var argument = property.synthesizeFunctionArgument()

                let copyCall = property.childNodeType
                    .synthesizeMemberCall(
                        symbol: argument.expression,
                        member: DeclReferenceExprSyntax(
                            baseName: "deepCopy",
                            argumentNames: DeclNameArgumentsSyntax(arguments: [])
                        )
                    )
                argument.expression = copyCall

                result.append(
                    (property.member, argument)
                )
            }
            for required in required {
                result.append(
                    (required.member, required.synthesizeFunctionArgument())
                )
            }

            return result.sorted {
                $0.syntax.position < $1.syntax.position
            }.map(\.param)
        }
    }

    /// Property tagged with `@NodeRequired`
    private struct NodeRequiredDecl {
        /// The attribute that the declaration was tagged with.
        var attribute: AttributeSyntax
        /// Name of member that originated this entry.
        var member: TokenSyntax
        /// A syntax element to attach diagnostic message to.
        var diagnosticSyntax: any SyntaxProtocol
        /// Type of the member
        var type: TypeSyntax

        /// An initial value provided on the declaration.
        var initialValue: InitializerClauseSyntax?

        static func fromDeclGroupSyntax(
            _ declaration: any DeclGroupSyntax,
            accessLevel: TokenSyntax? = nil,
            in context: MacroExpansionContext
        ) -> [Self] {

            let members = declaration.memberBlock.members
            guard !members.isEmpty else {
                return []
            }

            return members.flatMap {
                Self.fromProperty(
                    $0,
                    accessLevel: accessLevel,
                    in: context
                )
            }
        }

        static func fromProperty(
            _ member: MemberBlockItemSyntax,
            accessLevel: TokenSyntax? = nil,
            in context: MacroExpansionContext
        ) -> [Self] {

            guard let property = member.decl.as(VariableDeclSyntax.self) else {
                return []
            }
            guard
                let (attribute, attributeType) = _nodeAttribute(
                    named: _nodeRequiredAttributeName, from: property
                )
            else {
                return []
            }

            var result: [Self] = []

            let identifier = "@NodeRequired"

            for binding in property.bindings.ext_bindings() {
                let name = binding.name.trimmed
                guard let type = binding.type ?? attributeType else {
                    context.diagnose(
                        binding.name.ext_warningDiagnostic(
                            message: "Could not resolve type for \(identifier) binding"
                        )
                    )
                    continue
                }

                let entry = Self(
                    attribute: attribute,
                    member: name,
                    diagnosticSyntax: binding.binding,
                    type: type,
                    initialValue: binding.binding.initializer
                )
                result.append(entry)
            }

            return result
        }

        /// Synthesizes a function parameter from this node's declaration.
        func synthesizeFunctionParameter(needsComma: Bool = false) -> FunctionParameterSyntax {
            FunctionParameterSyntax(
                firstName: member.trimmed,
                type: type.trimmed,
                defaultValue: initialValue?.trimmed.withLeadingSpace(),
                trailingComma: needsComma ? .commaToken().withTrailingSpace() : nil
            )
        }

        /// Synthesizes a function argument from this node's declaration.
        func synthesizeFunctionArgument(needsComma: Bool = false) -> LabeledExprListSyntax.Element {
            LabeledExprListSyntax.Element(
                label: member.trimmed,
                colon: .colonToken().withTrailingSpace(),
                expression: "\(member.trimmed)" as ExprSyntax
            )
        }
    }

    /// Property tagged with `@NodeProperty`
    private struct NodePropertyDecl {
        /// The attribute that the declaration was tagged with.
        var attribute: AttributeSyntax
        /// Name of member that originated this entry.
        var member: TokenSyntax
        /// A syntax element to attach diagnostic message to.
        var diagnosticSyntax: any SyntaxProtocol

        var accessLevel: TokenSyntax?
        var comments: Trivia
        var childNodeType: ChildNodeType

        var memberName: String { member.description }
        var synthesizedName: String { String(memberName.trimmingPrefix("_")) }
        var synthesizedNameExpr: DeclReferenceExprSyntax {
            DeclReferenceExprSyntax(baseName: .identifier(synthesizedName))
        }

        static func fromDeclGroupSyntax(
            _ declaration: any DeclGroupSyntax,
            accessLevel: TokenSyntax? = nil,
            in context: MacroExpansionContext
        ) -> [Self] {

            let members = declaration.memberBlock.members
            guard !members.isEmpty else {
                return []
            }

            return members.flatMap {
                Self.fromProperty(
                    $0,
                    accessLevel: accessLevel,
                    in: context
                )
            }
        }

        static func fromProperty(
            _ member: MemberBlockItemSyntax,
            accessLevel: TokenSyntax? = nil,
            in context: MacroExpansionContext
        ) -> [Self] {

            guard let property = member.decl.as(VariableDeclSyntax.self) else {
                return []
            }
            guard
                let (attribute, attributeType) = _nodeAttribute(
                    named: _nodePropertyAttributeName,
                    from: property
                )
            else {
                return []
            }

            var result: [Self] = []

            let identifier = "@NodeRequired"

            for binding in property.bindings.ext_bindings() {
                let name = binding.name.trimmed
                guard let type = binding.type ?? attributeType else {
                    context.diagnose(
                        binding.name.ext_warningDiagnostic(
                            message: "Could not resolve type for \(identifier) binding"
                        )
                    )
                    continue
                }
                guard name.description.hasPrefix("_") else {
                    context.diagnose(
                        binding.name.ext_warningDiagnostic(
                            message: "Bindings with \(identifier) need to start with underscored name, e.g. '_\(name)'"
                        )
                    )
                    continue
                }

                let entry = Self(
                    attribute: attribute,
                    member: name,
                    diagnosticSyntax: binding.binding,
                    accessLevel: accessLevel,
                    comments: member.ext_docComments(),
                    childNodeType: .fromType(type)
                )
                result.append(entry)
            }

            return result
        }

        /// Synthesizes a function parameter from this node's declaration.
        func synthesizeFunctionParameter(needsComma: Bool = false) -> FunctionParameterSyntax {
            FunctionParameterSyntax(
                firstName: .identifier(synthesizedName),
                type: childNodeType.type.trimmed,
                trailingComma: needsComma ? .commaToken().withTrailingSpace() : nil
            )
        }

        /// Synthesizes a function argument from this node's declaration.
        func synthesizeFunctionArgument(needsComma: Bool = false) -> LabeledExprListSyntax.Element {
            LabeledExprListSyntax.Element(
                label: .identifier(synthesizedName),
                colon: .colonToken().withTrailingSpace(),
                expression: ExprSyntax(synthesizedNameExpr.trimmed)
            )
        }

        /// Synthesizes the equivalent `<value>.parent = <expr>` expression
        /// according to this node's type.
        func synthesizeAssignParent(_ expr: ExprSyntax) -> ExprSyntax {
            return childNodeType.synthesizeAssign(
                symbol: "\(member.trimmed)",
                member: .init(baseName: .identifier("parent")),
                expr
            )
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

            var isOptional: Bool {
                switch self {
                case .optional:
                    return true
                case .simple, .array:
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

            /// Synthesizes an expression that assigns `expr` to a `member` of
            /// `symbol` based on this node type.
            func synthesizeAssign(
                symbol: ExprSyntax,
                member: DeclReferenceExprSyntax,
                _ expr: ExprSyntax
            ) -> ExprSyntax {

                switch self {
                case .simple:
                    return "\(symbol).\(member) = \(expr)"
                case .array:
                    return """
                        \(symbol).forEach({
                            $0.\(member) = \(expr)
                        })
                        """
                case .optional:
                    return "\(symbol)?.\(member) = \(expr)"
                }
            }

            /// Synthesizes an expression that invokes `<member>()` on a base
            /// expression `symbol` of the type defined by this value, returning
            /// the resulting value of the call.
            func synthesizeMemberCall(
                symbol: ExprSyntax,
                member: DeclReferenceExprSyntax
            ) -> ExprSyntax {
                switch self {
                case .simple:
                    return "\(symbol).\(member)"
                case .array:
                    return """
                        \(symbol).map({
                            $0.\(member)
                        })
                        """
                case .optional:
                    return "\(symbol)?.\(member)"
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
        var overrideDeepCopyType: String?
    }
}
