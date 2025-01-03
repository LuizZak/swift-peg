import SwiftAST

// MARK: - Protocol generation

extension SwiftCodeGen {
    func generateProducerProtocol(
        _ protocolInfo: ProducerProtocolInfo
    ) throws -> ProtocolDecl {
        let associatedTypes = try generateProducerAssociatedTypes(protocolInfo)
        let functions = try generateProducerMethods(protocolInfo)

        let members: [ProtocolMemberDecl] = functions.map { .function($0) }

        return .init(
            leadingComments: [],
            accessLevel: .public,
            name: protocolInfo.name,
            associatedTypes: associatedTypes,
            members: members
        )
    }

    fileprivate func generateProducerAssociatedTypes(
        _ protocolInfo: ProducerProtocolInfo
    ) throws -> [AssociatedTypeDecl] {
        var result: [AssociatedTypeDecl] = []

        // Required associated types
        result.append(.init(name: "Mark"))
        result.append(.init(name: "Token"))

        // Productions
        for rule in grammar.rules {
            guard let associatedType = protocolInfo.ruleReturnType[rule.name] else {
                continue
            }

            let associatedTypeDecl = try generateProducerAssociatedType(associatedType)

            result.append(associatedTypeDecl)
        }

        return result
    }

    fileprivate func generateProducerAssociatedType(
        _ associatedType: CommonAbstract.IdentifierSwiftType
    ) throws -> AssociatedTypeDecl {
        return .init(name: associatedType.description)
    }

    fileprivate func generateProducerMethods(
        _ protocolInfo: ProducerProtocolInfo
    ) throws -> [ProtocolFunctionMemberDecl] {

        var result: [ProtocolFunctionMemberDecl] = []

        // Organize methods by rule name
        let byRule: [String: [ProducerMethodInfo]] =
            Dictionary(grouping: protocolInfo.ruleProducers) {
                $0.ruleName
            }

        for rule in grammar.rules {
            guard let methods = byRule[rule.name] else {
                continue
            }

            let ruleMethods = try generateProducerMethods(
                protocolInfo,
                methods
            )

            result.append(contentsOf: ruleMethods)
        }

        return result
    }

    fileprivate func generateProducerMethods(
        _ protocolInfo: ProducerProtocolInfo,
        _ producerMethods: [ProducerMethodInfo]
    ) throws -> [ProtocolFunctionMemberDecl] {
        var result: [ProtocolFunctionMemberDecl] = []

        for producerMethod in producerMethods {
            let signature = try generateProducerMethodSignature(
                protocolInfo,
                producerMethod
            )

            result.append(
                .init(
                    leadingComments: [],
                    signature: signature
                )
            )
        }

        return result
    }

    fileprivate func generateProducerMethodSignature(
        _ protocolInfo: ProducerProtocolInfo,
        _ producerMethod: ProducerMethodInfo
    ) throws -> FunctionSignature {
        var parameters: [ParameterSignature] = []

        parameters.append(.init(label: "_mark", name: "_mark", type: "Mark"))
        for param in producerMethod.bindingParameters {
            let label = escapeIdentifier(param.name)

            parameters.append(
                .init(label: label, name: label, type: param.type.asSwiftASTType)
            )
        }

        return FunctionSignature(
            attributes: [],
            name: producerMethod.methodName,
            parameters: parameters,
            returnType: .nominal(producerMethod.returnType.asSwiftASTType),
            traits: [.throwing]
        )
    }
}

// MARK: - Default implementation generation

extension SwiftCodeGen {

    func generateDefaultProducerImplementationInfo() -> ProducerProtocolImplementationInfo {
        ProducerProtocolImplementationInfo(
            kind: .default
        )
    }

    func generateVoidProducerImplementationInfo() -> ProducerProtocolImplementationInfo {
        ProducerProtocolImplementationInfo(
            kind: .void
        )
    }

    func generateDefaultProducerProtocol(
        _ protocolInfo: ProducerProtocolInfo,
        _ implementationInfo: ProducerProtocolImplementationInfo
    ) throws -> ClassDecl {

        let prefix: String
        switch implementationInfo.kind {
        case .default:
            prefix = "Default"

        case .void:
            prefix = "Void"
        }

        var members: [MemberDecl] = []
        members.append(
            contentsOf: try generateDefaultProducerTypealiases(
                protocolInfo,
                implementationInfo
            ).map { .typealias($0) }
        )
        members.append(
            contentsOf: try generateDefaultProducerMethods(
                protocolInfo,
                implementationInfo
            ).map { .function($0) }
        )

        return .init(
            leadingComments: [],
            accessLevel: .public,
            name: "\(prefix)\(protocolInfo.name)",
            genericArguments: [
                .init(name: "RawTokenizer", type: "RawTokenizerType")
            ],
            inheritances: [
                .typeName(protocolInfo.name)
            ],
            members: members
        )
    }

    func generateDefaultProducerTypealiases(
        _ protocolInfo: ProducerProtocolInfo,
        _ implementationInfo: ProducerProtocolImplementationInfo
    ) throws -> [TypealiasDecl] {

        var result: [TypealiasDecl] = []

        // public typealias Mark = Tokenizer<RawTokenizer>.Mark
        result.append(
            .init(
                accessLevel: .public,
                alias: "Mark",
                type: .nested(
                    .init(
                        base: .generic("Tokenizer", parameters: ["RawTokenizer"]),
                        nested: .typeName("Mark")
                    )
                )
            )
        )
        // public typealias Token = Tokenizer<RawTokenizer>.Token
        result.append(
            .init(
                accessLevel: .public,
                alias: "Token",
                type: .nested(
                    .init(
                        base: .generic("Tokenizer", parameters: ["RawTokenizer"]),
                        nested: .typeName("Token")
                    )
                )
            )
        )

        for rule in grammar.rules {
            guard let identifier = protocolInfo.ruleReturnType[rule.name] else {
                continue
            }

            let returnType: CommonAbstract.SwiftType
            switch implementationInfo.kind {
            case .default:
                returnType = bindingEngine.returnTypeForRule(rule).unwrapped

            case .void:
                returnType = "Void"
            }

            result.append(
                .init(
                    accessLevel: .public,
                    alias: identifier.description,
                    type: returnType.asSwiftASTType
                )
            )
        }

        return result
    }

    func generateDefaultProducerMethods(
        _ protocolInfo: ProducerProtocolInfo,
        _ implementationInfo: ProducerProtocolImplementationInfo
    ) throws -> [FunctionMemberDecl] {
        var result: [FunctionMemberDecl] = []

        for rule in grammar.rules {
            let methods = protocolInfo.allRuleProducer(for: rule)

            for method in methods {
                let decl = try generateDefaultProducerMethod(
                    protocolInfo,
                    implementationInfo,
                    rule: rule,
                    method: method
                )

                result.append(decl)
            }
        }

        return result
    }

    func generateDefaultProducerMethod(
        _ protocolInfo: ProducerProtocolInfo,
        _ implementationInfo: ProducerProtocolImplementationInfo,
        rule: InternalGrammar.Rule,
        method: ProducerMethodInfo
    ) throws -> FunctionMemberDecl {
        var signature = try generateProducerMethodSignature(protocolInfo, method)
        signature.attributes.append(.init(name: "inlinable"))

        let body: CompoundStatement = []

        switch implementationInfo.kind {
        case .default:
            let alt = rule.alts[method.altIndex]

            let exp = generateOnAltMatchBlockInterior(alt, rule.type)

            if implicitReturns {
                body.appendStatement(
                    .return(exp)
                )
            } else {
                body.appendStatement(
                    .expression(exp)
                )
            }

        case .void:
            break
        }

        return .init(
            leadingComments: [],
            accessLevel: .public,
            signature: signature,
            body: body
        )
    }
}

// MARK: - Commons

extension SwiftCodeGen {
    /// How '<Parser>Producer' is expected to be parameterized within the parser
    /// subclass.
    func producerGenericParameterName() -> String {
        return "Producer"
    }

    /// How '<Parser>Producer' is expected to be referenced as a member of the
    /// parser subclass.
    func producerMemberName() -> String {
        return "_producer"
    }

    /// Generates a producer protocol info for the current grammar loaded.
    func producerProtocolInfo() -> ProducerProtocolInfo {
        var ruleReturnAliases: [String: CommonAbstract.IdentifierSwiftType] = [:]

        for rule in grammar.rules {
            ruleReturnAliases[rule.name] = producerAssociatedTypeAsSwiftType(for: rule)
        }

        let aliasedBindingEngine = bindingEngine
            .withCustomRuleResultMappings(ruleReturnAliases.mapValues {
                CommonAbstract.SwiftType.nominal($0)
            })

        var result: ProducerProtocolInfo = .init(
            name: producerProtocolName(),
            ruleReturnType: ruleReturnAliases,
            ruleProducers: []
        )

        for rule in grammar.rules {
            let producers = producerRuleMethods(
                for: rule,
                bindingEngine: aliasedBindingEngine
            )

            result.ruleProducers.append(contentsOf: producers)
        }

        return result
    }

    func producerProtocolName() -> String {
        "\(parserName)Producer"
    }

    func producerAssociatedType(for rule: InternalGrammar.Rule) -> String {
        let name = rule.name.uppercasedFirstLetter

        return "\(name)Production"
    }

    func producerAssociatedTypeAsSwiftType(for rule: InternalGrammar.Rule) -> CommonAbstract.IdentifierSwiftType {
        return .init(identifier: producerAssociatedType(for: rule))
    }

    func producerRuleMethods(
        for rule: InternalGrammar.Rule,
        bindingEngine: BindingEngine
    ) -> [ProducerMethodInfo] {

        let results = (0..<rule.alts.count).map { altIndex in
            producerRuleMethodInfo(
                for: rule,
                altIndex: altIndex,
                bindingEngine: bindingEngine
            )
        }

        return results
    }

    func producerRuleMethodInfo(
        for rule: InternalGrammar.Rule,
        altIndex: Int,
        bindingEngine: BindingEngine
    ) -> ProducerMethodInfo {
        let alt = rule.alts[altIndex]

        let ruleResult = producerAssociatedTypeAsSwiftType(for: rule)

        let methodName = "produce_\(rule.name)_alt\(altIndex + 1)"

        var result = ProducerMethodInfo(
            ruleName: rule.name,
            altIndex: altIndex,
            methodName: methodName,
            bindingParameters: [],
            returnType: ruleResult,
            action: alt.action
        )

        declContext.push()
        defer { declContext.pop() }

        declContext.defineLocal(suggestedName: "_mark", type: .marker)

        let bindings = bindingEngine.computeBindings(alt)

        for binding in bindings {
            guard let label = binding.label else {
                continue
            }

            let deduplicated = declContext.defineLocal(
                suggestedName: label
            )

            result.bindingParameters.append(
                .init(name: deduplicated.name, type: binding.type)
            )
        }

        return result
    }

    /// Information for generating '<ParserName>Producer' protocol implementation
    /// types.
    struct ProducerProtocolImplementationInfo {
        /// The kind of producer to generate.
        var kind: ProducerKind

        /// Specifies the kind of producer to generate with code-generation
        /// methods.
        enum ProducerKind {
            /// Specifies the default producer to generate, or the producer that
            /// contains the alt actions.
            case `default`

            /// Specifies a `Void` producer to generate.
            case void
        }
    }

    /// Information for '<ParserName>Producer' protocol to be generated.
    struct ProducerProtocolInfo {
        /// Name of protocol.
        var name: String

        /// Aliasing information for rules, mapping each rule to an associatedtype
        /// within the generated protocol.
        var ruleReturnType: [String: CommonAbstract.IdentifierSwiftType]

        /// List of rule alternative production methods.
        var ruleProducers: [ProducerMethodInfo]

        func allRuleProducer(for rule: InternalGrammar.Rule) -> [ProducerMethodInfo] {
            allRuleProducer(forRuleName: rule.name)
        }

        func allRuleProducer(forRuleName ruleName: String) -> [ProducerMethodInfo] {
            ruleProducers.filter { $0.ruleName == ruleName }
        }

        func ruleProducer(forRuleName ruleName: String, altIndex: Int) -> ProducerMethodInfo? {
            ruleProducers.first { $0.ruleName == ruleName && $0.altIndex == altIndex }
        }
    }

    /// Information for '<ParserName>Producer' protocol methods to be generated.
    struct ProducerMethodInfo {
        var ruleName: String
        var altIndex: Int

        var methodName: String
        /// Parameters, as bindings taken by the method.
        var bindingParameters: [Parameter]
        var isThrowing: Bool = true
        var returnType: CommonAbstract.IdentifierSwiftType
        var action: InternalGrammar.Action?

        struct Parameter {
            var name: String
            var type: CommonAbstract.SwiftType
        }

        /// Makes a standard call expression that invokes this producer method,
        /// assuming all parameters are available as equal identifiers in the
        /// call site.
        func _makeCallExpression(
            _ lead: Expression,
            _ escapeIdentifier: (String, _ isLabel: Bool) -> String
        ) -> Expression {
            let params = ["_mark"] + bindingParameters.map(\.name)

            return lead.dot(methodName)
                .call(params.map { param in
                    FunctionArgument(label: escapeIdentifier(param, true), expression: .identifier(escapeIdentifier(param, false)))
                })
        }
    }
}
