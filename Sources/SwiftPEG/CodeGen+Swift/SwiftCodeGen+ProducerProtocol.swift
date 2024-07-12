// MARK: - Protocol generation

extension SwiftCodeGen {
    func generateProducerProtocol(
        _ protocolInfo: ProducerProtocolInfo
    ) throws {
        buffer.emit("public protocol \(protocolInfo.name) ")
        try buffer.emitMembersBlock {
            try generateProducerAssociatedTypes(protocolInfo)

            buffer.ensureDoubleNewline()

            try generateProducerMethods(protocolInfo)
        }

        buffer.ensureDoubleNewline()
    }

    fileprivate func generateProducerAssociatedTypes(
        _ protocolInfo: ProducerProtocolInfo
    ) throws {
        // Required associated types
        buffer.emitLine("associatedtype Mark")
        buffer.emitLine("associatedtype Token")

        buffer.ensureDoubleNewline()

        // Productions
        for rule in grammar.rules {
            guard let associatedType = protocolInfo.ruleReturnType[rule.name] else {
                continue
            }

            try generateProducerAssociatedType(associatedType)
        }
    }

    fileprivate func generateProducerAssociatedType(
        _ associatedType: CommonAbstract.IdentifierSwiftType
    ) throws {
        buffer.emitLine("associatedtype \(associatedType)")
    }

    fileprivate func generateProducerMethods(
        _ protocolInfo: ProducerProtocolInfo
    ) throws {

        // Organize methods by rule name
        let byRule: [String: [ProducerMethodInfo]] =
            Dictionary(grouping: protocolInfo.ruleProducers) {
                $0.ruleName
            }

        for rule in grammar.rules {
            guard let methods = byRule[rule.name] else {
                continue
            }

            try generateProducerMethods(
                protocolInfo,
                methods
            )

            buffer.ensureDoubleNewline()
        }
    }

    fileprivate func generateProducerMethods(
        _ protocolInfo: ProducerProtocolInfo,
        _ producerMethods: [ProducerMethodInfo]
    ) throws {
        for producerMethod in producerMethods {
            try generateProducerMethodSignature(
                protocolInfo,
                producerMethod
            )
        }
    }

    fileprivate func generateProducerMethodSignature(
        _ protocolInfo: ProducerProtocolInfo,
        _ producerMethod: ProducerMethodInfo
    ) throws {
        let parameters: [String] =
            ["_mark: Mark"] + producerMethod.bindingParameters.map { param in
                "\(param.name): \(param.type)"
            }

        buffer.emit("func \(producerMethod.methodName)")
        buffer.emit("(")
        buffer.emitWithSeparators(parameters, separator: ", ")
        buffer.emit(")")

        if producerMethod.isThrowing {
            buffer.emit(" throws")
        }

        buffer.emitLine(" -> \(producerMethod.returnType)")
    }
}

// MARK: - Default implementation generation

extension SwiftCodeGen {

    func generateDefaultProducerProtocol(
        _ protocolInfo: ProducerProtocolInfo
    ) throws {
        buffer.emit("public class Default\(protocolInfo.name)<RawTokenizer: RawTokenizerType> ")
        try buffer.emitMembersBlock {
            try generateDefaultProducerTypealiases(protocolInfo)
            try generateDefaultProducerMethods(protocolInfo)
        }

        buffer.ensureDoubleNewline()
    }

    func generateDefaultProducerTypealiases(
        _ protocolInfo: ProducerProtocolInfo
    ) throws {

        buffer.emitLine("public typealias Mark = Tokenizer<RawTokenizer>.Mark")
        buffer.emitLine("public typealias Token = Tokenizer<RawTokenizer>.Token")

        buffer.ensureDoubleNewline()

        for rule in grammar.rules {
            guard let identifier = protocolInfo.ruleReturnType[rule.name] else {
                continue
            }

            let returnType = bindingEngine.returnTypeForRule(rule).unwrapped

            buffer.emitLine("public typealias \(identifier) = \(returnType)")
        }

        buffer.ensureDoubleNewline()
    }

    func generateDefaultProducerMethods(
        _ protocolInfo: ProducerProtocolInfo
    ) throws {
        for rule in grammar.rules {
            let methods = protocolInfo.allRuleProducer(for: rule)

            for method in methods {
                try generateDefaultProducerMethod(
                    protocolInfo,
                    rule: rule,
                    method: method
                )
            }

            buffer.ensureDoubleNewline()
        }
    }

    func generateDefaultProducerMethod(
        _ protocolInfo: ProducerProtocolInfo,
        rule: InternalGrammar.Rule,
        method: ProducerMethodInfo
    ) throws {
        buffer.emit("public ")
        try generateProducerMethodSignature(protocolInfo, method)
        buffer.backtrackWhitespace()
        buffer.ensureSpaceSeparator()

        buffer.emitBlock {
            let alt = rule.alts[method.altIndex]

            if implicitReturns {
                buffer.emit("return ")
            }

            generateOnAltMatchBlockInterior(alt, rule.type)
        }
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
        func makeCallExpression() -> String {
            var result = methodName
            result += "("

            let params = ["_mark"] + bindingParameters.map(\.name)

            result += params.map {
                "\($0): \($0)"
            }.joined(separator: ", ")

            result += ")"
            return result
        }
    }
}
