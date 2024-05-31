extension GrammarProcessor {
    /// Validates token syntaxes, ensuring that the identifiers contained within
    /// point to other token syntaxes, and the rules are not implemented in a
    /// recursive fashion.
    ///
    /// Returns a processed version of the tokens for emitting by a code generator.
    func validateTokenSyntaxes(
        _ tokens: [SwiftPEGGrammar.TokenDefinition]
    ) throws -> [InternalGrammar.TokenDefinition] {

        let byName = try validateTokenNames(tokens)
        try ensureTokenSyntaxesAreNonReentrant(tokens, byName: byName)

        diagnoseAltOrder(tokens)

        return tokens.map(InternalGrammar.TokenDefinition.from)
    }

    /// Validates that token names are not repeated; returns a dictionary mapping
    /// each token to its name.
    func validateTokenNames(
        _ tokens: [SwiftPEGGrammar.TokenDefinition]
    ) throws -> [String: SwiftPEGGrammar.TokenDefinition] {
        var byName: [String: SwiftPEGGrammar.TokenDefinition] = [:]

        for token in tokens {
            let name = String(token.name.string)
            if let prior = byName[name] {
                throw recordAndReturn(GrammarProcessorError.repeatedTokenName(
                    name,
                    token,
                    prior: prior
                ))
            }

            byName[name] = token
        }

        return byName
    }

    // TODO: Implement alt/atom order diagnostics for tokens
    /// Diagnoses alt order issues in token syntaxes
    func diagnoseAltOrder(_ tokens: [SwiftPEGGrammar.TokenDefinition]) {
#if false
        func inspect(token: SwiftPEGGrammar.TokenDefinition, _ alts: [CommonAbstract.TokenAlt]) {

        }
        func inspect(token: SwiftPEGGrammar.TokenDefinition, _ atoms: [CommonAbstract.TokenAtom]) {

        }

        for token in tokens {
            let collector = TokenAltCollector()
            collector.visit(token)

            for alts in collector.alts {
                inspect(token: token, alts)
            }
            for atoms in collector.grouped {
                inspect(token: token, atoms)
            }
        }
#endif
    }

    /// Token syntaxes do not support reentrance; they cannot be recursive on
    /// any level, either directly or indirectly.
    func ensureTokenSyntaxesAreNonReentrant(
        _ tokens: [SwiftPEGGrammar.TokenDefinition],
        byName: [String: SwiftPEGGrammar.TokenDefinition]
    ) throws {
        typealias Graph = GenericDirectedGraph<SwiftPEGGrammar.TokenDefinition>

        let graph = Graph()
        var nodesByName: [String: Graph.Node] = [:]

        for token in tokens {
            let name = String(token.name.string)
            nodesByName[name] = graph.addNode(token)
        }

        for token in tokens {
            let collector = TokenSyntaxIdentifierCollector()
            collector.visit(token)

            guard let tokenNode = graph.nodes.first(where: { $0.value === token }) else {
                break
            }

            for identifier in collector.identifiers {
                guard let referenceNode = nodesByName[identifier] else {
                    throw recordAndReturn(GrammarProcessorError.unknownReferenceInToken(
                        identifier,
                        token
                    ))
                }

                graph.addEdge(from: tokenNode, to: referenceNode)
            }
        }

        let components = graph.stronglyConnectedComponents()
        for component in components where component.count > 1 {
            // Find the cycle to diagnose
            for node in component {
                for cycle in graph.findCycles(from: node) where cycle.count > 1 {
                    guard let nodeBack = cycle.firstIndex(of: cycle[cycle.count - 1]) else {
                        continue
                    }

                    let diagnoseCycle = cycle[nodeBack...]
                    throw recordAndReturn(
                        GrammarProcessorError.recursivityInTokens(diagnoseCycle.map(\.value))
                    )
                }
            }
        }
    }

    private class TokenSyntaxVisitor {
        func visit(_ node: SwiftPEGGrammar.TokenDefinition) {
            if let syntax = node.tokenSyntax {
                visit(syntax)
            }
        }

        func visit(_ node: CommonAbstract.TokenSyntax) {
            node.alts.forEach(visit)
        }

        func visit(_ node: CommonAbstract.TokenAlt) {
            node.items.forEach(visit)
        }

        func visit(_ node: CommonAbstract.TokenItem) {
            node.atoms.forEach(visit)
        }

        func visit(_ node: CommonAbstract.TokenAtom) {
            node.excluded.forEach(visit)
            visit(node.terminal)
        }

        func visit(_ node: CommonAbstract.TokenExclusion) {
        }

        func visit(_ node: CommonAbstract.TokenTerminal) {
        }
    }

    private class TokenAltCollector: TokenSyntaxVisitor {
        var alts: [[CommonAbstract.TokenAlt]] = []
        var grouped: [[CommonAbstract.TokenAtom]] = []

        override func visit(_ node: CommonAbstract.TokenSyntax) {
            super.visit(node)

            alts.append(node.alts)
        }

        override func visit(_ node: CommonAbstract.TokenItem) {
            super.visit(node)

            switch node {
            case .group(let atoms), .oneOrMore(let atoms), .zeroOrMore(let atoms):
                if atoms.count > 1 {
                    grouped.append(atoms)
                }
            default:
                break
            }
        }
    }

    private class TokenSyntaxIdentifierCollector: TokenSyntaxVisitor {
        var identifiers: Set<String> = []

        override func visit(_ node: CommonAbstract.TokenExclusion) {
            switch node {
            case .identifier(let identifier):
                identifiers.insert(identifier)
            case .string:
                break
            }
        }

        override func visit(_ node: CommonAbstract.TokenTerminal) {
            switch node {
            case .identifier(let identifier):
                identifiers.insert(identifier)
            case .literal:
                break
            case .rangeLiteral:
                break
            case .characterPredicate:
                break
            case .any:
                break
            }
        }
    }
}
