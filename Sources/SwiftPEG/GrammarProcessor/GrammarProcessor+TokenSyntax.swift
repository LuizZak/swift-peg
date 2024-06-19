import MiniDigraph

extension GrammarProcessor {
    fileprivate typealias TokenDefinition = InternalGrammar.TokenDefinition

    /// Validates token syntaxes, ensuring that the identifiers contained within
    /// point to other token syntaxes, and the rules are not implemented in a
    /// recursive fashion.
    ///
    /// Returns a processed version of the tokens for emitting by a code generator.
    func validateTokenSyntaxes(
        _ tokens: [SwiftPEGGrammar.TokenDefinition]
    ) throws -> ([InternalGrammar.TokenDefinition], TokenOcclusionGraph) {

        var fragmentReferenceCount: [String: Int] =
            Dictionary(grouping: tokens.filter(\.isFragment)) {
                String($0.name.string)
            }.mapValues { _ in 0 }

        let byName = try validateTokenNames(tokens)
        let sorted = try ensureTokenSyntaxesAreNonReentrant(
            tokens,
            byName: byName,
            fragmentReferenceCount: &fragmentReferenceCount
        )

        diagnoseAltOrder(sorted)
        diagnoseUnfulfillableAtoms(sorted)

        var inlined = try applyFragmentInlining(
            sorted.reversed(),
            fragmentReferenceCount: fragmentReferenceCount
        )
        inlined = sortTokens(inlined)

        let occlusionGraph = computeTokenOcclusions(inlined)

        return (inlined, occlusionGraph)
    }

    /// Computes the relationship between static tokens and dynamic tokens that
    /// parse on the same input.
    fileprivate func computeTokenOcclusions(_ tokens: [TokenDefinition]) -> TokenOcclusionGraph {
        let nonFragmentTokens = tokens.filter { !$0.isFragment }

        var graph = DirectedGraph<String>()
        let staticTokens: [(TokenDefinition, String)] = nonFragmentTokens.compactMap { token in
            if let staticToken = token.tokenSyntax?.staticTerminal() {
                return (token, staticToken.contents)
            } else {
                return nil
            }
        }
        let staticTokensByLiteral =
            Dictionary(grouping: staticTokens, by: \.1)
            .mapValues({ $0.map(\.0) })
        let dynamicTokens = nonFragmentTokens.filter({ $0.tokenSyntax?.isStatic() == false })

        let interpreter = TokenSyntaxInterpreter(tokenDefinitions: tokens)
        for dynamicToken in dynamicTokens {
            guard let tokenSyntax = dynamicToken.tokenSyntax else {
                continue
            }

            for (literal, staticTokens) in staticTokensByLiteral {
                guard interpreter.tokenSyntaxFullyParses(tokenSyntax, input: literal) else {
                    continue
                }

                if !graph.containsNode(dynamicToken.name) {
                    graph.addNode(dynamicToken.name)
                }
                for staticToken in staticTokens {
                    if !graph.containsNode(staticToken.name) {
                        graph.addNode(staticToken.name)
                    }

                    graph.addEdge(from: dynamicToken.name, to: staticToken.name)
                }
            }
        }

        return TokenOcclusionGraph(graph: graph)
    }

    /// Sorts the input tokens by the order they should be parsed to decrease
    /// the chance of token occlusion due to parse order.
    fileprivate func sortTokens(_ tokens: [TokenDefinition]) ->  [TokenDefinition] {
        func tokenShouldPrecede(_ token1: TokenDefinition, _ token2: TokenDefinition) -> Bool {
            // Apply light sorting to favor static terminal tokens before attempting
            // dynamic tokens
            switch (token1.tokenSyntax?.staticTerminal(), token2.tokenSyntax?.staticTerminal()) {
            case (let lhs?, let rhs?):
                if lhs.contents.hasPrefix(rhs.contents) {
                    return true
                }
                if rhs.contents.hasPrefix(lhs.contents) {
                    return false
                }

                return token1.name < token2.name
            case (_?, nil):
                return true

            case (nil, _?):
                return false

            case (nil, nil):
                return token1.name < token2.name
            }
        }

        var tokens = tokens
        tokens = tokens.sorted(by: tokenShouldPrecede)

        return tokens
    }

    /// Validates that token names are not repeated; returns a dictionary mapping
    /// each token to its name.
   fileprivate  func validateTokenNames(
        _ tokens: [SwiftPEGGrammar.TokenDefinition]
    ) throws -> [String: SwiftPEGGrammar.TokenDefinition] {
        var byName: [String: SwiftPEGGrammar.TokenDefinition] = [:]

        for token in tokens {
            let name = String(token.name.string)
            if let prior = byName[name] {
                throw recordAndReturn(
                    .repeatedTokenName(
                        name,
                        token,
                        prior: prior
                    )
                )
            }

            byName[name] = token
        }

        return byName
    }

    /// Diagnoses atoms that have a combination of exclusion + terminal that cannot
    /// be fulfilled by any input.
    fileprivate func diagnoseUnfulfillableAtoms(_ tokens: [SwiftPEGGrammar.TokenDefinition]) {
        for token in tokens {
            guard let syntax = token.tokenSyntax else {
                continue
            }

            let atoms = syntax.alts.flatMap(\.items).flatMap(\.atoms)

            for atom in atoms where atom.isUnfulfillable {
                diagnostics.append(
                    .unfulfillableAtomInToken(token: token, atom)
                )
            }
        }
    }

    // TODO: Implement alt/atom order diagnostics for tokens
    /// Diagnoses alt order issues in token syntaxes
    fileprivate func diagnoseAltOrder(_ tokens: [SwiftPEGGrammar.TokenDefinition]) {
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
    ///
    /// Returns a list of tokens sorted in topological order.
    fileprivate func ensureTokenSyntaxesAreNonReentrant(
        _ tokens: [SwiftPEGGrammar.TokenDefinition],
        byName: [String: SwiftPEGGrammar.TokenDefinition],
        fragmentReferenceCount: inout [String: Int]
    ) throws -> [SwiftPEGGrammar.TokenDefinition] {

        var graph = StringDirectedGraph()

        for token in tokens {
            let name = String(token.name.string)
            graph.addNode(name)
        }

        for token in tokens {
            let collector = TokenSyntaxIdentifierCollector()
            collector.fragmentReferenceCount = fragmentReferenceCount
            collector.visit(token)

            fragmentReferenceCount = collector.fragmentReferenceCount

            guard let tokenNode = graph.nodes.first(where: { $0 == token.name.string }) else {
                break
            }

            for identifier in collector.identifiers {
                guard let referenceNode = byName[identifier] else {
                    throw recordAndReturn(
                        .unknownReferenceInToken(
                            identifier,
                            token
                        )
                    )
                }

                graph.addEdge(from: tokenNode, to: String(referenceNode.name.string))
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

                    let diagnoseCycle = cycle[nodeBack...].compactMap({
                        byName[$0]
                    })

                    throw recordAndReturn(
                        .recursivityInTokens(diagnoseCycle)
                    )
                }
            }
        }

        guard let sorted = graph.topologicalSorted() else {
            throw recordAndReturn(
                .message("Could not topologically sort tokens; this may indicate that there are cycles in the token definitions.")
            )
        }

        return sorted.compactMap { byName[$0] }
    }

    /// Applies inlining of fragments into token definitions wherever they may
    /// be possible.
    fileprivate func applyFragmentInlining(
        _ tokenDefinitions: [SwiftPEGGrammar.TokenDefinition],
        fragmentReferenceCount: [String: Int]
    ) throws -> [TokenDefinition] {

        var tokens = tokenDefinitions.map(TokenDefinition.from)

        let fragmentLookup: (String) -> TokenDefinition? = { name in
            return tokens.first(where: { $0.isFragment && $0.name == name })
        }

        for (i, token) in tokens.enumerated() {
            let rewriter = TokenFragmentInliner(fragmentLookup: fragmentLookup)

            let inlined = rewriter.visit(token)
            tokens[i] = inlined
        }

        // Remove inlined fragments by reference counting
        let visitor = TokenSyntaxIdentifierCollector()
        for fragment in tokens where fragment.isFragment {
            visitor.fragmentReferenceCount[fragment.name] = 0
        }
        visitor.visitAll(tokens)

        var unreferencedTokens = Set(
            visitor.fragmentReferenceCount
                .filter({ $0.value == 0 })
                .map(\.key)
        )
        for token in tokens where !token.isFragment {
            unreferencedTokens.remove(token.name)
        }

        tokens = tokens.filter { token in
            return !token.isFragment || !unreferencedTokens.contains(token.name)
        }

        return tokens
    }

    private class TokenSyntaxVisitor {
        func visit(_ node: SwiftPEGGrammar.TokenDefinition) {
            if let syntax = node.tokenSyntax {
                visit(syntax)
            }
        }

        func visit(_ node: InternalGrammar.TokenDefinition) {
            if let syntax = node.tokenSyntax {
                visit(syntax)
            }
        }

        func visit(_ node: CommonAbstract.TokenSyntax) {
            node.alts.forEach(visit)
        }

        func visit(_ node: CommonAbstract.TokenAlt) {
            node.items.forEach(visit)
            node.trailExclusions.forEach(visit)
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

    private class TokenFragmentInliner {
        let fragmentLookup: (String) -> TokenDefinition?

        init(
            fragmentLookup: @escaping (String) -> TokenDefinition?
        ) {
            self.fragmentLookup = fragmentLookup
        }

        func fragment(named name: String) -> TokenDefinition? {
            fragmentLookup(name)
        }

        // MARK: Inline as alt

        private func _inlinedAsAlts(_ node: TokenDefinition) -> [CommonAbstract.TokenAlt]? {
            node.tokenSyntax.flatMap(_inlinedAsAlts)
        }

        private func _inlinedAsAlts(_ node: CommonAbstract.TokenSyntax) -> [CommonAbstract.TokenAlt]? {
            var result: [CommonAbstract.TokenAlt] = []
            for alt in node.alts {
                guard let asAlt = _inlinedAsAlts(alt) else {
                    // If one alt cannot be turned into a alt, then the syntax
                    // is not alt-able
                    return nil
                }

                result.append(contentsOf: asAlt)
            }
            return result
        }

        private func _inlinedAsAlts(_ node: CommonAbstract.TokenAlt) -> [CommonAbstract.TokenAlt]? {
            // Nodes can only be turned into alts if they consist of one item
            if node.items.count > 1 {
                return [node]
            }

            var result: [CommonAbstract.TokenAlt] = []
            for item in node.items {
                guard var asAlt = _inlinedAsAlts(item) else {
                    // If one item cannot be turned into an alt, then the alt
                    // is not alt-able
                    return nil
                }
                asAlt = _appendTrailExclusions(asAlt, node)

                result.append(contentsOf: asAlt)
            }
            return result
        }

        private func _inlinedAsAlts(_ node: CommonAbstract.TokenItem) -> [CommonAbstract.TokenAlt]? {
            switch node {
            case .group(let group):
                let atoms = group.map(CommonAbstract.TokenItem.atom)
                return atoms.map {
                    CommonAbstract.TokenAlt(items: [$0], trailExclusions: [])
                }
            case .atom(let atom):
                return [
                    CommonAbstract.TokenAlt(items: [.atom(atom)], trailExclusions: [])
                ]
            default:
                return nil
            }
        }

        private func _appendTrailExclusions(_ alts: [CommonAbstract.TokenAlt], _ node: CommonAbstract.TokenAlt) -> [CommonAbstract.TokenAlt] {
            alts.map {
                .init(items: $0.items, trailExclusions: $0.trailExclusions + node.trailExclusions)
            }
        }

        // MARK: Inline as atom

        private func _inlinedAsAtoms(_ node: InternalGrammar.TokenDefinition) -> [CommonAbstract.TokenAtom]? {
            node.tokenSyntax.flatMap(_inlinedAsAtoms)
        }

        private func _inlinedAsAtoms(_ node: CommonAbstract.TokenSyntax) -> [CommonAbstract.TokenAtom]? {
            var result: [CommonAbstract.TokenAtom] = []
            for alt in node.alts {
                guard let asAtoms = _inlinedAsAtoms(alt) else {
                    // If one alt cannot be turned into an atom, then the syntax
                    // is not atom-able
                    return nil
                }

                result.append(contentsOf: asAtoms)
            }
            return result
        }

        private func _inlinedAsAtoms(_ node: CommonAbstract.TokenAlt) -> [CommonAbstract.TokenAtom]? {
            guard node.trailExclusions.isEmpty else {
                return nil
            }

            var result: [CommonAbstract.TokenAtom] = []
            for item in node.items {
                guard let asAtoms = _inlinedAsAtoms(item) else {
                    // If one item cannot be turned into an atom, then the syntax
                    // is not atom-able
                    return nil
                }

                result.append(contentsOf: asAtoms)
            }
            return result
        }

        private func _inlinedAsAtoms(_ node: CommonAbstract.TokenItem) -> [CommonAbstract.TokenAtom]? {
            switch node {
            case .group(let group):
                return group
            case .atom(let atom):
                return [atom]
            default:
                return nil
            }
        }

        // MARK: Inline as token exclusion

        private func _inlinedAsTokenExclusions(_ node: InternalGrammar.TokenDefinition) -> [CommonAbstract.TokenExclusion]? {
            node.tokenSyntax.flatMap(_inlinedAsTokenExclusions)
        }

        private func _inlinedAsTokenExclusions(_ node: CommonAbstract.TokenSyntax) -> [CommonAbstract.TokenExclusion]? {
            var result: [CommonAbstract.TokenExclusion] = []
            for alt in node.alts {
                guard let asExclusions = _inlinedAsTokenExclusions(alt) else {
                    // If one alt cannot be turned into an exclusion, then the
                    // syntax is not convertible to a sequence of exclusions
                    return nil
                }

                result.append(contentsOf: asExclusions)
            }
            return result
        }

        private func _inlinedAsTokenExclusions(_ node: CommonAbstract.TokenAlt) -> [CommonAbstract.TokenExclusion]? {
            guard node.trailExclusions.isEmpty else {
                return nil
            }
            guard node.items.count == 1 else {
                return nil
            }

            return _inlinedAsTokenExclusions(node.items[0])
        }

        private func _inlinedAsTokenExclusions(_ node: CommonAbstract.TokenItem) -> [CommonAbstract.TokenExclusion]? {
            switch node {
            case .atom(let atom):
                return _inlinedAsTokenExclusions(atom)
            case .group(let atoms) where atoms.count == 1:
                return _inlinedAsTokenExclusions(atoms[0])
            default:
                return nil
            }
        }

        private func _inlinedAsTokenExclusions(_ node: CommonAbstract.TokenAtom) -> [CommonAbstract.TokenExclusion]? {
            guard node.excluded.isEmpty else {
                return nil
            }

            return _inlinedAsTokenExclusions(node.terminal)
        }

        private func _inlinedAsTokenExclusions(_ node: CommonAbstract.TokenTerminal) -> [CommonAbstract.TokenExclusion]? {
            switch node {
            case .identifier(let identifier):
                return [.identifier(identifier)]
            case .literal(let literal):
                return [.literal(literal)]
            case .rangeLiteral(let start, let end):
                return [.rangeLiteral(start, end)]
            case .any, .characterPredicate:
                return nil
            }
        }

        // MARK: -

        func visit(_ node: InternalGrammar.TokenDefinition) -> InternalGrammar.TokenDefinition {
            var node = node
            node.tokenSyntax = node.tokenSyntax.map(visit)
            return node
        }

        func visit(_ node: CommonAbstract.TokenSyntax) -> CommonAbstract.TokenSyntax {
            var node = node
            node.alts = visit(node.alts)
            return node.flattened()
        }

        func visit(_ alts: [CommonAbstract.TokenAlt]) -> [CommonAbstract.TokenAlt] {
            // Strategy: Alts in a token syntax can be expanded by inlining alts
            // of fragments, if the fragment appears as the sole atom in an alt,
            // with no exclusions
            var result: [CommonAbstract.TokenAlt] = []
            result.reserveCapacity(alts.count)

            for alt in alts {
                let reducedAlt = visit(alt)

                switch reducedAlt.items.first {
                case .atom(let atom)
                    where reducedAlt.items.count == 1
                        && atom.excluded.isEmpty:
                    if
                        case .identifier(let identifier) = atom.terminal,
                        let fragment = self.fragment(named: identifier),
                        let alts = _inlinedAsAlts(fragment)
                    {
                        result.append(contentsOf: _appendTrailExclusions(alts, alt))
                    } else {
                        result.append(reducedAlt)
                    }
                default:
                    result.append(reducedAlt)
                }
            }

            return result
        }

        func visit(_ node: CommonAbstract.TokenAlt) -> CommonAbstract.TokenAlt {
            var node = node
            node.items = node.items.flatMap(visit)
            node.trailExclusions = node.trailExclusions.flatMap(visit)
            return node.flattened()
        }

        func visit(_ node: CommonAbstract.TokenItem) -> [CommonAbstract.TokenItem] {
            switch node {
            case .zeroOrMore(let atoms):
                return [.zeroOrMore(visit(atoms))]

            case .oneOrMore(let atoms):
                return [.oneOrMore(visit(atoms))]

            case .optionalGroup(let atoms):
                return [.optionalGroup(visit(atoms))]

            case .group(let atoms):
                return [group(visit(atoms))]

            case .optionalAtom(let atom):
                return [.optionalAtom(visit(atom))]

            case .atom(let atom):
                return [.atom(visit(atom))]
            }
        }

        func group(_ atoms: [CommonAbstract.TokenAtom]) -> CommonAbstract.TokenItem {
            return .group(atoms).flattened()
        }

        func visit(_ atoms: [CommonAbstract.TokenAtom]) -> [CommonAbstract.TokenAtom] {
            // Strategy: Atoms in a group-like item can be expanded by inlining
            // alts of fragments, if the fragment appears with no exclusions.
            // The fragment cannot contain structures that are not nestable in an
            // atom group, such as repetitions or optionals.
            var result: [CommonAbstract.TokenAtom] = []
            result.reserveCapacity(atoms.count)

            for atom in atoms {
                let reducedAtom = visit(atom)

                guard reducedAtom.excluded.isEmpty else {
                    result.append(reducedAtom)
                    continue
                }

                switch reducedAtom.terminal {
                case .identifier(let identifier):
                    guard let fragment = self.fragment(named: identifier) else {
                        break
                    }
                    guard let inlined = _inlinedAsAtoms(fragment) else {
                        break
                    }

                    result.append(contentsOf: inlined)
                    continue
                default:
                    break
                }

                result.append(reducedAtom)
            }

            return result
        }

        func visit(_ node: CommonAbstract.TokenAtom) -> CommonAbstract.TokenAtom {
            var node = node
            node.excluded = node.excluded.flatMap(visit)
            node.terminal = visit(node.terminal)
            return node
        }

        func visit(_ node: CommonAbstract.TokenExclusion) -> [CommonAbstract.TokenExclusion] {
            switch node {
            case .identifier(let identifier):
                guard let fragment = self.fragment(named: identifier) else {
                    return [node]
                }
                guard let fragmentSyntax = fragment.tokenSyntax else {
                    return [node]
                }

                if let staticTerminal = fragmentSyntax.staticTerminal() {
                    return [.literal(staticTerminal)]
                }

                // If an exclusion is a sequence of alts with simple terminals,
                // unpack the terminals and return them
                if let expanded = _inlinedAsTokenExclusions(fragment) {
                    return expanded
                }

                return [node]
            default:
                return [node]
            }
        }

        func visit(_ node: CommonAbstract.TokenTerminal) -> CommonAbstract.TokenTerminal {
            switch node {
            case .identifier(let identifier):
                guard let fragment = self.fragment(named: identifier) else {
                    return node
                }

                if let terminal = fragment.tokenSyntax?.asTerminal() {
                    return terminal
                }

                return node
            default:
                return node
            }
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
        var fragmentReferenceCount: [String: Int] = [:]

        func visitAll(_ tokens: [SwiftPEGGrammar.TokenDefinition]) {
            for token in tokens {
                visit(token)
            }
        }

        func visitAll(_ tokens: [TokenDefinition]) {
            for token in tokens {
                visit(token)
            }
        }

        override func visit(_ node: CommonAbstract.TokenExclusion) {
            switch node {
            case .identifier(let identifier):
                incrementReference(identifier)

                identifiers.insert(identifier)
            case .literal, .rangeLiteral:
                break
            }
        }

        override func visit(_ node: CommonAbstract.TokenTerminal) {
            switch node {
            case .identifier(let identifier):
                incrementReference(identifier)

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

        private func incrementReference(_ identifier: String) {
            if let count = fragmentReferenceCount[identifier] {
                fragmentReferenceCount[identifier] = count + 1
            }
        }
    }

    private class TokenSyntaxPrefixChecker {
        func isTokenPrefix(_ lhs: TokenDefinition, of rhs: TokenDefinition) -> Bool {
            guard let lhsSyntax = lhs.tokenSyntax, let rhsSyntax = rhs.tokenSyntax else {
                return false
            }

            return lhsSyntax.isPrefix(of: rhsSyntax)
        }
    }
}
