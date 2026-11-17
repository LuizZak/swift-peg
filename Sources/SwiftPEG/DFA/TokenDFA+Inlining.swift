extension TokenDFA {
    /// Applies inlining of terminals that refer to token definitions within this
    /// token DFA.
    func inline(_ tokenDefinitions: [InternalGrammar.TokenDefinition]) {
        let lookup: InliningLookup = InliningLookup()
        for token in tokenDefinitions {
            lookup[token.name] = token
        }

        inline(lookup)
    }

    /// Applies inlining of terminals that refer to token definitions within this
    /// token DFA.
    func inline(_ lookup: InliningLookup) {
        for edge in edges {
            attemptInline(edge, lookup)
        }
    }

    class InliningLookup {
        private var byName: [String: InternalGrammar.TokenDefinition] = [:]
        private var dfaCache: [String: TokenDFA?] = [:]

        subscript(name: String) -> InternalGrammar.TokenDefinition? {
            get { byName[name] }
            set { byName[name] = newValue }
        }

        func cacheAll(_ tokens: [InternalGrammar.TokenDefinition]) {

        }

        func dfaForToken(_ token: InternalGrammar.TokenDefinition) -> TokenDFA? {
            if let cached = dfaCache[token.name] {
                return cached
            }

            let dfa = TokenDFA.from(token)
            dfaCache[token.name] = dfa

            return dfa
        }
    }
}

fileprivate extension TokenDFA {
    func attemptInline(_ edge: Edge, _ lookup: InliningLookup) {
        switch edge.condition {
        case .terminal(.identifier(let ident)):
            guard let token = lookup[ident] else {
                return
            }

            attemptInline(edge, token, lookup)

        default:
            break
        }
    }

    func attemptInline(
        _ edge: Edge,
        _ token: InternalGrammar.TokenDefinition,
        _ lookup: InliningLookup
    ) {
        guard let dfa = lookup.dfaForToken(token) else {
            return
        }

        let exitNodes = dfa.nodes.filter({ $0.isAccept })

        // DFA requires accept states
        guard !exitNodes.isEmpty else {
            return
        }

        // Find start node
        guard let startNode = dfa.findNode(dfa.startNodeId) else {
            return
        }
        let incomingEdges = dfa.edges(from: startNode)
        let outgoingEdges = exitNodes.flatMap(dfa.edges(towards:))

        // Splice the graph in, connecting the accept states to the end point of
        // this node
        let idOffset = merge(with: dfa)
        let startNodeAdjusted = startNode.id + idOffset
        let exitNodesAdjusted = exitNodes.map({ node in
            return node.id + idOffset
        })

        func shouldRemove(_ nodeId: Int) -> Bool {
            if nodeId == startNodeAdjusted {
                return true
            }
            if exitNodesAdjusted.contains(nodeId) {
                return true
            }

            return false
        }

        edges.remove(edge)
        edges = edges.filter { e in
            if shouldRemove(e.start) || shouldRemove(e.end) {
                false
            } else {
                true
            }
        }
        nodes = nodes.filter { node in
            !shouldRemove(node.id)
        }

        // If an edge is present in both incoming and outgoing edges, then it is
        // a direct connection between start and accept nodes
        let direct = incomingEdges.intersection(outgoingEdges)

        for direct in direct {
            addEdge(from: edge.start, to: edge.end, condition: direct.condition)
        }

        for incomingEdge in incomingEdges where !direct.contains(incomingEdge) {
            addEdge(from: edge.start, to: incomingEdge.end + idOffset, condition: incomingEdge.condition)
        }
        for outgoingEdge in outgoingEdges where !direct.contains(outgoingEdge) {
            addEdge(from: outgoingEdge.start + idOffset, to: edge.end, condition: outgoingEdge.condition)
        }
    }

    /// Merges the contents of the nodes and edges of another DFA into this DFA.
    ///
    /// The starting node ID of `self` remains the same, and the incoming nodes
    /// and edges have their ID shifted to avoid collisions with existing IDs
    /// within this graph.
    ///
    /// Returns the numerical offset applied to the ID of all incoming nodes and
    /// edges.
    func merge(with other: TokenDFA) -> Int {
        let maxId = self.nodes.map(\.id).max() ?? -1
        let idOffset = maxId + 1

        for var node in other.nodes {
            node.id += idOffset
            nodes.insert(node)
        }
        for var edge in other.edges {
            edge.start += idOffset
            edge.end += idOffset
            edges.insert(edge)
        }

        return idOffset
    }
}
