import MiniDigraph

extension TokenDFA {
    static func from(_ syntax: CommonAbstract.TokenSyntax) -> TokenDFA {
        let dfa = TokenDFA()

        let result = dfa.build(syntax)
        dfa.populate(from: result)

        return dfa
    }

    fileprivate func populate(from finalized: FinalizedDFA) {
        // Assign IDs
        var idMap: [RealizableNode: Int] = [:]

        for node in finalized.nodes {
            idMap[node] = addNode(isAccept: node.isAccept).id
        }
        for edge in finalized.edges {
            guard let start = idMap[edge.start] else {
                fatalError("\(#function): Found edge labeled \(edge.label) referring to nodes not listed in finalized DFA?")
            }
            guard let end = idMap[edge.end] else {
                fatalError("\(#function): Found edge labeled \(edge.label) referring to nodes not listed in finalized DFA?")
            }

            addEdge(from: start, to: end, label: edge.label)
        }
    }
}

fileprivate extension TokenDFA {
    func build(_ syntax: CommonAbstract.TokenSyntax) -> FinalizedDFA {
        let start = RealizableNode()

        var result = FinalizedDFA(nodes: [start], edges: [])

        for alt in syntax.alts {
            result = result.merged(with: build(alt, entry: start))
        }

        return result
    }

    func build(_ alt: CommonAbstract.TokenAlt, entry: RealizableNode) -> FinalizedDFA {
        var result = FinalizedDFA(nodes: [], edges: [])

        enum State {
            case initial(RealizableNode)
            case ongoing(RealizableNode, [PendingEdge])
        }

        func addEdge(_ start: RealizableNode, _ end: RealizableNode, label: String) {
            result.edges.append(
                makeRealizedEdge(from: start, to: end, label: label)
            )
        }
        func addEdges(_ start: RealizableNode, _ end: RealizableNode, labels: [String]) {
            for label in labels {
                addEdge(start, end, label: label)
            }
        }
        func addEdgesAndNodes(_ start: RealizableNode, _ end: PartialDFA, _ pending: [PendingEdge]) -> (RealizableNode, [PendingEdge]) {

            switch end {
            case .zeroOrMore(_, let node):
                for label in end.labels {
                    addEdge(start, start, label: label)
                }

                if node.isAccept {
                    start.isAccept = true
                }

                return (start, [])

            case .oneOrMore(_, let node):
                result.nodes.append(end.node)

                for label in end.labels {
                    addEdge(start, node, label: label)
                    addEdge(node, node, label: label)
                }

                return (node, [])

            case .optional:
                result.nodes.append(end.node)

                for label in end.labels {
                    addEdge(start, end.node, label: label)
                }

                for pending in pending {
                    addEdge(pending.start, end.node, label: pending.label ?? end.labels[0])
                }

                return (end.node, [.init(start: start, label: nil)])

            default:
                result.nodes.append(end.node)

                for label in end.labels {
                    addEdge(start, end.node, label: label)
                }

                for pending in pending {
                    addEdge(pending.start, end.node, label: pending.label ?? end.labels[0])
                }

                return (end.node, [])
            }
        }

        var state = State.initial(entry)

        for (i, item) in alt.items.enumerated() {
            let next = build(item)
            if i == alt.items.count - 1 {
                next.node.isAccept = true
            }

            switch state {
            case .initial(let node):
                let result = addEdgesAndNodes(node, next, [])
                state = .ongoing(result.0, result.1)

            case .ongoing(let node, let pending):
                let result = addEdgesAndNodes(node, next, pending)
                state = .ongoing(result.0, result.1)
            }
        }

        return result
    }

    func build(_ item: CommonAbstract.TokenItem) -> PartialDFA {
        switch item {
        case .atom(let atom):
            let node = makeRealizableNode()

            return .terminal(label: atom.terminal.description, node)

        case .group(let atoms):
            let node = makeRealizableNode()

            return .choice(labels: atoms.map(\.terminal.description), node)

        case .optionalAtom(let atom):
            let node = makeRealizableNode()

            return .optional(
                labels: [atom.terminal.description],
                node
            )

        case .optionalGroup(let atoms):
            let node = makeRealizableNode()

            return .optional(
                labels: atoms.map(\.terminal.description),
                node
            )

        case .zeroOrMore(let atoms):
            let node = makeRealizableNode()

            return .zeroOrMore(
                labels: atoms.map(\.terminal.description),
                node
            )

        case .oneOrMore(let atoms):
            let node = makeRealizableNode()

            return .oneOrMore(
                labels: atoms.map(\.terminal.description),
                node
            )
        }
    }

    func makeRealizableNode() -> RealizableNode {
        RealizableNode()
    }

    func makeRealizedEdge(
        from start: RealizableNode,
        to end: RealizableNode,
        label: String
    ) -> RealizedEdge {

        RealizedEdge(start: start, end: end, label: label)
    }
}

fileprivate extension TokenDFA {
    struct FinalizedDFA {
        var nodes: [RealizableNode]
        var edges: [RealizedEdge]

        func merged(with other: Self) -> Self {
            Self(
                nodes: nodes + other.nodes,
                edges: edges + other.edges
            )
        }
    }

    enum PartialDFA {
        case terminal(label: String, RealizableNode)
        case choice(labels: [String], RealizableNode)
        case optional(labels: [String], RealizableNode)
        case zeroOrMore(labels: [String], RealizableNode)
        case oneOrMore(labels: [String], RealizableNode)

        var labels: [String] {
            switch self {
            case .terminal(let label, _):
                return [label]

            case .choice(let labels, _):
                return labels

            case .oneOrMore(let labels, _):
                return labels

            case .optional(let labels, _):
                return labels

            case .zeroOrMore(let labels, _):
                return labels
            }
        }

        var node: RealizableNode {
            switch self {
            case .choice(_, let node):
                return node

            case .oneOrMore(_, let node):
                return node

            case .optional(_, let node):
                return node

            case .terminal(_, let node):
                return node

            case .zeroOrMore(_, let node):
                return node
            }
        }
    }

    class RealizableNode: Hashable {
        var isAccept: Bool = false

        func hash(into hasher: inout Hasher) {
            hasher.combine(ObjectIdentifier(self))
        }

        static func == (lhs: RealizableNode, rhs: RealizableNode) -> Bool {
            lhs === rhs
        }
    }

    /// An edge that has been realized at both ends.
    struct RealizedEdge {
        var start: RealizableNode
        var end: RealizableNode
        var label: String
    }

    struct PendingEdge {
        var start: RealizableNode
        var label: String?
    }
}
