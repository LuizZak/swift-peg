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
        let start = makeRealizableNode(isAccept: false)

        var result = FinalizedDFA(nodes: [start], edges: [])

        for alt in syntax.alts {
            result = result.merged(with: build(alt, entry: start))
        }

        return result
    }

    func build(_ alt: CommonAbstract.TokenAlt, entry: RealizableNode) -> FinalizedDFA {
        var result = FinalizedDFA(nodes: [], edges: [])

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
        func makeNode(isAccept: Bool) -> RealizableNode {
            let node = makeRealizableNode(isAccept: isAccept)
            result.nodes.append(node)
            return node
        }
        func addEdgesAndNodes(
            _ start: RealizableNode,
            _ end: PartialDFA,
            _ pending: [PendingEdge],
            isAccept: Bool
        ) -> (RealizableNode, [PendingEdge]) {

            switch end {
            case .zeroOrMore(let labels):
                for label in labels {
                    addEdge(start, start, label: label)
                }

                for pending in pending {
                    addEdge(pending.start, start, label: pending.label ?? end.labels[0])
                }

                if isAccept {
                    start.isAccept = true
                }

                return (start, [])

            case .oneOrMore(let labels):
                let node = makeNode(isAccept: isAccept)

                for label in labels {
                    addEdge(start, node, label: label)
                    addEdge(node, node, label: label)
                }

                for pending in pending {
                    addEdge(pending.start, node, label: pending.label ?? end.labels[0])
                }

                return (node, [])

            case .optional(let labels):
                let node = makeNode(isAccept: isAccept)

                for label in labels {
                    addEdge(start, node, label: label)
                }

                for pending in pending {
                    addEdge(pending.start, node, label: pending.label ?? labels[0])
                }

                return (node, [.init(start: start, label: nil)])

            default:
                let node = makeNode(isAccept: isAccept)

                for label in end.labels {
                    addEdge(start, node, label: label)
                }

                for pending in pending {
                    addEdge(pending.start, node, label: pending.label ?? end.labels[0])
                }

                return (node, [])
            }
        }

        var latest = entry
        var pending: [PendingEdge] = []

        for (i, item) in alt.items.enumerated() {
            let next = build(item)

            var isAccept: Bool = false
            if i == alt.items.count - 1 {
                isAccept = true
            }

            let result = addEdgesAndNodes(latest, next, pending, isAccept: isAccept)
            latest = result.0
            pending = result.1
        }

        return result
    }

    func build(_ item: CommonAbstract.TokenItem) -> PartialDFA {
        switch item {
        case .atom(let atom):
            return .terminal(
                label: atom.terminal.description
            )

        case .group(let atoms):
            return .choice(
                labels: atoms.map(\.terminal.description)
            )

        case .optionalAtom(let atom):
            return .optional(
                labels: [atom.terminal.description]
            )

        case .optionalGroup(let atoms):
            return .optional(
                labels: atoms.map(\.terminal.description)
            )

        case .zeroOrMore(let atoms):
            return .zeroOrMore(
                labels: atoms.map(\.terminal.description)
            )

        case .oneOrMore(let atoms):
            return .oneOrMore(
                labels: atoms.map(\.terminal.description)
            )
        }
    }

    func makeRealizableNode(isAccept: Bool) -> RealizableNode {
        let node = RealizableNode()
        node.isAccept = isAccept
        return node
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
        case terminal(label: String)
        case choice(labels: [String])
        case optional(labels: [String])
        case zeroOrMore(labels: [String])
        case oneOrMore(labels: [String])

        var labels: [String] {
            switch self {
            case .terminal(let label):
                return [label]

            case .choice(let labels):
                return labels

            case .oneOrMore(let labels):
                return labels

            case .optional(let labels):
                return labels

            case .zeroOrMore(let labels):
                return labels
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
