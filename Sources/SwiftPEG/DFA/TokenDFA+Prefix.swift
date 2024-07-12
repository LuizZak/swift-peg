import MiniDigraph

extension TokenDFA {
    /// Returns `true` if this DFA can be considered a prefix of `other`.
    ///
    /// DFAs are a prefix of one another if they consume the same input, with the
    /// latter DFA being allowed to consume more input.
    ///
    /// A DFA is always considered to be a prefix of itself, and of other DFAs
    /// that have the same nodes, edges, and starting node.
    func isPrefix(of other: TokenDFA) -> Bool {
        if self === other {
            return true
        }
        if self.startNodeId == other.startNodeId && self.nodes == other.nodes && self.edges == other.edges {
            return true
        }

        return _isPrefix(other)
    }
}

fileprivate extension TokenDFA {
    func _isPrefix(_ other: TokenDFA) -> Bool {
        let selfAcceptPaths = self._findAcceptPaths()
        let otherAcceptPaths = other._findAcceptPaths()

        consumedStringPath:
        if let selfAcceptPaths, let otherAcceptPaths {
            guard let selfConsumedStrings = selfAcceptPaths.optionalMap({ $0.consumedString() }) else {
                break consumedStringPath
            }
            guard let otherConsumedStrings = otherAcceptPaths.optionalMap({ $0.consumedString() }) else {
                break consumedStringPath
            }

            for selfConsumed in selfConsumedStrings {
                for otherConsumed in otherConsumedStrings {
                    if !selfConsumed.isPrefix(of: otherConsumed) {
                        break consumedStringPath
                    }
                }
            }

            return true
        }

        let walker = DFAWalker(
            prefix: self,
            prefixStart: findNode(startNodeId)!,
            other: other,
            otherStart: other.findNode(other.startNodeId)!
        )

        return walker.walk()
    }

    func _findAcceptPaths() -> [VisitElement]? {
        guard let startNode = findNode(startNodeId) else {
            return nil
        }
        guard findCycles(from: startNode).isEmpty else {
            return nil
        }

        var result: [VisitElement] = []

        breadthFirstVisit(start: startNode) { visit in
            if visit.node.isAccept {
                result.append(visit)
            }

            return true
        }

        return result
    }
}

fileprivate extension DirectedGraphRecordingVisitElement<TokenDFA.Edge, TokenDFA.Node> {
    func consumedString() -> ConsumedString? {
        var result = ConsumedString()

        for edge in allEdges {
            switch edge.condition {
            case .terminal(.any):
                result.append(.any)

            case .terminal(.literal(let literal)):
                result.append(.literal(literal.contents))

            case .terminal(.rangeLiteral(let low, let high)):
                result.append(.range(low.contents, high.contents))

            case .terminal(.characterPredicate),
                .terminal(.identifier):
                return nil

            case .epsilon:
                break
            }
        }

        return result
    }
}

fileprivate class DFAWalker {
    var prefix: TokenDFA
    var other: TokenDFA
    var consumedPrefix: ConsumedString
    var consumedOther: ConsumedString

    var nextPrefix: TokenDFA.Node
    var nextOther: TokenDFA.Node

    var visitedPrefix: Set<TokenDFA.Node.ID> = []
    var visitedOther: Set<TokenDFA.Node.ID> = []

    init(
        prefix: TokenDFA,
        prefixStart: TokenDFA.Node,
        consumedPrefix: ConsumedString = .init(),
        other: TokenDFA,
        otherStart: TokenDFA.Node,
        consumedOther: ConsumedString = .init()
    ) {
        self.prefix = prefix
        self.nextPrefix = prefixStart
        self.consumedPrefix = consumedPrefix
        self.other = other
        self.nextOther = otherStart
        self.consumedOther = consumedOther
    }

    func walk() -> Bool {
        return check()
    }

    private func check() -> Bool {
        if nextPrefix.isAccept {
            return consumedPrefix.isPrefix(of: consumedOther)
        }

        let edgesPrefix = prefix.edges(from: nextPrefix)
        let edgesOther = other.edges(from: nextOther)

        var hasMatch = false
        for edgePrefix in edgesPrefix {
            guard !visitedPrefix.contains(edgePrefix.end) else {
                continue
            }

            var hasEdgeMatch = false
            for edgeOther in edgesOther {
                guard !visitedOther.contains(edgeOther.end) else {
                    continue
                }

                guard
                    edgePrefix.condition.isSubset(of: edgeOther.condition)
                    || edgePrefix.condition.isPrefix(of: edgeOther.condition)
                else {
                    continue
                }

                guard let consumedPrefix = ConsumedString.Element.from(edgePrefix) else {
                    return false
                }
                guard let consumedOther = ConsumedString.Element.from(edgeOther) else {
                    return false
                }

                let next = advance(
                    prefixEdge: edgePrefix,
                    consumingPrefix: consumedPrefix,
                    otherEdge: edgeOther,
                    consumingOther: consumedOther
                )

                if !next.walk() {
                    return false
                }

                hasMatch = true
                hasEdgeMatch = true
            }

            if !hasEdgeMatch {
                return false
            }
        }

        return hasMatch
    }

    private func copy() -> DFAWalker {
        return DFAWalker(
            prefix: prefix,
            prefixStart: nextPrefix,
            other: other,
            otherStart: nextOther
        )
    }

    private func advance(
        prefixEdge: TokenDFA.Edge,
        consumingPrefix: ConsumedString.Element,
        otherEdge: TokenDFA.Edge,
        consumingOther: ConsumedString.Element
    ) -> DFAWalker {
        let copy = copy()
        copy.nextPrefix = prefix.endNode(for: prefixEdge)
        copy.consumedPrefix.append(consumingPrefix)
        copy.nextOther = other.endNode(for: otherEdge)
        copy.consumedOther.append(consumingOther)
        copy.visitedPrefix.insert(prefixEdge.end)
        copy.visitedOther.insert(otherEdge.end)
        return copy
    }
}

fileprivate struct ConsumedString: CustomStringConvertible {
    var elements: [Element] = []

    var description: String {
        elements.map(\.description).joined()
    }

    mutating func append(_ element: Element) {
        switch (elements.last, element) {
        case (.literal(let literal), .literal(let next)):
            elements.removeLast()
            elements.append(.literal(literal + next))

        default:
            elements.append(element)
        }
    }

    func isPrefix(of other: Self) -> Bool {
        for (i, (lhs, rhs)) in zip(self.elements, other.elements).enumerated() {
            if i == self.elements.count - 1 {
                return lhs.isPrefix(of: rhs)
            }

            if lhs != rhs {
                return false
            }
        }

        return true
    }

    fileprivate enum Element: Equatable, CustomStringConvertible {
        case literal(String)
        case range(String, String)
        case any

        var description: String {
            switch self {
            case .literal(let literal):
                return literal.debugDescription

            case .range(let low, let high):
                return "\(low.debugDescription)...\(high.debugDescription)"

            case .any:
                return "*"
            }
        }

        func isPrefix(of other: Self) -> Bool {
            switch (self, other) {
            case (.literal(let lhs), .literal(let rhs)):
                return rhs.hasPrefix(lhs)

            case (.range(let lhsLow, let lhsHigh), .range(let rhsLow, let rhsHigh)):
                return lhsLow >= rhsLow && lhsHigh <= rhsHigh

            case (.range(let lhsLow, let lhsHigh), .literal(let rhs)):
                return (lhsLow...lhsHigh).contains(rhs)

            case (.literal(let lhs), .range(let rhsLow, let rhsHigh)):
                return rhsLow == rhsHigh && rhsLow.hasPrefix(lhs)

            case (.any, _):
                return true

            case (_, .any):
                return false
            }
        }

        static func from(_ edge: TokenDFA.Edge) -> Self? {
            switch edge.condition {
            case .terminal(.any):
                return .any

            case .terminal(.literal(let literal)):
                return .literal(literal.contents)

            case .terminal(.rangeLiteral(let low, let high)):
                return .range(low.contents, high.contents)

            case .terminal(.characterPredicate),
                .terminal(.identifier):
                return nil

            case .epsilon:
                return nil
            }
        }
    }
}

fileprivate extension Sequence {
    func optionalMap<T>(_ mapper: (Element) -> T?) -> [T]? {
        let results = self.map(mapper)
        if results.contains(where: { $0 == nil }) {
            return nil
        }

        return results.compactMap({ $0 })
    }
}
