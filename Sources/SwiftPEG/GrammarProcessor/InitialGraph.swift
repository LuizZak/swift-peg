/// A graph that records initial rule invocations for left-recursive analysis.
final class InitialGraph: DirectedGraphBase<RuleNode, RuleEdge> {
    @discardableResult
    func addEdge(from startRule: String, to endRule: String) -> RuleEdge {
        guard let start = nodes.first(where: { $0.ruleName == startRule }) else {
            fatalError("Rule with name '\(startRule)' doesn't exist in this graph.")
        }
        guard let end = nodes.first(where: { $0.ruleName == endRule }) else {
            fatalError("Rule with name '\(endRule)' doesn't exist in this graph.")
        }

        return addEdge(from: start, to: end)
    }

    override func addEdge(from start: RuleNode, to end: RuleNode) -> RuleEdge {
        let edge = RuleEdge(start: start, end: end)

        edges.append(edge)

        return edge
    }
}

class RuleNode: DirectedGraphNode {
    let ruleName: String

    init(ruleName: String) {
        self.ruleName = ruleName
    }

    convenience init(rule: SwiftPEGGrammar.Rule) {
        self.init(ruleName: String(rule.name.name.string))
    }
}

final class RuleEdge: DirectedGraphBaseEdgeType {
    let start: RuleNode
    let end: RuleNode

    internal init(start: RuleNode, end: RuleNode) {
        self.start = start
        self.end = end
    }

    func copy() -> Self {
        return .init(start: start, end: end)
    }
}
