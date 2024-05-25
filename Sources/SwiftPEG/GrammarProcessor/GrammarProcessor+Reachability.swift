extension GrammarProcessor {
    /// Computes all rules that are unreachable by examining rule dependencies
    /// within the grammar, starting from a given start rule name.
    /// 
    /// Returns a set containing all rules that could not be reached from the
    /// setting start rule.
    func computeUnreachableRules(
        in grammar: SwiftPEGGrammar.Grammar,
        startRuleName: String,
        rules: [String: SwiftPEGGrammar.Rule]
    ) throws -> Set<String> {

        printIfVerbose("Computing reachable rules...")

        let graph = InitialGraph()
        graph.addNodes(rules.keys.map(RuleNode.init))

        // Search the graph
        guard let startNode = graph.nodes.first(where: { $0.ruleName == startRuleName }) else {
            throw GrammarProcessor.GrammarProcessorError.message("Invalid starting rule name '\(startRuleName)': rule not found.")
        }

        for (ruleName, rule) in rules {
            let allNames = rule.allNames()
            printIfVerbose("\(ruleName): \(allNames.joined(separator: ", "))")

            for name in allNames where rules[name] != nil {
                graph.addEdge(from: ruleName, to: name)
            }
        }

        var unreached: Set<String> = Set(rules.keys)
        unreached.remove(startRuleName)
        graph.breadthFirstVisit(start: startNode) { visit in
            unreached.remove(visit.node.ruleName)
            return true
        }

        return unreached
    }
}
