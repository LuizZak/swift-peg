extension GrammarProcessor {
    /// Diagnoses unreachable rules, when starting from a given entry rule name.
    func diagnoseUnreachableRules(
        in grammar: SwiftPEGGrammar.Grammar,
        _ knownRules: [String: SwiftPEGGrammar.Rule],
        entryRuleName: String
    ) throws {

        let unreachableRules = try computeUnreachableRules(in: grammar, startRuleName: entryRuleName, rules: knownRules)

        for unreachable in unreachableRules.sorted() { // Sort results to ensure stable and predictable diagnostics issuing
            guard let rule = knownRules[unreachable] else {
                continue
            }

            rule.isReachable = false

            diagnostics.append(
                .unreachableRule(rule, startRuleName: entryRuleName)
            )
        }
    }

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

        guard let startNode = graph.nodes.first(where: { $0.ruleName == startRuleName }) else {
            throw GrammarProcessor.GrammarProcessorError.message("Invalid starting rule name '\(startRuleName)': rule not found.")
        }

        // Connect rule graph
        for (ruleName, rule) in rules {
            let allNames = rule.allNames()
            printIfVerbose("\(ruleName): \(allNames.joined(separator: ", "))")

            for name in allNames where rules[name] != nil {
                graph.addEdge(from: ruleName, to: name)
            }
        }

        var unreachable: Set<String> = Set()

        let components = graph.connectedComponents()
        for component in components where !component.contains(startNode) {
            guard let first = component.first else {
                // Component empty?
                continue
            }

            // Do topological sort and report the first node, in an attempt to
            // reduce noise in case there is a large number of connected unreachable
            // rules in this component
            guard component.count > 1 else {
                // Component is a single rule
                unreachable.insert(first.ruleName)
                continue
            }

            let subgraph = graph.subgraph(of: component)
            guard let sorted = subgraph.topologicalSorted(), !sorted.isEmpty else {
                // Fallback: report on previously randomly-picked first component
                // and move on
                unreachable.insert(first.ruleName)
                continue
            }

            unreachable.insert(sorted[0].ruleName)
        }

        return unreachable

        /*
        // Search the graph
        var unreached: Set<String> = Set(rules.keys)
        unreached.remove(startRuleName)
        graph.breadthFirstVisit(start: startNode) { visit in
            unreached.remove(visit.node.ruleName)
            return true
        }

        // Reduced redundant diagnostics by only diagnosing the start of large
        // chains of connected unreachable rules
        var graphs: [InitialGraph] = []
        for rule in unreached {
            for subgraph in graphs {
                if 
            }
        }

        return unreached
        */
    }
}
