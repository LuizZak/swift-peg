extension GrammarProcessor {
    /// Computes nullable rules.
    ///
    /// Rules are nullable if they can be satisfied while matching no input.
    func computeNullables(_ rules: [String: Metagrammar.Rule]) throws {
        let visitor = NullabilityVisitor(rulesMap: rules)
        for rule in grammar.rules {
            _ = rule.visitNullable(visitor)
        }

        printIfVerbose("Computing initial names...")

        let graph = InitialGraph()
        graph.addNodes(rules.keys.map(RuleNode.init))

        for (ruleName, rule) in rules {
            let initialNames = rule.initialNames()
            printIfVerbose("\(ruleName): \(initialNames.joined(separator: ", "))")

            for name in initialNames where rules[name] != nil {
                graph.addEdge(from: ruleName, to: name)
            }
        }

        printIfVerbose("Computing left-recursion...")

        for component in graph.stronglyConnectedComponents() {
            if component.count == 1, let first = component.first {
                // Check if the rule is re-entrant into itself
                if graph.edge(from: first, to: first) != nil {
                    guard let rule = rules[first.ruleName] else { continue }

                    rule.isLeftRecursive = true
                    rule.isLeftRecursiveLead = true
                }
            } else {
                for ruleName in component {
                    guard let rule = rules[ruleName.ruleName] else { continue }

                    rule.isLeftRecursive = true
                }

                var leaders = Set(component)
                let subgraph = graph.subgraph(of: leaders)
                for start in component {
                    for cycle in subgraph.findCycles(from: start) {
                        leaders.subtract(component.subtracting(cycle))
                    }
                }

                if let first = leaders.first {
                    guard let rule = rules[first.ruleName] else { continue }

                    rule.isLeftRecursiveLead = true
                } else {
                    throw GrammarProcessorError.unresolvedLeftRecursion(ruleNames: component.map(\.ruleName))
                }
            }
        }

        if verbose {
            for rule in grammar.rules {
                guard rule.isLeftRecursive else { continue }

                if rule.isLeftRecursiveLead {
                    printIfVerbose("Left recursive rule (leader): \(rule.name.name.string)")
                } else {
                    printIfVerbose("Left recursive rule: \(rule.name.name.string)")
                }
            }
        }
    }
}

class NullabilityVisitor {
    private var _rules: [Metagrammar.Rule] = []
    private var _rulesMap: [String: Metagrammar.Rule]

    init(rulesMap: [String: Metagrammar.Rule]) {
        self._rulesMap = rulesMap
    }

    func rule(named name: String) -> Metagrammar.Rule? {
        _rulesMap[name]
    }

    func markVisit(_ rule: Metagrammar.Rule) {
        _rules.append(rule)
    }

    func didVisit(_ rule: Metagrammar.Rule) -> Bool {
        return _rules.contains(where: { $0 === rule })
    }
}
