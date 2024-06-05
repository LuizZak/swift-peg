extension GrammarProcessor {
    /// Computes nullable rules.
    ///
    /// Rules are nullable if they can be satisfied while matching no input.
    func computeNullables(in grammar: SwiftPEGGrammar.Grammar, _ rules: [String: SwiftPEGGrammar.Rule]) throws {
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

        try computeLeftRecursive(in: grammar, graph, rules)
    }

    /// Computes left-recursive rules in a given grammar
    func computeLeftRecursive(
        in grammar: SwiftPEGGrammar.Grammar,
        _ graph: InitialGraph,
        _ rules: [String : SwiftPEGGrammar.Rule]
    ) throws {
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
                    throw recordAndReturn(
                        .unresolvedLeftRecursion(ruleNames: component.map(\.ruleName))
                    )
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
    private var _rules: [SwiftPEGGrammar.Rule] = []
    private var _rulesMap: [String: SwiftPEGGrammar.Rule]

    init(rulesMap: [String: SwiftPEGGrammar.Rule]) {
        self._rulesMap = rulesMap
    }

    func rule(named name: String) -> SwiftPEGGrammar.Rule? {
        _rulesMap[name]
    }

    func markVisit(_ rule: SwiftPEGGrammar.Rule) {
        _rules.append(rule)
    }

    func didVisit(_ rule: SwiftPEGGrammar.Rule) -> Bool {
        return _rules.contains(where: { $0 === rule })
    }
}
