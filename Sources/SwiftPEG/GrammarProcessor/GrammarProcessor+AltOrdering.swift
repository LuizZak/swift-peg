extension GrammarProcessor {
    /// Validates sequential alts across all rules in `grammar`, checking that
    /// each subsequent alt is either disjointed from the first alt, or a smaller
    /// subset, warning about alt orders that prevent an alt from ever being tried.
    func diagnoseAltOrder(in grammar: SwiftPEGGrammar.Grammar) {
        let visitor = AltOrderVisitor { [self] (alts, rule) in
            self.diagnoseAltOrder(alts, in: rule)
        }
        let walker = NodeWalker(visitor: visitor)

        do {
            try walker.walk(grammar)
        } catch {

        }
    }

    /// Validates sequential alts, checking that each subsequent alt is either
    /// disjointed from the first alt, or a smaller subset, warning about alt
    /// orders that prevent an alt from ever being tried.
    func diagnoseAltOrder(_ alts: [SwiftPEGGrammar.Alt], in rule: SwiftPEGGrammar.Rule) {
        for index in 0..<alts.count {
            let alt = alts[index]
            let altInt = InternalGrammar.Alt.from(alt).reduced

            for nextIndex in index..<alts.count where index != nextIndex {
                let nextAlt = alts[nextIndex]

                // Nullable alt?
                guard let altInt else {
                    diagnostics.append(
                        .altOrderIssue(rule: rule, alt, alwaysSucceedsBefore: nextAlt)
                    )
                    continue
                }

                guard let nextAltInt = InternalGrammar.Alt.from(nextAlt).reduced else {
                    continue
                }

                if altInt.isSubset(of: nextAltInt) {
                    diagnostics.append(
                        .altOrderIssue(rule: rule, alt, alwaysSucceedsBefore: nextAlt)
                    )
                }
            }
        }
    }

    final class AltOrderVisitor: SwiftPEGGrammar.GrammarNodeVisitorType {
        var currentRule: SwiftPEGGrammar.Rule?
        var callback: ([SwiftPEGGrammar.Alt], SwiftPEGGrammar.Rule) -> Void

        init(_ callback: @escaping ([SwiftPEGGrammar.Alt], SwiftPEGGrammar.Rule) -> Void) {
            self.callback = callback
        }

        func willVisit(_ node: Node) {
            if let rule = node as? SwiftPEGGrammar.Rule {
                currentRule = rule
            }
        }

        func visit(_ node: SwiftPEGGrammar.Rule) throws -> NodeVisitChildrenResult {
            callback(node.alts, node)
            return .visitChildren
        }

        func visit(_ node: SwiftPEGGrammar.GroupAtom) throws -> NodeVisitChildrenResult {
            if let currentRule {
                callback(node.alts, currentRule)
            }
            return .visitChildren
        }

        func visit(_ node: SwiftPEGGrammar.OptionalItems) throws -> NodeVisitChildrenResult {
            if let currentRule {
                callback(node.alts, currentRule)
            }
            return .visitChildren
        }
    }
}
