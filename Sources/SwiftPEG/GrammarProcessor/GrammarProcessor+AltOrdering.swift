extension GrammarProcessor {
    /// Validates sequential alts across all rules in `grammar`, checking that
    /// each subsequent alt is either disjointed from the first alt, or a smaller
    /// subset, warning about alt orders that prevent an alt from ever being tried.
    func diagnoseAltOrder(in grammar: SwiftPEGGrammar.Grammar) {
        var permuteCache = AltPermuteCache()
        let visitor = AltOrderVisitor { [self] (alts, rule) in
            self.diagnoseAltOrder(alts, in: rule, permuteCache: &permuteCache)
        }
        let walker = NodeWalker(visitor: visitor)

        try? walker.walk(grammar)
    }

    /// Validates sequential alts, checking that each subsequent alt is either
    /// disjointed from the first alt, or a smaller subset, warning about alt
    /// orders that prevent an alt from ever being tried.
    func diagnoseAltOrder(
        _ alts: [SwiftPEGGrammar.Alt],
        in rule: SwiftPEGGrammar.Rule,
        permuteCache: inout AltPermuteCache
    ) {
        for index in 0..<alts.count {
            let alt = alts[index]
            let altInt = InternalGrammar.Alt.from(alt)

            for nextIndex in index..<alts.count where index != nextIndex {
                let nextAlt = alts[nextIndex]
                let nextAltInt = InternalGrammar.Alt.from(nextAlt)
                
                if _checkAltShadows(earlier: altInt, latter: nextAltInt, permuteCache: &permuteCache) {
                    diagnostics.append(
                        .altOrderIssue(rule: rule, alt, alwaysSucceedsBefore: nextAlt)
                    )
                }
            }
        }
    }

    private func _checkAltShadows(
        earlier: InternalGrammar.Alt,
        latter: InternalGrammar.Alt,
        permuteCache: inout AltPermuteCache
    ) -> Bool {

        // If the earlier alt is nullable, then it always succeeds before any
        // other alt
        guard let earlierRed = earlier.reduced else {
            return true
        }
        guard let latterRed = latter.reduced else {
            return false
        }
        if earlierRed.isPrefix(of: latterRed) {
            return true
        }

        // Check a shallow permutation set for the earlier alt
        let earlierPerm = permuteCache.permute(earlier, depthLimit: 3)

        // If any permutation of 'earlier' is a prefix of the reduction of
        // 'latter', then 'earlier' shadows 'latter'.
        for earlier in earlierPerm {
            if earlier.isPrefix(of: latterRed) {
                return true
            }
        }

        return false
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

    struct AltPermuteCache {
        typealias Value = [InternalGrammar.Alt]
        var cache: [Key: Value] = [:]

        mutating func permute(_ alt: InternalGrammar.Alt, depthLimit: Int) -> Value {
            let key = Key(alt: alt, permutationDepth: depthLimit)
            if let cached = cache[key] {
                return cached
            }

            let value = AltReducer(alt).permuted(depthLimit: depthLimit)
            cache[key] = value
            return value
        }

        struct Key: Hashable {
            var alt: InternalGrammar.Alt
            var permutationDepth: Int
        }
    }
}
