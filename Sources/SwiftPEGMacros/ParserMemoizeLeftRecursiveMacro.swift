import SwiftSyntax
import SwiftSyntaxMacros

/// Macro used to memoize parser method invocations.
public struct ParserMemoizeLeftRecursiveMacro: PeerMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {

        do {
            let impl = ParserMemoizeLeftRecursiveMacroImplementation(
                of: node,
                providingPeersOf: declaration,
                in: context
            )
            return try impl.expand()
        } catch MacroError.diagnostic(let diag) {
            context.diagnose(diag)
            return []
        } catch {
            throw error
        }
    }
}

class ParserMemoizeLeftRecursiveMacroImplementation: ParserMemoizeMacroImplementation {
    override func expandCacheMiss(
        cache: ExprSyntax,
        invocation: ExprSyntax,
        typeToCache: TypeSyntax
    ) -> CodeBlockItemListSyntax {

        return """
        // Perform left-recursion by priming the parser cache with a failure on the
        // first attempt, with successes being fed back into the cache until the next
        // parse attempt consumes no more tokens than the last.
        let mark = self.mark()

        \(cache)[key] = CacheEntry<\(typeToCache)>(mark: mark, reach: self.reach, result: nil)
        var lastResult: \(typeToCache)
        var lastMark = mark

        while true {
            self.restore(mark)
            let priorReach = self.resetReach(mark)

            let result = \(invocation)

            let endMark = self.mark()
            let reach = self.resetReach(priorReach)
            self.updateReach(reach)

            if result == nil {
                break
            }
            if endMark <= lastMark {
                break
            }

            lastResult = result
            lastMark = endMark
            \(cache)[key] = CacheEntry<\(typeToCache)>(mark: lastMark, reach: reach, result: result)
        }

        self.restore(lastMark)
        self.updateReach(reach)

        let result = lastResult
        let endMark: Mark
        if result != nil {
            endMark = self.mark()
        } else {
            endMark = mark
            self.restore(endMark)
        }

        \(cache)[key] = CacheEntry<\(typeToCache)>(mark: endMark, reach: reach, result: result)
        """
    }
}
