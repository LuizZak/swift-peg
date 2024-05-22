import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

class ParserMemoizeLeftRecursiveMacroTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "memoizedLeftRecursive": ParserMemoizeLeftRecursiveMacro.self,
    ]

    func testMemoizeLeftRecursiveMacro() {
        assertMacroExpansion("""
            @memoizedLeftRecursive("method")
            func __method__() -> Any {
                return 0
            }
            """,
            expandedSource: """
            func __method__() -> Any {
                return 0
            }

            /// Memoized version of `__method__`.
            func method() -> Any {
                let key = makeKey("method", arguments: nil)
                if let cached: CacheEntry<Any> = self.cache[key] {
                    self.restore(cached.mark)
                    return cached.result
                }
                // Perform left-recursion by priming the parser cache with a failure on the
                // first attempt, with successes being fed back into the cache until the next
                // parse attempt consumes no more tokens than the last.
                let mark = self.mark()

                self.cache[key] = CacheEntry<Any>(mark: mark, reach: self.reach, result: nil)
                var lastResult: Any
                var lastMark = mark

                while true {
                    self.restore(mark)
                    let priorReach = self.resetReach(mark)

                    let result = __method__()

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
                    self.cache[key] = CacheEntry<Any>(mark: lastMark, reach: reach, result: result)
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

                self.cache[key] = CacheEntry<Any>(mark: endMark, reach: reach, result: result)

                return result
            }
            """,
            macros: testMacros)
    }
}
