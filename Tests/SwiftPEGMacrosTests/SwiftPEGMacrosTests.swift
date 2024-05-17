import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

let testMacros: [String: Macro.Type] = [
    "memoized": ParserMemoizeMacro.self
]

class SwiftPEGMacrosTests: XCTestCase {
    func testMemoizeMacro_noArguments() {
        assertMacroExpansion("""
            class A {
                @memoized("method")
                func __method__() throws -> Any {
                    return 0
                }
            }
            """,
            expandedSource: """
            class A {
                func __method__() throws -> Any {
                    return 0
                }

                /// Memoized version of `__method__`.
                open func method() throws -> Any {
                    let args: [AnyHashable] = []
                    let key = makeKey("method", arguments: args)
                    if let cached: CacheEntry<Any> = self.cache.fetch(key) {
                        self.restore(cached.mark)
                        return cached.result
                    }
                    let result = try __method__()
                    cache.store(
                        key,
                        value: CacheEntry(mark: self.mark(), result: result)
                    )

                    return result
                }
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_withArguments() {
        assertMacroExpansion("""
            class A {
                @memoized("method")
                func __method__(a: Int, _ b: Float, c _c: Bool) throws -> Any {
                    return 0
                }
            }
            """,
            expandedSource: """
            class A {
                func __method__(a: Int, _ b: Float, c _c: Bool) throws -> Any {
                    return 0
                }

                /// Memoized version of `__method__`.
                open func method(a: Int, _ b: Float, c _c: Bool) throws -> Any {
                    let args: [AnyHashable] = [AnyHashable(a), AnyHashable(b), AnyHashable(_c)]
                    let key = makeKey("method", arguments: args)
                    if let cached: CacheEntry<Any> = self.cache.fetch(key) {
                        self.restore(cached.mark)
                        return cached.result
                    }
                    let result = try __method__(a: a, b, c: _c)
                    cache.store(
                        key,
                        value: CacheEntry(mark: self.mark(), result: result)
                    )

                    return result
                }
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_nilReturn() {
        assertMacroExpansion("""
            class A {
                @memoized("method")
                func __method__() throws -> Int? {
                    return nil
                }
            }
            """,
            expandedSource: """
            class A {
                func __method__() throws -> Int? {
                    return nil
                }

                /// Memoized version of `__method__`.
                open func method() throws -> Int? {
                    let args: [AnyHashable] = []
                    let key = makeKey("method", arguments: args)
                    if let cached: CacheEntry<Int?> = self.cache.fetch(key) {
                        self.restore(cached.mark)
                        return cached.result
                    }
                    let result = try __method__()
                    cache.store(
                        key,
                        value: CacheEntry(mark: self.mark(), result: result)
                    )

                    return result
                }
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_copiesDocumentation() {
        assertMacroExpansion("""
            class A {
                /// A doc comment
                @memoized("method")
                func __method__() throws -> Int? {
                    return nil
                }
            }
            """,
            expandedSource: """
            class A {
                /// A doc comment
                func __method__() throws -> Int? {
                    return nil
                }

                /// A doc comment
                /// Memoized version of `__method__`.
                open func method() throws -> Int? {
                    let args: [AnyHashable] = []
                    let key = makeKey("method", arguments: args)
                    if let cached: CacheEntry<Int?> = self.cache.fetch(key) {
                        self.restore(cached.mark)
                        return cached.result
                    }
                    let result = try __method__()
                    cache.store(
                        key,
                        value: CacheEntry(mark: self.mark(), result: result)
                    )

                    return result
                }
            }
            """,
            macros: testMacros)
    }
}
