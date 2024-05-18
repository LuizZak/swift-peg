import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

class ParserMemoizeMacroTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "memoized": ParserMemoizeMacro.self,
    ]

    func testMemoizeMacro_customCacheTarget() {
        assertMacroExpansion("""
            @memoized("method", "other.cache[0]")
            func __method__() -> Any {
                return 0
            }
            """,
            expandedSource: """
            func __method__() -> Any {
                return 0
            }

            /// Memoized version of `__method__`.
            open func method() -> Any {
                let args: [AnyHashable] = []
                let key = makeKey("method", arguments: args)
                if let cached: CacheEntry<Any> = other.cache[0].fetch(key) {
                    self.restore(cached.mark)
                    return cached.result
                }
                let result = __method__()
                other.cache[0].store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_nonThrowing() {
        assertMacroExpansion("""
            @memoized("method")
            func __method__() -> Any {
                return 0
            }
            """,
            expandedSource: """
            func __method__() -> Any {
                return 0
            }

            /// Memoized version of `__method__`.
            open func method() -> Any {
                let args: [AnyHashable] = []
                let key = makeKey("method", arguments: args)
                if let cached: CacheEntry<Any> = self.cache.fetch(key) {
                    self.restore(cached.mark)
                    return cached.result
                }
                let result = __method__()
                self.cache.store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_noArguments() {
        assertMacroExpansion("""
            @memoized("method")
            func __method__() throws -> Any {
                return 0
            }
            """,
            expandedSource: """
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
                self.cache.store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_withArguments() {
        assertMacroExpansion("""
            @memoized("method")
            func __method__(a: Int, _ b: Float, c _c: Bool) throws -> Any {
                return 0
            }
            """,
            expandedSource: """
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
                self.cache.store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_optionalReturn() {
        assertMacroExpansion("""
            @memoized("method")
            func __method__() throws -> Int? {
                return nil
            }
            """,
            expandedSource: """
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
                self.cache.store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_copiesDocumentation() {
        assertMacroExpansion("""
            /// A doc comment
            @memoized("method")
            func __method__() throws -> Int? {
                return nil
            }
            """,
            expandedSource: """
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
                self.cache.store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """,
            macros: testMacros)
    }

    // MARK: Diagnostics test

    /// Declaration must be a function
    func testMemoizeMacro_diagnostic_notAttachedToFunction() {
        assertDiagnostics("""
            @memoized("method")
            var a: Int = 0
            """,
            expandedSource: """
            var a: Int = 0
            """, [
                DiagnosticSpec(
                    message: "Only functions can be memoized with this macro",
                    line: 1,
                    column: 1
                ),
            ])
    }

    /// Declaration must have a non-Void return type
    func testMemoizeMacro_diagnostic_voidReturnType() {
        assertDiagnostics("""
            @memoized("method1")
            func m1() {
            }

            @memoized("method2")
            func m2() -> Void {
            }

            @memoized("method3")
            func m3() -> () {
            }
            """,
            expandedSource: """
            func m1() {
            }
            func m2() -> Void {
            }
            func m3() -> () {
            }
            """, [
                DiagnosticSpec(
                    message: "Cannot memoize Void method",
                    line: 1,
                    column: 1
                ),
                DiagnosticSpec(
                    message: "Cannot memoize Void method",
                    line: 5,
                    column: 1
                ),
                DiagnosticSpec(
                    message: "Cannot memoize Void method",
                    line: 9,
                    column: 1
                ),
            ])
    }

    /// Target memoized method name cannot be empty
    func testMemoizeMacro_diagnostic_emptyMemoizedTarget() {
        assertDiagnostics("""
            @memoized("")
            func m() -> Int {
                return 0
            }
            """,
            expandedSource: """
            func m() -> Int {
                return 0
            }
            """, [
                DiagnosticSpec(
                    message: "Memoized method name cannot be empty",
                    line: 1,
                    column: 11,
                    highlights: ["\"\""]
                ),
            ])
    }

    /// Target memoized method name cannot be the same name as the method the
    /// macro is attached to
    func testMemoizeMacro_diagnostic_memoizedNamedHasSameName() {
        assertDiagnostics("""
            @memoized("m")
            func m() -> Int {
                return 0
            }
            """,
            expandedSource: """
            func m() -> Int {
                return 0
            }
            """, [
                DiagnosticSpec(
                    message: "Memoized method cannot have the same name as non-memoized m",
                    line: 1,
                    column: 11,
                    highlights: ["\"m\""]
                ),
            ])
    }

    /// Asynchronous methods cannot be memoized
    func testMemoizeMacro_diagnostic_asyncNotSupported() {
        assertDiagnostics("""
            @memoized("method")
            func m() async -> Any {
                return 0
            }
            """,
            expandedSource: """
            func m() async -> Any {
                return 0
            }
            """, [
                DiagnosticSpec(
                    message: "Memoizing asynchronous functions is not currently supported",
                    line: 1,
                    column: 1
                ),
            ])
    }

    /// Memoization cache target cannot be empty string
    func testMemoizeMacro_diagnostic_emptyCacheTarget() {
        assertDiagnostics("""
            @memoized("method1", "")
            func m1() -> Any {
            }

            @memoized("method1", cacheTarget: "")
            func m1() -> Any {
            }
            """,
            expandedSource: """
            func m1() -> Any {
            }
            func m1() -> Any {
            }
            """, [
                DiagnosticSpec(
                    message: "Memoization cache name cannot be empty",
                    line: 1,
                    column: 22,
                    highlights: ["\"\""]
                ),
                DiagnosticSpec(
                    message: "Memoization cache name cannot be empty",
                    line: 5,
                    column: 22,
                    highlights: ["cacheTarget: \"\""]
                ),
            ])
    }

    /// Macro requires arguments
    func testMemoizeMacro_diagnostic_noArguments() {
        assertDiagnostics("""
            @memoized
            func m1() -> Any {
            }

            @memoized()
            func m2() -> Any {
            }
            """,
            expandedSource: """
            func m1() -> Any {
            }
            func m2() -> Any {
            }
            """, [
                DiagnosticSpec(
                    message: "Macro expects at least one argument",
                    line: 1,
                    column: 1
                ),
                DiagnosticSpec(
                    message: "Macro expects at least one argument",
                    line: 5,
                    column: 1
                ),
            ])
    }

    /// Report extraneous arguments as errors
    func testMemoizeMacro_diagnostic_extraArguments() {
        assertDiagnostics("""
            @memoized("method1", "self.cache", "")
            func m1() -> Any {
            }

            @memoized("method1", "self.cache", c: 0, d)
            func m2() -> Any {
            }
            """,
            expandedSource: """
            func m1() -> Any {
            }
            func m2() -> Any {
            }
            """, [
                DiagnosticSpec(
                    message: "Unexpected arguments",
                    line: 1,
                    column: 36,
                    highlights: ["\"\""]
                ),
                DiagnosticSpec(
                    message: "Unexpected arguments",
                    line: 5,
                    column: 36,
                    highlights: ["c: 0,", "d"]
                ),
            ])
    }

    // MARK: - Test internals

    private func assertDiagnostics(
        _ macro: String,
        expandedSource: String,
        _ diagnostics: [DiagnosticSpec],
        file: StaticString = #file,
        line: UInt = #line
    ) {
        assertMacroExpansion(
            macro,
            expandedSource: expandedSource,
            diagnostics: diagnostics,
            macros: testMacros,
            file: file,
            line: line
        )
    }
}
