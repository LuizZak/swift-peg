import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

class ParserMemoizeGeneratingMacroTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "memoized": ParserMemoizeGeneratingMacro.self,
    ]

    func testMemoizeMacro_noArguments() {
        assertMacroExpansion("""
            class A {
                #memoized("method", { () throws -> Void in
                    body()
                })
            }
            """,
            expandedSource: """
            class A {
                func method() throws -> Void {
                    body()
                }
            }
            """,
            macros: testMacros)
    }

    func testMemoizeMacro_withArguments() {
        assertMacroExpansion("""
            class A {
                #memoized("method", { (a: Int) throws -> Void in
                    body()
                })
            }
            """,
            expandedSource: """
            class A {
                func method(a: Int) throws -> Void {
                    body()
                }
            }
            """,
            macros: testMacros)
    }
}
