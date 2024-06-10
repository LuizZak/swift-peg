import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

class EnumAsGetterGeneratorTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "GenerateAsGetter": EnumAsGetterGenerator.self,
    ]

    func testMacro_enum() {
        assertMacroExpansion("""
            @GenerateAsGetter
            enum AnEnum {
                case a(Int)
                case b(Int, label: String)
                case c
            }
            """,
            expandedSource: #"""
            enum AnEnum {
                case a(Int)
                case b(Int, label: String)
                case c

                var asA: Int? {
                    switch self {
                    case .a(let _0):
                        return _0
                    default:
                        return nil
                    }
                }

                var asB: (Int, label: String)? {
                    switch self {
                    case .b(let _0, let _1):
                        return (_0, _1)
                    default:
                        return nil
                    }
                }
            }
            """#,
            macros: testMacros)
    }

    func testMacro_enumCase() {
        assertMacroExpansion("""
            enum AnEnum {
                case a(Int)
                @GenerateAsGetter
                case b(Int, label: String)
            }
            """,
            expandedSource: #"""
            enum AnEnum {
                case a(Int)
                case b(Int, label: String)

                var asB: (Int, label: String)? {
                    switch self {
                    case .b(let _0, let _1):
                        return (_0, _1)
                    default:
                        return nil
                    }
                }
            }
            """#,
            macros: testMacros)
    }

    func testMacro_respectsAccessLevelParameter() {
        assertMacroExpansion("""
            enum AnEnum {
                case a(Int)
                @GenerateAsGetter(accessLevel: "fileprivate")
                case b(Int, label: String)
            }
            """,
            expandedSource: #"""
            enum AnEnum {
                case a(Int)
                case b(Int, label: String)

                fileprivate var asB: (Int, label: String)? {
                    switch self {
                    case .b(let _0, let _1):
                        return (_0, _1)
                    default:
                        return nil
                    }
                }
            }
            """#,
            macros: testMacros)
    }

    func testMacro_diagnostics_enumCase_noAssociatedValue() {
        assertDiagnostics("""
            enum AnEnum {
                @GenerateAsGetter
                case a
            }
            """,
            expandedSource: """
            enum AnEnum {
                case a
            }
            """, [
                DiagnosticSpec(
                    message: "Cannot synthesize var asA for case with no parameters",
                    line: 3,
                    column: 10,
                    highlights: ["a"]
                ),
            ])
    }

    func testMacro_diagnostics_enum_noGeneratedMembers() {
        assertDiagnostics("""
            @GenerateAsGetter
            enum AnEnum {
                case a
            }
            """,
            expandedSource: """
            enum AnEnum {
                case a
            }
            """, [
                DiagnosticSpec(
                    message: "Enumeration with no cases with associated values generates no members with EnumAsGetterGenerator",
                    line: 1,
                    column: 1,
                    severity: .warning,
                    highlights: ["@GenerateAsGetter"]
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
