import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport


@testable import SwiftPEGMacros

class EnumIsCaseGeneratorTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "GenerateIsCase": EnumIsCaseGenerator.self,
    ]

    func testMacro_emptyEnum() {
        assertMacroExpansion("""
            @GenerateIsCase
            enum AnEnum {
            }
            """,
            expandedSource: #"""
            enum AnEnum {
            }
            """#,
            macros: testMacros)
    }

    func testMacro_member() {
        assertMacroExpansion("""
            @GenerateIsCase
            enum AnEnum {
                case foo(Int)
                case b
                case bar(String, Double)
                case fezBez
            }
            """,
            expandedSource: #"""
            enum AnEnum {
                case foo(Int)
                case b
                case bar(String, Double)
                case fezBez

                var isFoo: Bool {
                    switch self {
                    case .foo:
                        return true
                    default:
                        return false
                    }
                }

                var isB: Bool {
                    switch self {
                    case .b:
                        return true
                    default:
                        return false
                    }
                }

                var isBar: Bool {
                    switch self {
                    case .bar:
                        return true
                    default:
                        return false
                    }
                }

                var isFezBez: Bool {
                    switch self {
                    case .fezBez:
                        return true
                    default:
                        return false
                    }
                }
            }
            """#,
            macros: testMacros)
    }

    func testMacro_peer() {
        assertMacroExpansion("""
            enum AnEnum {
                case foo(Int)
                @GenerateIsCase
                case b
                @GenerateIsCase
                case bar(String, Double)
                case fezBez
            }
            """,
            expandedSource: #"""
            enum AnEnum {
                case foo(Int)
                case b

                var isB: Bool {
                    switch self {
                    case .b:
                        return true
                    default:
                        return false
                    }
                }
                case bar(String, Double)

                var isBar: Bool {
                    switch self {
                    case .bar:
                        return true
                    default:
                        return false
                    }
                }
                case fezBez
            }
            """#,
            macros: testMacros)
    }

    func testMacro_respectsAccessLevelParameter() {
        assertMacroExpansion("""
            @GenerateIsCase(accessLevel: "private")
            enum AnEnum {
                case foo(Int)
                case bar(String, Double)
            }
            """,
            expandedSource: #"""
            enum AnEnum {
                case foo(Int)
                case bar(String, Double)

                private var isFoo: Bool {
                    switch self {
                    case .foo:
                        return true
                    default:
                        return false
                    }
                }

                private var isBar: Bool {
                    switch self {
                    case .bar:
                        return true
                    default:
                        return false
                    }
                }
            }
            """#,
            macros: testMacros)
    }
}
