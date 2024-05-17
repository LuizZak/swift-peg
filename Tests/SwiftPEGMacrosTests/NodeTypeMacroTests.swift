import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

class NodeTypeMacroTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "NodeType": NodeTypeMacro.self,
    ]

    func testNodeTypeMacro_simpleType() {
        assertMacroExpansion("""
            @NodeType<BaseNode>
            class Node: BaseNode {
                @NodeProperty
                private var _subNode1: SubNode
                @NodeProperty
                private var _subNode2: OtherSubNode

                private var _notANode: SomeOtherClass
            }
            """,
            expandedSource: #"""
            class Node: BaseNode {
                @NodeProperty
                private var _subNode1: SubNode
                @NodeProperty
                private var _subNode2: OtherSubNode

                private var _notANode: SomeOtherClass

                /// Synthesized with `NodeTypeMacro`.
                var subNode1: SubNode {
                    get {
                        _subNode1
                    }
                    set {
                        _subNode1.parent = nil
                        _subNode1 = newValue
                        _subNode1.parent = self
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                var subNode2: OtherSubNode {
                    get {
                        _subNode2
                    }
                    set {
                        _subNode2.parent = nil
                        _subNode2 = newValue
                        _subNode2.parent = self
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                override var children: [BaseNode] {
                    Self.makeNodeList(nodes: subNode1, subNode2)
                }
            }
            """#,
            macros: testMacros)
    }

    func testNodeTypeMacro_arrayType() {
        assertMacroExpansion("""
            @NodeType<BaseNode>
            class Node: BaseNode {
                @NodeProperty
                private var _nodes: [SubNode]
            }
            """,
            expandedSource: #"""
            class Node: BaseNode {
                @NodeProperty
                private var _nodes: [SubNode]

                /// Synthesized with `NodeTypeMacro`.
                var nodes: [SubNode] {
                    get {
                        _nodes
                    }
                    set {
                        _nodes.forEach({
                            $0.parent = nil
                        })
                        _nodes = newValue
                        _nodes.forEach({
                            $0.parent = self
                        })
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                override var children: [BaseNode] {
                    Self.makeNodeList(lists: nodes)
                }
            }
            """#,
            macros: testMacros)
    }

    func testNodeTypeMacro_optionalType() {
        assertMacroExpansion("""
            @NodeType<BaseNode>
            class Node: BaseNode {
                @NodeProperty
                private var _maybeNode: SubNode?
            }
            """,
            expandedSource: #"""
            class Node: BaseNode {
                @NodeProperty
                private var _maybeNode: SubNode?

                /// Synthesized with `NodeTypeMacro`.
                var maybeNode: SubNode? {
                    get {
                        _maybeNode
                    }
                    set {
                        _maybeNode?.parent = nil
                        _maybeNode = newValue
                        _maybeNode?.parent = self
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                override var children: [BaseNode] {
                    Self.makeNodeList(nodes: maybeNode)
                }
            }
            """#,
            macros: testMacros)
    }

    func testNodeTypeMacro_mixed_simpleType_arrayType() {
        assertMacroExpansion("""
            @NodeType<BaseNode>
            class Node: BaseNode {
                @NodeProperty
                private var _nodes: [SubNode]
                @NodeProperty
                private var _subNode2: OtherSubNode
            }
            """,
            expandedSource: #"""
            class Node: BaseNode {
                @NodeProperty
                private var _nodes: [SubNode]
                @NodeProperty
                private var _subNode2: OtherSubNode

                /// Synthesized with `NodeTypeMacro`.
                var nodes: [SubNode] {
                    get {
                        _nodes
                    }
                    set {
                        _nodes.forEach({
                            $0.parent = nil
                        })
                        _nodes = newValue
                        _nodes.forEach({
                            $0.parent = self
                        })
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                var subNode2: OtherSubNode {
                    get {
                        _subNode2
                    }
                    set {
                        _subNode2.parent = nil
                        _subNode2 = newValue
                        _subNode2.parent = self
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                override var children: [BaseNode] {
                    Self.makeNodeList(lists: nodes, nodes: subNode2)
                }
            }
            """#,
            macros: testMacros)
    }

    func testNodeTypeMacro_copiesComments() {
        assertMacroExpansion("""
            @NodeType<BaseNode>
            class Node: BaseNode {
                /// First subnode
                @NodeProperty
                private var _subNode1: SubNode
                /// Second subnode
                @NodeProperty
                private var _subNode2: OtherSubNode

                private var _notANode: SomeOtherClass
            }
            """,
            expandedSource: #"""
            class Node: BaseNode {
                /// First subnode
                @NodeProperty
                private var _subNode1: SubNode
                /// Second subnode
                @NodeProperty
                private var _subNode2: OtherSubNode

                private var _notANode: SomeOtherClass

                /// First subnode
                /// Synthesized with `NodeTypeMacro`.
                var subNode1: SubNode {
                    get {
                        _subNode1
                    }
                    set {
                        _subNode1.parent = nil
                        _subNode1 = newValue
                        _subNode1.parent = self
                    }
                }

                /// Second subnode
                /// Synthesized with `NodeTypeMacro`.
                var subNode2: OtherSubNode {
                    get {
                        _subNode2
                    }
                    set {
                        _subNode2.parent = nil
                        _subNode2 = newValue
                        _subNode2.parent = self
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                override var children: [BaseNode] {
                    Self.makeNodeList(nodes: subNode1, subNode2)
                }
            }
            """#,
            macros: testMacros)
    }

    func testNodeTypeMacro_inheritsAccessLevelFromClass() {
        assertMacroExpansion("""
            @NodeType<BaseNode>
            open class Node: BaseNode {
                /// First subnode
                @NodeProperty
                private var _subNode1: SubNode
                /// Second subnode
                @NodeProperty
                private var _subNode2: OtherSubNode

                private var _notANode: SomeOtherClass
            }
            """,
            expandedSource: #"""
            open class Node: BaseNode {
                /// First subnode
                @NodeProperty
                private var _subNode1: SubNode
                /// Second subnode
                @NodeProperty
                private var _subNode2: OtherSubNode

                private var _notANode: SomeOtherClass

                /// First subnode
                /// Synthesized with `NodeTypeMacro`.
                open var subNode1: SubNode {
                    get {
                        _subNode1
                    }
                    set {
                        _subNode1.parent = nil
                        _subNode1 = newValue
                        _subNode1.parent = self
                    }
                }

                /// Second subnode
                /// Synthesized with `NodeTypeMacro`.
                open var subNode2: OtherSubNode {
                    get {
                        _subNode2
                    }
                    set {
                        _subNode2.parent = nil
                        _subNode2 = newValue
                        _subNode2.parent = self
                    }
                }

                /// Synthesized with `NodeTypeMacro`.
                override open var children: [BaseNode] {
                    Self.makeNodeList(nodes: subNode1, subNode2)
                }
            }
            """#,
            macros: testMacros)
    }
}
