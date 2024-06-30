import XCTest
import SwiftSyntax
import SwiftParser
import MiniGraphviz

@testable import SwiftPEG

internal func assertGraphviz(
    dfa: TokenDFA,
    matches expected: String,
    file: StaticString = #filePath,
    line: UInt = #line
) {
    let text =
        dfa
        .asGraphviz()
        .generateFile(
            options: .init(simplifyGroups: false)
        )

    if text == expected {
        return
    }

    if isRecordModeOn() {
        recordedGraphs.append(
            .init(
                file: "\(file)",
                line: Int(line),
                newGraphviz: text
            )
        )
    }

    XCTFail(
        """
        Expected produced graph to be

        \(expected)

        But found:

        \(text)

        Diff:

        \(text.makeDifferenceMarkString(against: expected))
        """,
        file: file,
        line: line
    )
}

internal func printGraphviz(dfa: TokenDFA) {
    let string = dfa.asGraphviz()
    print(string)
}

extension TokenDFA {
    func validate(
        file: StaticString = #file,
        line: UInt = #line
    ) {
        for edge in edges {
            assertNotNil(
                findNode(edge.start),
                message: "Validation failure: Found edge referencing non-existing node id \(edge.start)",
                file: file,
                line: line
            )
            assertNotNil(
                findNode(edge.end),
                message: "Validation failure: Found edge referencing non-existing node id \(edge.end)",
                file: file,
                line: line
            )
        }
    }

    func asGraphviz() -> GraphViz {
        let graph = GraphViz()

        // Sort nodes
        let sortedNodes = nodes.sorted { (lhs, rhs) in
            lhs.id < rhs.id
        }

        // Sort edges
        let sortedEdges = edges.sorted { (lhs, rhs) in
            if lhs.start < rhs.start {
                return true
            } else if lhs.start == rhs.start {
                return lhs.end < rhs.end
            } else {
                return false
            }
        }

        for node in sortedNodes {
            var attributes: GraphViz.Attributes = [:]
            if node.isAccept {
                attributes["shape"] = "doublecircle"
            } else {
                attributes["shape"] = "circle"
            }
            graph.createNode(
                label: "s\(node.id)",
                attributes: attributes
            )
        }
        for edge in sortedEdges {
            let start = "s\(edge.start)"
            let end = "s\(edge.end)"

            graph.addConnection(
                fromLabel: start,
                toLabel: end,
                label: edge.condition.description
            )
        }

        return graph
    }
}
