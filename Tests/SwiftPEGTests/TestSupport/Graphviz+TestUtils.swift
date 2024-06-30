import XCTest
import SwiftSyntax
import SwiftParser

@testable import SwiftPEG

func isRecordModeOn() -> Bool {
    recordMode || ProcessInfo.processInfo.environment["RECORD_GRAPHVIZ"] == "1"
}

internal var recordMode: Bool = false
internal var recordedGraphs: [GraphvizUpdateEntry] = []

func updateAllRecordedGraphviz() throws {
    guard isRecordModeOn() && !recordedGraphs.isEmpty else {
        return
    }
    defer { recordedGraphs.removeAll() }

    print("Updating test cases, please wait...")

    // Need to apply from bottom to top to avoid early rewrites offsetting later
    // rewrites
    let sorted = recordedGraphs.sorted(by: { $0.line > $1.line })

    for entry in sorted {
        try updateGraphvizCode(entry: entry)
    }

    print("Success!")

    recordMode = false
}

func throwErrorIfInGraphvizRecordMode(file: StaticString = #file) throws {
    struct TestError: Error, CustomStringConvertible {
        var description: String
    }

    if recordMode {
        throw TestError(description: "Failing tests until record mode is turned off in test file \(file)")
    }
}

func updateGraphvizCode(entry: GraphvizUpdateEntry) throws {
    let path = URL(fileURLWithPath: entry.file)

    let syntax = try Parser.parse(source: String(contentsOf: path))

    let converter = SourceLocationConverter(fileName: entry.file, tree: syntax)
    let rewriter = GraphvizUpdateRewriter(entry: entry, locationConverter: converter)

    let newSyntax = rewriter.visit(syntax)

    guard syntax.description != newSyntax.description else {
        return
    }

    try newSyntax.description.write(to: path, atomically: true, encoding: .utf8)
}

private class GraphvizUpdateRewriter: SyntaxRewriter {
    private var _convertNextString: Bool = false

    let entry: GraphvizUpdateEntry
    let locationConverter: SourceLocationConverter

    convenience init(
        file: String,
        line: Int,
        newGraphviz: String,
        locationConverter: SourceLocationConverter
    ) {

        self.init(
            entry: .init(file: file, line: line, newGraphviz: newGraphviz),
            locationConverter: locationConverter
        )
    }

    init(entry: GraphvizUpdateEntry, locationConverter: SourceLocationConverter) {
        self.entry = entry
        self.locationConverter = locationConverter
    }

    override func visit(_ node: FunctionCallExprSyntax) -> ExprSyntax {
        guard matchesEntryLine(node) else {
            return super.visit(node)
        }
        guard let ident = node.calledExpression.as(DeclReferenceExprSyntax.self) else {
            return super.visit(node)
        }
        guard matchesAssertIdentifier(ident) else {
            return super.visit(node)
        }

        let args = node.arguments
        guard args.count == 2 || args.count == 3 else {
            return super.visit(node)
        }

        _convertNextString = true
        defer { _convertNextString = false }

        return super.visit(node)
    }

    override func visit(_ node: StringLiteralExprSyntax) -> ExprSyntax {
        if _convertNextString {
            return ExprSyntax(updatingExpectedString(node))
        }

        return super.visit(node)
    }

    private func updatingExpectedString(_ exp: StringLiteralExprSyntax) -> StringLiteralExprSyntax {
        let content = formatGraphviz(entry.newGraphviz)

        let result = StringLiteralExprSyntax(
            openingQuote: .multilineStringQuoteToken(),
            segments: [
                .stringSegment(
                    .init(content: TokenSyntax.stringSegment(content))
                )
            ],
            closingQuote: .multilineStringQuoteToken()
        )

        return result
    }

    private func formatGraphviz(_ string: String, indentationInSpaces: Int = 16) -> String {
        let indentation = String(repeating: " ", count: indentationInSpaces)
        let lines = string.split(separator: "\n", omittingEmptySubsequences: false)
        let lineSeparator = "\n\(indentation)"

        return lineSeparator + lines.joined(separator: lineSeparator) + lineSeparator
    }

    private func matchesAssertIdentifier(_ syntax: DeclReferenceExprSyntax) -> Bool {
        return syntax.baseName.trimmed.description == "assertGraphviz"
    }

    private func matchesEntryLine(_ syntax: SyntaxProtocol) -> Bool {
        let loc = location(of: syntax)

        return loc.line == entry.line
    }

    private func location(of syntax: SyntaxProtocol) -> SourceLocation {
        syntax.sourceRange(converter: locationConverter).start
    }
}

internal struct GraphvizUpdateEntry {
    var file: String
    var line: Int
    var newGraphviz: String
}
