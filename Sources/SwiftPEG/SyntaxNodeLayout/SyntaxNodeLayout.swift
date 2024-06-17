/// Describes the static structure of a syntax node from a grammar rule.
indirect enum SyntaxNodeLayout: Equatable {
    /// A token terminal, with a given identifier/kind.
    case token(String)

    /// A rule name.
    case rule(String)

    /// Indicates the optional presence of a layout.
    case optional(SyntaxNodeLayout)

    /// Indicates that the layout alternates between one of the given layouts.
    case oneOf([SyntaxNodeLayout])

    /// Indicates that the layout is a collection of the given layout.
    case arrayOf(SyntaxNodeLayout)

    /// Indicates a fixed structure, containing a set of member layouts that are
    /// distinguished by identifiers.
    case fixed([FixedLayoutEntry])

    var isToken: Bool {
        switch self {
        case .token: true
        default: false
        }
    }

    var isRule: Bool {
        switch self {
        case .rule: true
        default: false
        }
    }

    var isOptional: Bool {
        switch self {
        case .optional: true
        default: false
        }
    }

    var isOneOf: Bool {
        switch self {
        case .oneOf: true
        default: false
        }
    }

    var isArrayOf: Bool {
        switch self {
        case .arrayOf: true
        default: false
        }
    }

    var isFixed: Bool {
        switch self {
        case .fixed: true
        default: false
        }
    }

    /// Recursively flattens constructions of 'oneOf' that contain only one
    /// element into that element itself.
    func flattened() -> Self {
        switch self {
        case .oneOf(let items) where items.count == 1:
            return items[0].flattened()

        case .oneOf(let items):
            return .oneOf(items.map({ $0.flattened() }))

        case .fixed(let entries):
            let entries = entries.map({
                FixedLayoutEntry(label: $0.label, layout: $0.layout.flattened())
            })

            return .fixed(entries)

        case .optional(let inner):
            return .optional(inner.flattened())

        case .arrayOf(let inner):
            return .arrayOf(inner.flattened())

        case .token, .rule:
            return self
        }
    }

    /// Recursively ensures that fixed layouts don't have members sharing the
    /// same name by deduplicating entries with a provided deduplicating delegate.
    func deduplicating(with delegate: some DeduplicateDelegate) -> Self {
        switch self {
        case .fixed(let entries):
            var unique = Set(entries.map(\.label))
            if unique.count == entries.count {
                return .fixed(entries)
            }

            var newEntries: [FixedLayoutEntry] = []
            var currentNames: Set<String> = []

            for entry in entries {
                var identifier = entry.label
                if currentNames.contains(identifier) {
                    identifier = delegate.deduplicate(identifier: identifier, currentIdentifiers: unique)
                }

                currentNames.insert(identifier)
                unique.insert(identifier)

                newEntries.append(.init(label: identifier, layout: entry.layout))
            }

            return .fixed(newEntries)

        case .oneOf(let layouts):
            let layouts = layouts.map { $0.deduplicating(with: delegate) }
            return .oneOf(layouts)

        case .optional(let layout):
            let layout = layout.deduplicating(with: delegate)
            return .optional(layout)

        case .arrayOf(let layout):
            let layout = layout.deduplicating(with: delegate)
            return .arrayOf(layout)

        case .token, .rule:
            return self
        }
    }

    /// Fetches the minimal size that this layout would take, in terms of 'members',
    /// in an occupying type.
    func minimumSize() -> Int {
        var result = 0

        switch self {
        case .token, .rule:
            result = 1

        case .fixed(let members):
            result = members.count

        case .arrayOf:
            result = 1

        case .oneOf(let layouts):
            for layout in layouts {
                result = max(result, layout.minimumSize())
            }

        case .optional:
            result = 1
        }

        return result
    }

    /// If this syntax node is a `.oneOf` of exclusively `.fixed` layouts, returns
    /// the result of factoring out the common elements of all fixed layouts into
    /// the root layout.
    ///
    /// The transformation turns a layout of:
    /// ```
    /// - one of:
    ///   - fixed:
    ///     - a: A
    ///     - b: B
    ///   - fixed:
    ///     - c: C
    ///     - a: A
    ///   - fixed:
    ///     - a: A
    ///     - d: D
    /// ```
    ///
    /// Into:
    /// ```
    /// - fixed:
    ///   - a: A
    ///   - b: optional: B
    ///   - c: optional: C
    ///   - d: optional: D
    /// ```
    func factoringCommonFixedElements() -> Self? {
        guard case .oneOf(let elements) = self else {
            return nil
        }

        if elements.count == 1 {
            return nil
        }

        var fixedEntries: [[FixedLayoutEntry]] = []

        for element in elements {
            guard case .fixed(let entries) = element else {
                return nil
            }

            fixedEntries.append(entries)
        }

        guard let factored = fixedEntries.factorCommon() else {
            return nil
        }

        var remaining: [FixedLayoutEntry] = []
        for entries in fixedEntries {
            for entry in entries {
                remaining.append(entry)
            }
        }

        remaining = remaining.map { entry in
            .init(label: entry.label, layout: .optional(entry.layout))
        }

        return .fixed(factored + remaining)
    }

    static func makeFixed(_ layout: KeyValuePairs<String, SyntaxNodeLayout>) -> Self {
        let entries = layout.map(FixedLayoutEntry.init)

        return .fixed(entries)
    }

    struct FixedLayoutEntry: Equatable {
        var label: String
        var layout: SyntaxNodeLayout
    }

    /// Provides deduplication support for syntax nodes.
    protocol DeduplicateDelegate {
        /// Requests the deduplication of a given identifier, with a given set of
        /// current identifiers, sans the duplicated identifier, of the current
        /// context.
        func deduplicate(identifier: String, currentIdentifiers: Set<String>) -> String
    }

    /// Simple deduplicating delegate that simply increments a numeric suffix to
    /// each identifier to deduplicate them.
    struct CounterDeduplicateDelegate: DeduplicateDelegate {
        func deduplicate(
            identifier: String,
            currentIdentifiers: Set<String>
        ) -> String {

            var counter = 1
            var current: String {
                "\(identifier)\(counter)"
            }

            while currentIdentifiers.contains(current) {
                counter += 1
            }

            return current
        }
    }
}

public struct SyntaxNode: Equatable {
    var name: String
    var ruleName: String
    var layout: SyntaxNodeLayout

    internal init(name: String, ruleName: String, layout: SyntaxNodeLayout) {
        self.name = name
        self.ruleName = ruleName
        self.layout = layout
    }

    internal init(name: String, layout: SyntaxNodeLayout) {
        self.name = name
        self.ruleName = name
        self.layout = layout
    }
}

// MARK: Debug print

extension SyntaxNodeLayout.FixedLayoutEntry {
    fileprivate func debugPrint(to output: DebugPrinter) {
        func line(_ text: String) {
            output.line(hasSiblings: true, text)
        }

        line("\(label.debugDescription):")
        output.indented(hasSiblings: false) {
            layout.debugPrint(to: output)
        }
    }
}

extension SyntaxNodeLayout {
    fileprivate func debugPrint(to output: DebugPrinter) {
        func line(hasSiblings: Bool = true, _ text: String) {
            output.line(hasSiblings: hasSiblings, text)
        }

        switch self {
        case .token(let kind):
            line(hasSiblings: false, "token: \(kind.debugDescription)")

        case .rule(let rule):
            line(hasSiblings: false, "rule: \(rule.debugDescription)")

        case .optional(let layout):
            line("optional:")
            output.indented(hasSiblings: false) {
                layout.debugPrint(to: output)
            }

        case .arrayOf(let layout):
            line("arrayOf:")
            output.indented(hasSiblings: false) {
                layout.debugPrint(to: output)
            }

        case .fixed(let entries):
            line("fixed:")

            for (i, entry) in entries.enumerated() {
                let isLast = i == entries.count - 1
                output.indented(hasSiblings: !isLast) {
                    entry.debugPrint(to: output)
                }
            }

        case .oneOf(let layouts):
            line("oneOf:")

            for (i, layout) in layouts.enumerated() {
                let isLast = i == layouts.count - 1
                output.indented(hasSiblings: !isLast) {
                    layout.debugPrint(to: output)
                }
            }
        }
    }
}

extension SyntaxNode: CustomDebugStringConvertible {
    public var debugDescription: String {
        let printer = DebugPrinter()
        debugPrint(to: printer)
        return printer.finish()
    }

    fileprivate func debugPrint(to output: DebugPrinter) {
        output.line(hasSiblings: true, "name: \(name)")
        output.line(hasSiblings: true, "ruleName: \(ruleName)")
        output.line(hasSiblings: true, "layout:")

        output.indented(hasSiblings: false) {
            layout.debugPrint(to: output)
        }
    }
}

private class DebugPrinter {
    private var buffer: String
    private var indentation = "│"
    private var lastHadSiblings = false
    private var indentStack: [IndentDepth] = []
    private var indentDepth: Int {
        indentStack.count
    }
    private var indentationString: String {
        if indentDepth == 0 {
            return ""
        }

        let indent = indentStack.dropLast().map(\.indentString).joined()

        let lastChar: String
        if indentStack.last?.hasSiblings == true {
            lastChar = "├"
        } else {
            lastChar = "╰"
        }

        return indent + lastChar
    }

    init() {
        buffer = ""
    }

    func finish() -> String {
        return buffer.trimmingWhitespaceTrail()
    }

    func indent(hasSiblings: Bool) {
        lastHadSiblings = false
        let depth = IndentDepth(hasSiblings: hasSiblings)
        indentStack.append(depth)
    }

    func unindent() {
        lastHadSiblings = false
        indentStack.removeLast()
    }

    func indented(hasSiblings: Bool, _ block: () -> Void) {
        indent(hasSiblings: hasSiblings)
        block()
        unindent()
    }

    func line(hasSiblings: Bool, _ line: String) {
        buffer += indentationString
        if hasSiblings {
            if lastHadSiblings {
                buffer += "├ "
            } else {
                buffer += "┬ "
            }
            lastHadSiblings = true
        } else {
            buffer += "─ "
        }
        buffer += line
        buffer += "\n"
    }

    private struct IndentDepth {
        var hasSiblings: Bool

        var indentString: String {
            if hasSiblings {
                "│"
            } else {
                " "
            }
        }
    }
}
