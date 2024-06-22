/// Describes the static structure of a syntax node from a grammar rule.
public indirect enum SyntaxNodeLayout: Equatable {
    /// A token terminal, with a given identifier/kind.
    case token(String)

    /// A rule name.
    case rule(String)

    /// Indicates the optional presence of a layout.
    case optional(SyntaxNodeLayout)

    /// Indicates that the layout alternates between one of the given layouts.
    case oneOf([SyntaxNodeLayout])

    /// Indicates that the layout is a collection of the given layout.
    case collectionOf(SyntaxNodeLayout)

    /// Indicates a fixed structure, containing a set of member layouts that are
    /// distinguished by identifiers.
    case fixed([FixedLayoutEntry])

    /// Returns the array of children sub-layouts if this layout contains inner
    /// layouts, or an empty array, if not.
    public var subLayouts: [SyntaxNodeLayout] {
        switch self {
        case .optional(let inner), .collectionOf(let inner):
            return [inner]

        case .oneOf(let layouts):
            return layouts

        case .fixed(let fields):
            return fields.map(\.layout)

        case .token, .rule:
            return []
        }
    }

    public var isToken: Bool {
        switch self {
        case .token: true
        default: false
        }
    }

    public var isRule: Bool {
        switch self {
        case .rule: true
        default: false
        }
    }

    public var isOptional: Bool {
        switch self {
        case .optional: true
        default: false
        }
    }

    public var isOneOf: Bool {
        switch self {
        case .oneOf: true
        default: false
        }
    }

    public var isCollectionOf: Bool {
        switch self {
        case .collectionOf: true
        default: false
        }
    }

    public var isFixed: Bool {
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

        case .collectionOf(let inner):
            return .collectionOf(inner.flattened())

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

        case .collectionOf(let layout):
            let layout = layout.deduplicating(with: delegate)
            return .collectionOf(layout)

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

        case .collectionOf:
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
    /// the longest common prefix of layouts between all inner layouts.
    ///
    /// If the layouts have no common prefix, an empty array is returned.
    ///
    /// If this syntax node layout is not a `.oneOf` of exclusively `.fixed`
    /// layouts, or there is only one element within the `.oneOf` collection,
    /// `nil` is returned, instead.
    func commonFixedLayoutsPrefix() -> [FixedLayoutEntry]? {
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

        guard let index = fixedEntries.greatestCommonPrefixIndex() else {
            return nil
        }

        return Array(fixedEntries[0][0..<index])
    }

    /// If this syntax node layout is a `.oneOf` of exclusively `.fixed` layouts,
    /// where each layout can be constructed by the presence or absence of
    /// elements contained within the longest layout production, returns that
    /// construction, with elements that are not common between all fixed layouts
    /// marked as optional.
    func factorFixedAlternationLayout() -> Self? {
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

        guard
            let longestIndex = fixedEntries.indices.max(by: {
                fixedEntries[$0].count < fixedEntries[$1].count
            }),
            // Alternating layouts can only occur if one entry contains subsets
            // of all others, meaning only one entry has the greatest count of
            // items.
            fixedEntries.filter({ $0.count == fixedEntries[longestIndex].count }).count == 1
        else {
            return nil
        }

        guard let commonIndices = fixedEntries.greatestCommonIndices().map({ $0[longestIndex] }) else {
            return nil
        }

        let longest = fixedEntries[longestIndex]
        var unseenLongestIndices = Set(longest.indices)

        for (index, entry) in fixedEntries.enumerated() {
            guard index != longestIndex else {
                continue
            }

            let pair = [longest, entry]
            guard let common = pair.greatestCommonIndices() else {
                // Found layout that is disjoint with longest layout
                return nil
            }
            guard common[1] == Array(entry.indices) else {
                // Found layout that was unmatched in longest layout
                return nil
            }

            unseenLongestIndices.subtract(common[0])
        }

        guard unseenLongestIndices.isEmpty else {
            return nil
        }

        var newEntries: [FixedLayoutEntry] = []

        for (i, var element) in longest.enumerated() {
            let isOptional = !commonIndices.contains(i)
            if isOptional {
                element.layout = .optional(element.layout)
            }

            newEntries.append(element)
        }

        return .fixed(newEntries)
    }

    /// If this syntax node is a `.oneOf` of exclusively `.fixed` layouts, returns
    /// the result of factoring out the common elements of all fixed layouts into
    /// the root layout, keeping them in the order of appearance on the first
    /// fixed layout element.
    ///
    /// The transformation turns a layout of:
    /// ```
    /// - one of:
    ///   - fixed:
    ///     - e: e
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
    ///   - e: optional: E
    ///   - b: optional: B
    ///   - a: A
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

        guard let indices = fixedEntries.greatestCommonIndices() else {
            return nil
        }

        var emittedCommon = false
        var remaining: [FixedLayoutEntry] = []
        for (entries, commonIndices) in zip(fixedEntries, indices) {
            for (i, entry) in entries.enumerated() {
                guard !commonIndices.contains(i) else {
                    if !emittedCommon {
                        remaining.append(entry)
                    }
                    continue
                }

                var newEntry = entry
                newEntry.layout = .optional(entry.layout)
                remaining.append(newEntry)
            }

            emittedCommon = true
        }

        return .fixed(remaining)
    }

    static func makeFixed(_ layout: KeyValuePairs<String, SyntaxNodeLayout>) -> Self {
        let entries = layout.map(FixedLayoutEntry.init)

        return .fixed(entries)
    }

    public struct FixedLayoutEntry: Equatable {
        public var label: String
        public var layout: SyntaxNodeLayout

        public init(label: String, layout: SyntaxNodeLayout) {
            self.label = label
            self.layout = layout
        }
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
    public var name: String
    public var ruleName: String
    public var layout: SyntaxNodeLayout

    public init(name: String, ruleName: String, layout: SyntaxNodeLayout) {
        self.name = name
        self.ruleName = ruleName
        self.layout = layout
    }

    public init(name: String, layout: SyntaxNodeLayout) {
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

        case .collectionOf(let layout):
            line("collectionOf:")

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
        output.line(hasSiblings: true, "name: \(name.debugDescription)")
        output.line(hasSiblings: true, "ruleName: \(ruleName.debugDescription)")
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
