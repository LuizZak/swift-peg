/// Takes an input processed grammar and produces syntax node layout candidates
/// for a grammar.
public class SyntaxNodeLayoutGen {
    var grammar: InternalGrammar.Grammar
    var tokens: [InternalGrammar.TokenDefinition]

    public convenience init(processedGrammar: ProcessedGrammar) {
        self.init(
            grammar: processedGrammar.grammar,
            tokens: processedGrammar.tokens
        )
    }

    public init(
        grammar: InternalGrammar.Grammar,
        tokens: [InternalGrammar.TokenDefinition]
    ) {
        self.grammar = grammar
        self.tokens = tokens
    }

    /// Generates the syntax nodes for the input grammar.
    public func generateSyntaxNodes() throws -> [SyntaxNode] {
        let lookup = Lookup(tokens: tokens)

        return try grammar.rules.map { rule in
            try syntaxNode(for: rule, lookup: lookup)
        }
    }

    func syntaxNode(for rule: InternalGrammar.Rule, lookup: Lookup) throws -> SyntaxNode {
        var possibleLayouts: [SyntaxNodeLayout] = []

        for alt in rule.alts {
            let layout = try layoutForAlt(alt, lookup: lookup)
            possibleLayouts.append(layout)
        }

        var layout = SyntaxNodeLayout.oneOf(possibleLayouts)
        layout = layout.flattened()

        // Attempt to factor out the layout into known common patterns
        if let alternating = layout.factorFixedAlternationLayout() {
            layout = alternating
        } else if let factored = layout.factoringCommonFixedElements() {
            layout = factored
        }

        // Deduplicate identifiers
        let delegate = SyntaxNodeLayout.CounterDeduplicateDelegate()
        layout = layout.deduplicating(with: delegate)

        return .init(name: rule.name, ruleName: rule.name, layout: layout)
    }

    func layoutForAlt(_ alt: InternalGrammar.Alt, lookup: Lookup) throws -> SyntaxNodeLayout {
        var layouts: [LabeledLayout] = []

        for namedItem in alt.namedItems {
            guard let layout = try layoutForNamedItem(namedItem, lookup: lookup) else {
                continue
            }

            layouts.append(layout)
        }

        // Compact into fixed layout
        var fixedEntries: [SyntaxNodeLayout.FixedLayoutEntry] = []
        for layout in layouts {
            let fallbackName = "_n\(fixedEntries.count)"
            let fixedEntry = layout.asFixedLayoutEntry(fallbackName: fallbackName)

            fixedEntries.append(fixedEntry)
        }

        return .fixed(fixedEntries)
    }

    func layoutForNamedItem(_ namedItem: InternalGrammar.NamedItem, lookup: Lookup) throws -> LabeledLayout? {
        switch namedItem {
        case .item(let name?, let item, _):
            let layout = try layoutForItem(item, lookup: lookup)
            return .init(name: name, layout: layout.layout)

        case .item(_, let item, _):
            let layout = try layoutForItem(item, lookup: lookup)
            return layout

        case .lookahead:
            return nil
        }
    }

    func layoutForItem(_ item: InternalGrammar.Item, lookup: Lookup) throws -> LabeledLayout {
        switch item {
        case .atom(let atom):
            return try layoutForAtom(atom, lookup: lookup)

        case .optional(let atom):
            var layout = try layoutForAtom(atom, lookup: lookup)
            layout.layout = .optional(layout.layout)

            return layout

        case .optionalItems(let alts):
            let layouts = try alts.map {
                try layoutForAlt($0, lookup: lookup)
            }
            let layout = SyntaxNodeLayout.oneOf(layouts).flattened()

            return LabeledLayout(name: nil, layout: .optional(layout))

        case .gather(let sep, let node, _):
            let sepLayout = try layoutForAtom(sep, lookup: lookup)
            let nodeLayout = try layoutForAtom(node, lookup: lookup)

            var layout: LabeledLayout

            if let sepName = sepLayout.name, let nodeName = nodeLayout.name {
                layout = .init(
                    name: nodeLayout.name,
                    layout: .makeFixed([
                        nodeName: nodeLayout.layout,
                        sepName: .optional(sepLayout.layout),
                    ])
                )
            } else if let nodeName = nodeLayout.name {
                layout = .init(layout: .oneOf([
                    nodeLayout.layout,
                    .makeFixed([
                        nodeName: nodeLayout.layout,
                        "\(nodeName)_sep": .optional(sepLayout.layout),
                    ]),
                ]))
            } else {
                layout = nodeLayout
            }

            layout.layout = .collectionOf(layout.layout)

            return layout

        case .zeroOrMore(let atom, _),
            .oneOrMore(let atom, _):
            var layout = try layoutForAtom(atom, lookup: lookup)
            layout.layout = .collectionOf(layout.layout)

            return layout
        }
    }

    func layoutForAtom(_ atom: InternalGrammar.Atom, lookup: Lookup) throws -> LabeledLayout {
        switch atom {
        case .group(let alts):
            let layouts = try alts.map {
                try layoutForAlt($0, lookup: lookup)
            }
            var layout = SyntaxNodeLayout.oneOf(layouts)

            // Attempt to reduce nested fixed layouts
            layout = layout.flattened()

            switch layout {
            case .fixed(let entries) where entries.count == 1:
                return LabeledLayout(entries[0])
            default:
                break
            }

            return LabeledLayout(name: nil, layout: layout)

        case .ruleName(let name):
            let layout = SyntaxNodeLayout.rule(name)

            return LabeledLayout(name: name, layout: layout)

        case .anyToken(let tok), .token(let tok):
            let layout = SyntaxNodeLayout.token(tok)

            return LabeledLayout(name: tok, layout: layout)

        case .string(_, let trimmed):
            guard let name = lookup.tokenForTrimmedLiteral(trimmed) else {
                throw Error.unknownTokenLiteral(trimmedLiteral: trimmed)
            }
            let layout = SyntaxNodeLayout.token(name)

            return LabeledLayout(name: name, layout: layout)
        }
    }

    struct LabeledLayout {
        var name: String?
        var layout: SyntaxNodeLayout

        init(name: String? = nil, layout: SyntaxNodeLayout) {
            self.name = name
            self.layout = layout
        }

        init(_ fixedLayoutEntry: SyntaxNodeLayout.FixedLayoutEntry) {
            self.name = fixedLayoutEntry.label
            self.layout = fixedLayoutEntry.layout
        }

        func asFixedLayoutEntry(fallbackName: String) -> SyntaxNodeLayout.FixedLayoutEntry {
            return SyntaxNodeLayout.FixedLayoutEntry(
                label: name ?? fallbackName,
                layout: layout
            )
        }
    }

    class Lookup {
        var tokens: [InternalGrammar.TokenDefinition]

        init(tokens: [InternalGrammar.TokenDefinition]) {
            self.tokens = tokens
        }

        func tokenForTrimmedLiteral(_ literal: String) -> String? {
            for token in tokens {
                guard let string = token.string else {
                    continue
                }

                if string == literal {
                    return token.name
                }
            }

            return nil
        }
    }

    /// Errors raised during syntax node layout generation.
    public enum Error: Swift.Error, CustomStringConvertible {
        /// Error raised when a token literal was found that could not be mapped
        /// to a token.
        case unknownTokenLiteral(trimmedLiteral: String)

        public var description: String {
            switch self {
            case .unknownTokenLiteral(let trimmedLiteral):
                return "Unrecognized string literal could not be mapped to any known token: \(trimmedLiteral.debugDescription)"
            }
        }
    }
}
