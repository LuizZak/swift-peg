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

        // Attempt to factor out the layout
        let factored = layout.factoringCommonFixedElements()
        layout = factored ?? layout

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
            let layout = SyntaxNodeLayout.oneOf(layouts)

            return LabeledLayout(name: nil, layout: .optional(layout))

        case .zeroOrMore(let atom, _),
            .oneOrMore(let atom, _),
            .gather(_, let atom, _):
            var layout = try layoutForAtom(atom, lookup: lookup)
            layout.layout = .arrayOf(layout.layout)

            return layout
        }
    }

    func layoutForAtom(_ atom: InternalGrammar.Atom, lookup: Lookup) throws -> LabeledLayout {
        switch atom {
        case .group(let alts):
            let layouts = try alts.map {
                try layoutForAlt($0, lookup: lookup)
            }
            let layout = SyntaxNodeLayout.oneOf(layouts)

            return LabeledLayout(name: nil, layout: layout)

        case .ruleName(let name):
            let layout = SyntaxNodeLayout.rule(name)

            return LabeledLayout(name: name, layout: layout)

        case .anyToken(let tok), .token(let tok):
            let layout = SyntaxNodeLayout.token(tok)

            return LabeledLayout(name: tok, layout: layout)

        case .string(_, let trimmed):
            let name = lookup.tokenForTrimmedLiteral(trimmed)
            let layout = SyntaxNodeLayout.token(name ?? trimmed)

            return LabeledLayout(name: name, layout: layout)
        }
    }

    struct LabeledLayout {
        var name: String?
        var layout: SyntaxNodeLayout

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
}
