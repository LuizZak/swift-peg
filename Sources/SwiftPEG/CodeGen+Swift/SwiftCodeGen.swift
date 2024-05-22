/// Produces Swift code for parsing a grammar.
public class SwiftCodeGen {
    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the raw contents to print atop the generated parser code.
    public static let parserHeader: String = "parserHeader"

    /// Name of optional meta-property (`@<name> <value>`) from grammar file that
    /// indicates the name of the parser class to extend with the parsing methods.
    /// Assumes that the type exists already.
    public static let parserName: String = "parserName"

    let parserName: String
    let grammar: GrammarProcessor.Grammar
    let buffer: CodeStringBuffer
    var declContext: DeclarationsContext

    var remaining: [GrammarProcessor.Rule] = []
    var ruleAliases: [String: String] = [:]

    /// Initializes a new `SwiftCodeGen`, preparing to generate a given grammar.
    /// 
    /// - Parameters:
    ///   - grammar: The grammar to generate.
    public init(grammar: GrammarProcessor.Grammar) {
        self.grammar = grammar
        parserName = grammar.parserName() ?? "Parser"
        buffer = CodeStringBuffer()
        declContext = DeclarationsContext()
    }

    /// Generates Swift parser code.
    public func generateParser() throws -> String {
        buffer.resetState()

        if let header = grammar.parserHeader() {
            buffer.emitLine(header)
        }

        declContext = DeclarationsContext()
        declContext.push() // No need to pop as the context is meant to be replaced in new generate calls

        self.remaining = grammar.rules

        buffer.emit("extension \(parserName) ")
        try buffer.emitMembersBlock {
            try generateRemainingRules()
        }

        return buffer.finishBuffer()
    }

    func generateRemainingRules() throws {
        while !remaining.isEmpty {
            let next = remaining.removeFirst()
            try generateRule(next)
        }
    }

    func generateRule(_ rule: GrammarProcessor.Rule) throws {
        let type = rule.type?.name ?? "Node"
        let name = alias(for: rule)

        // Derive a doc comment for the generated rule
        let linePrefix = "///"

        buffer.emitLine("\(linePrefix) ```")
        buffer.emit("\(linePrefix) \(rule.name)")
        if let type = rule.type {
            buffer.emit("[\(type.name)]")
        }
        buffer.emitLine(":")
        for alt in rule.alts {
            buffer.emitLine("\(linePrefix)     | \(alt)")
        }
        buffer.emitLine("\(linePrefix)     ;")
        buffer.emitLine("\(linePrefix) ```")

        // @memoized/@memoizedLeftRecursive
        if rule.isRecursiveLeader {
            buffer.emitLine(#"@memoizedLeftRecursive("\#(name)")"#)
        } else {
            buffer.emitLine(#"@memoized("\#(name)")"#)
        }

        // @inlinable
        buffer.emitLine("@inlinable")
        
        // func <rule>() -> <node>
        buffer.emit("public func __\(name)() throws -> \(type)? ")
        try buffer.emitBlock {
            declContext.push()
            defer { declContext.pop() }
            declContext.defineLocal(suggestedName: "mark")
            declContext.defineLocal(suggestedName: "cut")

            buffer.emitLine("let mark = self.mark()")
            if hasCut(rule) {
                buffer.emitLine("var cut = CutFlag()")
            }

            for alt in rule.alts {
                try generateAlt(alt, in: rule)
            }

            buffer.emitLine("return nil")
        }

        // Separate rule methods
        buffer.ensureDoubleNewline()
    }

    func generateAlt(_ alt: GrammarProcessor.Alt, in rule: GrammarProcessor.Rule) throws {
        if alt.items.isEmpty { return }

        declContext.push()
        defer { declContext.pop() }

        buffer.emitNewline()

        // if block
        buffer.emitLine("if")
        try buffer.indented {
            try generateNamedItems(alt.items, in: rule)
        }
        buffer.ensureNewline()

        // return <result>
        buffer.emitBlock {
            generateAltReturn(alt, in: rule)
        }
        
        // Alt failure results in a restore to a previous mark
        buffer.emitNewline()
        buffer.emitLine("self.restore(mark)")

        if hasCut(rule) {
            buffer.emitNewline()
            buffer.emit("if cut.isOn ")
            buffer.emitBlock {
                buffer.emitLine("return nil")
            }
        }
    }

    /// Generates `return <alt result>` for a successful alt match.
    func generateAltReturn(
        _ alt: GrammarProcessor.Alt,
        in rule: GrammarProcessor.Rule
    ) {
        buffer.emit("return ")

        if let action = alt.action {
            buffer.emitLine(action.string)
            return
        }

        // If no action is specified, attempt to return instead the named
        // item within the alt, if it's the only named item in the alt.
        if alt.items.count == 1, let alias = alias(for: alt.items[0]) {
            buffer.emitLine(escapeIdentifier(alias))
            return
        }

        // Fallback: Return an initialization of the associated node type.
        buffer.emitLine("\(rule.type?.name ?? "Node")()")
    }

    /// Generates items as a sequence of optional bindings.
    func generateNamedItems(
        _ namedItems: [GrammarProcessor.NamedItem],
        in rule: GrammarProcessor.Rule
    ) throws {
        let commaEmitter = buffer.startConditionalEmitter()
        for namedItem in namedItems {
            try generateNamedItem(namedItem, commaEmitter, in: rule)
        }
    }

    func generateNamedItem(
        _ namedItem: GrammarProcessor.NamedItem,
        _ commaEmitter: CodeStringBuffer.ConditionalEmitter,
        in rule: GrammarProcessor.Rule
    ) throws {

        commaEmitter.conditional { buffer in
            buffer.emitLine(",")
        }

        let alias = self.alias(for: namedItem) ?? "_"

        switch namedItem {
        case .item(_, let item, _):
            var resolvedName = alias
            if alias != "_" {
                resolvedName = declContext.defineLocal(suggestedName: alias, type: nil).name
            }

            buffer.emit("let \(escapeIdentifier(resolvedName)) = ")
            try generateItem(item, in: rule)

        case .lookahead(let lookahead):
            try generateLookahead(lookahead, in: rule)
        }
    }

    func generateItem(_ item: GrammarProcessor.Item, in rule: GrammarProcessor.Rule) throws {
        switch item {
        case .optional(let atom):
            buffer.emit("try self.optional(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .optionalItems(let alts):
            let aux = enqueueAuxiliaryRule(for: rule, suffix: "_opt", alts)
            buffer.emit("try self.\(aux)()")

        case .gather(let sep, let node):
            // TODO: Decompose gathers
            break

        case .zeroOrMore(let atom):
            buffer.emit("try self.repeatZeroOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")
            
        case .oneOrMore(let atom):
            buffer.emit("try self.repeatOneOrMore(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .atom(let atom):
            try generateAtom(atom, in: rule)
        }
    }

    func generateLookahead(
        _ lookahead: GrammarProcessor.Lookahead,
        in rule: GrammarProcessor.Rule
    ) throws {
        switch lookahead {
        case .positive(let atom):
            buffer.emit("try self.positiveLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .negative(let atom):
            buffer.emit("try self.negativeLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .cut:
            buffer.emit("cut.toggleOn()")
        }
    }

    func generateAtom(
        _ atom: GrammarProcessor.Atom,
        in rule: GrammarProcessor.Rule
    ) throws {
        switch atom {
        case .group(let group):
            let aux = enqueueAuxiliaryRule(for: rule, suffix: "_group_", group)

            buffer.emit("try self.\(aux)()")

        case .ruleName(let ident):
            buffer.emit("try self.\(escapeIdentifier(ident))()")

        case .token(let ident):
            buffer.emit("try self.\(escapeIdentifier(ident))()")

        // Token literal
        case .string(let string, let raw):
            var result = string

            // Avoid emitting single-quoted string literals
            if string.hasPrefix("'") {
                result = #""\#(raw)""#
            } else {
                result = string
            }

            // Escape backslashes contents
            result = result.replacing("\\", with: #"\\"#)

            buffer.emit("try self.expect(\(result))")
        }
    }
}

// MARK: Auxiliary method management

extension SwiftCodeGen {
    /// Enqueues an auxiliary rule to be generated based on a given rule as context.
    /// Returns the deduplicated, unique method name to use as a reference for
    /// further code generation.
    func enqueueAuxiliaryRule(
        for rule: GrammarProcessor.Rule,
        suffix: String,
        _ alts: [GrammarProcessor.Alt]
    ) -> String {

        let name = "_\(rule.name)_\(suffix)"
        return enqueueAuxiliaryRule(.init(name: name, alts: alts))
    }

    /// Enqueues a given auxiliary rule, returning its deduplicated name for
    /// further referencing.
    func enqueueAuxiliaryRule(_ rule: GrammarProcessor.Rule) -> String {
        let decl = declContext.defineMethod(suggestedName: rule.name)
        var rule = rule
        rule.name = decl.name

        remaining.append(rule)

        return decl.name
    }
}

// MARK: Alias management

extension SwiftCodeGen {

    /// Escapes the given identifier to something that can be declared as a local
    /// or method name in Swift.
    func escapeIdentifier(_ ident: String) -> String {
        // Wildcard; return unchanged
        if ident == "_" {
            return ident
        }

        // Identifier already escaped; return unchanged
        if ident.hasPrefix("`") && ident.hasSuffix("`") {
            return ident
        }

        if GrammarProcessor.invalidBareIdentifiers.contains(ident) {
            return "`\(ident)`"
        }

        return ident
    }

    /// Returns the alias for referencing the given rule in code with `self.<rule alias>()`.
    func alias(for rule: GrammarProcessor.Rule) -> String {
        if let alias = self.ruleAliases[rule.name] {
            return alias
        }

        return rule.name
    }

    /// Returns the alias for referencing the given named item in a matched alt's
    /// `if let <alias> = <item>` statement.
    /// 
    /// Returns `nil` if no suitable alias was found.
    func alias(for namedItem: GrammarProcessor.NamedItem) -> String? {
        switch namedItem {
        case .item(let name?, _, _):
            return name
        case .item(_, let item, _):
            return alias(for: item)
        case .lookahead:
            return nil
        }
    }

    /// Returns the alias for referencing the given item in a matched alt's
    /// `if let <alias> = <item>` statement.
    /// 
    /// Returns `nil` if no suitable alias was found.
    func alias(for item: GrammarProcessor.Item) -> String? {
        switch item {
        case .atom(let atom),
            .zeroOrMore(let atom),
            .oneOrMore(let atom),
            .optional(let atom):
            return alias(for: atom)

        case .gather, .optionalItems:
            return nil
        }
    }

    /// Returns the alias for referencing the given atom in a matched alt's
    /// `if let <alias> = <item>` statement.
    /// 
    /// If the alt is a token, returns the token's identifier lowercased. If
    /// it's a rule name, return the rule name itself, otherwise returns `nil`.
    func alias(for atom: GrammarProcessor.Atom) -> String? {
        switch atom {
        case .token(let ident):
            return ident.lowercased()
            
        case .ruleName(let ident):
            return ident

        case .group, .string:
            return nil
        }
    }
}

// MARK: - Cut detection

extension SwiftCodeGen {

    /// Returns `true` if the rule makes use of cut (`~`) in one of its primary
    /// alts.
    func hasCut(_ node: GrammarProcessor.Rule) -> Bool {
        hasCut(node.alts)
    }

    /// Returns `true` if one of the given alts makes use of cut (`~`) in one of
    /// its primary items.
    func hasCut(_ node: [GrammarProcessor.Alt]) -> Bool {
        node.contains(where: hasCut)
    }

    /// Returns `true` if the given alt makes use of cut (`~`) in one of its
    /// primary items.
    func hasCut(_ node: GrammarProcessor.Alt) -> Bool {
        node.items.contains(where: hasCut)
    }

    /// Returns `true` if the given named item makes use of cut (`~`).
    func hasCut(_ node: GrammarProcessor.NamedItem) -> Bool {
        switch node {
        case .lookahead(.cut):
            return true
        default:
            return false
        }
    }
}

// MARK: - Convenience extensions

private extension GrammarProcessor.Grammar {
    func parserHeader() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.parserHeader)
    }

    func parserName() -> String? {
        return _stringOrIdentMeta(named: SwiftCodeGen.parserName)
    }

    private func _stringOrIdentMeta(named name: String) -> String? {
        guard
            let meta = metas.first(where: { $0.name == name })
        else {
            return nil
        }

        switch meta.value {
        case .string(let value)?:
            return value

        case .identifier(let value)?:
            return value

        case nil:
            return nil
        }
    }
}
