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
    let grammar: CodeGen.Grammar
    let buffer: CodeStringBuffer
    var declContext: DeclarationsContext

    var remaining: [CodeGen.Rule] = []
    var ruleAliases: [String: String] = [:]

    /// Initializes a new `SwiftCodeGen`, preparing to generate a given grammar.
    /// 
    /// - Parameters:
    ///   - grammar: The grammar to generate.
    public init(grammar: CodeGen.Grammar) {
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

    func generateRule(_ rule: CodeGen.Rule) throws {
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

    func generateAlt(_ alt: CodeGen.Alt, in rule: CodeGen.Rule) throws {
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
            buffer.emit("return ")
            if let action = alt.action {
                buffer.emitLine(action.string)
            } else {
                buffer.emitLine("\(rule.type?.name ?? "Node")()")
            }
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

    /// Generates items as a sequence of optional bindings.
    func generateNamedItems(
        _ namedItems: [CodeGen.NamedItem],
        in rule: CodeGen.Rule
    ) throws {
        let commaEmitter = buffer.startConditionalEmitter()
        for namedItem in namedItems {
            try generateNamedItem(namedItem, commaEmitter, in: rule)
        }
    }

    func generateNamedItem(
        _ namedItem: CodeGen.NamedItem,
        _ commaEmitter: CodeStringBuffer.ConditionalEmitter,
        in rule: CodeGen.Rule
    ) throws {

        commaEmitter.conditional { buffer in
            buffer.emitLine(",")
        }

        let alias = self.alias(for: namedItem) ?? "_"

        switch namedItem {
        case .item(_, let item, _):
            buffer.emit("let \(alias) = ")
            try generateItem(item, in: rule)

        case .lookahead(let lookahead):
            try generateLookahead(lookahead, in: rule)
        }
    }

    func generateItem(_ item: CodeGen.Item, in rule: CodeGen.Rule) throws {
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
        _ lookahead: CodeGen.Lookahead,
        in rule: CodeGen.Rule
    ) throws {
        switch lookahead {
        case .positive(let atom):
            buffer.emit("try self.positiveLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .negative(let atom):
            buffer.emit("try self.positiveLookahead(")
            try buffer.emitInlinedBlock {
                try generateAtom(atom, in: rule)
            }
            buffer.emit(")")

        case .cut:
            buffer.emit("cut.toggleOn()")
        }
    }

    func generateAtom(
        _ atom: CodeGen.Atom,
        in rule: CodeGen.Rule
    ) throws {
        switch atom {
        case .group(let group):
            let aux = enqueueAuxiliaryRule(for: rule, suffix: "_group_", group)

            buffer.emit("try self.\(aux)()")
        
        case .ruleName(let ident):
            buffer.emit("try self.\(ident)()")

        case .token(let ident):
            buffer.emit("try self.\(ident)()")

        // Token
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
        for rule: CodeGen.Rule,
        suffix: String,
        _ alts: [CodeGen.Alt]
    ) -> String {

        let name = "_\(rule.name)_\(suffix)"
        return enqueueAuxiliaryRule(.init(name: name, alts: alts))
    }

    /// Enqueues a given auxiliary rule, returning its deduplicated name for
    /// further referencing.
    func enqueueAuxiliaryRule(_ rule: CodeGen.Rule) -> String {
        let decl = declContext.defineMethod(suggestedName: rule.name)
        var rule = rule
        rule.name = decl.name

        remaining.append(rule)

        return decl.name
    }
}

// MARK: Alias management

extension SwiftCodeGen {

    func alias(for rule: CodeGen.Rule) -> String {
        if let alias = self.ruleAliases[rule.name] {
            return alias
        }

        return rule.name
    }

    func alias(for namedItem: CodeGen.NamedItem) -> String? {
        switch namedItem {
        case .item(let name?, _, _):
            return name
        case .item(_, let item, _):
            return alias(for: item)
        case .lookahead:
            return nil
        }
    }

    func alias(for item: CodeGen.Item) -> String? {
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

    func alias(for atom: CodeGen.Atom) -> String? {
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
    func hasCut(_ node: CodeGen.Rule) -> Bool {
        hasCut(node.alts)
    }

    /// Returns `true` if one of the given alts makes use of cut (`~`) in one of
    /// its primary items.
    func hasCut(_ node: [CodeGen.Alt]) -> Bool {
        node.contains(where: hasCut)
    }

    /// Returns `true` if the given alt makes use of cut (`~`) in one of its
    /// primary items.
    func hasCut(_ node: CodeGen.Alt) -> Bool {
        node.items.contains(where: hasCut)
    }

    /// Returns `true` if the given named item makes use of cut (`~`).
    func hasCut(_ node: CodeGen.NamedItem) -> Bool {
        switch node {
        case .lookahead(.cut):
            return true
        default:
            return false
        }
    }
}

// MARK: - Convenience extensions

private extension CodeGen.Grammar {
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
