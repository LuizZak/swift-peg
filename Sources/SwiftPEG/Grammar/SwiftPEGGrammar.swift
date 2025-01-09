/// Namespace for SwiftPEG grammar grammar objects.
public enum SwiftPEGGrammar {}

extension SwiftPEGGrammar {
    public typealias Token = GrammarParserToken

    /// Base class for grammar nodes.
    public class GrammarNode: Node {
        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: NodeVisitorType {
            if
                let cast = visitor as? any GrammarNodeVisitorType,
                let result = try self.accept(cast) as? Visitor.VisitResult
            {
                return result
            }

            return try super.accept(visitor)
        }

        /// Accepts a given grammar-node visitor into this node.
        public func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try super.accept(visitor)
        }
    }

    /// A grammar file.
    ///
    /// Represents the construct:
    /// ```
    /// grammar
    ///     | metas=meta* rules=rule+
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Grammar: GrammarNode {
        /// List of meta-properties described in the grammar.
        @NodeProperty
        private var _metas: [Meta] = []

        /// List of rules in the grammar.
        @NodeProperty
        private var _rules: [Rule] = []

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A grammar meta-property.
    ///
    /// Represents the construct:
    /// ```
    /// meta:
    ///     | "@" name=IDENTIFIER values=metaValue* ';'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Meta: GrammarNode {
        /// The name of this meta-property.
        @NodeRequired
        public var name: Token

        /// The values associated with this meta-property.
        @NodeProperty
        var _values: [MetaValue]

        public override var shortDebugDescription: String {
            "@\(name.string) \(values.map(\.shortDebugDescription).joined(separator: " "))"
        }

        public init(name: Token, value: MetaValue?) {
            self._values = value.map { [$0] } ?? []
            self.name = name

            super.init()

            self._values.forEach({
                $0.parent = self
            })
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// Base class for meta property value nodes.
    ///
    /// Represents the constructs:
    /// ```
    /// metaValue:
    ///     | metaValueIdent
    ///     | metaValueString
    ///     ;
    /// ```
    public class MetaValue: GrammarNode {
        /// Performs a deep copy of this node.
        public func deepCopy() -> MetaValue {
            fatalError("Must be overridden by subclasses.")
        }
    }

    /// A value of a meta-property that is an identifier.
    ///
    /// Represents the construct:
    /// ```
    /// metaValueIdent: IDENTIFIER ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "MetaValue")
    public final class MetaIdentifierValue: MetaValue {
        /// The associated identifier value.
        @NodeRequired
        public var identifier: Token

        public override var shortDebugDescription: String { String(identifier.string) }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A value of a meta-property that is a string.
    ///
    /// Represents the construct:
    /// ```
    /// metaValueString: STRING ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "MetaValue")
    public final class MetaStringValue: MetaValue {
        /// The associated string value.
        @NodeRequired
        public var string: GrammarString

        public override var shortDebugDescription: String { string.asStringLiteral() }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A grammar rule.
    ///
    /// Represents the construct:
    /// ```
    /// rule:
    ///     | ruleName ruleParameters? ":" action? failAction? '|'? alts ';'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Rule: GrammarNode, CustomStringConvertible {
        /// The name of this rule.
        @NodeProperty
        var _name: RuleName

        /// An optional list of parameters associated with this rule.
        @NodeProperty
        var _parameters: RuleParameters?

        /// An optional action that is executed at the start of the rule's parse
        /// method.
        @NodeProperty
        var _action: Action?

        /// An optional action that is executed when the rule's parse method fails.
        @NodeProperty
        var _failAction: Action?

        /// List of one or more alts associated with this rule.
        @NodeProperty
        var _alts: [Alt]

        /// Flag used by `GrammarProcessor` to indicate whether this rule is left-recursive.
        var isLeftRecursive: Bool = false

        /// If `isLeftRecursive` is `true`, indicates whether this rule is the
        /// leader of the left recursion.
        var isLeftRecursiveLead: Bool = false

        /// Flag used by `GrammarProcessor` to indicate that this rule is reachable
        /// from a chosen starting rule.
        var isReachable: Bool = true

        /// Convenience for `String(name.name.string)`
        public var ruleName: String {
            String(name.name.string)
        }

        public var description: String {
            "\(type(of: self))(ruleName: \(ruleName))"
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        /// Internal nullability visitor method to check if this rule is nullable.
        func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            // Recursive?
            if visitor.didVisit(self) {
                return false
            }
            visitor.markVisit(self)

            for alt in alts {
                if alt.visitNullable(visitor) {
                    return true
                }
            }
            return false
        }

        /// After nullability computation, this method is used to gather the
        /// initial rules that this rule invokes, for use in left-recursive
        /// rule detection.
        func initialNames() -> Set<String> {
            return alts.reduce([]) { $0.union($1.initialNames()) }
        }

        /// Returns a set of all named identifiers referenced within this rule.
        func allNames() -> Set<String> {
            alts.reduce([]) { $0.union($1.allNames()) }
        }
    }

    /// A grammar rule's name.
    ///
    /// Represents the construct:
    /// ```
    /// ruleName:
    ///     | name=IDENTIFIER '[' ~ type=swiftType ']'
    ///     | name=IDENTIFIER
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class RuleName: GrammarNode {
        /// The rule's name.
        @NodeRequired
        public var name: Token

        /// A type name directly associated with this rule.
        @NodeRequired
        public var type: CommonAbstract.SwiftType?

        public override var shortDebugDescription: String { String(name.string) }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A grammar rule's parameter list.
    ///
    /// Represents the construct:
    /// ```
    /// ruleParameters: '(' ','.ruleParameter+ ')' ;
    /// ```
    @GeneratedNodeType<Node>
    public final class RuleParameters: GrammarNode {
        /// The list of parameters associated with this rule parameter list.
        @NodeProperty
        var _parameters: [RuleParameter]
    }

    /// A grammar rule's parameter list's argument.
    ///
    /// Represents the construct:
    /// ```
    /// ruleParameter: IDENTIFIER ':' swiftType ;
    /// ```
    @GeneratedNodeType<Node>
    public final class RuleParameter: GrammarNode {
        /// The name associated with this parameter.
        @NodeRequired
        public var name: Token

        /// The type associated with this parameter.
        @NodeRequired
        public var type: CommonAbstract.SwiftType
    }

    /// An alternative, or a sequence of items that must succeed sequentially
    /// for the alt to succeed.
    ///
    /// Represents the construct:
    /// ```
    /// alt:
    ///     | altLabel? namedItems action? failAction?
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Alt: GrammarNode {
        /// An optional label associated with this alternative.
        @NodeProperty
        var _altLabel: AltLabel? = nil

        /// The items belonging to this alt.
        @NodeProperty
        var _namedItems: [NamedItem]

        /// An optional action associated with this alt, executed when the alt
        /// succeeds.
        @NodeProperty
        var _action: Action?

        /// An optional action associated with this alt, executed when the alt
        /// fails, before proceeding with any alternative alt.
        @NodeProperty
        var _failAction: Action?

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        /// Internal nullability visitor method to check if rules are nullable.
        func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            for item in namedItems {
                if !item.visitNullable(visitor) {
                    return false
                }
            }

            return true
        }

        func initialNames() -> Set<String> {
            var names: Set<String> = []
            for item in namedItems {
                names.formUnion(item.initialNames())
                if !item.nullable {
                    break
                }
            }
            return names
        }

        /// Returns a set of all named identifiers referenced within this node.
        func allNames() -> Set<String> {
            namedItems.reduce([]) { $0.union($1.allNames()) }
        }
    }

    /// An optional label attached to an alternative, used during syntax node
    /// layout.
    ///
    /// Represents the construct:
    /// ```
    /// altLabel:
    ///     | name=IDENTIFIER ':'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class AltLabel: GrammarNode {
        /// The identifier associated with this alt label.
        @NodeRequired
        public var name: Token
    }

    /// An item, or segment of an alt, for which a name can be attributed.
    ///
    /// Represents the construct:
    /// ```
    /// namedItem:
    ///     | name=IDENTIFIER '[' type=swiftType ']' '=' ~ item
    ///     | name=IDENTIFIER '=' ~ item
    ///     | item
    ///     | lookahead
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class NamedItem: GrammarNode {
        /// An optional name associated with this item.
        @NodeRequired
        public var name: Token?

        /// Item associated with this named item.
        @NodeProperty
        var _item: Item?

        /// A type hint for this named production.
        @NodeRequired
        public var type: CommonAbstract.SwiftType?

        /// Lookahead associated with this named item.
        @NodeProperty
        var _lookahead: LookaheadOrCut?

        var nullable: Bool = false

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        /// Internal nullability visitor method to check if rules are nullable.
        func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            let result: Bool
            if let item {
                result = item.visitNullable(visitor)
            } else if let lookahead {
                result = lookahead.visitNullable(visitor)
            } else {
                // Invalid named item?
                result = false
            }
            nullable = result
            return nullable
        }

        func initialNames() -> Set<String> {
            if let item {
                return item.initialNames()
            }

            return []
        }

        /// Returns a set of all named identifiers referenced within this node.
        func allNames() -> Set<String> {
            return item?.allNames() ?? lookahead?.allNames() ?? []
        }
    }

    /// Base class for a node that represents either a positive/negative lookahead,
    /// or a cut node.
    ///
    /// Represents the construct:
    /// ```
    /// lookahead:
    ///     | '&''&' atom
    ///     | '&' atom
    ///     | '!' atom
    ///     | '~'
    ///     ;
    /// ```
    public class LookaheadOrCut: GrammarNode {
        /// Internal nullability visitor method to check if rules are nullable.
        func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        /// Performs a deep-copy of this node.
        public func deepCopy() -> LookaheadOrCut {
            fatalError("Must be overridden by subclasses.")
        }

        /// Returns a set of all named identifiers referenced within this node.
        func allNames() -> Set<String> {
            []
        }
    }

    /// A forced atom.
    ///
    /// Forced atoms are in-grammar requirements that a production must succeed
    /// otherwise a syntax error is raised on the spot.
    ///
    /// Represents the construct:
    ///
    /// ```
    /// '&''&' atom ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "LookaheadOrCut")
    public final class Forced: LookaheadOrCut {
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "&&" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// A positive lookahead.
    ///
    /// Positive lookaheads are required to match the associated atom in order
    /// to match, but do not consume the atom in the process.
    ///
    /// Represents the construct:
    ///
    /// ```
    /// '&' atom ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "LookaheadOrCut")
    public final class PositiveLookahead: LookaheadOrCut {
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "&" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// A negative lookahead.
    ///
    /// Positive lookaheads are required to _not_ match the associated atom in
    /// order to match, but do not consume the atom in the process.
    ///
    /// Represents the construct:
    /// ```
    /// '!' atom ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "LookaheadOrCut")
    public final class NegativeLookahead: LookaheadOrCut {
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "!" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// A cut ('~') node.
    ///
    /// Cuts insert forced failure points in sequences of items in an alt, such
    /// that if the parsing fails, the parsing does not proceed further into
    /// other alts of the same rule.
    ///
    /// Represents the construct:
    /// ```
    /// '~' ;
    /// ```
    public final class Cut: LookaheadOrCut {
        public override var shortDebugDescription: String { "~" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// Base class for items; an atom or similar single-value construct that is
    /// part of an alt.
    ///
    /// Represents the construct:
    /// ```
    /// item:
    ///     | '[' ~ alts ']'
    ///     | atom '?'
    ///     | atom '*'
    ///     | atom '+'
    ///     | sep=atom '.' node=atom '+'
    ///     | atom
    ///     ;
    /// ```
    public class Item: GrammarNode {
        /// Internal nullability visitor method to check if rules are nullable.
        func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        func initialNames() -> Set<String> {
            return []
        }

        /// Returns a set of all named identifiers referenced within this node.
        func allNames() -> Set<String> {
            return []
        }

        /// Performs a deep copy of this node.
        public func deepCopy() -> Item {
            fatalError("Must be overridden by subclasses.")
        }
    }

    /// An optional set of items.
    ///
    /// Optional items are consumed if present but are not required in order to
    /// match.
    ///
    /// Represents the construct:
    /// ```
    /// '[' ~ alts ']' ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Item")
    public final class OptionalItems: Item {
        /// The alts that are optionally wrapped.
        @NodeProperty
        var _alts: [Alt]

        public override var shortDebugDescription: String { "?" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        override func initialNames() -> Set<String> {
            return alts.reduce([]) { $0.union($1.initialNames()) }
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            alts.reduce([]) { $0.union($1.allNames()) }
        }
    }

    /// An optional item attached to a single atom.
    ///
    /// Optional items are consumed if present but are not required in order to
    /// match.
    ///
    /// Represents the construct:
    /// ```
    /// atom '?' ;
    /// ```
    ///
    /// This is a short-form of the more general `OptionalItems` node:
    /// ```
    /// '[' ~ alts ']' ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Item")
    public final class OptionalItem: Item {
        /// The atom that is optionally wrapped.
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "?" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// An item that must match its associated atom zero or more times to succeed.
    ///
    /// Optional items are consumed for as long as they match, but are not
    /// required in order to match.
    ///
    /// Represents the construct:
    /// ```
    /// atom '*' ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Item")
    public final class ZeroOrMoreItem: Item {
        /// The atom that is wrapped.
        @NodeProperty
        var _atom: Atom

        /// The repetition mode for this one-or-more item.
        @NodeRequired
        public var repetitionMode: CommonAbstract.RepetitionMode = .standard

        public override var shortDebugDescription: String { "*\(repetitionMode._suffixString)" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// An item that must match its associated atom one or more times to succeed.
    ///
    /// The first item is always required to match, with remaining optional items
    /// being consumed for as long as they match.
    ///
    /// Represents the construct:
    /// ```
    /// atom '+' ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Item")
    public final class OneOrMoreItem: Item {
        /// The atom that is wrapped.
        @NodeProperty
        var _atom: Atom

        /// The repetition mode for this one-or-more item.
        @NodeRequired
        public var repetitionMode: CommonAbstract.RepetitionMode = .standard

        public override var shortDebugDescription: String { "+\(repetitionMode._suffixString)" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// A gather match, or a sequence of atoms that are separated by another atom.
    ///
    /// Represents the construct:
    /// ```
    /// sep=atom '.' node=atom '+' ;
    /// ```
    ///
    /// It is a shorthand form of the more general compound construct:
    /// ```
    /// node0=atom (sep=atom node1=atom)+ ;
    /// ```
    ///
    /// Gathers are preferred over the above construct due to only binding two
    /// atoms, instead of three, including the same atom at different positions,
    /// which is cumbersome to handle in code.
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Item")
    public final class GatherItem: Item {
        /// The separator that is matched in between items.
        @NodeProperty
        var _sep: Atom

        /// The item that is separated.
        @NodeProperty
        var _item: Atom

        /// The repetition mode for this gather item.
        @NodeRequired
        public var repetitionMode: CommonAbstract.RepetitionMode = .standard

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        override func initialNames() -> Set<String> {
            return item.initialNames()
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            sep.allNames().union(item.allNames())
        }
    }

    /// An item consisting of an atom.
    ///
    /// Represents the construct:
    /// ```
    /// atom ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Item")
    public final class AtomItem: Item {
        @NodeProperty
        var _atom: Atom

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            atom.allNames()
        }
    }

    /// Base class for atoms of an alternative. Atoms are the smallest unit of
    /// division within a sequence of items, and always have to match fully.
    ///
    /// Represents the constructs:
    /// ```
    /// atom:
    ///     | '(' ~ alts ')'
    ///     | IDENTIFIER
    ///     | string
    ///     ;
    /// ```
    public class Atom: GrammarNode {
        /// Internal nullability visitor method to check if rules are nullable.
        func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        func initialNames() -> Set<String> {
            return []
        }

        /// Returns a set of all named identifiers referenced within this atom.
        func allNames() -> Set<String> {
            return []
        }

        public func deepCopy() -> Atom {
            fatalError("Must be overridden by subclasses.")
        }
    }

    /// A group atom with a sequence of alts in parenthesis.
    ///
    /// Represents the construct:
    /// ```
    /// '(' ~ alts ')' ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Atom")
    public final class GroupAtom: Atom {
        /// The alts that are grouped.
        @NodeProperty
        var _alts: [Alt]

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            for alt in alts {
                if alt.visitNullable(visitor) {
                    return true
                }
            }
            return false
        }

        override func initialNames() -> Set<String> {
            return alts.reduce([]) { $0.union($1.initialNames()) }
        }

        override func allNames() -> Set<String> {
            return alts.reduce([]) { $0.union($1.allNames()) }
        }
    }

    /// A string literal atom.
    ///
    /// Represents the construct:
    /// ```
    /// string ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Atom")
    public final class StringAtom: Atom {
        /// The string associated with this atom.
        ///
        /// - note: Includes the quotes.
        @NodeRequired
        public var string: GrammarString

        /// Returns the value of `self.value` with any surrounding string quotes
        /// stripped.
        ///
        /// Convenience for `self.string.valueTrimmingQuotes`.
        public var valueTrimmingQuotes: String {
            string.rawContents()
        }

        public override var shortDebugDescription: String { string.asStringLiteral() }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }
    }

    /// An identifier atom.
    ///
    /// Represents the construct:
    /// ```
    /// atom: IDENTIFIER atomParameters? ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Atom")
    public final class IdentAtom: Atom {
        /// The identifier associated with this atom.
        @NodeRequired
        public var identifier: Token

        /// An optional list of arguments associated with this atom.
        @NodeProperty
        var _parameters: AtomParameters?

        /// The identity of this atom.
        @NodeRequired
        public var identity: Identity

        /// Convenience for `self.identifier.identifier`.
        public var name: Substring {
            identifier.string
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            if let rule = visitor.rule(named: String(name)) {
                return rule.visitNullable(visitor)
            }

            return false
        }

        override func initialNames() -> Set<String> {
            return [String(name)]
        }

        /// Returns a set of all named identifiers referenced within this node.
        override func allNames() -> Set<String> {
            return [String(name)]
        }

        /// Specifies the identity of an identifier atom.
        public enum Identity: Hashable {
            /// The identifier refers to a rule in the grammar.
            case ruleName

            /// The identifier refers to a token in the grammar.
            case token

            /// The identifier refers to the special 'any token' in the grammar.
            case anyToken

            /// The identifier is unresolved.
            case unresolved
        }
    }

    /// A parameter list associated with a named atom item.
    ///
    /// Represents the construct:
    /// ```
    /// atomParameters: '(' ','.atomParameter+ ')' ;
    /// ```
    @GeneratedNodeType<Node>
    public final class AtomParameters: Node {
        /// The parameters associated with this parameter list.
        @NodeProperty
        var _parameters: [AtomParameter]
    }

    /// A parameter associated with a named atom item.
    ///
    /// Represents the construct:
    /// ```
    /// atomParameter: IDENTIFIER ':' action ;
    /// ```
    @GeneratedNodeType<Node>
    public final class AtomParameter: Node {
        /// The label associated with this atom parameter.
        @NodeRequired
        public var label: Token

        /// The action associated with this atom parameter.
        @NodeProperty
        var _action: Action
    }

    /// An action of an alt. Represents a segment of code that is inserted on
    /// the generated code for when the alt associated with an action is matched
    /// or not.
    ///
    /// Represents the constructs:
    /// ```
    /// action: '{' ~ balancedTokens? '}' ;
    /// ```
    /// and:
    /// ```
    /// failAction: '!!' '{' ~ balancedTokens? '}' ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Action: GrammarNode {
        /// Balanced tokens contained within this action.
        @NodeRequired
        public var balancedTokens: TokenSequence?

        public override var shortDebugDescription: String {
            guard let balancedTokens = balancedTokens else {
                return ""
            }

            return "{\(balancedTokens)}"
        }

        public var rawAction: String {
            balancedTokens?.raw() ?? ""
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> Visitor.VisitResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// Represents a token file declaration.
    ///
    /// Represents the construct:
    /// ```
    /// tokensFileDeclaration:
    ///     | tokenDefinition
    ///     | tokenChannelDeclaration
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public class TokenFileDeclaration: GrammarNode {

    }

    /// Represents a token definition collected from a tokens file.
    ///
    /// Represents the construct:
    /// ```
    /// tokenDefinition:
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER '[' tokenCodeReference=STRING ']' ':' ~ tokenSyntax ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER '[' tokenCodeReference=STRING ']' ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER ':' ~ tokenSyntax ';'
    ///     | tokenOrFragmentSpecifier name=IDENTIFIER ';'
    ///     ;
    ///
    /// tokenOrFragmentSpecifier:
    ///     | '$'
    ///     | '%'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "TokenFileDeclaration")
    public final class TokenDefinition: TokenFileDeclaration {
        /// The identifier for the token.
        @NodeRequired
        public var name: Token

        @NodeRequired
        public var isFragment: Bool

        /// The statically-resolvable reference to the token definition.
        /// Is expected to resolve to a valid token/token kind when paired with
        /// a parser's `PEGParser.expect(_:)`/`PEGParser.expect(kind:)` calls.
        @NodeRequired
        public var tokenCodeReference: GrammarString?

        /// The syntax of the token.
        @NodeRequired
        public var tokenSyntax: CommonAbstract.TokenSyntax?
    }

    /// Represents a tokens channel declaration.
    ///
    /// Represents the construct:
    /// ```
    /// tokenChannelDeclaration:
    ///     | '@' IDENTIFIER IDENTIFIER '~>' tokenChannelTarget ';'
    ///     | '@' IDENTIFIER IDENTIFIER? ';'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "TokenFileDeclaration")
    public final class TokenChannelDeclaration: TokenFileDeclaration {
        @NodeRequired
        public var name: Token?

        @NodeProperty
        var _target: TokenChannelTarget?
    }

    /// Represents a tokens channel target declaration.
    ///
    /// Represents the construct:
    /// ```
    /// tokenChannelTarget:
    ///     | IDENTIFIER
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class TokenChannelTarget: GrammarNode {
        @NodeRequired
        public var identifier: Token
    }

    /// Protocol for visiting Grammar node types.
    public protocol GrammarNodeVisitorType: NodeVisitorType {
        /// Visits a Grammar node.
        func visit(_ node: Grammar) throws -> VisitResult

        /// Visits a Meta-property node.
        func visit(_ node: Meta) throws -> VisitResult

        /// Visits a Meta-property identifier value node.
        func visit(_ node: MetaIdentifierValue) throws -> VisitResult

        /// Visits a Meta-property string value node.
        func visit(_ node: MetaStringValue) throws -> VisitResult

        /// Visits a Rule node.
        func visit(_ node: Rule) throws -> VisitResult

        /// Visits a Rule Name node.
        func visit(_ node: RuleName) throws -> VisitResult

        /// Visits an Alt node.
        func visit(_ node: Alt) throws -> VisitResult

        /// Visits a Named Item node.
        func visit(_ node: NamedItem) throws -> VisitResult

        /// Visits a Forced node.
        func visit(_ node: Forced) throws -> VisitResult

        /// Visits a Positive Lookahead node.
        func visit(_ node: PositiveLookahead) throws -> VisitResult

        /// Visits a Negative Lookahead node.
        func visit(_ node: NegativeLookahead) throws -> VisitResult

        /// Visits a Cut node.
        func visit(_ node: Cut) throws -> VisitResult

        /// Visits an Optional Alts node.
        func visit(_ node: OptionalItems) throws -> VisitResult

        /// Visits an Optional Atom node.
        func visit(_ node: OptionalItem) throws -> VisitResult

        /// Visits a Zero Or More Item node.
        func visit(_ node: ZeroOrMoreItem) throws -> VisitResult

        /// Visits a One Or More Item node.
        func visit(_ node: OneOrMoreItem) throws -> VisitResult

        /// Visits a Gather Item node.
        func visit(_ node: GatherItem) throws -> VisitResult

        /// Visits an Atom Item node.
        func visit(_ node: AtomItem) throws -> VisitResult

        /// Visits a Group Atom node.
        func visit(_ node: GroupAtom) throws -> VisitResult

        /// Visits a String Atom node.
        func visit(_ node: StringAtom) throws -> VisitResult

        /// Visits an Identifier Atom node.
        func visit(_ node: IdentAtom) throws -> VisitResult

        /// Visits an Action node.
        func visit(_ node: Action) throws -> VisitResult
    }
}

public extension SwiftPEGGrammar.GrammarNodeVisitorType where VisitResult == NodeVisitChildrenResult {
    func visit(_ node: SwiftPEGGrammar.Grammar) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Meta) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.MetaIdentifierValue) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.MetaStringValue) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Rule) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.RuleName) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Alt) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.NamedItem) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Forced) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.PositiveLookahead) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.NegativeLookahead) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Cut) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.OptionalItems) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.OptionalItem) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.ZeroOrMoreItem) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.OneOrMoreItem) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.GatherItem) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.AtomItem) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.GroupAtom) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.StringAtom) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.IdentAtom) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Action) throws -> NodeVisitChildrenResult { .visitChildren }
}
