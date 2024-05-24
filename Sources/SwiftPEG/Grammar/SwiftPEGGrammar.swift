import SwiftPEGMacros

/// Namespace for SwiftPEG grammar grammar objects.
public enum SwiftPEGGrammar {}

#if true

extension SwiftPEGGrammar {
    public typealias Token = SwiftPEGGrammar.GrammarToken

    /// Base class for grammar nodes.
    public class GrammarNode: Node {
        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: NodeVisitorType {
            if let cast = visitor as? GrammarNodeVisitorType {
                return try self.accept(cast)
            }

            return try super.accept(visitor)
        }

        /// Accepts a given grammar-node visitor into this node.
        public func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try super.accept(visitor)
        }
    }

    /// A grammar file.
    /// 
    /// Represents the construct:
    /// ```
    /// grammar[Grammar]:
    ///     | metas rules { Grammar.Grammar(metas: metas, rules: rules) }
    ///     | rules { Grammar.Grammar(metas: [], rules: rules) }
    ///     ;
    /// 
    /// metas: meta+ ;
    /// rules: rule+ ;
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A grammar meta-property.
    /// 
    /// Represents the construct:
    /// ```
    /// meta:
    ///     | '@' name=IDENT metaValue?
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Meta: GrammarNode {
        /// The name of this meta-property.
        @NodeRequired
        public var name: Token

        /// The value associated with this meta-property.
        @NodeProperty
        var _value: MetaValue?

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
    /// metaValueIdent: IDENT ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "MetaValue")
    public final class MetaIdentifierValue: MetaValue {
        /// The associated identifier value.
        @NodeRequired
        public var identifier: Token

        public override var shortDebugDescription: String { String(identifier.string) }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
        public var string: Token

        public override var shortDebugDescription: String { String(string.string) }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A grammar rule.
    /// 
    /// Represents the construct:
    /// ```
    /// rule:
    ///     | ruleName ':' alts
    ///     | ruleName ':' '|' alts   # "more_alts" on pegen's metagrammar.gram
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Rule: GrammarNode {
        /// The name of this rule.
        @NodeProperty
        var _name: RuleName

        /// List of one or more alts associated with this rule.
        @NodeProperty
        var _alts: [Alt]

        /// Flag used by `GrammarProcessor` to indicate whether this rule is left-recursive.
        var isLeftRecursive: Bool = false

        /// If `isLeftRecursive` is `true`, indicates whether this rule is the
        /// leader of the left recursion.
        var isLeftRecursiveLead: Bool = false

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
    }

    /// A grammar rule's name.
    /// 
    /// Represents the construct:
    /// ```
    /// ruleName:
    ///     | name=IDENT ('[' type=swiftType ']')?
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class RuleName: GrammarNode {
        /// The rule's name.
        @NodeRequired
        public var name: Token

        /// A type name directly associated with this rule.
        @NodeProperty
        var _type: SwiftType?

        public override var shortDebugDescription: String { String(name.string) }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// An alternative, or a sequence of items that must succeed sequentially
    /// for the alt to succeed.
    /// 
    /// Represents the construct:
    /// ```
    /// alt:
    ///     | namedItems action?
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Alt: GrammarNode {
        /// The items belonging to this alt.
        @NodeProperty
        var _namedItems: [NamedItem]

        /// An optional action associated with this alt.
        @NodeProperty
        var _action: Action?

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
    }

    /// An item, or segment of an alt, for which a name can be attributed.
    /// 
    /// Represents the construct:
    /// ```
    /// namedItem:
    ///     | name=IDENT '[' type=swiftType ']' '=' ~ item
    ///     | name=IDENT '=' ~ item
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
        @NodeProperty
        var _type: SwiftType?

        /// Lookahead associated with this named item.
        @NodeProperty
        var _lookahead: LookaheadOrCut?

        var nullable: Bool = false

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
    }

    /// Base class for a node that represents either a positive/negative lookahead,
    /// or a cut node.
    /// 
    /// Represents the construct:
    /// ```
    /// lookahead:
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        override func initialNames() -> Set<String> {
            return alts.reduce([]) { $0.union($1.initialNames()) }
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
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

        public override var shortDebugDescription: String { "*" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return true
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
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

        public override var shortDebugDescription: String { "+" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
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

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        override func initialNames() -> Set<String> {
            return item.initialNames()
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }

        override func visitNullable(_ visitor: NullabilityVisitor) -> Bool {
            return false
        }

        override func initialNames() -> Set<String> {
            return atom.initialNames()
        }
    }

    /// Base class for atoms of an alternative. Atoms are the smallest unit of
    /// division within a sequence of items, and always have to match fully.
    ///
    /// Represents the constructs:
    /// ```
    /// atom:
    ///     | '(' ~ alts ')'
    ///     | IDENT
    ///     | STRING
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
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
    }

    /// A string literal atom.
    /// 
    /// Represents the construct:
    /// ```
    /// STRING ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Atom")
    public final class StringAtom: Atom {
        /// The string associated with this atom.
        /// 
        /// - note: Includes the quotes.
        @NodeRequired
        public var string: Token

        /// Returns the value of `self.value` with any surrounding string quotes
        /// stripped.
        /// 
        /// Convenience for `self.string.valueTrimmingQuotes`.
        public var valueTrimmingQuotes: Substring {
            string.processedString
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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
    /// atom: IDENT ;
    /// ```
    @GeneratedNodeType<Node>(overrideDeepCopyType: "Atom")
    public final class IdentAtom: Atom {
        /// The identifier associated with this atom.
        @NodeRequired
        public var identifier: Token

        /// The identity of this atom.
        @NodeRequired
        public var identity: Identity

        /// Convenience for `self.identifier.identifier`.
        public var name: Substring {
            identifier.string
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
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

        /// Specifies the identity of an identifier atom.
        public enum Identity: Hashable {
            /// The identifier refers to a rule in the grammar.
            case ruleName

            /// The identifier refers to a token in the grammar.
            case token

            /// The identifier is unresolved.
            case unresolved
        }
    }

    /// Describes the type of a grammar production.
    /// 
    /// Represents the construct:
    /// ```
    /// swiftType[SwiftType]:
    ///     | '[' ~ type=swiftType ']' { SwiftType(name: "[" + type.name + "]") }
    ///     | '(' ~ types=swiftTypeList ')' { SwiftType(name: "(" + types.map(\.name).joined(separator: ", ") + ")") }
    ///     | name=IDENT '<' ~ types=swiftTypeList '>' { SwiftType(name: name.name + "<" + types.map(\.name).joined(separator: ", ") + ">") }
    ///     | name=IDENT '.' inner=swiftType { SwiftType(name: name + "." + inner.name) }
    ///     | name=IDENT '?' { SwiftType(name: name + "?") }
    ///     | name=IDENT
    ///     ;
    /// ```
    /// 
    /// And in list form:
    /// ```
    /// swiftTypeList[[SwiftType]]:
    ///     | type=swiftType ',' types=swiftTypeList { [type] + types }
    ///     | type=swiftType { [type] }
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class SwiftType: GrammarNode {
        /// The name of the type.
        @NodeRequired
        public var name: Substring

        public override var shortDebugDescription: String { String(self.name) }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// An action of an alt. Represents a segment of code that is inserted on
    /// the generated code for when the alt associated with an action is matched.
    /// 
    /// Represents the construct:
    /// ```
    /// action: '{' ~ balancedTokens? '}' ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Action: GrammarNode {
        /// Balanced tokens contained within this action.
        @NodeProperty
        var _balancedTokens: BalancedTokens?

        public override var shortDebugDescription: String {
            guard let balancedTokens = balancedTokens else {
                return ""
            }

            return "{ \(balancedTokens.tokens.map(\.token.string).joined()) }"
        }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// A collection of balanced tokens for use in an action.
    ///
    /// Represents the construct:
    /// ```
    /// balancedToken:
    ///     | token=WHITESPACE { self.setLocation(.init(tokens: [.init(token)]), at: mark) }
    ///     | '{' balancedTokens '}' { .init(tokens: ["{"] + balancedTokens.tokens + ["}"]) }
    ///     | '[' balancedTokens ']' { .init(tokens: ["["] + balancedTokens.tokens + ["]"]) }
    ///     | '<' balancedTokens '>' { .init(tokens: ["<"] + balancedTokens.tokens + [">"]) }
    ///     | '(' balancedTokens ')' { .init(tokens: ["("] + balancedTokens.tokens + [")"]) }
    ///     | '[' ~ ']' { .init(tokens: ["(", ")"]) }
    ///     | '{' ~ '}' { .init(tokens: ["{", "}"]) }
    ///     | '<' ~ '>' { .init(tokens: ["<", ">"]) }
    ///     | '(' ~ ')' { .init(tokens: ["(", ")"]) }
    ///     ;
    ///
    /// balancedTokenAtom[Token]:
    ///     | WHITESPACE    # Includes newlines
    ///     | IDENT
    ///     | DIGITS
    ///     | STRING
    ///     | ':'
    ///     | ';'
    ///     | '|'
    ///     | '='
    ///     | '~'
    ///     | '*'
    ///     | '+'
    ///     | '?'
    ///     | ','
    ///     | '.'
    ///     | '@'
    ///     | '$'
    ///     | '/'
    ///     | '\\'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class BalancedTokens: GrammarNode {
        /// A list of tokens contained within this balanced token set.
        @NodeRequired
        public var tokens: [TokenNode<GrammarToken, GrammarRawTokenizer.Location>]

        public override var shortDebugDescription: String { "[\(tokens.map { #""\#($0.token)""# }.joined(separator: ", "))]" }

        /// Accepts a given grammar-node visitor into this node.
        public override func accept<Visitor>(_ visitor: Visitor) throws -> NodeVisitChildrenResult where Visitor: GrammarNodeVisitorType {
            try visitor.visit(self)
        }
    }

    /// Represents a token definition collected from a tokens file.
    ///
    /// Represents the construct:
    /// ```
    /// tokenDefinition:
    ///     | '$' name=IDENTIFIER '[' expectArgs=STRING ']' ':' literal=STRING ';' 
    ///     | '$' name=IDENTIFIER ':' literal=STRING ';'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class TokenDefinition: GrammarNode {
        /// The identifier for the token.
        @NodeRequired
        public var name: Token

        /// An optional 'expect' call that fetches the token from the parser.
        @NodeRequired
        public var expectArgs: Token?

        /// The string literal associated with the token.
        @NodeRequired
        public var literal: Token
    }

    /// A token in a grammar.
    public enum GrammarToken: TokenType, ExpressibleByStringLiteral {
        public typealias TokenKind = GrammarTokenKind
        public typealias TokenString = Substring

        /// `*`
        /// 
        /// Alias for `Self.star`
        public static let asterisk = Self.star

        /// `.`
        /// 
        /// Alias for `Self.period`
        public static let dot = Self.period

        /// A set of whitespace or newlines, with no other non-whitespace character
        /// in between.
        case whitespace(Substring)

        /// A Swift-compatible identifier token.
        case identifier(Substring)

        /// A digit sequence.
        case digits(Substring)

        /// A string literal token.
        /// Includes the quotes.
        case string(StringLiteral)

        /// `(`
        case leftParen
        /// `)`
        case rightParen

        /// `{`
        case leftBrace
        /// `}`
        case rightBrace

        /// `[`
        case leftSquare
        /// `]`
        case rightSquare

        /// `<`
        case leftAngle
        /// `>`
        case rightAngle

        /// `:`
        case colon
        /// `;`
        case semicolon
        /// `|`
        case bar

        /// `=`
        case equals
        /// `~`
        case tilde
        /// `*`
        case star
        /// `+`
        case plus
        /// `-`
        case minus

        /// `?`
        case questionMark
        /// `!`
        case exclamationMark
        /// `&`
        case ampersand
        /// `,`
        case comma
        /// `.`
        case period
        /// `@`
        case at
        /// `$`
        case dollarSign

        /// `/`
        case forwardSlash
        /// `\`
        case backslash

        @inlinable
        public var kind: GrammarTokenKind {
            switch self {
            case .whitespace: return .whitespace
            case .identifier: return .identifier
            case .digits: return .digits
            case .string: return .string
            case .leftParen: return .leftParen
            case .rightParen: return .rightParen
            case .leftBrace: return .leftBrace
            case .rightBrace: return .rightBrace
            case .leftSquare: return .leftSquare
            case .rightSquare: return .rightSquare
            case .leftAngle: return .leftAngle
            case .rightAngle: return .rightAngle
            case .colon: return .colon
            case .semicolon: return .semicolon
            case .bar: return .bar
            case .equals: return .equals
            case .tilde: return .tilde
            case .star: return .star
            case .plus: return .plus
            case .minus: return .minus
            case .questionMark: return .questionMark
            case .exclamationMark: return .exclamationMark
            case .ampersand: return .ampersand
            case .comma: return .comma
            case .period: return .period
            case .at: return .at
            case .dollarSign: return .dollarSign
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            }
        }

        @inlinable
        public var string: TokenString {
            switch self {
            case .whitespace(let value): return value
            case .identifier(let value): return value
            case .digits(let value): return value
            case .string(let value): return value.quotedContents
            case .leftParen: return "("
            case .rightParen: return ")"
            case .leftBrace: return "{"
            case .rightBrace: return "}"
            case .leftSquare: return "["
            case .rightSquare: return "]"
            case .leftAngle: return "<"
            case .rightAngle: return ">"
            case .colon: return ":"
            case .semicolon: return ";"
            case .bar: return "|"
            case .equals: return "="
            case .tilde: return "~"
            case .star: return "*"
            case .plus: return "+"
            case .minus: return "-"
            case .questionMark: return "?"
            case .exclamationMark: return "!"
            case .ampersand: return "&"
            case .comma: return ","
            case .period: return "."
            case .at: return "@"
            case .dollarSign: return "$"
            case .forwardSlash: return "/"
            case .backslash: return "\\"
            }
        }

        /// Returns a version of `self.string` that is suitable to be used as
        /// a bare literal value. Removes quotes from string literals.
        @inlinable
        public var processedString: TokenString {
            switch self {
            case .whitespace(let value): return value
            case .identifier(let value): return value
            case .digits(let value): return value
            case .string(let value): return value.contents
            case .leftParen: return "("
            case .rightParen: return ")"
            case .leftBrace: return "{"
            case .rightBrace: return "}"
            case .leftSquare: return "["
            case .rightSquare: return "]"
            case .leftAngle: return "<"
            case .rightAngle: return ">"
            case .colon: return ":"
            case .semicolon: return ";"
            case .bar: return "|"
            case .equals: return "="
            case .tilde: return "~"
            case .star: return "*"
            case .plus: return "+"
            case .minus: return "-"
            case .questionMark: return "?"
            case .exclamationMark: return "!"
            case .ampersand: return "&"
            case .comma: return ","
            case .period: return "."
            case .at: return "@"
            case .dollarSign: return "$"
            case .forwardSlash: return "/"
            case .backslash: return "\\"
            }
        }

        /// Returns the UTF8 length of this token.
        @inlinable
        public var tokenUTF8Length: Int {
            string.utf8.count
        }

        @inlinable
        public var length: Int {
            string.count
        }

        /// Attempts to construct a token from a given string literal value.
        ///
        /// - Note: If the construction fails, an assertion is raised, and should
        /// only be used as a convenience within a parser.
        @inlinable
        public init(stringLiteral value: String) {
            guard let token = Self.from(string: value[...]) else {
                fatalError("\(Self.self): Unknown token literal '\(value)'")
            }

            self = token
        }

        @inlinable
        public static func produceDummy(_ kind: TokenKind) -> Self {
            switch kind {
            case .whitespace: return .whitespace(" ")
            case .identifier: return .identifier("<dummy>")
            case .digits: return .digits("<dummy>")
            case .string: return .string(.singleQuote("<dummy>"))
            case .leftParen: return .leftParen
            case .rightParen: return .rightParen
            case .leftBrace: return .leftBrace
            case .rightBrace: return .rightBrace
            case .leftSquare: return .leftSquare
            case .rightSquare: return .rightSquare
            case .leftAngle: return .leftAngle
            case .rightAngle: return .rightAngle
            case .colon: return .colon
            case .semicolon: return .semicolon
            case .bar: return .bar
            case .equals: return .equals
            case .tilde: return .tilde
            case .star: return .star
            case .plus: return .plus
            case .minus: return .minus
            case .questionMark: return .questionMark
            case .exclamationMark: return .exclamationMark
            case .ampersand: return .ampersand
            case .comma: return .comma
            case .period: return .period
            case .at: return .at
            case .dollarSign: return .dollarSign
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            }
        }

        /// Returns a parsed token from the given substring.
        /// If the token is not recognized, `nil` is returned, instead.
        @inlinable
        public static func from(string: Substring) -> Self? {
            guard let first = string.first else {
                return nil
            }

            switch first {
            case "(": return .leftParen
            case ")": return .rightParen
            case "{": return .leftBrace
            case "}": return .rightBrace
            case "[": return .leftSquare
            case "]": return .rightSquare
            case "<": return .leftAngle
            case ">": return .rightAngle
            case ":": return .colon
            case ";": return .semicolon
            case "|": return .bar
            case "=": return .equals
            case "~": return .tilde
            case "*": return .star
            case "+": return .plus
            case "-": return .minus
            case "?": return .questionMark
            case "!": return .exclamationMark
            case "&": return .ampersand
            case ",": return .comma
            case ".": return .period
            case "@": return .at
            case "$": return .dollarSign
            case "/": return .forwardSlash
            case "\\": return .backslash
            case "'", "\"":
                if let string = StringLiteral.from(string: string) {
                    return .string(string)
                }
                return nil
            case let c where c.isWhitespace:
                if let match = Self._parseWhitespace(string) {
                    return .whitespace(match)
                }
                return nil
            case let c where c.isWholeNumber:
                if let digits = Self._parseDigits(string) {
                    return .digits(digits)
                }
                return nil
            default:
                // Try identifier
                if let ident = Self._parseIdentifier(string) {
                    return .identifier(ident)
                }
                return nil
            }
        }

        @inlinable
        static func _parseWhitespace<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
            var stream = StringStreamer(source: string)
            guard !stream.isEof else { return nil }

            switch stream.next() {
            case let c where c.isWhitespace:
                break
            default:
                return nil
            }

            loop:
            while !stream.isEof {
                switch stream.peek() {
                case let c where c.isWhitespace:
                    stream.advance()
                default:
                    break loop
                }
            }

            return stream.substring
        }

        @inlinable
        static func _parseIdentifier<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
            var stream = StringStreamer(source: string)
            guard !stream.isEof else { return nil }

            switch stream.next() {
            case let c where c.isLetter:
                break
            case "_":
                break
            default:
                return nil
            }

            loop:
            while !stream.isEof {
                switch stream.peek() {
                case let c where c.isLetter:
                    stream.advance()
                case let c where c.isWholeNumber:
                    stream.advance()
                case "_":
                    stream.advance()
                default:
                    break loop
                }
            }

            return stream.substring
        }

        @inlinable
        static func _parseDigits<S: StringProtocol>(_ string: S) -> Substring? where S.SubSequence == Substring {
            var stream = StringStreamer(source: string)
            guard !stream.isEof else { return nil }

            switch stream.next() {
            case let c where c.isWholeNumber:
                break
            default:
                return nil
            }

            loop:
            while !stream.isEof {
                switch stream.peek() {
                case let c where c.isWholeNumber:
                    stream.advance()
                default:
                    break loop
                }
            }

            return stream.substring
        }

        /// Specifies a variant of a string literal.
        /// 
        /// Associated values represent the string's contents, not including the
        /// quotes.
        public enum StringLiteral: Hashable, CustomStringConvertible {
            /// `'<...>'`
            case singleQuote(Substring)
            
            /// `"<...>"`
            case doubleQuote(Substring)

            /// `"""<...>"""`
            /// Supports newlines within
            case tripleQuote(Substring)

            /// Returns contents of the string, without surrounding quotes.
            @inlinable
            public var contents: Substring {
                switch self {
                case .singleQuote(let string):
                    return string.dropFirst().dropLast()
                case .doubleQuote(let string):
                    return string.dropFirst().dropLast()
                case .tripleQuote(let string):
                    // Ignore first newline past triple quote
                    if string.hasPrefix("\"\"\"\n") {
                        return string.dropFirst(4).dropLast(3)
                    }
                    return string.dropFirst(3).dropLast(3)
                }
            }

            /// Returns the full representation of this literal, including quotes.
            @inlinable
            public var quotedContents: Substring {
                switch self {
                case .singleQuote(let string),
                    .doubleQuote(let string),
                    .tripleQuote(let string):
                    return string
                }
            }

            /// Returns the full representation of this literal, including quotes.
            @inlinable
            public var description: String {
                String(quotedContents)
            }

            /// Returns a parsed string literal from the given substring.
            /// If no string literal is recognized, `nil` is returned, instead.
            @inlinable
            public static func from(string: Substring) -> Self? {
                var stream = StringStreamer(source: string)
                guard !stream.isEof else { return nil }

                let terminator: String
                let multiline: Bool

                if stream.advanceIfNext("\"\"\"") {
                    terminator = "\"\"\""
                    multiline = true
                } else if stream.advanceIfNext("\"") {
                    terminator = "\""
                    multiline = false
                } else if stream.advanceIfNext("'") {
                    terminator = "'"
                    multiline = false
                } else {
                    return nil
                }

                var expectsEscapeSequence = false
                while !stream.isEof {
                    if expectsEscapeSequence {
                        // TODO: Handle escape sequences appropriately
                        stream.advance()
                        expectsEscapeSequence = false
                    } else {
                        if stream.advanceIfNext(terminator) {
                            if terminator == "\"\"\"" {
                                return .tripleQuote(stream.substring)
                            } else if terminator == "\"" {
                                return .doubleQuote(stream.substring)
                            } else if terminator == "'" {
                                return .singleQuote(stream.substring)
                            }
                        }

                        let next = stream.next()

                        // Newlines are only allowed in triple-quoted strings
                        if next == "\n" {
                            if !multiline {
                                return nil
                            }
                        }
                        // Check escape sequence
                        else if next == "\\" {
                            expectsEscapeSequence = true
                        }
                    }
                }

                // Missing terminator?
                return nil
            }
        }
    }

    /// Specifies kinds for grammar tokens.
    public enum GrammarTokenKind: String, TokenKindType, CaseIterable, ExpressibleByStringLiteral {
        /// `*`
        /// 
        /// Alias for `Self.star`
        public static let asterisk = Self.star

        /// `.`
        /// 
        /// Alias for `Self.period`
        public static let dot = Self.period

        /// Whitespace characters.
        case whitespace = "WHITESPACE"

        /// A Swift-compatible identifier token.
        case identifier = "IDENTIFIER"

        /// A digit sequence.
        case digits = "DIGITS"

        /// A string literal token.
        /// Includes the quotes.
        case string = "STRING"

        /// `(`
        case leftParen = "("
        /// `)`
        case rightParen = ")"

        /// `{`
        case leftBrace = "{"
        /// `}`
        case rightBrace = "}"

        /// `[`
        case leftSquare = "["
        /// `]`
        case rightSquare = "]"

        /// `<`
        case leftAngle = "<"
        /// `>`
        case rightAngle = ">"

        /// `:`
        case colon = ":"
        /// `;`
        case semicolon = ";"
        /// `|`
        case bar = "|"

        /// `=`
        case equals = "="
        /// `~`
        case tilde = "~"
        /// `*`
        case star = "*"
        /// `+`
        case plus = "+"
        /// `-`
        case minus = "-"

        /// `?`
        case questionMark = "?"
        /// `!`
        case exclamationMark = "!"
        /// `&`
        case ampersand = "&"
        /// `,`
        case comma = ","
        /// `.`
        case period = "."
        /// `@`
        case at = "@"
        /// `$`
        case dollarSign = "$"

        /// `/`
        case forwardSlash = "/"
        /// `\`
        case backslash = "\\"

        @inlinable
        public var description: String {
            self.rawValue
        }

        @inlinable
        public init(stringLiteral: String) {
            guard let value = Self(rawValue: stringLiteral) else {
                fatalError("Unknown grammar token kind '\(stringLiteral)'")
            }

            self = value
        }
    }

    /// Protocol for visiting Grammar node types.
    public protocol GrammarNodeVisitorType: NodeVisitorType {
        /// Visits a Grammar node.
        func visit(_ node: Grammar) throws -> NodeVisitChildrenResult

        /// Visits a Meta-property node.
        func visit(_ node: Meta) throws -> NodeVisitChildrenResult

        /// Visits a Meta-property identifier value node.
        func visit(_ node: MetaIdentifierValue) throws -> NodeVisitChildrenResult

        /// Visits a Meta-property string value node.
        func visit(_ node: MetaStringValue) throws -> NodeVisitChildrenResult

        /// Visits a Rule node.
        func visit(_ node: Rule) throws -> NodeVisitChildrenResult

        /// Visits a Rule Name node.
        func visit(_ node: RuleName) throws -> NodeVisitChildrenResult

        /// Visits an Alt node.
        func visit(_ node: Alt) throws -> NodeVisitChildrenResult

        /// Visits a Named Item node.
        func visit(_ node: NamedItem) throws -> NodeVisitChildrenResult

        /// Visits a Positive Lookahead node.
        func visit(_ node: PositiveLookahead) throws -> NodeVisitChildrenResult

        /// Visits a Negative Lookahead node.
        func visit(_ node: NegativeLookahead) throws -> NodeVisitChildrenResult

        /// Visits a Cut node.
        func visit(_ node: Cut) throws -> NodeVisitChildrenResult

        /// Visits an Optional Alts node.
        func visit(_ node: OptionalItems) throws -> NodeVisitChildrenResult

        /// Visits an Optional Atom node.
        func visit(_ node: OptionalItem) throws -> NodeVisitChildrenResult

        /// Visits a Zero Or More Item node.
        func visit(_ node: ZeroOrMoreItem) throws -> NodeVisitChildrenResult

        /// Visits a One Or More Item node.
        func visit(_ node: OneOrMoreItem) throws -> NodeVisitChildrenResult

        /// Visits a Gather Item node.
        func visit(_ node: GatherItem) throws -> NodeVisitChildrenResult

        /// Visits an Atom Item node.
        func visit(_ node: AtomItem) throws -> NodeVisitChildrenResult

        /// Visits a Group Atom node.
        func visit(_ node: GroupAtom) throws -> NodeVisitChildrenResult

        /// Visits a String Atom node.
        func visit(_ node: StringAtom) throws -> NodeVisitChildrenResult

        /// Visits an Identifier Atom node.
        func visit(_ node: IdentAtom) throws -> NodeVisitChildrenResult

        /// Visits a Swift Type node.
        func visit(_ node: SwiftType) throws -> NodeVisitChildrenResult

        /// Visits an Action node.
        func visit(_ node: Action) throws -> NodeVisitChildrenResult

        /// Visits a Balanced Tokens node.
        func visit(_ node: BalancedTokens) throws -> NodeVisitChildrenResult
    }
}

public extension SwiftPEGGrammar.GrammarNodeVisitorType {
    func visit(_ node: SwiftPEGGrammar.Grammar) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Meta) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.MetaIdentifierValue) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.MetaStringValue) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Rule) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.RuleName) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Alt) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.NamedItem) throws -> NodeVisitChildrenResult { .visitChildren }
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
    func visit(_ node: SwiftPEGGrammar.SwiftType) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.Action) throws -> NodeVisitChildrenResult { .visitChildren }
    func visit(_ node: SwiftPEGGrammar.BalancedTokens) throws -> NodeVisitChildrenResult { .visitChildren }
}

#endif
