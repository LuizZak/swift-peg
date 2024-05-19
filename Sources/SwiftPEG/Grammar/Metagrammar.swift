import SwiftPEGMacros

/// Namespace for SwiftPEG metagrammar grammar objects.
public enum Metagrammar {}

#if true

extension Metagrammar {
    /// A grammar file.
    /// 
    /// Represents the construct:
    /// ```
    /// grammar: metas? rules ;
    /// 
    /// metas: meta+ ;
    /// rules: rule+ ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Grammar: Node {
        /// List of meta-properties described in the grammar.
        @NodeProperty
        private var _metas: [Meta] = []

        /// List of rules in the grammar.
        @NodeProperty
        private var _rules: [Rule] = []
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
    public final class Meta: Node {
        /// The name of this meta-property.
        @NodeProperty
        var _name: IdentifierToken

        /// The value associated with this meta-property.
        @NodeProperty
        var _value: MetaValue?
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
    public class MetaValue: Node { }

    /// A value of a meta-property that is an identifier.
    /// 
    /// Represents the construct:
    /// ```
    /// metaValueIdent: IDENT ;
    /// ```
    @GeneratedNodeType<Node>
    public final class MetaIdentifierValue: MetaValue {
        /// The associated identifier value.
        @NodeProperty
        var _identifier: IdentifierToken
    }

    /// A value of a meta-property that is a string.
    /// 
    /// Represents the construct:
    /// ```
    /// metaValueString: STRING ;
    /// ```
    @GeneratedNodeType<Node>
    public final class MetaStringValue: MetaValue {
        /// The associated string value.
        @NodeProperty
        var _string: StringToken
    }

    /// A grammar rule.
    /// 
    /// Represents the construct:
    /// ```
    /// rule:
    ///     | ruleName ':' alts
    ///     | ruleName ':' '|' alts   # "more_alts" on pegen_experiments/metagrammar.gram
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class Rule: Node {
        /// The name of this rule.
        @NodeProperty
        var _name: RuleName

        /// List of one or more alts associated with this rule.
        @NodeProperty
        var _alts: [Alt]
    }

    /// A grammar rule's name.
    /// 
    /// Represents the construct:
    /// ```
    /// ruleName:
    ///     | name=IDENT ('[' type=IDENT ']')?
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class RuleName: Node {
        /// The rule's name.
        @NodeProperty
        var _name: IdentifierToken

        /// A type name directly associated with this rule.
        @NodeProperty
        var _type: IdentifierToken?
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
    public final class Alt: Node {
        /// The items belonging to this alt.
        @NodeProperty
        var _namedItems: [NamedItem]

        /// An optional action associated with this alt.
        @NodeProperty
        var _action: Action?
    }

    /// An item, or segment of an alt, for which a name can be attributed.
    /// 
    /// Represents the construct:
    /// ```
    /// namedItem:
    ///     | name=IDENT '[' type=IDENT ']' '=' ~ item
    ///     | name=IDENT '=' ~ item
    ///     | item
    ///     | lookahead
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class NamedItem: Node {
        /// An optional name associated with this item.
        @NodeProperty
        var _name: IdentifierToken?

        /// Item associated with this named item.
        @NodeProperty
        var _item: Item?

        /// Lookahead associated with this named item.
        @NodeProperty
        var _lookahead: LookaheadOrCut?
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
    public class LookaheadOrCut: Node { }

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
    @GeneratedNodeType<Node>
    public final class PositiveLookahead: LookaheadOrCut {
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "&" }
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
    @GeneratedNodeType<Node>
    public final class NegativeLookahead: LookaheadOrCut {
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "!" }
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
    public class Item: Node { }

    /// An optional set of items.
    /// 
    /// Optional items are consumed if present but are not required in order to
    /// match.
    /// 
    /// Represents the construct:
    /// ```
    /// '[' ~ alts ']' ;
    /// ```
    @GeneratedNodeType<Node>
    public final class OptionalItems: Item {
        /// The alts that are optionally wrapped.
        @NodeProperty
        var _alts: [Alt]

        public override var shortDebugDescription: String { "?" }
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
    @GeneratedNodeType<Node>
    public final class OptionalItem: Item {
        /// The atom that is optionally wrapped.
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "?" }
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
    @GeneratedNodeType<Node>
    public final class ZeroOrMoreItem: Item {
        /// The atom that is wrapped.
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "*" }
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
    @GeneratedNodeType<Node>
    public final class OneOrMoreItem: Item {
        /// The atom that is wrapped.
        @NodeProperty
        var _atom: Atom

        public override var shortDebugDescription: String { "+" }
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
    @GeneratedNodeType<Node>
    public final class GatherItem: Item {
        /// The separator that is matched in between items.
        @NodeProperty
        var _sep: Atom

        /// The item that is separated.
        @NodeProperty
        var _item: Atom
    }

    /// An item consisting of an atom.
    /// 
    /// Represents the construct:
    /// ```
    /// atom ;
    /// ```
    @GeneratedNodeType<Node>
    public final class AtomItem: Item {
        @NodeProperty
        var _atom: Atom
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
    public class Atom: Node { }

    /// A group atom with a sequence of alts in parenthesis.
    /// 
    /// Represents the construct:
    /// ```
    /// '(' ~ alts ')' ;
    /// ```
    @GeneratedNodeType<Node>
    public final class GroupAtom: Atom {
        /// The alts that are grouped.
        @NodeProperty
        var _alts: [Alt]
    }

    /// A string literal atom.
    /// 
    /// Represents the construct:
    /// ```
    /// STRING ;
    /// ```
    @GeneratedNodeType<Node>
    public final class StringAtom: Atom {
        /// The string associated with this atom.
        /// 
        /// - note: Includes the quotes.
        @NodeProperty
        public var _string: StringToken

        /// Returns the value of `self.value` with any surrounding string quotes
        /// stripped.
        /// 
        /// Convenience for `self.string.valueTrimmingQuotes`.
        public var valueTrimmingQuotes: String {
            _string.valueTrimmingQuotes
        }
    }

    /// An identifier atom.
    /// 
    /// Represents the construct:
    /// ```
    /// atom: IDENT ;
    /// ```
    @GeneratedNodeType<Node>
    public final class IdentAtom: Atom {
        /// The identifier associated with this atom.
        @NodeProperty
        var _identifier: IdentifierToken

        /// Convenience for `self.identifier.identifier`.
        var name: String {
            _identifier.identifier
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
    public final class Action: Node {
        /// Balanced tokens contained within this action.
        @NodeProperty
        var _balancedTokens: BalancedTokens?

        public override var shortDebugDescription: String {
            guard let balancedTokens = balancedTokens else {
                return ""
            }

            return "{ \(balancedTokens.tokens.joined()) }"
        }
    }

    /// A collection of balanced tokens for use in an action.
    ///
    /// Represents the construct:
    /// ```
    /// balancedTokens:
    ///     | '[' ~ balancedTokens* ']'
    ///     | '{' ~ balancedTokens* '}'
    ///     | '(' ~ balancedTokens* ')'
    ///     | balancedToken+
    ///     ;
    ///
    /// balancedToken:
    ///     | IDENT
    ///     | DIGITS
    ///     | STRING
    ///     | WHITESPACE  # Includes newline
    ///     | '*'
    ///     | '+'
    ///     | '-'
    ///     | '.'
    ///     | '?'
    ///     | '|'
    ///     | '&'
    ///     | '~'
    ///     ;
    /// ```
    @GeneratedNodeType<Node>
    public final class BalancedTokens: Node {
        /// A list of tokens contained within this balanced token set.
        @NodeRequired
        public var tokens: [String]

        public override var shortDebugDescription: String { "[\(tokens.map { #""\#($0)""# }.joined(separator: ", "))]" }
    }

    /// Base meta-grammar string token.
    /// Represents a single- or double-quoted string, as well as triple-double-quoted
    /// strings that support multiple lines.
    /// 
    /// Represents the (pseudo-)construct:
    /// ```
    /// STRING:
    ///     | '\'' ~ <all except newline and '> '\''
    ///     | '"' ~ <all except newline and "> '"'
    ///     | '"""' ~ <all except """> '"""'
    /// ```
    public final class StringToken: TokenNode {
        /// The string associated with this atom.
        /// 
        /// - note: Includes the quotes.
        public var value: String {
            token as! String
        }

        /// Returns the value of `self.value` with any surrounding string quotes
        /// stripped.
        public var valueTrimmingQuotes: String {
            let tripleQuote = "\"\"\""
            if value.hasPrefix(tripleQuote) && value.hasSuffix(tripleQuote) && value.count >= 6 {
                return String(value.dropFirst(3).dropLast(3))
            }

            return String(value.dropFirst().dropLast())
        }

        public override var shortDebugDescription: String { value }

        public init() {
            super.init(token: "")
        }

        public init(token: String) {
            super.init(token: token)
        }
    }

    /// Base meta-grammar identifier token.
    /// Represents an ASCII-representable sequence of characters that can be used
    /// as variable names in Swift.
    /// 
    /// Represents the construct:
    /// ```
    /// IDENT ;
    /// ```
    public final class IdentifierToken: TokenNode {
        public var identifier: String {
            token as! String
        }

        public override var shortDebugDescription: String { identifier }

        public init() {
            super.init(token: "")
        }

        public init(token: String) {
            super.init(token: token)
        }
    }

    /// A token in a metagrammar.
    public enum MetagrammarToken: TokenType, ExpressibleByStringLiteral {
        public typealias TokenKind = MetagrammarTokenKind
        public typealias TokenString = String

        /// The regular expression pattern for matching `MetagrammarToken.identifier()`
        /// values from a string.
        public static let identifier_pattern = #/[A-Za-z_][0-9A-Za-z_]*/#

        /// The regular expression pattern for matching `MetagrammarToken.digits()`
        /// values from a string.
        public static let digits_pattern = #/[0-9]+/#

        /// `*`
        /// 
        /// Alias for `Self.star`
        public static let asterisk = Self.star

        /// `.`
        /// 
        /// Alias for `Self.period`
        public static let dot = Self.period

        /// A Swift-compatible identifier token.
        case identifier(String)

        /// A digit sequence.
        case digits(String)

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

        /// `/`
        case forwardSlash
        /// `\`
        case backslash

        @inlinable
        public var kind: MetagrammarTokenKind {
            switch self {
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
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            }
        }

        @inlinable
        public var string: TokenString {
            switch self {
            case .identifier(let value): return value
            case .digits(let value): return value
            case .string(let value): return value.description
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
            case .forwardSlash: return "/"
            case .backslash: return "\\"
            }
        }

        /// Returns the UTF8 length of this token.
        @inlinable
        public var tokenUTF8Length: Int {
            string.utf8.count
        }

        /// Attempts to construct a token from a given string literal value.
        ///
        /// - Note: If the construction fails, an assertion is raised, and should
        /// only be used as a convenience within a parser.
        @inlinable
        public init(stringLiteral value: String) {
            guard let token = Self.from(string: value) else {
                fatalError("\(Self.self): Unknown token literal '\(value)'")
            }

            self = token
        }

        @inlinable
        public static func produceDummy(_ kind: TokenKind) -> Self {
            switch kind {
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
            case .forwardSlash: return .forwardSlash
            case .backslash: return .backslash
            }
        }

        /// Returns a parsed token from the given string or substring.
        /// If the token is not recognized, `nil` is returned, instead.
        @inlinable
        public static func from<S: StringProtocol>(string: S) -> Self? {
            switch string.first {
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
            case "/": return .forwardSlash
            case "\\": return .backslash
            default:
                break
            }

            if let string = string as? Substring {
                // Try identifier
                if let ident = try? identifier_pattern.prefixMatch(in: string) {
                    return .identifier(String(ident.0))
                }
                // Try digits
                if let ident = try? digits_pattern.prefixMatch(in: string) {
                    return .digits(String(ident.0))
                }
                // Try string
                if let string = StringLiteral.from(string: string) {
                    return .string(string)
                }
            } else if let string = string as? String {
                // Try identifier
                if let ident = try? identifier_pattern.prefixMatch(in: string) {
                    return .identifier(String(ident.0))
                }
                // Try digits
                if let ident = try? digits_pattern.prefixMatch(in: string) {
                    return .digits(String(ident.0))
                }
                // Try string
                if let string = StringLiteral.from(string: string[...]) {
                    return .string(string)
                }
            }

            return nil
        }

        /// Specifies a variant of a string literal.
        /// 
        /// Associated values represent the string's contents, not including the
        /// quotes.
        public enum StringLiteral: Hashable, CustomStringConvertible {
            /// Regex used for fetching single- and double-quoted strings.
            public static let quoteRegex = #/("|')((?:\\\1|(?:(?!\1).))*)\1/#

            /// Regex used for fetching triple-quoted strings.
            public static let tripleQuoteRegex = #/(""")((?:\\\1|(?:(?!\1).)|\n)*)\1/#

            /// `'<...>'`
            case singleQuote(String)
            
            /// `"<...>"`
            case doubleQuote(String)

            /// `"""<...>"""`
            /// Supports newlines within
            case tripleQuote(String)

            /// Returns contents of the string, without surrounding quotes.
            @inlinable
            public var contents: String {
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
                switch self {
                case .singleQuote(let string):
                    return #"'\#(string)'"#
                case .doubleQuote(let string):
                    return #""\#(string)""#
                case .tripleQuote(let string):
                    return "\"\"\"\(string)\"\"\""
                }
            }

            /// Returns a parsed string literal from the given substring.
            /// If no string literal is recognized, `nil` is returned, instead.
            @inlinable
            public static func from(string: Substring) -> Self? {
                // Triple quote
                if let match = try? tripleQuoteRegex.prefixMatch(in: string) {
                    return .tripleQuote(String(match.output.1))
                }

                // Single quote
                guard let match = try? quoteRegex.prefixMatch(in: string) else {
                    return nil
                }

                switch match.output.1 {
                case "'":
                    return .singleQuote(String(match.output.2))
                case "\"":
                    return .doubleQuote(String(match.output.2))
                default:
                    return nil
                }
            }
        }
    }

    /// Specifies kinds for metagrammar tokens.
    public enum MetagrammarTokenKind: String, TokenKindType, CaseIterable, ExpressibleByStringLiteral {
        /// `*`
        /// 
        /// Alias for `Self.star`
        public static let asterisk = Self.star

        /// `.`
        /// 
        /// Alias for `Self.period`
        public static let dot = Self.period

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
                fatalError("Unknown metagrammar token kind '\(stringLiteral)'")
            }

            self = value
        }
    }
}

#endif
