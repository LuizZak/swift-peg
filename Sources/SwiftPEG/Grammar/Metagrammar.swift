import SwiftPEGMacros

/// Namespace for SwiftPEG metagrammar grammar objects.
public enum Metagrammar {}

extension Metagrammar {
    /// A grammar file.
    @NodeType<Node>
    public class Grammar: Node {
        /// List of rules in the grammar.
        @NodeProperty
        private var _rules: [Rule] = []

        /// List of meta-properties described in the grammar.
        @NodeProperty
        private var _metas: [Meta] = []
    }

    /// A grammar meta-property.
    @NodeType<Node>
    public class Meta: Node {

    }

    /// A grammar rule.
    @NodeType<Node>
    public class Rule: Node {

    }

    /// A grammar rule's name.
    @NodeType<Node>
    public class RuleName: Node {
        /// The rule's name.
        @NodeProperty
        var _name: IdentifierToken = IdentifierToken()

        /// A type name directly associated with this rule.
        @NodeProperty
        var _type: IdentifierToken?
    }

    /// Base grammar identifier token
    public class IdentifierToken: TokenNode {
        public var identifier: String {
            token as! String
        }

        public init() {
            super.init(token: "")
        }

        public init(token: String) {
            super.init(token: token)
        }
    }
}
