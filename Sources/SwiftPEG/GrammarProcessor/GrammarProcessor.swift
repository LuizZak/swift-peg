/// Performs pre-code generation operations and returns an internal representation
/// grammar suitable to be consumed by code generators.
public class GrammarProcessor {
    /// Regex for validating rule names.
    public static let ruleNameGrammar = #"[A-Za-z][0-9A-Za-z_]*"#

    /// Name of optional @meta-property that is queried for loading a .tokens
    /// file with extra token definition information.
    public static let tokensFile = "tokensFile"

    let grammar: SwiftPEGGrammar.Grammar
    var tokensFileName: String? = nil
    var tokensFile: [SwiftPEGGrammar.TokenDefinition] = []

    /// Rules remaining to be generated.
    var remaining: [String: SwiftPEGGrammar.Rule] = [:]

    /// Stores information about locally-created variables when processing alts
    /// of rules.
    var localVariableStack: [[String]] = []

    /// A list of non-Error diagnostics issued during grammar analysis.
    private(set) public var diagnostics: [GrammarProcessorDiagnostic] = []

    /// If `true`, prints diagnostics into stdout.
    public var verbose: Bool

    /// The delegate for this grammar processor.
    public weak var delegate: Delegate?

    /// Prepares a `GrammarProcessor` instance based on a given parsed grammar.
    public init(
        _ grammar: SwiftPEGGrammar.Grammar,
        delegate: Delegate?,
        verbose: Bool = false
    ) throws {
        self.grammar = grammar
        self.delegate = delegate
        self.verbose = verbose

        let knownRules = try validateRuleNames()
        tokensFile = try loadTokensFile()
        let tokens = try validateTokenReferences()
        try validateReferences(tokens: tokens)

        try computeNullables(knownRules)

        self.remaining = knownRules
    }

    /// Returns the reduced and tagged grammar for grammar analysis.
    public func generatedGrammar() -> InternalGrammar.Grammar {
        .from(grammar)
    }

    /// Returns the token definitions that where loaded from an associated tokens
    /// file.
    public func tokenDefinitions() -> [InternalGrammar.TokenDefinition] {
        tokensFile.map(InternalGrammar.TokenDefinition.from)
    }

    func validateRuleNames() throws -> [String: SwiftPEGGrammar.Rule] {
        var knownRules: [String: SwiftPEGGrammar.Rule] = [:]

        for rule in grammar.rules {
            let ruleName = try validateRuleName(rule)

            if let existing = knownRules[ruleName] {
                throw GrammarProcessorError.repeatedRuleName(ruleName, rule, prior: existing)
            } else {
                knownRules[ruleName] = rule
            }
        }

        return knownRules
    }

    func validateRuleName(_ rule: SwiftPEGGrammar.Rule) throws -> String {
        let ruleName = String(rule.name.name.string)
        if ruleName.isEmpty {
            throw GrammarProcessorError.invalidRuleName(desc: "Rule name cannot be empty", rule)
        }
        if try Regex(Self.ruleNameGrammar).wholeMatch(in: ruleName) == nil {
            throw GrammarProcessorError.invalidRuleName(desc: "Expected rule names to match regex '\(Self.ruleNameGrammar)'", rule)
        }

        return ruleName
    }

    func loadTokensFile() throws -> [SwiftPEGGrammar.TokenDefinition] {
        guard let tokensMeta = grammar.metas.first(where: { $0.name.string == Self.tokensFile }) else {
            return []
        }
        guard let fileName = tokensMeta.value as? SwiftPEGGrammar.MetaStringValue else {
            throw GrammarProcessorError.failedToLoadTokensFile(tokensMeta)
        }
        self.tokensFileName = String(fileName.string.processedString)

        guard let fileContents = try delegate?.grammarProcessor(self, loadTokensFileNamed: String(fileName.string.processedString)) else {
            throw GrammarProcessorError.failedToLoadTokensFile(tokensMeta)
        }

        let parser = GrammarParser(raw: GrammarRawTokenizer(source: fileContents))
        
        do {
            guard let tokens = try parser.tokensFile(), parser.tokenizer.isEOF else {
                throw GrammarProcessorError.tokensFileSyntaxError(tokensMeta, parser.makeSyntaxError())
            }
            
            return tokens
        } catch let error as ParserError {
            throw GrammarProcessorError.tokensFileSyntaxError(tokensMeta, error)
        } catch {
            throw error
        }
    }

    func validateTokenReferences() throws -> Set<String> {
        var metaTokens: [String: SwiftPEGGrammar.Meta] = [:]
        var issuedWarnings: Set<String> = []

        for token in tokenMetaProperties() {
            guard let value = token.value as? SwiftPEGGrammar.MetaIdentifierValue else {
                diagnostics.append(.tokenMissingName(token))
                continue
            }

            let name = String(value.identifier.string)
            guard !issuedWarnings.contains(name) else {
                continue
            }

            if let prior = metaTokens[name] {
                diagnostics.append(
                    .repeatedTokenDeclaration(name: name, token, prior: prior)
                )
                issuedWarnings.insert(name)
            } else {
                metaTokens[name] = token
            }
        }

        var tokensFromFile: Set<String> = []
        for token in tokensFile {
            tokensFromFile.insert(String(token.name.string))
        }

        return Set(metaTokens.keys).union(tokensFromFile)
    }

    func validateReferences(tokens: Set<String>) throws {
        var knownNames: [(String, SwiftPEGGrammar.IdentAtom.Identity)]
        knownNames = tokens.map {
            ($0, .token)
        }
        for rule in grammar.rules {
            let name = String(rule.name.name.string)
            knownNames.append(
                (name, .ruleName)
            )
        }

        let visitor = ReferenceVisitor(knownIdentifiers: knownNames)

        let walker = NodeWalker(visitor: visitor)
        try walker.walk(grammar)

        guard !visitor.unknownReferences.isEmpty else {
            // Ok!
            return
        }

        for ref in visitor.unknownReferences {
            guard let rule: SwiftPEGGrammar.Rule = ref.firstAncestor() else {
                fatalError("Found atom \(ref.name) @ \(ref.location) that has no Rule parent?")
            }

            throw GrammarProcessorError.unknownReference(ref, rule, tokensFileName: self.tokensFileName)
        }
    }

    func validateNamedItems() throws {
        let visitor = NamedItemVisitor { node in
            guard let name = node.name?.string, name != "_" else {
                return
            }

            if name == "cut" {
                // TODO: Lift this restriction by dynamically deriving a suitable name for a cut local?
                throw GrammarProcessorError.invalidNamedItem(
                    desc: "Name cannot be 'cut' as it interferes with locals generated by parsers",
                    node
                )
            }
            if name.hasPrefix("_") {
                throw GrammarProcessorError.invalidNamedItem(
                    desc: "Names cannot start with '_'",
                    node
                )
            }
        }

        let walker = NodeWalker(visitor: visitor)
        try walker.walk(grammar)
    }

    /// Gets all @token meta-properties in the grammar.
    func tokenMetaProperties() -> [SwiftPEGGrammar.Meta] {
        return grammar.metas.filter { $0.name.string == "token" }
    }

    func printIfVerbose(_ item: Any) {
        guard verbose else { return }

        print(item)
    }

    func toInternalRepresentation() -> [InternalGrammar.Rule] {
        grammar.rules.map(InternalGrammar.Rule.from)
    }

    /// An error that can be raised by `GrammarProcessor` during grammar analysis and parser
    /// generation.
    public enum GrammarProcessorError: Error, CustomStringConvertible {
        /// Rules found sharing the same name.
        case repeatedRuleName(String, SwiftPEGGrammar.Rule, prior: SwiftPEGGrammar.Rule)
        /// Rule found that has an invalid name.
        case invalidRuleName(desc: String, SwiftPEGGrammar.Rule)
        /// Named item found that has an invalid name.
        case invalidNamedItem(desc: String, SwiftPEGGrammar.NamedItem)
        /// An identifier was found in a rule that could not be resolved to a
        /// rule or token name.
        case unknownReference(SwiftPEGGrammar.IdentAtom, SwiftPEGGrammar.Rule, tokensFileName: String? = nil)

        /// An attempt at resolving a left recursion and find a leader rule from
        /// a set of rules has failed.
        case unresolvedLeftRecursion(ruleNames: [String])

        /// A '@tokensFile' meta-property references a file that could not be
        /// loaded.
        case failedToLoadTokensFile(SwiftPEGGrammar.Meta)

        /// A '@tokensFile' meta-property references a tokens file that contains
        /// syntax errors.
        case tokensFileSyntaxError(SwiftPEGGrammar.Meta, ParserError)

        /// A generic error with an attached message.
        case message(String)

        public var description: String {
            switch self {
            case .repeatedRuleName(let name, let rule, let prior):
                return "Rule '\(name)' re-declared @ \(rule.location). Original declaration @ \(prior.location)."
            case .invalidRuleName(let desc, let rule):
                return "Rule name '\(rule.name.name.string)' @ \(rule.location) is not valid: \(desc)"
            case .invalidNamedItem(let desc, let item):
                return "Named item '\(item.name?.string ?? "<nil>")' @ \(item.location) is not valid: \(desc)"
            case .unknownReference(let atom, let rule, let tokensFileName):
                return """
                Reference to unknown identifier '\(atom.name)' @ \(atom.location) in rule '\(rule.name.shortDebugDescription)'. \
                Did you forget to forward-declare a token with '@token \(atom.name);' or define it in '@tokensFile \"\(tokensFileName ?? "<file.tokens>")\"'?
                """
            case .unresolvedLeftRecursion(let ruleNames):
                return "Could not resolve left recursion with a lead rule in the set \(ruleNames)"
            case .failedToLoadTokensFile(let meta):
                return "@tokenFile @ \(meta.location) references a file that could not be loaded."
            case .tokensFileSyntaxError(let meta, let error):
                return "@tokenFile @ \(meta.location) failed to parse due to syntax errors: \(error)"
            case .message(let message):
                return message
            }
        }
    }

    /// A non-fatal GrammarProcessor diagnostic.
    public enum GrammarProcessorDiagnostic {
        /// A token has been declared multiple times with '@token'.
        /// Only reported the first time a duplicated token with a matching name
        /// is declared.
        case repeatedTokenDeclaration(name: String, SwiftPEGGrammar.GrammarNode, prior: SwiftPEGGrammar.GrammarNode)

        /// A '@token' meta-property is missing an identifier as its value.
        case tokenMissingName(SwiftPEGGrammar.Meta)

        public var description: String {
            switch self {
            case .repeatedTokenDeclaration(let name, let meta, let prior):
                return "Token '\(name)' @ \(meta.location) has been declared at least once @ \(prior.location)."
            case .tokenMissingName(let meta):
                return "Expected token @ \(meta.location) to have an identifier name."
            }
        }
    }

    /// Delegate for `GrammarProcessor` operations.
    public protocol Delegate: AnyObject {
        /// Called by a grammar processor to resolve references to `@tokensFile`
        /// meta-property.
        func grammarProcessor(
            _ processor: GrammarProcessor,
            loadTokensFileNamed name: String
        ) throws -> String
    }
}

// MARK: - Internal representation
extension GrammarProcessor {
    
}

// MARK: - Visitors

private extension GrammarProcessor {
    /// Visitor used to validate named references in rules.
    final class ReferenceVisitor: SwiftPEGGrammar.GrammarNodeVisitorType {
        private let knownIdentifiers: [(name: String, type: SwiftPEGGrammar.IdentAtom.Identity)]
        var unknownReferences: [SwiftPEGGrammar.IdentAtom] = []

        init(knownIdentifiers: some Sequence<(String, SwiftPEGGrammar.IdentAtom.Identity)>) {
            self.knownIdentifiers = Array(knownIdentifiers)
        }

        func visit(_ node: SwiftPEGGrammar.IdentAtom) throws -> NodeVisitChildrenResult {
            let ref = node.identifier.string

            if let identifier = knownIdentifiers.first(where: { $0.name == ref }) {
                node.identity = identifier.type
            } else {
                unknownReferences.append(node)
            }

            return .visitChildren
        }
    }

    /// Visitor used to validate named items in rules.
    final class NamedItemVisitor: SwiftPEGGrammar.GrammarNodeVisitorType {
        var callback: (SwiftPEGGrammar.NamedItem) throws -> Void

        init(_ callback: @escaping (SwiftPEGGrammar.NamedItem) throws -> Void) {
            self.callback = callback
        }

        func visit(_ node: SwiftPEGGrammar.NamedItem) throws -> NodeVisitChildrenResult {
            try callback(node)

            return .visitChildren
        }
    }
}
