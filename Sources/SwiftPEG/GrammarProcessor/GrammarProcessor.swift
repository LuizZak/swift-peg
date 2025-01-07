/// Performs pre-code generation operations and returns an internal representation
/// grammar suitable to be consumed by code generators.
public class GrammarProcessor {
    typealias KnownProperty = MetaPropertyManager.KnownProperty

    /// Regex for validating rule names.
    public static let ruleNameGrammar = #"[A-Za-z][0-9A-Za-z_]*"#

    let metaPropertyManager: MetaPropertyManager
    let tokensFileProp: KnownProperty
    let importFileProp: KnownProperty
    let tokenProp: KnownProperty
    let tokenTypeNameProp: KnownProperty
    let generationKindProp: KnownProperty
    let anyTokenProp: KnownProperty

    /// A list of non-Error diagnostics issued during grammar analysis.
    ///
    /// This is reset after each `process(_:)` call.
    internal(set) public var diagnostics: [GrammarProcessorDiagnostic] = []

    /// A list of Error diagnostics issued during grammar analysis.
    ///
    /// This is reset after each `process(_:)` call.
    internal(set) public var errors: [GrammarProcessorError] = []

    /// If `true`, prints diagnostics into stdout.
    public var verbose: Bool

    /// The delegate for this grammar processor.
    public weak var delegate: Delegate?

    /// Creates a new `GrammarProcessor` instance.
    public init(delegate: Delegate?, verbose: Bool = false) {
        let propManager = MetaPropertyManager()

        // @tokensFile
        let tokensFileProp = propManager.register(
            name: "tokensFile",
            description: "Optional property for specifying a file containing extra token definitions.",
            acceptedValues: .single([.string(description: "A file path, usually ending in .tokens.")]),
            repeatMode: .never
        )
        // @importFile
        let importFileProp = propManager.register(
            name: "importFile",
            description: "Optional property for specifying a file containing extra grammar definitions to import, along with an alias for namespacing.",
            acceptedValues: .structure(fields: [
                .string(description: "A file path, usually ending in .gram."),
                .exactIdentifier("as", description: "Required separator before alias name"),
                .identifier(description: "Alias for namespacing"),
            ]),
            repeatMode: .distinctValues
        )
        // @token
        let tokenProp = propManager.register(
            name: "token",
            description: "Forward-declares a token identifier.",
            acceptedValues: .list(element: [.identifier(description: "A unique identifier for the token reference.")]),
            repeatMode: .distinctValues
        )
        // @anyToken
        let anyTokenProp = propManager.register(
            name: "anyToken",
            description: "Declares the identifier of the 'any token' construct; a token reference that accepts any valid token from the grammar, including white spaces.",
            acceptedValues: .single([.identifier(description: "A unique identifier for the any token reference.")]),
            repeatMode: .never
        )
        // @generationKind
        let generationKindProp = propManager.register(
            name: "generationKind",
            description: "Specifies the kind of parser declaration to generate.",
            acceptedValues: .single([
                .exactIdentifier("class", description: "Requests that a class be generated. Requires a @tokenTypeName declaration."),
                .exactIdentifier("extension", description: "Requires that an extension with the parsing code be generated"),
            ]),
            repeatMode: .never
        )
        // @tokenTypeName
        let tokenTypeNameProp = propManager.register(
            name: "tokenTypeName",
            description: "Specifies the type name of tokens produced by the raw tokenizer associate with this grammar. Is required if @generationKind class or @generationKind struct is specified. Must be parsable as a Swift type.",
            acceptedValues: .single([.string()]),
            repeatMode: .never
        )

        self.metaPropertyManager = propManager
        self.tokensFileProp = tokensFileProp
        self.tokenProp = tokenProp
        self.importFileProp = importFileProp
        self.anyTokenProp = anyTokenProp
        self.generationKindProp = generationKindProp
        self.tokenTypeNameProp = tokenTypeNameProp
        self.delegate = delegate
        self.verbose = verbose
    }

    /// Processes the given grammar object, optionally providing a custom entry
    /// rule.
    ///
    /// If no custom entry rule name is provided, defaults to `"start"`.
    public func process(
        _ grammar: SwiftPEGGrammar.Grammar,
        entryRuleName: String = "start"
    ) throws -> ProcessedGrammar {

        metaPropertyManager.clearAdded()
        metaPropertyManager.add(from: grammar)
        metaPropertyManager.validateAll()

        for diagnostic in metaPropertyManager.diagnostics {
            diagnostics.append(
                .metaPropertyDiagnostic(diagnostic.metaProperty.node, diagnostic.description)
            )
        }

        let knownRules = try validateRuleNames(in: grammar)
        let dependencyGraph = generateDependencyGraph(for: grammar, knownRules: knownRules)
        let tokensFile = try loadTokensFile(from: grammar)
        let (processedTokens, tokenChannels, occlusionGraph) = try validateTokenSyntaxes(tokensFile)

        let (tokenNames, fragments) = try collectTokenNames(in: grammar, tokensFile: tokensFile)
        try validateReferences(in: grammar, tokens: tokenNames, fragments: fragments)
        diagnoseAltLabels(in: grammar)
        try validateNamedItems(in: grammar)

        diagnoseAltOrder(in: grammar)
        try computeNullables(in: grammar, knownRules)
        try diagnoseUnreachableRules(in: grammar, knownRules, entryRuleName: entryRuleName)

        try diagnoseNonStandardRepetitions(in: grammar)

        let internalGrammar = InternalGrammar.Grammar.from(grammar)

        return ProcessedGrammar(
            grammar: internalGrammar,
            tokens: processedTokens,
            tokenChannels: tokenChannels,
            ruleDependencyGraph: dependencyGraph,
            tokenOcclusionGraph: occlusionGraph
        )
    }

    func recordAndReturn(_ error: GrammarProcessorError) -> GrammarProcessorError {
        self.errors.append(error)
        return error
    }

    func record(_ errors: [GrammarProcessorError]) {
        self.errors.append(contentsOf: errors)
    }

    func generateDependencyGraph(
        for grammar: SwiftPEGGrammar.Grammar,
        knownRules: [String: SwiftPEGGrammar.Rule]
    ) -> RuleDependencyGraph {
        var graph = RuleDependencyGraph(nodes: [], edges: [])

        for rule in grammar.rules {
            graph.nodes.insert(rule.ruleName)

            for name in rule.allNames() {
                guard let endRule = knownRules[name] else {
                    continue
                }

                let edge = RuleDependencyGraph.Edge(start: rule.ruleName, end: endRule.ruleName)
                graph.edges.insert(edge)
            }
        }

        return graph
    }

    func validateRuleNames(in grammar: SwiftPEGGrammar.Grammar) throws -> [String: SwiftPEGGrammar.Rule] {
        var knownRules: [String: SwiftPEGGrammar.Rule] = [:]

        for rule in grammar.rules {
            let ruleName = try validateRuleName(rule)

            if let existing = knownRules[ruleName] {
                throw recordAndReturn(.repeatedRuleName(ruleName, rule, prior: existing))
            } else {
                knownRules[ruleName] = rule
            }
        }

        return knownRules
    }

    func validateRuleName(_ rule: SwiftPEGGrammar.Rule) throws -> String {
        let ruleName = String(rule.ruleName)
        if ruleName.isEmpty {
            throw recordAndReturn(.invalidRuleName(desc: "Rule name cannot be empty", rule))
        }
        if ruleName.hasPrefix("_") {
            throw recordAndReturn(.invalidRuleName(desc: "Rule names cannot start with '_'", rule))
        }
        if try Regex(Self.ruleNameGrammar).wholeMatch(in: ruleName) == nil {
            throw recordAndReturn(.invalidRuleName(desc: "Expected rule names to match regex '\(Self.ruleNameGrammar)'", rule))
        }

        return ruleName
    }

    func loadTokensFile(from grammar: SwiftPEGGrammar.Grammar) throws -> [SwiftPEGGrammar.TokenFileDeclaration] {
        guard let tokensMeta = metaPropertyManager.propertiesValidating(knownProperty: self.tokensFileProp).first else {
            return []
        }
        guard let tokensFileName = tokensMeta.values.first?.stringValue else {
            return []
        }

        guard let fileContents = try delegate?.grammarProcessor(self, loadTokensFileNamed: tokensFileName, ofGrammar: grammar) else {
            throw recordAndReturn(.failedToLoadTokensFile(tokensMeta.node))
        }

        let parser = GrammarParser(raw: GrammarRawTokenizer(source: fileContents))

        do {
            guard let tokens = try parser.tokensFile(), try parser.isEOF() else {
                throw recordAndReturn(.tokensFileSyntaxError(tokensMeta.node, parser.makeSyntaxError()))
            }

            return tokens
        } catch let error as ParserError {
            throw recordAndReturn(.tokensFileSyntaxError(tokensMeta.node, error))
        } catch let error as TokenizerError {
            throw recordAndReturn(.tokensFileTokenizerError(tokensMeta.node, error))
        } catch {
            throw error
        }
    }

    /// Collects all the names of known token definitions.
    func collectTokenNames(
        in grammar: SwiftPEGGrammar.Grammar,
        tokensFile: [SwiftPEGGrammar.TokenFileDeclaration]
    ) throws -> (tokens: Set<String>, fragments: Set<String>) {
        var metaTokens: Set<String> = []

        for token in tokenMetaProperties(in: grammar) {
            for value in token.values {
                guard let value = value as? SwiftPEGGrammar.MetaIdentifierValue else {
                    continue
                }

                let name = String(value.identifier.string)
                metaTokens.insert(name)
            }
        }

        var tokensFromFile: Set<String> = []
        var fragments: Set<String> = []
        for decl in tokensFile {
            guard let token = decl as? SwiftPEGGrammar.TokenDefinition else {
                continue
            }

            let tokenName = String(token.name.string)

            if token.isFragment {
                if token.tokenCodeReference != nil {
                    diagnostics.append(.fragmentSpecifiesStaticToken(token: token))
                }

                fragments.insert(tokenName)
            } else {
                tokensFromFile.insert(tokenName)
            }
        }

        let tokens = Set(metaTokens).union(tokensFromFile)
        return (tokens, fragments)
    }

    /// Validates that all identifier references in a given grammar are either
    /// rules within the grammar itself or a token name from the provided set.
    func validateReferences(
        in grammar: SwiftPEGGrammar.Grammar,
        tokens: Set<String>,
        fragments: Set<String>
    ) throws {
        // Populate references
        var knownNames: [(String, SwiftPEGGrammar.IdentAtom.Identity)]
        knownNames = tokens.map {
            ($0, .token)
        }
        for rule in grammar.rules {
            let name = String(rule.ruleName)
            knownNames.append(
                (name, .ruleName)
            )
        }
        // anyToken
        if let anyToken = metaPropertyManager.firstValue(of: anyTokenProp)?.stringValue {
            knownNames.append(
                (anyToken, .anyToken)
            )
        }

        // Associate identities to all identifiers
        let visitor = ReferenceVisitor(knownIdentifiers: knownNames)

        let walker = NodeWalker(visitor: visitor)
        try walker.walk(grammar)

        guard !visitor.unknownReferences.isEmpty else {
            // Ok!
            return
        }

        let tokensFile = metaPropertyManager.firstValue(of: tokensFileProp)?.stringValue

        var firstError: Error?
        for ref in visitor.unknownReferences {
            guard let rule: SwiftPEGGrammar.Rule = ref.firstAncestor() else {
                fatalError("Found atom \(ref.name) @ \(ref.location) that has no Rule parent?")
            }

            let error: GrammarProcessorError

            if fragments.contains(String(ref.name)) {
                error = recordAndReturn(
                    .referencedFragmentInParser(
                        ref, rule
                    )
                )
            } else {
                error = recordAndReturn(
                    .unknownReference(
                        ref, rule, tokensFileName: tokensFile
                    )
                )
            }

            firstError = firstError ?? error
        }

        throw firstError!
    }

    func diagnoseAltLabels(in grammar: SwiftPEGGrammar.Grammar) {
        var altsByParent: [(ObjectIdentifier, [(rule: SwiftPEGGrammar.Rule, label: SwiftPEGGrammar.AltLabel)])] = []
        let visitor = AltLabelVisitor { (rule, alt, label) in
            guard let parent = alt.parent.map(ObjectIdentifier.init) else {
                return
            }

            if let index = altsByParent.firstIndex(where: { $0.0 == parent }) {
                altsByParent[index].1.append(
                    (rule, label)
                )
            } else {
                altsByParent.append(
                    (parent, [(rule, label)])
                )
            }
        }

        let walker = NodeWalker(visitor: visitor)
        try? walker.walk(grammar)

        for (_, entries) in altsByParent {
            var firstDefinition: [String: SwiftPEGGrammar.AltLabel] = [:]

            for entry in entries {
                let key = entry.label.name.description

                if let prior = firstDefinition[key] {
                    diagnostics.append(
                        .repeatedAltLabels(
                            rule: entry.rule,
                            label: entry.label,
                            previousOccurrence: prior
                        )
                    )
                } else {
                    firstDefinition[key] = entry.label
                }
            }
        }
    }

    func validateNamedItems(in grammar: SwiftPEGGrammar.Grammar) throws {
        let visitor = NamedItemVisitor { node in
            guard let name = node.name?.string, name != "_" else {
                return
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

        record(visitor.errors)

        if let first = visitor.errors.first {
            throw first
        }
    }

    func diagnoseNonStandardRepetitions(in grammar: SwiftPEGGrammar.Grammar) throws {
        let visitor = NonStandardRepetitionVisitor { (rule, item, repetitionMode) in
            guard repetitionMode != .standard else {
                return
            }

            self.diagnostics.append(
                .nonStandardRepetitionAsLastItem(rule, item, repetitionMode)
            )
        }

        let walker = NodeWalker(visitor: visitor)
        try walker.walk(grammar)
    }

    /// Gets all @token meta-properties in the grammar.
    func tokenMetaProperties(in grammar: SwiftPEGGrammar.Grammar) -> [SwiftPEGGrammar.Meta] {
        metaPropertyManager.propertiesValidating(knownProperty: tokenProp).map(\.node)
    }

    func printIfVerbose(_ item: Any) {
        guard verbose else { return }

        print(item)
    }

    /// An error that can be raised by `GrammarProcessor` during grammar analysis and parser
    /// generation.
    public enum GrammarProcessorError: Error, CustomStringConvertible {
        // MARK: - Grammar file errors

        /// Rules found sharing the same name.
        case repeatedRuleName(String, SwiftPEGGrammar.Rule, prior: SwiftPEGGrammar.Rule)

        /// Rule found that has an invalid name.
        case invalidRuleName(desc: String, SwiftPEGGrammar.Rule)

        /// Named item found that has an invalid name.
        case invalidNamedItem(desc: String, SwiftPEGGrammar.NamedItem)

        /// An identifier was found in a rule that could not be resolved to a
        /// rule or token name.
        case unknownReference(SwiftPEGGrammar.IdentAtom, SwiftPEGGrammar.Rule, tokensFileName: String? = nil)

        /// An identifier was found in a rule that refers to token fragments,
        /// which are not queryable by the parser.
        case referencedFragmentInParser(SwiftPEGGrammar.IdentAtom, SwiftPEGGrammar.Rule)

        /// An attempt at resolving a left recursion and find a leader rule from
        /// a set of rules has failed.
        case unresolvedLeftRecursion(ruleNames: [String])

        /// An attempt to generate a class or struct with @generationKind while
        /// no @tokenTypeName was provided.
        case missingTokenTypeName

        /// A @generationKind meta property was provided with an unknown production
        /// kind.
        case unknownProductionType(SwiftPEGGrammar.Meta)

        /// A @tokenTypeName meta property was provided that could not be parsed
        /// as a valid Swift type.
        case invalidTokenTypeName(SwiftPEGGrammar.Meta)

        // MARK: - Tokens file errors

        /// Tokens found sharing the same name.
        case repeatedTokenName(String, SwiftPEGGrammar.TokenDefinition, prior: SwiftPEGGrammar.TokenDefinition)

        /// An identifier was found in a token syntax that could not be resolved
        /// to another token syntax.
        case unknownReferenceInToken(String, SwiftPEGGrammar.TokenDefinition)

        /// A recursion was found in a sequence of tokens.
        case recursivityInTokens([SwiftPEGGrammar.TokenDefinition])

        /// A '@tokensFile' meta-property references a file that could not be
        /// loaded.
        case failedToLoadTokensFile(SwiftPEGGrammar.Meta)

        /// A '@tokensFile' meta-property references a tokens file that contains
        /// syntax errors.
        case tokensFileSyntaxError(SwiftPEGGrammar.Meta, ParserError)

        /// A '@tokensFile' meta-property references a tokens file that contains
        /// tokenizer errors.
        case tokensFileTokenizerError(SwiftPEGGrammar.Meta, TokenizerError)

        // MARK: -

        /// A generic error with an attached message.
        case message(String)

        public var description: String {
            switch self {
            case .repeatedRuleName(let name, let rule, let prior):
                return "Rule '\(name)' re-declared @ \(rule.location). Original declaration @ \(prior.location)."

            case .repeatedTokenName(let name, let token, let prior):
                return "Token '\(name)' re-declared @ \(token.location). Original declaration @ \(prior.location)."

            case .invalidRuleName(let desc, let rule):
                return "Rule name '\(rule.ruleName)' @ \(rule.location) is not valid: \(desc)"

            case .invalidNamedItem(let desc, let item):
                return "Named item '\(item.name?.string ?? "<nil>")' @ \(item.location) is not valid: \(desc)"

            case .unknownReference(let atom, let rule, let tokensFileName):
                return """
                Reference to unknown identifier '\(atom.name)' @ \(atom.location) in rule '\(rule.name.shortDebugDescription)'. \
                Did you forget to forward-declare a token with '@token \(atom.name);' or define it in '@tokensFile \"\(tokensFileName ?? "<file.tokens>")\"'?
                """

            case .referencedFragmentInParser(let atom, let rule):
                return """
                Reference to token fragment '\(atom.name)' @ \(atom.location) found in parser in rule '\(rule.name.shortDebugDescription)'. \
                Token fragments cannot be referred by the parser, and can only be used as part of definition of tokens.
                """

            case .unknownReferenceInToken(let identifier, let token):
                return "Reference to unknown identifier '\(identifier)' in token '\(token.name)' @ \(token.location)."

            case .recursivityInTokens(let tokens):
                return "Recursivity in token definitions is not supported; recursive cycle: \(tokens.map(\.name.string).joined(separator: " -> ")) starting @ \(tokens[0].location)"

            case .unresolvedLeftRecursion(let ruleNames):
                return "Could not resolve left recursion with a lead rule in the set \(ruleNames)"

            case .failedToLoadTokensFile(let meta):
                return "\(meta.shortDebugDescription) (\(meta.location)) references a file that could not be loaded."

            case .tokensFileSyntaxError(let meta, let error):
                return "\(meta.shortDebugDescription) (\(meta.location)) failed to parse due to syntax errors: \(error)"

            case .tokensFileTokenizerError(let meta, let error):
                return "\(meta.shortDebugDescription) (\(meta.location)) failed to parse due to tokenizer errors: \(error)"

            case .missingTokenTypeName:
                return "@tokenTypeName is required by current meta-property specifies, but it was not found."

            case .unknownProductionType(let meta):
                return "\(meta.shortDebugDescription) (\(meta.location)) provided unknown production kind."

            case .invalidTokenTypeName(let meta):
                return "\(meta.shortDebugDescription) (\(meta.location)) could not be parsed as a valid Swift type identifier."

            case .message(let message):
                return message
            }
        }
    }

    /// A non-fatal GrammarProcessor diagnostic.
    public enum GrammarProcessorDiagnostic {
        // MARK: - Grammar file diagnostics

        /// A set of alternatives has two alternatives with the same label on the
        /// same depth.
        case repeatedAltLabels(
            rule: SwiftPEGGrammar.Rule,
            label: SwiftPEGGrammar.AltLabel,
            previousOccurrence: SwiftPEGGrammar.AltLabel
        )

        /// An alt that executes before another, if it succeeds, will always
        /// prevent the other alt from being attempted.
        case altOrderIssue(
            rule: SwiftPEGGrammar.Rule,
            SwiftPEGGrammar.Alt,
            alwaysSucceedsBefore: SwiftPEGGrammar.Alt
        )

        /// A rule is not reachable from the set start rule.
        case unreachableRule(SwiftPEGGrammar.Rule, startRuleName: String)

        /// Diagnostic reported by meta-property manager.
        case metaPropertyDiagnostic(SwiftPEGGrammar.Meta, String)

        /// A non-standard repetition (`<`/`>`) was found at the end of an
        /// alternative.
        case nonStandardRepetitionAsLastItem(SwiftPEGGrammar.Rule, SwiftPEGGrammar.Item, CommonAbstract.RepetitionMode)

        // MARK: - Tokens file diagnostics

        /// A fragment token definition declares a static token for the fragment,
        /// which is never used by code processors or code generators.
        case fragmentSpecifiesStaticToken(
            token: SwiftPEGGrammar.TokenDefinition
        )

        /// A token atom has a combination of exclusion + terminal that results
        /// in no input ever matching.
        case unfulfillableAtomInToken(
            token: SwiftPEGGrammar.TokenDefinition,
            CommonAbstract.TokenAtom
        )

        /// An alt of a token that executes before another, if it succeeds, will
        /// always prevent the other alt from being attempted.
        case tokenAltOrderIssue(
            token: SwiftPEGGrammar.TokenDefinition,
            CommonAbstract.TokenAlt,
            alwaysSucceedsBefore: CommonAbstract.TokenAlt
        )

        /// An atom of a token that executes before another, if it succeeds, will
        /// always prevent the other atom from being attempted.
        case tokenAtomOrderIssue(
            token: SwiftPEGGrammar.TokenDefinition,
            CommonAbstract.TokenAtom,
            alwaysSucceedsBefore: CommonAbstract.TokenAtom
        )

        public var description: String {
            switch self {
            case .repeatedAltLabels(let rule, let prior, let former):
                return """
                    Alternatives have repeated label \(prior.name.description) @ \(prior.location) \
                    defined before @ \(former.location) in rule \(rule.name) @ \(rule.location)
                    """

            case .altOrderIssue(let rule, let prior, let former):
                func describe(_ alt: InternalGrammar.Alt) -> String {
                    if alt == alt.reduced { return "'\(alt)'" }
                    return "'\(alt)' (when reduced as '\(alt.reduced?.description ?? "<empty>")')"
                }

                let priorInt = InternalGrammar.Alt.from(prior)
                let formerInt = InternalGrammar.Alt.from(former)

                return """
                    Alt \(describe(priorInt)) @ \(prior.location) always succeeds \
                    before \(describe(formerInt)) @ \(former.location) can be tried \
                    in rule \(rule.ruleName) @ \(rule.location).
                    """

            case .unreachableRule(let rule, let startRuleName):
                return "Rule '\(rule.ruleName)' @ \(rule.location) is not reachable from the set start rule '\(startRuleName)'."

            case .metaPropertyDiagnostic(_, let message):
                return message

            case .nonStandardRepetitionAsLastItem(_, let item, let mode):
                let itemInt = InternalGrammar.Item.from(item)
                let modeString = mode == .minimal ? "Minimal" : "Maximal"

                return """
                    \(modeString) repetition '\(itemInt)' @ \(item.location) at \
                    the end of an alternative will behave as a standard repetition.
                    """

            case .fragmentSpecifiesStaticToken(let token):
                return """
                    Token fragment %\(token.name) @ \(token.location) specifies \
                    a static token value, which is not relevant for fragments and \
                    will be ignored.
                    """

            case .unfulfillableAtomInToken(let token, let atom):
                return """
                    Token $\(token.name) @ \(token.location) contains atom '\(atom)' \
                    which has a token terminal + exclusion that cannot ever be \
                    fulfilled by any input, and will never match.
                    """

            case .tokenAltOrderIssue(let token, let prior, let former):
                func describe(_ alt: CommonAbstract.TokenAlt) -> String {
                    return #""\#(alt)""#
                }

                return """
                    Alt \(describe(prior)) always succeeds \
                    before \(describe(former)) can be tried \
                    in token definition \(token.name.string) @ \(token.location).
                    """

            case .tokenAtomOrderIssue(let token, let prior, let former):
                func describe(_ atom: CommonAbstract.TokenAtom) -> String {
                    return #""\#(atom)""#
                }

                return """
                    Grouped atom \(describe(prior)) always succeeds \
                    before \(describe(former)) can be tried \
                    in token definition \(token.name.string) @ \(token.location).
                    """
            }
        }
    }

    /// Delegate for `GrammarProcessor` operations.
    public protocol Delegate: AnyObject {
        /// Called by a grammar processor to resolve references to `@tokensFile`
        /// meta-property.
        func grammarProcessor(
            _ processor: GrammarProcessor,
            loadTokensFileNamed name: String,
            ofGrammar grammar: SwiftPEGGrammar.Grammar
        ) throws -> String

        /// Called by a grammar processor to resolve references to `@import`
        /// meta-property.
        func grammarProcessor(
            _ processor: GrammarProcessor,
            importFileNamed name: String,
            ofGrammar grammar: SwiftPEGGrammar.Grammar
        ) throws -> String
    }
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
        var errors: [GrammarProcessorError] = []

        init(_ callback: @escaping (SwiftPEGGrammar.NamedItem) throws -> Void) {
            self.callback = callback
        }

        func visit(_ node: SwiftPEGGrammar.NamedItem) throws -> NodeVisitChildrenResult {
            do {
                try callback(node)
            } catch let error as GrammarProcessorError {
                errors.append(error)
            }

            return .visitChildren
        }
    }

    /// Visitor used to diagnose non-standard repetitions (`<`/`>`) at the end
    /// of alternatives.
    final class NonStandardRepetitionVisitor: SwiftPEGGrammar.GrammarNodeVisitorType {
        typealias Callback = (SwiftPEGGrammar.Rule, SwiftPEGGrammar.Item, CommonAbstract.RepetitionMode) throws -> Void

        var currentRule: SwiftPEGGrammar.Rule?
        var callback: Callback

        init(_ callback: @escaping Callback) {
            self.callback = callback
        }

        func willVisit(_ node: Node) {
            if let rule = node as? SwiftPEGGrammar.Rule {
                currentRule = rule
            }
        }

        func visit(_ node: SwiftPEGGrammar.Alt) throws -> NodeVisitChildrenResult {
            guard let currentRule, let lastItem = node.namedItems.last?.item else {
                return .visitChildren
            }

            switch lastItem {
            case let node as SwiftPEGGrammar.ZeroOrMoreItem:
                try callback(currentRule, node, node.repetitionMode)
            case let node as SwiftPEGGrammar.OneOrMoreItem:
                try callback(currentRule, node, node.repetitionMode)
            case let node as SwiftPEGGrammar.GatherItem:
                try callback(currentRule, node, node.repetitionMode)
            default:
                break
            }

            return .visitChildren
        }

        func didVisit(_ node: Node) {
            if node === currentRule {
                currentRule = nil
            }
        }
    }

    /// Visitor used to diagnose repeated alternative labels in grammars.
    final class AltLabelVisitor: SwiftPEGGrammar.GrammarNodeVisitorType {
        typealias Callback = (SwiftPEGGrammar.Rule, SwiftPEGGrammar.Alt, SwiftPEGGrammar.AltLabel) throws -> Void

        var currentRule: SwiftPEGGrammar.Rule?
        var callback: Callback

        init(_ callback: @escaping Callback) {
            self.callback = callback
        }

        func willVisit(_ node: Node) {
            if let rule = node as? SwiftPEGGrammar.Rule {
                currentRule = rule
            }
        }

        func visit(_ node: SwiftPEGGrammar.Alt) throws -> NodeVisitChildrenResult {
            guard let currentRule else {
                return .visitChildren
            }

            if let altLabel = node.altLabel {
                try callback(currentRule, node, altLabel)
            }

            return .visitChildren
        }

        func didVisit(_ node: Node) {
            if node === currentRule {
                currentRule = nil
            }
        }
    }
}

// MARK: Helper extensions

extension SwiftPEGGrammar.Grammar {
    func hasMeta(named name: String) -> Bool {
        self.meta(named: name) != nil
    }

    func meta(named name: String) -> SwiftPEGGrammar.Meta? {
        metas.first(where: { $0.name.string == name })
    }
}
