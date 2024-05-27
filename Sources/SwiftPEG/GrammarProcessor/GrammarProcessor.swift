/// Performs pre-code generation operations and returns an internal representation
/// grammar suitable to be consumed by code generators.
public class GrammarProcessor {
    typealias KnownProperty = MetaPropertyManager.KnownProperty

    /// Regex for validating rule names.
    public static let ruleNameGrammar = #"[A-Za-z][0-9A-Za-z_]*"#

    /// Name of optional @meta-property that is queried for loading a .tokens
    /// file with extra token definition information.
    public static let tokensFile = "tokensFile"

    let metaPropertyManager: MetaPropertyManager
    let tokensFileProp: KnownProperty
    let tokenProp: KnownProperty

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
            acceptedValues: [.string(description: "A file path, usually ending in .tokens.")],
            repeatMode: .never
        )
        // @token
        let tokenProp = propManager.register(
            name: "token",
            description: "Forward-declares a token identifier.",
            acceptedValues: [.identifier(description: "A unique identifier for the token reference.")],
            repeatMode: .distinctValues
        )

        self.metaPropertyManager = propManager
        self.tokensFileProp = tokensFileProp
        self.tokenProp = tokenProp
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
        let tokensFile = try loadTokensFile(from: grammar)
        let tokens = try validateTokenReferences(in: grammar, tokensFile: tokensFile)
        try validateReferences(in: grammar, tokens: tokens)
        diagnoseAltOrder(in: grammar)
        try computeNullables(in: grammar, knownRules)
        try diagnoseUnreachableRules(in: grammar, knownRules, entryRuleName: entryRuleName)

        return ProcessedGrammar(
            grammar: .from(grammar),
            tokens: tokensFile.map(InternalGrammar.TokenDefinition.from)
        )
    }

    func recordAndReturn(_ error: GrammarProcessorError) -> GrammarProcessorError {
        self.errors.append(error)
        return error
    }

    func record(_ errors: [GrammarProcessorError]) {
        self.errors.append(contentsOf: errors)
    }

    func validateRuleNames(in grammar: SwiftPEGGrammar.Grammar) throws -> [String: SwiftPEGGrammar.Rule] {
        var knownRules: [String: SwiftPEGGrammar.Rule] = [:]

        for rule in grammar.rules {
            let ruleName = try validateRuleName(rule)

            if let existing = knownRules[ruleName] {
                throw recordAndReturn(GrammarProcessorError.repeatedRuleName(ruleName, rule, prior: existing))
            } else {
                knownRules[ruleName] = rule
            }
        }

        return knownRules
    }

    func validateRuleName(_ rule: SwiftPEGGrammar.Rule) throws -> String {
        let ruleName = String(rule.name.name.string)
        if ruleName.isEmpty {
            throw recordAndReturn(GrammarProcessorError.invalidRuleName(desc: "Rule name cannot be empty", rule))
        }
        if try Regex(Self.ruleNameGrammar).wholeMatch(in: ruleName) == nil {
            throw recordAndReturn(GrammarProcessorError.invalidRuleName(desc: "Expected rule names to match regex '\(Self.ruleNameGrammar)'", rule))
        }

        return ruleName
    }

    func loadTokensFile(from grammar: SwiftPEGGrammar.Grammar) throws -> [SwiftPEGGrammar.TokenDefinition] {
        guard let tokensMeta = metaPropertyManager.propertiesValidating(knownProperty: self.tokensFileProp).first else {
            return []
        }
        guard let tokensFileName = tokensMeta.value.stringValue else {
            return []
        }
        
        guard let fileContents = try delegate?.grammarProcessor(self, loadTokensFileNamed: tokensFileName, ofGrammar: grammar) else {
            throw recordAndReturn(GrammarProcessorError.failedToLoadTokensFile(tokensMeta.node))
        }

        let parser = GrammarParser(raw: GrammarRawTokenizer(source: fileContents))
        
        do {
            guard let tokens = try parser.tokensFile(), parser.tokenizer.isEOF else {
                throw recordAndReturn(GrammarProcessorError.tokensFileSyntaxError(tokensMeta.node, parser.makeSyntaxError()))
            }
            
            return tokens
        } catch let error as ParserError {
            throw recordAndReturn(GrammarProcessorError.tokensFileSyntaxError(tokensMeta.node, error))
        } catch {
            throw error
        }
    }

    func validateTokenReferences(
        in grammar: SwiftPEGGrammar.Grammar,
        tokensFile: [SwiftPEGGrammar.TokenDefinition]
    ) throws -> Set<String> {
        var metaTokens: Set<String> = []

        for token in tokenMetaProperties(in: grammar) {
            guard let value = token.value as? SwiftPEGGrammar.MetaIdentifierValue else {
                continue
            }

            let name = String(value.identifier.string)
            metaTokens.insert(name)
        }

        var tokensFromFile: Set<String> = []
        for token in tokensFile {
            tokensFromFile.insert(String(token.name.string))
        }

        return Set(metaTokens).union(tokensFromFile)
    }

    func validateReferences(
        in grammar: SwiftPEGGrammar.Grammar,
        tokens: Set<String>
    ) throws {
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

        var firstError: Error?
        for ref in visitor.unknownReferences {
            guard let rule: SwiftPEGGrammar.Rule = ref.firstAncestor() else {
                fatalError("Found atom \(ref.name) @ \(ref.location) that has no Rule parent?")
            }

            let error = recordAndReturn(
                GrammarProcessorError.unknownReference(
                    ref, rule, tokensFileName: grammar.tokensFile()
                )
            )

            firstError = firstError ?? error
        }

        throw firstError!
    }

    func validateNamedItems(in grammar: SwiftPEGGrammar.Grammar) throws {
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

        record(visitor.errors)

        if let first = visitor.errors.first {
            throw first
        }
    }

    /// Diagnoses unreachable rules, when starting from a given entry rule name.
    func diagnoseUnreachableRules(
        in grammar: SwiftPEGGrammar.Grammar,
        _ knownRules: [String: SwiftPEGGrammar.Rule],
        entryRuleName: String
    ) throws {

        let unreachableRules = try computeUnreachableRules(in: grammar, startRuleName: entryRuleName, rules: knownRules)

        for unreachable in unreachableRules.sorted() { // Sort results to ensure stable and predictable diagnostics issuing
            guard let rule = knownRules[unreachable] else {
                continue
            }

            rule.isReachable = false

            diagnostics.append(
                .unreachableRule(rule, startRuleName: entryRuleName)
            )
        }
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
        /// An alt that executes before another, if it succeeds, will always
        /// prevent the other alt from being attempted.
        case altOrderIssue(rule: SwiftPEGGrammar.Rule, SwiftPEGGrammar.Alt, alwaysSucceedsBefore: SwiftPEGGrammar.Alt)

        /// A rule is not reachable from the set start rule.
        case unreachableRule(SwiftPEGGrammar.Rule, startRuleName: String)

        /// Diagnostic reported by meta-property manager.
        case metaPropertyDiagnostic(SwiftPEGGrammar.Meta, String)

        public var description: String {
            switch self {
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
                    in rule \(rule.name.name.processedString) @ \(rule.location).
                    """
            
            case .unreachableRule(let rule, let startRuleName):
                return "Rule '\(rule.name.name.processedString)' @ \(rule.location) is not reachable from the set start rule '\(startRuleName)'."
            
            case .metaPropertyDiagnostic(_, let message):
                return message
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
}

// MARK: Helper extensions

extension SwiftPEGGrammar.Grammar {
    func tokensFile() -> String? {
        guard let tokensMeta = meta(named: GrammarProcessor.tokensFile) else {
            return nil
        }
        guard let stringValue = (tokensMeta.value as? SwiftPEGGrammar.MetaStringValue) else {
            return nil
        }

        return String(stringValue.string.processedString)
    }

    func hasMeta(named name: String) -> Bool {
        self.meta(named: name) != nil
    }

    func meta(named name: String) -> SwiftPEGGrammar.Meta? {
        metas.first(where: { $0.name.string == name }) 
    }
}