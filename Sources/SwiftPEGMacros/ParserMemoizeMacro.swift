import SwiftSyntax
import SwiftSyntaxMacros

/// Macro used to memoize parser method invocations.
public struct ParserMemoizeMacro: PeerMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {

        do {
            let impl = ParserMemoizeMacroImplementation(
                of: node,
                providingPeersOf: declaration,
                in: context
            )
            return try impl.expand()
        } catch MacroError.diagnostic(let diag) {
            context.diagnose(diag)
            return []
        } catch {
            throw error
        }
    }
}

class ParserMemoizeMacroImplementation {
    let node: AttributeSyntax
    let declaration: any DeclSyntaxProtocol
    let context: any MacroExpansionContext

    init(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) {
        self.node = node
        self.declaration = declaration
        self.context = context
    }

    func expand() throws -> [DeclSyntax] {
        // Validate declaration
        guard let declaration = declaration.as(FunctionDeclSyntax.self) else {
            throw MacroError.message("Only functions can be memoized with this macro")
        }
        guard
            let returnType = declaration.signature.returnClause?.type.trimmed,
            returnType.description != "Void" && returnType.description != "()"
        else {
            throw MacroError.message("Cannot memoize Void method")
        }

        // Fetch macro argument
        let macroArguments = try parseArguments()

        guard !macroArguments.memoizedName.isEmpty else {
            throw MacroError.diagnostic(
                macroArguments
                    .memoizedName_diagnosticSyntax
                    .ext_errorDiagnostic(message: "Memoized method name cannot be empty")
            )
        }

        let nonMemoizedMethod: TokenSyntax = declaration.name.trimmed
        let memoizedMethod: DeclReferenceExprSyntax = DeclReferenceExprSyntax(
            baseName: .identifier(macroArguments.memoizedName)
        )

        guard nonMemoizedMethod.description != memoizedMethod.description else {
            throw MacroError.diagnostic(
                macroArguments
                    .memoizedName_diagnosticSyntax
                    .ext_errorDiagnostic(message: "Memoized method cannot have the same name as non-memoized \(nonMemoizedMethod)")
            )
        }

        // Collect attributes to apply, as well
        let attributes = declaration.attributes.filter({
            $0.position != node.position
        }).map { $0.trimmed.addingLeadingTrivia(.newline).description }.joined()

        // For nodes with no explicit access control, inherit the declaration's
        let accessLevel = declaration
            .ext_accessLevel()?
            .name.trimmed
            .with(\.trailingTrivia, .spaces(1))

        let genericParams = declaration.genericParameterClause?.trimmed
        let genericWhere = declaration.genericWhereClause?.trimmed.withLeadingSpace()
        let leadingTrivia = declaration.ext_docComments()
        let typeToCache = returnType.trimmed
        let effects = declaration.signature.effectSpecifiers
        let parameters = declaration.signature.parameterClause.trimmed
        let arguments = declaration.signature.parameterClause.ext_parameters()
        let cache = macroArguments.cache
        let cacheParams: ExprSyntax =
            arguments.isEmpty
                ? "nil"
                : "[\(raw: arguments.map({ "AnyHashable(\($0.name.description))" }).joined(separator: ", "))]"
        let nonMemoArguments = arguments.map({ $0.label != nil ? "\($0.label!): \($0.name)" : "\($0.name)" }).joined(separator: ", ")
        var invocation: ExprSyntax = "\(nonMemoizedMethod)(\(raw: nonMemoArguments))"
        if effects?.asyncSpecifier != nil {
            invocation = "await \(invocation)"
        }
        if effects?.throwsSpecifier != nil {
            invocation = "try \(invocation)"
        }

        let cacheMiss = self.expandCacheMiss(
            cache: cache,
            invocation: invocation,
            typeToCache: typeToCache
        )

        _=CodeBlockItemListSyntax.init()

        // TODO: Reduce duplication when producing metadata and diagnostics constructs
        if !macroArguments.debugDiagnostics {
            return [
                """
                \(leadingTrivia)
                /// Memoized version of `\(nonMemoizedMethod)`.\(raw: attributes)
                \(accessLevel)func \(memoizedMethod)\(genericParams)\(parameters) \(effects)-> \(typeToCache)\(genericWhere) {
                    let key = makeKey("\(memoizedMethod)", arguments: \(cacheParams))
                    if let cached: CacheEntry<\(typeToCache)> = \(cache)[key] {
                        self.restore(cached.mark)
                        return cached.result
                    }
                    \(cacheMiss)

                    return result
                }
                """
            ]
        } else {
            return [
                """
                \(leadingTrivia)
                /// Memoized version of `\(nonMemoizedMethod)`.
                \(raw: attributes)
                \(accessLevel)func \(memoizedMethod)\(parameters) \(effects)-> \(typeToCache) {
                    let metadataKey = "\(memoizedMethod)(\(raw: arguments.map({ #"\#($0.name.description): \(\#($0.name.description))"# }).joined(separator: ", ")))"
                    \(cache).incrementMetadata(metadataKey)

                    let key = makeKey("\(memoizedMethod)", arguments: \(cacheParams))
                    if let cached: CacheEntry<\(typeToCache)> = \(cache).fetch(key) {
                        self.restore(cached.mark)
                        \(cache).incrementMetadata("cacheHit")
                        return cached.result
                    }
                    \(cache).incrementMetadata("cacheMiss")
                    \(cacheMiss)

                    return result
                }
                """
            ]
        }
    }

    func expandCacheMiss(
        cache: ExprSyntax,
        invocation: ExprSyntax,
        typeToCache: TypeSyntax
    ) -> CodeBlockItemListSyntax {
        return """
        let result = \(invocation)
        let mark = self.mark()
        let priorReach = self.resetReach(mark)
        \(cache)[key] = CacheEntry(mark: mark, reach: self.reach, result: result)
        let reach = self.resetReach(priorReach)
        self.updateReach(reach)
        """
    }

    func parseArguments() throws -> MacroArguments {
        guard let arguments = node.ext_arguments(), !arguments.isEmpty else {
            throw MacroError.message("Macro expects at least one argument")
        }

        if arguments.count > 2 {
            throw MacroError.diagnostic(
                arguments[2].diagnosticSyntax.ext_errorDiagnostic(
                    message: "Unexpected arguments",
                    highlights: arguments.dropFirst(2).map(\.diagnosticSyntax)
                )
            )
        }

        guard let memoizedName = arguments[0].value.as(StringLiteralExprSyntax.self) else {
            throw MacroError.message("Expected first argument to be name of memoized method to generate")
        }

        var cache: ExprSyntax = "self.cache"
        if arguments.count >= 2 {
            cache = arguments[1].value

            if let cacheStr = cache.as(StringLiteralExprSyntax.self) {
                let resolved = cacheStr.segments.description
                guard !resolved.isEmpty else {
                    throw MacroError.diagnostic(
                        arguments[1].diagnosticSyntax.ext_errorDiagnostic(
                            message: "Memoization cache name cannot be empty"
                        )
                    )
                }

                cache = "\(raw: resolved)"
            }
        }

        return MacroArguments(
            arguments: arguments,
            memoizedName: memoizedName.segments.description,
            cache: cache
        )
    }

    struct MacroArguments {
        var arguments: [MacroArgument]

        var memoizedName: String
        var cache: ExprSyntax
        var debugDiagnostics: Bool = false

        var memoizedName_diagnosticSyntax: Syntax {
            arguments[0].diagnosticSyntax
        }
        var cache_diagnosticSyntax: Syntax {
            Syntax(cache)
        }
    }
}
