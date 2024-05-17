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
            return try _expansion(
                of: node,
                providingPeersOf: declaration,
                in: context
            )
        } catch MacroError.diagnostic(let diag) {
            context.diagnose(diag)
            return []
        } catch {
            throw error
        }
    }

    static func _expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        // Validate declaration
        guard let declaration = declaration.as(FunctionDeclSyntax.self) else {
            throw MacroError.message("Only functions can be memoized with this macro")
        }
        guard declaration.signature.effectSpecifiers?.asyncSpecifier == nil else {
            throw MacroError.message("Memoizing asynchronous functions is not currently supported")
        }
        guard
            let returnType = declaration.signature.returnClause?.type.trimmed,
            returnType.description != "Void" && returnType.description != "()"
        else {
            throw MacroError.message("Cannot memoize Void method")
        }

        // Fetch macro argument
        let macroArguments = try parseArguments(node, in: context)

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

        let cache = macroArguments.cache

        let leadingTrivia = declaration.ext_docComments()
        let typeToCache = returnType.trimmed
        let effects = declaration.signature.effectSpecifiers
        let parameters = declaration.signature.parameterClause.trimmed
        let arguments = declaration.signature.parameterClause.ext_parameters()
        let cacheParams: ExprSyntax = "[\(raw: arguments.map({ "AnyHashable(\($0.name.description))" }).joined(separator: ", "))]"
        let nonMemoArguments = arguments.map({ $0.label != nil ? "\($0.label!): \($0.name)" : "\($0.name)" }).joined(separator: ", ")
        var invocation: ExprSyntax = "\(nonMemoizedMethod)(\(raw: nonMemoArguments))"
        if effects?.throwsSpecifier != nil {
            invocation = "try \(invocation)"
        }

        return [
            """
            \(leadingTrivia)
            /// Memoized version of `\(nonMemoizedMethod)`.
            open func \(memoizedMethod)\(parameters) \(effects)-> \(typeToCache) {
                let args: [AnyHashable] = \(cacheParams)
                let key = makeKey("\(memoizedMethod)", arguments: args)
                if let cached: CacheEntry<\(typeToCache)> = \(cache).fetch(key) {
                    self.restore(cached.mark)
                    return cached.result
                }
                let result = \(invocation)
                \(cache).store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """
        ]
    }

    static func parseArguments(
        _ node: AttributeSyntax,
        in context: some MacroExpansionContext
    ) throws -> MacroArguments {

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

        var memoizedName_diagnosticSyntax: Syntax {
            arguments[0].diagnosticSyntax
        }
        var cache_diagnosticSyntax: Syntax {
            Syntax(cache)
        }
    }
}
