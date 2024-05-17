import SwiftSyntax
import SwiftSyntaxMacros

/// Macro used to memoize parser method invocations.
public struct ParserMemoizeMacro: PeerMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingPeersOf declaration: some DeclSyntaxProtocol,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        
        guard let declaration = declaration.as(FunctionDeclSyntax.self) else {
            throw Error.message("Expected macro to be attached to a function declaration.")
        }

        // Nothing to memoize?
        guard let returnType = declaration.signature.returnClause?.type.trimmed, returnType != "Void" && returnType != "()" else {
            throw Error.message("Cannot memoize Void method")
        }

        // Fetch macro argument
        let arguments = try parseArguments(node)

        let nonMemoizedMethod: TokenSyntax = declaration.name
        let memoizedMethod: TokenSyntax = "\(raw: arguments.memoizedName)"

        guard nonMemoizedMethod.description != memoizedMethod.description else {
            throw Error.message("Memoized method cannot have the same name as non-memoized \(nonMemoizedMethod)")
        }

        let cache = arguments.cache

        let leadingTrivia = docComments(for: declaration)
        let typeToCache = returnType.trimmed
        let parameters = parameters(in: declaration.signature.parameterClause)
        let cacheParams: ExprSyntax = "[\(raw: parameters.map({ "AnyHashable(\($0.name.description))" }).joined(separator: ", "))]"
        let nonMemoArguments = parameters.map({ $0.label != nil ? "\($0.label!): \($0.name)" : "\($0.name)" }).joined(separator: ", ")

        return [
            """
            \(leadingTrivia)
            open func \(memoizedMethod)\(declaration.signature.parameterClause.trimmed) throws -> \(typeToCache) {
                let params: [AnyHashable] = \(cacheParams)
                let key = makeKey("\(memoizedMethod)", parameters: params)
                if let cached: CacheEntry<\(typeToCache)> = \(cache).fetch(key) {
                    self.restore(cached.mark)
                    return cached.result
                }
                let result = try \(nonMemoizedMethod)(\(raw: nonMemoArguments))
                cache.store(
                    key,
                    value: CacheEntry(mark: self.mark(), result: result)
                )

                return result
            }
            """
        ]
    }

    static func docComments(for decl: DeclSyntaxProtocol) -> Trivia {
        func isDocComment(_ piece: TriviaPiece) -> Bool {
            switch piece {
            case .docBlockComment(let string), .docLineComment(let string):
                return !string.isEmpty
            default:
                return false
            }
        }

        guard let startIndex = decl.leadingTrivia.firstIndex(where: isDocComment) else {
            return Trivia()
        }
        guard let endIndex = Array(decl.leadingTrivia).lastIndex(where: isDocComment) else {
            return Trivia()
        }

        return Trivia(pieces: decl.leadingTrivia[startIndex...endIndex])
    }

    static func parameters(in decl: FunctionParameterClauseSyntax) -> [(label: TokenSyntax?, name: TokenSyntax)] {
        var result: [(label: TokenSyntax?, name: TokenSyntax)] = []

        for parameter in decl.parameters {
            if let second = parameter.secondName {
                if parameter.firstName.tokenKind == .wildcard {
                    result.append((label: nil, name: second.trimmed))
                } else {
                    result.append((label: parameter.firstName.trimmed, name: second.trimmed))
                }
            } else {
                result.append((label: parameter.firstName.trimmed, name: parameter.firstName.trimmed))
            }
        }

        return result
    }

    static func parseArguments(_ node: AttributeSyntax) throws -> MacroArguments {
        guard let arguments = _argumentsForMacro(node), !arguments.isEmpty else {
            throw Error.message("Macro expects at least one argument")
        }

        if arguments.count > 2 {
            throw Error.message("Unexpected arguments: \(arguments[2...])")
        }

        guard let memoizedName = arguments[0].value.as(StringLiteralExprSyntax.self) else {
            throw Error.message("Expected first argument to be name of memoized method to generate")
        }

        var cache: ExprSyntax = "self.cache"
        if arguments.count >= 2 {
            cache = arguments[1].value
        }

        return MacroArguments(
            arguments: arguments,
            memoizedName: memoizedName.segments.description,
            cache: cache
        )
    }

    static func _argumentsForMacro(_ node: AttributeSyntax) -> [MacroArgument]? {
        guard let arguments = node.arguments else { return [] }

        var result: [MacroArgument] = []

        if let arguments = arguments.as(LabeledExprListSyntax.self) {
            for element in arguments {
                if let label = element.label {
                    result.append(.labeled(label, element.expression))
                } else {
                    result.append(.unlabeled(element.expression))
                }
            }
        } else {
            // Missing macro arguments?
            return nil
        }

        return result
    }

    struct MacroArguments {
        var arguments: [MacroArgument]

        var memoizedName: String
        var cache: ExprSyntax
    }

    enum MacroArgument {
        case unlabeled(ExprSyntax)
        case labeled(TokenSyntax, ExprSyntax)

        var value: ExprSyntax {
            switch self {
            case .unlabeled(let value), .labeled(_, let value):
                return value
            }
        }
    }

    enum Error: Swift.Error, CustomStringConvertible {
        case message(String)
        
        var description: String {
            switch self {
            case .message(let message):
                return message
            }
        }
    }
}
