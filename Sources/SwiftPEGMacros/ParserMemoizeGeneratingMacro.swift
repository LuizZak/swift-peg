import SwiftSyntax
import SwiftSyntaxMacros

/// Macro used to memoize parser method invocations.
public struct ParserMemoizeGeneratingMacro: DeclarationMacro {
    public static func expansion(
        of node: some SwiftSyntax.FreestandingMacroExpansionSyntax,
        in context: some SwiftSyntaxMacros.MacroExpansionContext
    ) throws -> [SwiftSyntax.DeclSyntax] {
        
        let arguments = try parseArguments(node.arguments)
        let body = arguments.closure.statements

        return [
            """
            func \(raw: arguments.name)(\(raw: arguments.parametersSyntax))\(raw: arguments.returnSyntax){
                \(body.trimmed)
            }
            """
        ]
    }

    static func parseArguments(_ node: LabeledExprListSyntax) throws -> MacroArguments {
        let arguments = node.ext_arguments()
        guard !arguments.isEmpty else {
            throw MacroError.message("Macro expects at least one argument")
        }

        if arguments.count != 2 {
            throw MacroError.message("Expected two arguments for macro declaration")
        }

        guard let name = arguments[0].value.as(StringLiteralExprSyntax.self) else {
            throw MacroError.message("Expected first argument to be name of memoized method to generate")
        }
        guard let closure = arguments[1].value.as(ClosureExprSyntax.self) else {
            throw MacroError.message("Expected second argument to be a closure defining the body of the method")
        }
        guard let signature = closure.signature else {
            throw MacroError.message("Expected closure to have a signature")
        }
        guard let parameterClause = signature.parameterClause else {
            throw MacroError.message("Expected closure to have a parameter clause")
        }
        guard case .parameterClause(let parameters) = parameterClause else {
            throw MacroError.message("Expected closure to have a full parameter clause")
        }

        var params: [MacroArguments.Parameter] = []
        for (i, parameter) in parameters.parameters.enumerated() {
            guard let type = parameter.type else {
                throw MacroError.message("Expected closure parameter #\(i) to have a type")
            }

            let name = parameter.secondName ?? parameter.firstName
            let label = parameter.secondName == nil ? nil : parameter.secondName

            params.append((label: label?.trimmed, name: name.trimmed, type: type.trimmed))
        }

        return MacroArguments(
            name: name.segments.description,
            closure: closure,
            parameters: params,
            isThrowing: signature.effectSpecifiers?.throwsSpecifier != nil,
            returnType: signature.returnClause?.type.trimmed
        )
    }

    struct MacroArguments {
        typealias Parameter = (label: TokenSyntax?, name: TokenSyntax, type: TypeSyntax)

        var name: String
        var closure: ClosureExprSyntax
        var parameters: [Parameter]
        var isThrowing: Bool
        var returnType: TypeSyntax?

        var parametersSyntax: String {
            parameters.map { param in
                "\(param.label.map({ "\($0) " }) ?? "")\(param.name): \(param.type)"
            }.joined(separator: ", ")
        }

        var returnSyntax: String {
            let effects = isThrowing ? "throws " : ""
            if let returnType {
                return "\(effects)-> \(returnType.description) "
            } else {
                return effects
            }
        }
    }
}
