import SwiftSyntax

enum MacroArgument: CustomStringConvertible {
    case unlabeled(Syntax, ExprSyntax)
    case labeled(Syntax, TokenSyntax, ExprSyntax)

    var value: ExprSyntax {
        switch self {
        case .unlabeled(_, let value), .labeled(_, _, let value):
            return value
        }
    }

    var description: String {
        switch self {
        case .unlabeled(let syntax, _), .labeled(let syntax, _, _):
            return syntax.description
        }
    }

    /// The syntax item that contains this argument's construction.
    /// Used during diagnostics to highlight correct sections of the code.
    var diagnosticSyntax: Syntax {
        switch self {
        case .labeled(let syntax, _, _), .unlabeled(let syntax, _):
            return syntax
        }
    }
}

extension AttributeSyntax {
    func ext_arguments() -> [MacroArgument]? {
        guard let arguments = arguments else { return [] }

        var result: [MacroArgument] = []

        if let arguments = arguments.as(LabeledExprListSyntax.self) {
            for element in arguments {
                let syntax = Syntax(element)

                if let label = element.label {
                    result.append(.labeled(syntax, label, element.expression))
                } else {
                    result.append(.unlabeled(syntax, element.expression))
                }
            }
        } else {
            // Missing macro arguments?
            return nil
        }

        return result
    }
}
