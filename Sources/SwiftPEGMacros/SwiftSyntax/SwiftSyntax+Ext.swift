import SwiftSyntax
import SwiftSyntaxMacroExpansion
import SwiftDiagnostics

extension FunctionParameterClauseSyntax {
    func ext_parameters() -> [(label: TokenSyntax?, name: TokenSyntax)] {
        var result: [(label: TokenSyntax?, name: TokenSyntax)] = []

        for parameter in parameters {
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
}

extension LabeledExprListSyntax {
    func ext_arguments() -> [(label: TokenSyntax?, value: ExprSyntax)] {
        var result: [(label: TokenSyntax?, ExprSyntax)] = []
        
        for expr in self {
            result.append((label: expr.label, expr.expression))
        }

        return result
    }
}

extension SyntaxProtocol {
    /// Helper for generating targeted macro diagnostic error messages from a
    /// specific syntax node from source code.
    func ext_errorDiagnostic(
        message: String
    ) -> Diagnostic {
        return Diagnostic(
            node: self,
            message: MacroExpansionErrorMessage(message)
        )
    }

    /// Helper for generating targeted macro diagnostic error messages from a
    /// specific syntax node from source code.
    func ext_errorDiagnostic<S: SyntaxProtocol>(
        message: String,
        highlights: (any Sequence<S>)?
    ) -> Diagnostic {
        let highlights = highlights?.map(Syntax.init(_:))

        return Diagnostic(
            node: self,
            message: MacroExpansionErrorMessage(message),
            highlights: (highlights == nil || highlights?.count == 0) ? nil : highlights
        )
    }
    
    /// Helper for generating targeted macro diagnostic warning messages from a
    /// specific syntax node from source code.
    func ext_warningDiagnostic(
        message: String
    ) -> Diagnostic {
        return Diagnostic(
            node: self,
            message: MacroExpansionWarningMessage(message)
        )
    }

    /// Extracts all leading trivia on this syntax that is of doc comment
    /// line/block type.
    /// The trivia only includes the comments themselves, with no leading/trailing
    /// whitespace, except for whitespace between the comments, if more than one
    /// comment exists.
    func ext_docComments() -> Trivia {
        func isDocComment(_ piece: TriviaPiece) -> Bool {
            switch piece {
            case .docBlockComment(let string), .docLineComment(let string):
                return !string.isEmpty
            default:
                return false
            }
        }

        guard let startIndex = leadingTrivia.firstIndex(where: isDocComment) else {
            return Trivia()
        }
        guard let endIndex = Array(leadingTrivia).lastIndex(where: isDocComment) else {
            return Trivia()
        }

        return Trivia(pieces: leadingTrivia[startIndex...endIndex])
    }
}

extension SyntaxProtocol {
    /// Convenience for `Syntax(self)`.
    var asSyntax: Syntax {
        return Syntax(self)
    }

    /// Sets the leading trivia to be a number of spaces of the given count.
    func withLeadingSpace(count: Int = 1) -> Self {
        with(\.leadingTrivia, .spaces(count))
    }

    /// Sets the trailing trivia to be a number of spaces of the given count.
    func withTrailingSpace(count: Int = 1) -> Self {
        with(\.trailingTrivia, .spaces(count))
    }
    
    /// Adds to the leading trivia a number of spaces of the given count.
    func addingLeadingSpace(count: Int = 1) -> Self {
        addingLeadingTrivia(.spaces(count))
    }
    
    /// Adds to the trailing trivia a number of spaces of the given count.
    func addingTrailingSpace(count: Int = 1) -> Self {
        addingTrailingTrivia(.spaces(count))
    }
    
    /// Adds to the leading trivia a given `Trivia`.
    /// Trivia is added at the end of the current trivia value.
    func addingLeadingTrivia(_ trivia: Trivia) -> Self {
        with(\.leadingTrivia, leadingTrivia + trivia)
    }
    
    /// Adds to the trailing trivia a given `Trivia`.
    /// Trivia is added at the end of the current trivia value.
    func addingTrailingTrivia(_ trivia: Trivia) -> Self {
        with(\.trailingTrivia, trailingTrivia + trivia)
    }
    
    /// Adds one leading/trailing spaces to the current trivia.
    func addingSurroundingSpaces() -> Self {
        addingLeadingSpace().addingTrailingSpace()
    }
    
    /// Replaces the trivia with a single newline.
    func onNewline() -> Self {
        with(\.leadingTrivia, .newlines(1))
    }
}

extension TypeSyntaxProtocol {
    var asTypeSyntax: TypeSyntax {
        return TypeSyntax(self)
    }
}

extension DeclSyntaxProtocol {
    var asDeclSyntax: DeclSyntax {
        return DeclSyntax(self)
    }
}

extension ExprSyntaxProtocol {
    var asExprSyntax: ExprSyntax {
        return ExprSyntax(self)
    }
}

extension StmtSyntaxProtocol {
    var asStmtSyntax: StmtSyntax {
        return StmtSyntax(self)
    }
}

extension PatternSyntaxProtocol {
    var asPatternSyntax: PatternSyntax {
        return PatternSyntax(self)
    }
}

extension ExprSyntaxProtocol {
    func inCodeBlock() -> CodeBlockItemSyntax {
        .init(item: inCodeBlockItem())
    }

    func inCodeBlockItem() -> CodeBlockItemSyntax.Item {
        .expr(self.asExprSyntax)
    }
}

extension StmtSyntaxProtocol {
    func inCodeBlock() -> CodeBlockItemSyntax {
        .init(item: inCodeBlockItem())
    }

    func inCodeBlockItem() -> CodeBlockItemSyntax.Item {
        .stmt(self.asStmtSyntax)
    }
}

extension DeclSyntaxProtocol {
    func inCodeBlock() -> CodeBlockItemSyntax {
        .init(item: inCodeBlockItem())
    }

    func inCodeBlockItem() -> CodeBlockItemSyntax.Item {
        .decl(self.asDeclSyntax)
    }
}
