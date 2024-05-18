import SwiftSyntax

extension Collection where Element == FunctionParameterSyntax {
    /// Appends commas separating the elements of this collection of function
    /// parameter syntaxes.
    func ext_commaSeparated(comma: TokenSyntax) -> [FunctionParameterSyntax] {
        let total = self.count

        return self.lazy.enumerated().map { (i, param) in
            if i == total - 1 {
                return param
            } else {
                return param.with(\.trailingComma, comma)
            }
        }
    }
}
