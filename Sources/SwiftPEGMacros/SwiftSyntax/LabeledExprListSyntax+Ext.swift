import SwiftSyntax

extension Collection where Element == LabeledExprSyntax {
    /// Appends commas separating the elements of this collection of labeled
    /// expression syntaxes.
    func ext_commaSeparated(comma: TokenSyntax) -> [LabeledExprSyntax] {
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
