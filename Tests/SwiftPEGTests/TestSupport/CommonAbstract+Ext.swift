import SwiftPEG

extension CommonAbstract.TokenSyntax: ExpressibleByStringLiteral {
    public init(stringLiteral value: StringLiteralType) {
        self.init(alts: [
            .init(atoms: [
                .terminal(.literal(value))
            ])
        ])
    }
}
