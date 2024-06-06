import SwiftPEG

extension CommonAbstract.TokenSyntax: ExpressibleByStringLiteral {
    public init(stringLiteral value: StringLiteralType) {
        self.init(alts: [
            .init(items: [
                .atom(
                    .init(excluded: [], terminal: .literal(.fromCode(contents: value)))
                )
            ], trailExclusions: [])
        ])
    }
}

extension CommonAbstract.TokenAtom: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(excluded: [], terminal: .literal(.fromCode(contents: value)))
    }
}

func ... (lhs: String, rhs: String) -> CommonAbstract.TokenTerminal {
    .rangeLiteral(.fromCode(contents: lhs), .fromCode(contents: rhs))
}

func ... (lhs: String, rhs: String) -> CommonAbstract.TokenAtom {
    .init(terminal: lhs...rhs)
}
