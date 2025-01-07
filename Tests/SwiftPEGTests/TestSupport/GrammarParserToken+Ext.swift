import SwiftPEG

extension GrammarParserToken: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        var stream = StringStream(source: value)
        self = .from(stream: &stream)!
    }
}

// Static constructors

extension GrammarParserToken {
    static func comment(_ value: some StringProtocol) -> Self {
        .init(kind: .comment, string: Substring(value))
    }

    static func whitespace(_ value: some StringProtocol) -> Self {
        .init(kind: .whitespace, string: Substring(value))
    }

    static func string(_ value: some StringProtocol) -> Self {
        .init(kind: .string, string: Substring(value))
    }

    static func identifier(_ value: some StringProtocol) -> Self {
        .init(kind: .identifier, string: Substring(value))
    }

    static func digits(_ value: some StringProtocol) -> Self {
        .init(kind: .digits, string: Substring(value))
    }

    static var leftParen: Self {
        .init(kind: .leftParen, string: Substring(Self.TokenKind.leftParen.description))
    }

    static var rightParen: Self {
        .init(kind: .rightParen, string: Substring(Self.TokenKind.rightParen.description))
    }

    static var leftBrace: Self {
        .init(kind: .leftBrace, string: Substring(Self.TokenKind.leftBrace.description))
    }

    static var rightBrace: Self {
        .init(kind: .rightBrace, string: Substring(Self.TokenKind.rightBrace.description))
    }

    static var leftSquare: Self {
        .init(kind: .leftSquare, string: Substring(Self.TokenKind.leftSquare.description))
    }

    static var rightSquare: Self {
        .init(kind: .rightSquare, string: Substring(Self.TokenKind.rightSquare.description))
    }

    static var leftAngle: Self {
        .init(kind: .leftAngle, string: Substring(Self.TokenKind.leftAngle.description))
    }

    static var rightAngle: Self {
        .init(kind: .rightAngle, string: Substring(Self.TokenKind.rightAngle.description))
    }

    static var colon: Self {
        .init(kind: .colon, string: Substring(Self.TokenKind.colon.description))
    }

    static var semicolon: Self {
        .init(kind: .semicolon, string: Substring(Self.TokenKind.semicolon.description))
    }

    static var bar: Self {
        .init(kind: .bar, string: Substring(Self.TokenKind.bar.description))
    }

    static var equals: Self {
        .init(kind: .equals, string: Substring(Self.TokenKind.equals.description))
    }

    static var tilde: Self {
        .init(kind: .tilde, string: Substring(Self.TokenKind.tilde.description))
    }

    static var star: Self {
        .init(kind: .star, string: Substring(Self.TokenKind.star.description))
    }

    static var plus: Self {
        .init(kind: .plus, string: Substring(Self.TokenKind.plus.description))
    }

    static var minus: Self {
        .init(kind: .minus, string: Substring(Self.TokenKind.minus.description))
    }

    static var questionMark: Self {
        .init(kind: .questionMark, string: Substring(Self.TokenKind.questionMark.description))
    }

    static var exclamationMark: Self {
        .init(kind: .exclamationMark, string: Substring(Self.TokenKind.exclamationMark.description))
    }

    static var doubleExclamationMark: Self {
        .init(kind: .doubleExclamationMark, string: Substring(Self.TokenKind.doubleExclamationMark.description))
    }

    static var ampersand: Self {
        .init(kind: .ampersand, string: Substring(Self.TokenKind.ampersand.description))
    }

    static var comma: Self {
        .init(kind: .comma, string: Substring(Self.TokenKind.comma.description))
    }

    static var period: Self {
        .init(kind: .period, string: Substring(Self.TokenKind.period.description))
    }

    static var ellipsis: Self {
        .init(kind: .ellipsis, string: Substring(Self.TokenKind.ellipsis.description))
    }

    static var backtick: Self {
        .init(kind: .backtick, string: Substring(Self.TokenKind.backtick.description))
    }

    static var at: Self {
        .init(kind: .at, string: Substring(Self.TokenKind.at.description))
    }

    static var dollarSign: Self {
        .init(kind: .dollarSign, string: Substring(Self.TokenKind.dollarSign.description))
    }

    static var forwardSlash: Self {
        .init(kind: .forwardSlash, string: "")
    }

    static var backslash: Self {
        .init(kind: .backslash, string: "")
    }
}
