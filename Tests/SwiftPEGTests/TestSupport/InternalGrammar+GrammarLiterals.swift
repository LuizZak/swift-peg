@testable import SwiftPEG

// MARK: Literal initialization

extension InternalGrammar.Alt: ExpressibleByArrayLiteral {
    public init(arrayLiteral elements: InternalGrammar.NamedItem...) {
        self.init(items: elements)
    }
}

extension InternalGrammar.NamedItem: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self = .item(.init(stringLiteral: value))
    }
}

extension InternalGrammar.Item: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self = .atom(.init(stringLiteral: value))
    }
}

extension InternalGrammar.Atom: ExpressibleByStringLiteral {
    var asString: Self {
        switch self {
        case .token(let tok): return .string(#""\#(tok)""#, trimmed: tok)
        case .ruleName(let name): return .string(#""\#(name)""#, trimmed: name)
        case .group, .string: return self
        }
    }

    public init(stringLiteral value: String) {
        if value.hasPrefix("'") && value.hasSuffix("'") && value.count > 2 {
            self = .string(value, trimmed: String(value.dropFirst().dropLast()))
        } else if value.allSatisfy({ $0.isUppercase }) {
            self = .token(value)
        } else {
            self = .ruleName(value)
        }
    }
}

extension InternalGrammar.Action: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(string: value)
    }
}

extension InternalGrammar.SwiftType: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(name: value)
    }
}

// MARK: Operator support

extension InternalGrammar.Alt {
    /// Constructs `alts: alt | alt`
    static func | (lhs: Self, rhs: Self) -> [Self] {
        return [lhs, rhs]
    }

    /// Constructs `alts: alts | alt`
    static func | (lhs: [Self], rhs: Self) -> [Self] {
        return lhs + [rhs]
    }
}

extension InternalGrammar.NamedItem {
    /// Constructs `namedItems: namedItem+`
    static func .. (lhs: Self, rhs: Self) -> [Self] {
        return [lhs, rhs]
    }

    /// Constructs `namedItems: namedItems namedItem`
    static func .. (lhs: [Self], rhs: Self) -> [Self] {
        return lhs + [rhs]
    }
}

extension InternalGrammar.Atom {
    /// Constructs `self?`
    var opt: InternalGrammar.Item {
        return .optional(self)
    }

    /// Constructs `value+`
    static postfix func + (_ value: Self) -> InternalGrammar.Item {
        .oneOrMore(value)
    }

    /// Constructs `value*`
    static postfix func * (_ value: Self) -> InternalGrammar.Item {
        .oneOrMore(value)
    }
}

// MARK: - SupportsWith

extension InternalGrammar.Grammar: SupportsWith { }
extension InternalGrammar.MetaProperty: SupportsWith { }
extension InternalGrammar.Rule: SupportsWith { }
extension InternalGrammar.Alt: SupportsWith { }
extension InternalGrammar.Action: SupportsWith { }
extension InternalGrammar.NamedItem: SupportsWith { }
extension InternalGrammar.Item: SupportsWith { }
extension InternalGrammar.Lookahead: SupportsWith { }
extension InternalGrammar.Atom: SupportsWith { }
extension InternalGrammar.SwiftType: SupportsWith { }

protocol SupportsWith {
    func with<V>(_ keypath: WritableKeyPath<Self, V>, value: V) -> Self
}

extension SupportsWith {
    func with<V>(_ keypath: WritableKeyPath<Self, V>, value: V) -> Self {
        var copy = self
        copy[keyPath: keypath] = value
        return copy
    }
}

// MARK: - Operator definitions

postfix operator +
postfix operator *
infix operator .. : MultiplicationPrecedence
