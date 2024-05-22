@testable import SwiftPEG

// MARK: Literal initialization

extension GrammarProcessor.Alt: ExpressibleByArrayLiteral {
    public init(arrayLiteral elements: GrammarProcessor.NamedItem...) {
        self.init(items: elements)
    }
}

extension GrammarProcessor.NamedItem: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        if value.allSatisfy({ $0.isUppercase }) {
            self = .item(name: nil, .atom(.token(value)), type: nil)
        } else {
            self = .item(name: nil, .atom(.ruleName(value)), type: nil)
        }
    }
}

extension GrammarProcessor.Atom: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        if value.allSatisfy({ $0.isUppercase }) {
            self = .token(value)
        } else {
            self = .ruleName(value)
        }
    }
}

extension GrammarProcessor.Action: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(string: value)
    }
}

// MARK: Operator support

extension GrammarProcessor.Alt {
    /// Constructs `alts: alt | alt`
    static func | (lhs: Self, rhs: Self) -> [Self] {
        return [lhs, rhs]
    }

    /// Constructs `alts: alts | alt`
    static func | (lhs: [Self], rhs: Self) -> [Self] {
        return lhs + [rhs]
    }
}

extension GrammarProcessor.NamedItem {
    /// Constructs `namedItems: namedItem+`
    static func .. (lhs: Self, rhs: Self) -> [Self] {
        return [lhs, rhs]
    }

    /// Constructs `namedItems: namedItems namedItem`
    static func .. (lhs: [Self], rhs: Self) -> [Self] {
        return lhs + [rhs]
    }
}

extension GrammarProcessor.Atom {
    /// Constructs `self?`
    var opt: GrammarProcessor.Item {
        return .optional(self)
    }

    /// Constructs `value+`
    static postfix func + (_ value: Self) -> GrammarProcessor.Item {
        .oneOrMore(value)
    }

    /// Constructs `value*`
    static postfix func * (_ value: Self) -> GrammarProcessor.Item {
        .oneOrMore(value)
    }
}

// MARK: - SupportsWith

extension GrammarProcessor.Grammar: SupportsWith { }
extension GrammarProcessor.MetaProperty: SupportsWith { }
extension GrammarProcessor.Rule: SupportsWith { }
extension GrammarProcessor.Alt: SupportsWith { }
extension GrammarProcessor.Action: SupportsWith { }
extension GrammarProcessor.NamedItem: SupportsWith { }
extension GrammarProcessor.Item: SupportsWith { }
extension GrammarProcessor.Lookahead: SupportsWith { }
extension GrammarProcessor.Atom: SupportsWith { }
extension GrammarProcessor.SwiftType: SupportsWith { }

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
