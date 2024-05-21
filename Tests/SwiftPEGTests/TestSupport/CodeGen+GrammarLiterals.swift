@testable import SwiftPEG

// MARK: Literal initialization

extension CodeGen.Alt: ExpressibleByArrayLiteral {
    public init(arrayLiteral elements: CodeGen.NamedItem...) {
        self.init(items: elements)
    }
}

extension CodeGen.NamedItem: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        if value.allSatisfy({ $0.isUppercase }) {
            self = .item(name: nil, .atom(.token(value)), type: nil)
        } else {
            self = .item(name: nil, .atom(.ruleName(value)), type: nil)
        }
    }
}

extension CodeGen.Atom: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        if value.allSatisfy({ $0.isUppercase }) {
            self = .token(value)
        } else {
            self = .ruleName(value)
        }
    }
}

extension CodeGen.Action: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(string: value)
    }
}

// MARK: Operator support

extension CodeGen.Alt {
    /// Constructs `alts: alt | alt`
    static func | (lhs: Self, rhs: Self) -> [Self] {
        return [lhs, rhs]
    }

    /// Constructs `alts: alts | alt`
    static func | (lhs: [Self], rhs: Self) -> [Self] {
        return lhs + [rhs]
    }
}

extension CodeGen.NamedItem {
    /// Constructs `namedItems: namedItem+`
    static func .. (lhs: Self, rhs: Self) -> [Self] {
        return [lhs, rhs]
    }

    /// Constructs `namedItems: namedItems namedItem`
    static func .. (lhs: [Self], rhs: Self) -> [Self] {
        return lhs + [rhs]
    }
}

extension CodeGen.Atom {
    /// Constructs `self?`
    var opt: CodeGen.Item {
        return .optional(self)
    }

    /// Constructs `value+`
    static postfix func + (_ value: Self) -> CodeGen.Item {
        .oneOrMore(value)
    }

    /// Constructs `value*`
    static postfix func * (_ value: Self) -> CodeGen.Item {
        .oneOrMore(value)
    }
}

// MARK: - SupportsWith

extension CodeGen.Grammar: SupportsWith { }
extension CodeGen.MetaProperty: SupportsWith { }
extension CodeGen.Rule: SupportsWith { }
extension CodeGen.Alt: SupportsWith { }
extension CodeGen.Action: SupportsWith { }
extension CodeGen.NamedItem: SupportsWith { }
extension CodeGen.Item: SupportsWith { }
extension CodeGen.Lookahead: SupportsWith { }
extension CodeGen.Atom: SupportsWith { }
extension CodeGen.SwiftType: SupportsWith { }

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
