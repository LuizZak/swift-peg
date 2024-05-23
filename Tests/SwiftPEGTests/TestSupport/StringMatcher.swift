/// Matches strings, either partially, fully or by prefix-/suffix-
enum StringMatcher: Equatable, CustomStringConvertible {
    /// Matches `term` exactly and fully.
    case exact(StringMatchPattern)

    /// Matches `*term*`, case sensitive.
    case contains(StringMatchPattern)

    /// Matches `term*`, case sensitive.
    case prefix(StringMatchPattern)

    /// Matches `*term`, case sensitive.
    case suffix(StringMatchPattern)

    func matches<S>(_ str: S) -> Bool where S: StringProtocol, S.SubSequence == Substring {
        switch self {
        case .exact(let exp):
            return exp.exact(str)

        case .contains(let exp):
            return exp.contains(str)

        case .prefix(let exp):
            return exp.prefix(str)

        case .suffix(let exp):
            return exp.suffix(str)
        }
    }

    var description: String {
        switch self {
        case .exact(let exp):
            return exp.description

        case .contains(let exp):
            return "*\(exp)*"

        case .prefix(let exp):
            return "\(exp)*"

        case .suffix(let exp):
            return "*\(exp)"
        }
    }

    enum StringMatchPattern: Hashable, ExpressibleByStringLiteral {
        case string(String)
        case regex(pattern: String)

        var description: String {
            switch self {
            case .string(let value):
                return value

            case .regex(let value):
                return "/\(value)/"
            }
        }

        init(stringLiteral value: StringLiteralType) {
            self = .string(value)
        }

        /// Matches `term` exactly.
        func exact<S>(_ term: S) -> Bool where S: StringProtocol, S.SubSequence == Substring {
            switch self {
            case .string(let s): term == s
            case .regex(let pattern): (try? (try? Regex(pattern))?.wholeMatch(in: Substring(term))) != nil
            }
        }

        /// Matches `*term*`, case sensitive.
        func contains<S>(_ term: S) -> Bool where S: StringProtocol, S.SubSequence == Substring {
            switch self {
            case .string(let s): term.contains(s)
            case .regex(let pattern): (try? (try? Regex(pattern))?.firstMatch(in: Substring(term))) != nil
            }
        }

        /// Matches `term*`, case sensitive.
        func prefix<S>(_ term: S) -> Bool where S: StringProtocol, S.SubSequence == Substring {
            switch self {
            case .string(let s): term.hasPrefix(s)
            case .regex(let pattern): (try? (try? Regex(pattern))?.prefixMatch(in: Substring(term))) != nil
            }
        }

        /// Matches `*term`, case sensitive.
        func suffix<S>(_ term: S) -> Bool where S: StringProtocol, S.SubSequence == Substring {
            switch self {
            case .string(let s): term.hasSuffix(s)
            case .regex(let pattern): (try? (try? Regex("(.*)\(pattern)"))?.wholeMatch(in: Substring(term))) != nil
            }
        }
    }

    /// The matching mode to use with `StringMatcher.regex`
    enum RegexMatchingMode {
        /// Regex may partially match the tested string. The default mode.
        case partial

        /// Regex needs to fully match the entire tested string.
        case fullMatch
    }
}

extension StringMatcher: ExpressibleByStringLiteral {
    init(stringLiteral value: String) {
        self = .exact(.string(value))
    }
}
