/// Internal string escaping utilities
enum StringEscaping {
    /// Attempts to escape the contents of a given string, returning it as a
    /// string surrounded by quotes, fit for printing as a string literal.
    @usableFromInline
    static func escapeAsStringLiteral<S: StringProtocol>(_ string: S) -> String {
        return #""\#(escape(string))""#
    }

    /// Attempts to escape the contents of a given string.
    @usableFromInline
    static func escape<S: StringProtocol>(_ string: S) -> String {
        escape(string, terminator: "\"")
    }

    /// Attempts to escape the contents of a given string, with a given terminator
    /// to find and escape.
    @usableFromInline
    static func escape<S: StringProtocol>(_ string: S, terminator: String) -> String {
        string
            .replacingOccurrences(of: "\\", with: #"\\"#)
            .replacingOccurrences(of: terminator, with: #"\\#(terminator)"#)
            .replacingOccurrences(of: "\n", with: #"\n"#)
            .replacingOccurrences(of: "\t", with: #"\t"#)
            .replacingOccurrences(of: "\r", with: #"\r"#)
    }

    /// Attempts to escape terminators from a given string.
    @usableFromInline
    static func escapeTerminators<S: StringProtocol>(_ string: S, terminator: String) -> String {
        string.replacingOccurrences(of: terminator, with: #"\\#(terminator)"#)
    }
}
