/// Internal string escaping utilities
enum StringEscaping {
    /// Attempts to escape the contents of a given string, returning it as a
    /// string surrounded by quotes, fit for printing as a string literal.
    @usableFromInline
    static func escapeAsStringLiteral<S: StringProtocol>(_ string: S) -> String {
        let replaced = string
            .replacingOccurrences(of: "\\", with: #"\\"#)
            .replacingOccurrences(of: "\"", with: #"\""#)
            .replacingOccurrences(of: "\n", with: #"\n"#)
        
        return #""\#(replaced)""#
    }
}
