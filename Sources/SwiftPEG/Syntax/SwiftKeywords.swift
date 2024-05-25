enum SwiftKeywords {
    /// Set of all keywords in Swift that must be escaped with `backticks` to
    /// alow their usage as definitions.
    static let keywords: Set<String> = _computeKeywords()

    private static func _computeKeywords() -> Set<String> {
        return [
            "Any", "as", "associatedtype", "break", "case", "catch", "class",
            "continue", "default", "defer", "deinit", "do", "else", "enum",
            "extension", "fallthrough", "false", "fileprivate", "for", "func",
            "guard", "if", "import", "in", "init", "inout", "internal", "is",
            "let", "nil", "operator", "precedencegroup", "private", "protocol",
            "Protocol", "public", "repeat", "rethrows", "return", "self", "Self",
            "static", "struct", "subscript", "super", "switch", "throw", "throws",
            "true", "try", "Type", "typealias", "var", "where", "while",
        ]
    }
}
