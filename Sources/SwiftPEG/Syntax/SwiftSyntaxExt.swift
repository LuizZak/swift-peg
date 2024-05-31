enum SwiftSyntaxExt {
    /// Returns `true` if a given string is a valid Swift identifier in full.
    ///
    /// Takes a conservative approach that is more strict than the one Swift
    /// actually uses.
    static func isIdentifier(_ string: some StringProtocol) -> Bool {
        guard let first = string.first else {
            return false
        }

        switch first {
        case "a"..."z", "A"..."Z", "_":
            return string.dropFirst().allSatisfy({ character in
                switch character {
                case "0"..."9", "a"..."z", "A"..."Z", "_":
                    return true
                default:
                    return false
                }
            })
        default:
            return false
        }
    }
}
