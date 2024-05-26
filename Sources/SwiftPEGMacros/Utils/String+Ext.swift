extension StringProtocol {
    /// Returns a copy of `self` with the first letter uppercased.
    var uppercasedFirstLetter: String {
        if isEmpty {
            return String(self)
        }
        
        return prefix(1).uppercased() + dropFirst()
    }
}
