extension BidirectionalCollection {
    /// Returns a comma-separated string containing each element in this collection,
    /// with the last element separator being replaced with an oxford-comma
    /// ', or'.
    @usableFromInline
    func asNaturalLanguageList(_ describer: (Element) -> String) -> String {
        var result = ""

        if count <= 2 {
            result = map(describer).joined(separator: " or ")
        } else if let last = self.last {
            result = dropLast().map(describer).joined(separator: ", ")
            result += ", or \(describer(last))"
        }

        return result
    }
}

extension BidirectionalCollection where Element: CustomStringConvertible {
    /// Returns a comma-separated string containing each element in this collection,
    /// with the last element separator being replaced with an 'or'.
    @usableFromInline
    func asNaturalLanguageList() -> String {
        asNaturalLanguageList(\.description)
    }
}

extension Array where Element: Hashable {
    /// Returns a copy of this array with all duplicated elements according
    /// to `Equatable.==` removed, while preserving the order of the items.
    @usableFromInline
    func removingDuplicates() -> Self {
        return Self(unsafeUninitializedCapacity: count) { (buffer, count) in
            var seen: Set<Element> = []
            seen.reserveCapacity(self.capacity)
            
            for element in self {
                guard seen.insert(element).inserted else {
                    continue
                }

                buffer[count] = element
                count += 1
            }
        }
    }
}
