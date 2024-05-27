extension Sequence {
    /// Returns a comma-separated string containing each element in this sequence,
    /// with the last element separator being replaced with an oxford-comma
    /// ', or'.
    /// 
    /// ```
    /// [0, 1, 2].asNaturalLanguageList(\.description)
    /// [0, 1].asNaturalLanguageList(\.description)
    /// [0]..asNaturalLanguageList(\.description)
    /// ```
    /// Returns:
    /// ```
    /// "0, 1, or 2"
    /// "0 or 1"
    /// "0"
    /// ```
    @usableFromInline
    func asNaturalLanguageList<S: StringProtocol>(_ describer: (Element) -> S) -> String {
        var result = ""
        let items = Array(self)

        if items.count <= 2 {
            result = items.map(describer).joined(separator: " or ")
        } else if let last = items.last {
            result = items.dropLast().map(describer).joined(separator: ", ")
            result += ", or \(describer(last))"
        }

        return result
    }
}

extension BidirectionalCollection {
    /// Returns a comma-separated string containing each element in this collection,
    /// with the last element separator being replaced with an oxford-comma
    /// ', or'.
    /// 
    /// ```
    /// [0, 1, 2].asNaturalLanguageList(\.description)
    /// [0, 1].asNaturalLanguageList(\.description)
    /// [0]..asNaturalLanguageList(\.description)
    /// ```
    /// Returns:
    /// ```
    /// "0, 1, or 2"
    /// "0 or 1"
    /// "0"
    /// ```
    @usableFromInline
    func asNaturalLanguageList<S: StringProtocol>(_ describer: (Element) -> S) -> String {
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
    /// 
    /// ```
    /// [0, 1, 2].asNaturalLanguageList()
    /// [0, 1].asNaturalLanguageList()
    /// [0]..asNaturalLanguageList()
    /// ```
    /// Returns:
    /// ```
    /// "0, 1, or 2"
    /// "0 or 1"
    /// "0"
    /// ```
    @usableFromInline
    func asNaturalLanguageList() -> String {
        asNaturalLanguageList(\.description)
    }
}

extension BidirectionalCollection where Element: StringProtocol {
    /// Returns a comma-separated string containing each element in this collection,
    /// with the last element separator being replaced with an 'or'.
    /// Elements will be further processed using the provided option set.
    /// 
    /// ```
    /// let items = ["An item", "Another item", "Final item"]
    /// print(items.asNaturalLanguageList(options: .caseAware))
    /// ```
    /// Prints:
    /// ```
    /// "An item, another item, or final item"
    /// ```
    @usableFromInline
    func asNaturalLanguageList(options: NaturalLanguageListOptions) -> String {
        func process(_ index: Int, _ item: Element) -> String {
            var description = item.description
            if options.contains(.caseAware) {
                if index == 0 {
                    description = description.uppercasedFirstLetter
                } else {
                    description = description.lowercasedFirstLetter
                }
            }

            return description
        }

        return enumerated().asNaturalLanguageList(process)
    }
}

@usableFromInline
struct NaturalLanguageListOptions: OptionSet {
    /// Indicates the first item in a natural language list should have its
    /// leading letter, if any, uppercased, while the remaining items have their
    /// leading letters, if any, lowercased.
    static let caseAware = NaturalLanguageListOptions(rawValue: 0b00001)

    @usableFromInline
    var rawValue: Int

    @usableFromInline
    init(rawValue: Int) {
        self.rawValue = rawValue
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
