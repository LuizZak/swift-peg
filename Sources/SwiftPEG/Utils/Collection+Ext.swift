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

extension MutableCollection where Element: Equatable {
    func indexOfFirstDifference(_ other: some Collection<Element>) -> Int? {
        zip(self, other).enumerated().first(where: { $1.0 != $1.1 })?.offset
    }

    /// Returns `true` if `element` occurs in all collections contained within
    /// this collection.
    func allContain<T: Equatable>(_ element: T) -> Bool where Element: Collection<T> {
        allSatisfy { $0.contains(element) }
    }

    /// Factors out the longest prefix in common between this collection of
    /// array elements, replacing elements in-place, and returning the common
    /// prefix that was factored out.
    ///
    /// If no common prefix was factored out, `nil` is returned, instead.
    ///
    /// If this collection contains only one element, the result is the element,
    /// and this collection's element is emptied out.
    mutating func factorPrefix<T: Equatable>() -> [T]? where Element == [T] {
        var longestSequence: [T] = []

        // Collect longest sequence first
        for subSequence in self {
            if longestSequence.count < subSequence.count {
                longestSequence = subSequence
            }
        }

        // Sequentially trim the layout until the result is the largest common
        // prefix across all layouts
        for subSequence in self {
            let index = longestSequence.indexOfFirstDifference(subSequence)

            if let index {
                longestSequence.removeSubrange(index...)
            } else if subSequence.count < longestSequence.count {
                longestSequence.removeSubrange(subSequence.count...)
            }
        }

        if longestSequence.isEmpty {
            return nil
        }

        for index in indices {
            let subSequence = self[index]

            let trimmed = Array(subSequence[longestSequence.count...])

            self[index] = trimmed
        }

        return longestSequence
    }

    /// Factors out the longest sequence of elements in common between this
    /// collection of array elements, replacing elements in-place, and returning
    /// the common elements that were factored out.
    ///
    /// If no common elements were factored out, `nil` is returned, instead.
    ///
    /// If this collection contains only one element, the result is the element,
    /// and this collection's element is emptied out.
    mutating func factorCommon<T: Equatable>() -> [T]? where Element == [T] {
        guard
            let leastArrayIndex = self.indices.min(by: { self[$0].count < self[$1].count }),
            !self[leastArrayIndex].isEmpty
        else {
            return nil
        }
        guard count > 1 else {
            defer {
                self[startIndex] = []
            }
            return self[startIndex]
        }

        let leastArray = self[leastArrayIndex]
        let leastCommon = leastArray.filter(allContain)
        guard !leastCommon.isEmpty else {
            return nil
        }

        let leastCommonIndices: [[Int]] = self.map {
            $0.leastIndices(of: leastCommon).map { $0 ?? -1 }
        }

        // Find the greatest increasing sequence of integers in leastCommonIndices
        var greatestCommonMonotone: Range<Int>?

        for list in leastCommonIndices {
            guard let nextMonotone = list.greatestMonotoneRange() else {
                return nil
            }

            if let greatestMonotone = greatestCommonMonotone {
                if !greatestMonotone.overlaps(nextMonotone) {
                    return nil
                }

                let lower = Swift.max(greatestMonotone.lowerBound, nextMonotone.lowerBound)
                let upper = Swift.min(greatestMonotone.upperBound, nextMonotone.upperBound)

                greatestCommonMonotone = lower..<upper
            } else {
                greatestCommonMonotone = nextMonotone
            }
        }

        guard let greatestCommonMonotone else {
            return nil
        }

        // Use greatest increasing monotone to construct result, and to remove
        // the items from their original collections.
        var result: [T] = []
        for i in greatestCommonMonotone {
            result.append(leastCommon[i])
        }

        for (index, leastCommon) in zip(self.indices, leastCommonIndices) {
            for i in greatestCommonMonotone.reversed() {
                let inArray = leastCommon[i]
                self[index].remove(at: inArray)
            }
        }

        return result
    }

    /// Returns the least indices of elements, mapped 1-to-1, to a given collection
    /// of elements in common with this collection.
    ///
    /// If an element appears twice in `elements`, an attempt is made to map to
    /// different indices within this collection, otherwise, the repeated items
    /// may share the same index.
    func leastIndices(of elements: some Collection<Element>) -> [Index?] where Index: Hashable {
        var used: Set<Index> = []

        var result: [Index?] = []
        for element in elements {
            var nextIndex = self.indices.firstIndex { index in
                !used.contains(index) && self[index] == element
            }
            if nextIndex == nil {
                nextIndex = self.firstIndex(of: element)
            }

            result.append(nextIndex)

            if let nextIndex {
                used.insert(nextIndex)
            }
        }

        return result
    }

    /// Returns the end of a monotonic range within this collection, starting at
    /// a given index.
    ///
    /// Returns the index, starting from `index`, of the first element that is
    /// strictly less than the previous element.
    ///
    /// If the element at `index` is already greater than the next element, the
    /// result is `index`.
    func monotoneEnd(after index: Index) -> Index where Element: Comparable {
        assert(indices.contains(index))

        var current = self[index]

        var stop = self.index(after: index)

        while stop < endIndex, self[stop] >= current {
            current = self[stop]

            stop = self.index(after: stop)
        }

        return stop
    }

    func greatestMonotoneRange() -> Range<Index>? where Element: Comparable, Index == Int {
        var largest: Range<Index>?

        for i in indices {
            let end = self.monotoneEnd(after: i)
            let range = i..<end

            if let prev = largest {
                largest = range.count > prev.count ? range : prev
            } else {
                largest = range
            }
        }

        return largest
    }
}
