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

extension Collection where Element: Equatable {
    /// Returns the offset of the first element that is different between
    /// `self` and `other`.
    ///
    /// If `self` and `other` are equal, or have the same elements, but have
    /// different lengths, the result is `nil`.
    func indexOfFirstDifference(_ other: some Collection<Element>) -> Int? {
        zip(self, other).enumerated().first(where: { $1.0 != $1.1 })?.offset
    }

    /// Returns `true` if `element` occurs in all collections contained within
    /// this collection.
    func allContain<T: Equatable>(
        _ element: T,
        comparator: (T, T) -> Bool = (==)
    ) -> Bool where Element: Collection<T> {

        allSatisfy { $0.contains(where: { comparator(element, $0) }) }
    }

    /// Returns the least indices of elements, mapped 1-to-1, to a given collection
    /// of elements in common with this collection.
    ///
    /// If an element appears twice in `elements`, an attempt is made to map to
    /// different indices within this collection, otherwise, the repeated items
    /// may share the same index.
    func leastIndices(
        of elements: some Collection<Element>,
        comparator: (Element, Element) -> Bool = (==)
    ) -> [Index?] where Index: Hashable {

        var used: Set<Index> = []

        var result: [Index?] = []
        for element in elements {
            var nextIndex = self.indices.firstIndex { index in
                !used.contains(index) && comparator(self[index], element)
            }
            if nextIndex == nil {
                nextIndex = self.firstIndex(where: { comparator($0, element) })
            }

            result.append(nextIndex)

            if let nextIndex {
                used.insert(nextIndex)
            }
        }

        return result
    }

    /// Returns the end index of the longest sequence of elements that are a
    /// prefix of all elements of this collection of arrays.
    ///
    /// If this collection contains only one array, the result is that array's
    /// `endIndex`.
    ///
    /// If the elements share no common prefix, or this collection is empty,
    /// `nil` is returned, instead.
    func greatestCommonPrefixIndex<T: Equatable>() -> Int? where Element == [T] {
        guard let least = self.min(by: { $0.count < $1.count }), !least.isEmpty else {
            return nil
        }

        var longest = least.count
        for element in self {
            let firstDifference =
                element.indexOfFirstDifference(least)
                    ?? least.endIndex

            longest = Swift.min(firstDifference, longest)
        }

        if longest == 0 {
            return nil
        }

        return longest
    }

    /// Finds the longest sequence of elements in common between this collection
    /// of array elements, returning an array-of-arrays of the indices for each
    /// of the arrays.
    ///
    /// If no common elements were factored out, `nil` is returned, instead.
    ///
    /// If this collection contains only one element, the result are all of the
    /// element's indices.
    func greatestCommonIndices<T: Equatable>(
        comparator: (T, T) -> Bool = (==)
    ) -> [[Int]]? where Element == [T] {

        guard
            let leastArrayIndex = self.indices.min(by: { self[$0].count < self[$1].count }),
            !self[leastArrayIndex].isEmpty
        else {
            return nil
        }
        guard count > 1 else {
            let indices = Array(self[startIndex].indices)
            return [indices]
        }

        let leastArray = self[leastArrayIndex]
        let leastCommon = leastArray.filter({ allContain($0, comparator: comparator) })
        guard !leastCommon.isEmpty else {
            return nil
        }

        let leastCommonIndices: [[Int]] = self.map {
            $0.leastIndices(of: leastCommon, comparator: comparator).map { $0 ?? -1 }
        }

        // Find the greatest increasing sequence of integers in leastCommonIndices
        var greatestCommonMonotone: Range<Int>?

        for list in leastCommonIndices {
            guard let nextMonotone = list.greatestMonotoneRange(by: <) else {
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

        var result: [[Int]] = []
        for indices in leastCommonIndices {
            var partial: [Int] = []

            for i in greatestCommonMonotone {
                let inArray = indices[i]
                partial.append(inArray)
            }

            result.append(partial)
        }

        return result
    }

    /// Returns the end of a monotonic range within this collection, starting at
    /// a given index.
    ///
    /// Returns the index, starting from `index`, of the first element that
    /// `comparator` returns `false` when compared to the previous element.
    ///
    /// If the element at `index` already compares as false with the next element,
    /// the result is `index + 1`.
    ///
    /// - Parameters:
    ///   - index:
    ///         The index to start the search at.
    ///   - comparator:
    ///         A comparator that will be invoked with sequential items as
    ///         `comparator(self[n], self[n + 1])`.
    ///         Defaults to `<=`.
    /// - Returns:
    ///     The index of the first element where `comparator(current, next)`
    ///     returns false.
    ///
    /// - complexity: O(n) where n is the length of the collection.
    func monotoneEnd(
        after index: Index,
        by comparator: (Element, Element) -> Bool = (<=)
    ) -> Index where Element: Comparable {
        assert(indices.contains(index))

        var current = self[index]

        var stop = self.index(after: index)

        while stop < endIndex, comparator(current, self[stop]) {
            current = self[stop]

            stop = self.index(after: stop)
        }

        return stop
    }

    /// Returns the range of indices that point to the greatest, contiguous range
    /// of elements within this collection that are in increasing monotone order
    /// (i.e., each element is equal to or greater than the previous).
    ///
    /// If no element is less than or equal to the next in this collection, a
    /// range containing only one of any of the element of the collection is
    /// returned, instead.
    ///
    /// Returns `nil` if the collection is empty.
    ///
    /// - Parameters:
    ///   - comparator:
    ///         A comparator that will be invoked with sequential items as
    ///         `comparator(self[n], self[n + 1])`.
    ///         Defaults to `<=`.
    /// - Returns:
    ///     The greatest sequential range of indices where
    ///     `comparator(self[index], self[index + 1])` is true.
    ///
    /// - complexity: O(n) where n is the length of the collection.
    func greatestMonotoneRange(
        by comparator: (Element, Element) -> Bool = (<=)
    ) -> Range<Index>? where Element: Comparable, Index == Int {
        var largest: Range<Index>?

        var index = self.startIndex
        while index < self.endIndex {
            let end = self.monotoneEnd(after: index, by: comparator)
            defer { index = end }

            let range = index..<end

            if let prev = largest {
                largest = range.count > prev.count ? range : prev
            } else {
                largest = range
            }
        }

        return largest
    }
}

extension MutableCollection where Element: Equatable {
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
        guard let commonIndices = self.greatestCommonIndices() else {
            return nil
        }

        var result: [T]?

        for (index, commonIndices) in zip(self.indices, commonIndices) {
            var innerResult: [T]?
            if result == nil {
                innerResult = []
            }

            for inArray in commonIndices.reversed() {
                let element = self[index].remove(at: inArray)

                innerResult?.append(element)
            }

            if result == nil {
                // Result is reversed due to the inner loop traversing the indices
                // in reverse order
                result = innerResult?.reversed()
            }
        }

        return result
    }
}
