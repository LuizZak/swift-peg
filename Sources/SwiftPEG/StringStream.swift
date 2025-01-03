/// Internal type used to seek string types character-by-character.
/// Stream advancing is not pre-checked and must be validated with `isEof` or
/// `isEofPast`.
public struct StringStream<StringType: StringProtocol> {
    /// The string being streamed.
    public let source: StringType

    /// The current index from which substrings will be sliced from.
    /// Starts from `source.startIndex` and is incremented by `markSubstringStart()`.
    /// Always `<= self.index` and `>= source.startIndex`.
    public var substringStartIndex: StringType.Index

    /// The current index. Always > `source.startIndex`, and may point beyond
    /// the string's end.
    public var index: StringType.Index

    /// Returns the range `self.substringStartIndex..<self.index`.
    @inlinable
    public var range: Range<StringType.Index> {
        substringStartIndex..<index
    }

    /// Returns a substring with range `self.substringStartIndex..<self.index`.
    @inlinable
    public var substring: StringType.SubSequence {
        source[range]
    }

    /// Returns the length of the substring in `self.substring`.
    @inlinable
    public var substringLength: Int {
        guard substringStartIndex < index else {
            return 0
        }

        return source.distance(from: substringStartIndex, to: index)
    }

    /// Returns `true` if the current index points past the end of the indexable
    /// space of the string.
    @inlinable
    public var isEof: Bool { index >= source.endIndex }

    @inlinable
    public init(source: StringType) {
        self.source = source
        self.index = source.startIndex
        self.substringStartIndex = source.startIndex
    }

    /// Returns a copy of the internal state of this string stream for future
    /// backtracking operations.
    @inlinable
    public func save() -> State {
        State(index: index, substringStartIndex: substringStartIndex)
    }

    /// Restores the state of this string stream to a provided state.
    @inlinable
    public mutating func restore(_ state: State) {
        self.index = state.index
        self.substringStartIndex = state.substringStartIndex
    }

    /// Records the current stream position as the start of future `self.substring`
    /// slices.
    @inlinable
    public mutating func markSubstringStart() {
        self.substringStartIndex = self.index
    }

    /// Returns `true` if the current index, advanced by `offset`, points past
    /// the end of the indexable space of the string.
    @inlinable
    public func isEofPast(_ offset: Int) -> Bool {
        guard let index = source.index(index, offsetBy: offset, limitedBy: source.endIndex) else {
            return false
        }

        return index >= source.endIndex
    }

    /// Performs an unchecked peek at the current character in the stream.
    @inlinable
    public func peek() -> StringType.Element {
        return source[index]
    }

    /// Performs an unchecked peek at the current character offset + `offset` in
    /// the stream.
    @inlinable
    public func peek(_ offset: Int) -> StringType.Element {
        let idx = source.index(index, offsetBy: offset)

        return source[idx]
    }

    /// Performs a checked peek at the current character offset, returning the
    /// character if the current index is not at end-of-stream, and `nil`
    /// otherwise.
    @inlinable
    public func safePeek() -> StringType.Element? {
        isEof ? nil : source[index]
    }

    /// Checks that the next characters in the stream match `match`.
    /// If the first index is within bounds but `match` is larger than the rest
    /// of the string, the method doesn't raise a runtime error and simply returns
    /// `false`.
    @inlinable
    public func isNext<S: StringProtocol>(_ match: S) -> Bool {
        return source[index...].hasPrefix(match)
    }

    /// Checks that the next character in the stream is within a given range.
    @inlinable
    public func isNextInRange(_ range: Range<Character>) -> Bool {
        return range.contains(source[index])
    }

    /// Checks that the next character in the stream is within a given range.
    @inlinable
    public func isNextInRange(_ range: ClosedRange<Character>) -> Bool {
        return range.contains(source[index])
    }

    @inlinable
    public func isNext(where predicate: (StringType.Element) -> Bool) -> Bool {
        if isEof { return false }

        return predicate(source[index])
    }

    /// Checks that the next characters in the stream match `match`, and if it
    /// does, advances the stream by `match.count`, returning `true` to indicate
    /// a match + advance.
    ///
    /// If the first index is within bounds but `match` is larger than the rest
    /// of the string, the method doesn't raise a runtime error and simply returns
    /// `false`.
    @discardableResult
    @inlinable
    public mutating func advanceIfNext<S: StringProtocol>(_ match: S) -> Bool {
        guard isNext(match) else {
            return false
        }

        source.formIndex(&index, offsetBy: match.count)
        return true
    }

    /// Performs an unchecked advance to the next string index, returning the
    /// character that was skipped over.
    @inlinable
    public mutating func next() -> StringType.Element {
        defer { source.formIndex(after: &index) }

        return peek()
    }

    /// Performs an unchecked advance to the next string index.
    @inlinable
    public mutating func advance() {
        source.formIndex(after: &index)
    }

    /// Performs an unchecked advance of the string index by a given distance
    /// forward.
    @inlinable
    public mutating func advance(_ distance: Int) {
        source.formIndex(&index, offsetBy: distance)
    }

    /// Contains information about the state of a string stream, allowing for
    /// recording and reversing of the stream.
    public struct State: Equatable {
        public var index: StringType.Index
        public var substringStartIndex: StringType.Index

        @usableFromInline
        internal init(index: String.Index, substringStartIndex: String.Index) {
            self.index = index
            self.substringStartIndex = substringStartIndex
        }
    }
}

extension StringStream where StringType.SubSequence == Substring {
    /// Advances the stream if the given regex matches at the current string's
    /// position.
    @inlinable
    public mutating func advanceIfNext(matches regex: Regex<Substring>) -> Bool {
        let remaining = source[index...]
        guard let match = remaining.prefixMatch(of: regex) else {
            return false
        }

        let length = match.count
        advance(length)

        return true
    }

    /// Advances the stream if the given regex matches at the current string's
    /// position.
    @inlinable
    public mutating func advanceIfNext(matches regex: Regex<AnyRegexOutput>) -> Bool {
        let remaining = source[index...]
        guard let match = remaining.prefixMatch(of: regex) else {
            return false
        }

        let length = match.output.count
        advance(length)

        return true
    }
}

extension StringStream {
    @usableFromInline
    func debugDisplay() -> String {
        var buffer = ""
        var bufferLower = ""

        var idx = source.startIndex
        for char in source {
            defer { source.formIndex(after: &idx) }
            let computed = char.isNewline ? "\\n" : String(char)
            buffer += computed

            if idx == self.substringStartIndex && idx == self.index {
                bufferLower += "I" + String(repeating: " ", count: computed.count - 1)
            } else if idx == self.substringStartIndex {
                bufferLower += "]" + String(repeating: " ", count: computed.count - 1)
            } else if idx == self.index {
                bufferLower += "[" + String(repeating: " ", count: computed.count - 1)
            } else {
                bufferLower += String(repeating: " ", count: computed.count)
            }
        }

        return "\(buffer)\n\(bufferLower)"
    }
}
