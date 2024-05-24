/// Internal type used to seek string types character-by-character.
/// Stream advancing is not pre-checked and must be validated with `isEof` or
/// `isEofPast`.
@usableFromInline
struct StringStreamer<StringType: StringProtocol> {
    /// The string being streamed.
    @usableFromInline
    let source: StringType

    /// The current index. Always > `source.startIndex`, and may point beyond
    /// the string's end.
    @usableFromInline
    var index: StringType.Index

    /// Returns the range `source.startIndex..<self.index`.
    @inlinable
    var range: Range<StringType.Index> {
        source.startIndex..<index
    }

    /// Returns a substring with range `source.startIndex..<self.index`.
    @inlinable
    var substring: StringType.SubSequence {
        source[range]
    }

    /// Returns `true` if the current index points past the end of the indexable
    /// space of the string.
    @inlinable
    var isEof: Bool { index >= source.endIndex }

    @inlinable
    init(source: StringType) {
        self.source = source
        self.index = source.startIndex
    }

    /// Returns `true` if the current index, advanced by `offset`, points past
    /// the end of the indexable space of the string.
    @inlinable
    func isEofPast(_ offset: Int) -> Bool {
        source.index(index, offsetBy: offset, limitedBy: source.endIndex) != nil
    }

    /// Performs an unchecked peek at the current character in the stream.
    @inlinable
    func peek() -> StringType.Element {
        return source[index]
    }

    /// Performs an unchecked peek at the current character offset + `offset` in
    /// the stream.
    @inlinable
    func peek(_ offset: Int) -> StringType.Element {
        let idx = source.index(index, offsetBy: offset)

        return source[idx]
    }

    /// Checks that the next characters in the stream match `match`.
    /// If the first index is within bounds but `match` is larger than the rest
    /// of the string, the method doesn't raise a runtime error and simply returns
    /// `false`.
    @inlinable
    func isNext<S: StringProtocol>(_ match: S) -> Bool {
        return source[index...].hasPrefix(match)
    }

    @inlinable
    func isNext(where predicate: (StringType.Element) -> Bool) -> Bool {
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
    @inlinable
    mutating func advanceIfNext<S: StringProtocol>(_ match: S) -> Bool {
        guard isNext(match) else {
            return false
        }

        source.formIndex(&index, offsetBy: match.count)
        return true
    }

    /// Performs an unchecked advance to the next string index, returning the
    /// character that was skipped over.
    @inlinable
    mutating func next() -> StringType.Element {
        defer { source.formIndex(after: &index) }

        return peek()
    }

    /// Performs an unchecked advance to the next string index.
    @inlinable
    mutating func advance() {
        source.formIndex(after: &index)
    }

    @usableFromInline
    enum Error: Swift.Error {
        case eof
    }
}
