import XCTest

@testable import SwiftPEG

class StringStreamTests: XCTestCase {
    func testRange_atStart_returnsZeroRange() {
        let sut = makeSut("a")

        assertEqual(sut.range, makeRange(sut, start: 0, end: 0))
    }

    func testRange_nonStart_returnsNonZeroRange() {
        var sut = makeSut("abc")
        sut.advance()

        assertEqual(sut.range, makeRange(sut, start: 0, end: 1))
    }

    func testSubstring_atStart_returnsEmptySubstring() {
        let sut = makeSut("a")

        assertEqual(sut.substring, "")
    }

    func testSubstring_nonStart_returnsNonEmptySubstring() {
        var sut = makeSut("abc")
        sut.advance()

        assertEqual(sut.substring, "a")
    }

    func testSubstring_nonStart_nonStartSubstringStartIndex_returnsNonEmptySubstring() {
        var sut = makeSut("abc")
        sut.advance()
        sut.markSubstringStart()
        sut.advance()

        assertEqual(sut.substring, "b")
    }

    func testIsEof_emptyString_returnsTrue() {
        let sut = makeSut("")

        assertTrue(sut.isEof)
    }

    func testIsEof_emptySubstring_returnsTrue() {
        let sut = makeSut("a".dropFirst())

        assertTrue(sut.isEof)
    }

    func testIsEof_nonEmptyString_returnsFalse() {
        let sut = makeSut("a")

        assertFalse(sut.isEof)
    }

    func testIsEof_nonEmptySubstring_returnsFalse() {
        let sut = makeSut("a"[...])

        assertFalse(sut.isEof)
    }

    func testRecordRestore() {
        var sut = makeSut("abcd")
        sut.advance()
        sut.markSubstringStart()
        sut.advance()

        let state = sut.save()

        sut.advance()
        sut.markSubstringStart()

        sut.restore(state)

        assertEqual(sut.substringStartIndex, makeIndex(sut, 1))
        assertEqual(sut.index, makeIndex(sut, 2))
    }

    func testIsEofPast_zero_returnsIsEofValue() {
        var sut = makeSut("a")

        assertFalse(sut.isEofPast(0))
        
        sut.advance()

        assertTrue(sut.isEofPast(0))
    }

    func testIsEofPast_noZero() {
        var sut = makeSut("ab")

        assertFalse(sut.isEofPast(1))
        
        sut.advance()

        assertTrue(sut.isEofPast(1))
    }

    func testPeek() {
        var sut = makeSut("abc")

        assertEqual(sut.peek(), "a")

        sut.advance()

        assertEqual(sut.peek(), "b")
    }

    func testPeekOffset() {
        var sut = makeSut("abc")

        assertEqual(sut.peek(1), "b")

        sut.advance()

        assertEqual(sut.peek(1), "c")
    }

    func testIsNext_smallMatchString() {
        var sut = makeSut("abc")

        assertTrue(sut.isNext("a"))

        sut.advance()

        assertFalse(sut.isNext("a"))
        assertTrue(sut.isNext("b"))
    }

    func testIsNext_largerMatchString() {
        var sut = makeSut("abc")

        assertTrue(sut.isNext("abc"))

        sut.advance()

        assertFalse(sut.isNext("abc"))
    }

    func testIsNext_atEof_alwaysReturnsFalseForNonEmptyStrings() {
        var sut = makeSut("a")
        sut.advance()

        assertTrue(sut.isNext(""))
        assertFalse(sut.isNext("a"))
        assertFalse(sut.isNext("b"))
    }

    func testAdvanceIfNext_advancesOnMatch() {
        var sut = makeSut("abc")

        assertTrue(sut.advanceIfNext("a"))

        assertEqual(sut.index, makeIndex(sut, 1))
    }

    func testAdvanceIfNext_doesNotAdvanceOnFailedMatch() {
        var sut = makeSut("abc")

        assertFalse(sut.advanceIfNext("b"))

        assertEqual(sut.index, makeIndex(sut, 0))
    }

    func testAdvanceIfNext_largerMatchString() {
        var sut = makeSut("abc")

        assertTrue(sut.advanceIfNext("abc"))

        assertEqual(sut.index, makeIndex(sut, 3))

        assertFalse(sut.advanceIfNext("abc"))
    }

    func testNext() {
        var sut = makeSut("abc")

        assertEqual(sut.next(), "a")
        assertEqual(sut.index, makeIndex(sut, 1))
        assertEqual(sut.next(), "b")
        assertEqual(sut.index, makeIndex(sut, 2))
        assertEqual(sut.next(), "c")
        assertEqual(sut.index, makeIndex(sut, 3))
    }

    func testAdvance() {
        var sut = makeSut("abc")

        sut.advance()
        assertEqual(sut.index, makeIndex(sut, 1))
        sut.advance()
        assertEqual(sut.index, makeIndex(sut, 2))
        sut.advance()
        assertEqual(sut.index, makeIndex(sut, 3))
    }
}

// MARK: - Test internals

private func makeSut<StringType>(_ source: StringType) -> StringStream<StringType> {
    return .init(source: source)
}

private func makeIndex<StringType>(
    _ sut: StringStream<StringType>,
    _ index: Int
) -> StringType.Index {

    sut.source.index(sut.source.startIndex, offsetBy: index)
}

private func makeRange<StringType>(
    _ sut: StringStream<StringType>,
    start: Int,
    end: Int
) -> Range<StringType.Index> {

    let startIndex = makeIndex(sut, start)
    let endIndex = makeIndex(sut, end)

    return startIndex..<endIndex
}
