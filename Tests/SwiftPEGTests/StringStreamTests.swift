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

    func testSubstringLength_atStart_returnsZero() {
        let sut = makeSut("a")

        assertEqual(sut.substringLength, 0)
    }

    func testSubstringLength_nonStart() {
        var sut = makeSut("abc")
        sut.advance()

        assertEqual(sut.substringLength, 1)
    }

    func testSubstringLength_negativeRange() {
        var sut = makeSut("abc")
        sut.advance()
        sut.markSubstringStart()
        sut.advance(-1)

        assertEqual(sut.substringLength, 0)
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

    func testSafePeek() {
        var sut = makeSut("ab")

        assertEqual(sut.safePeek(), "a")
        sut.advance()
        assertEqual(sut.safePeek(), "b")

        sut.advance()

        assertNil(sut.safePeek())
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

    func testIsNextInRange_openRange() {
        let sut = makeSut("J")

        assertTrue(sut.isNextInRange("J"..<"Z"))
        assertTrue(sut.isNextInRange("A"..<"Z"))
        assertFalse(sut.isNextInRange("A"..<"J"))
        assertFalse(sut.isNextInRange("a"..<"j"))
        assertFalse(sut.isNextInRange("j"..<"z"))
        assertFalse(sut.isNextInRange("a"..<"z"))
    }

    func testIsNextInRange_closedRange() {
        let sut = makeSut("J")

        assertTrue(sut.isNextInRange("A"..."J"))
        assertTrue(sut.isNextInRange("J"..."Z"))
        assertTrue(sut.isNextInRange("A"..."Z"))
        assertFalse(sut.isNextInRange("a"..."j"))
        assertFalse(sut.isNextInRange("j"..."z"))
        assertFalse(sut.isNextInRange("a"..."z"))
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

    func testAdvanceDistance() {
        var sut = makeSut("abcdefg")

        sut.advance(1)
        assertEqual(sut.index, makeIndex(sut, 1))
        sut.advance(2)
        assertEqual(sut.index, makeIndex(sut, 3))
        sut.advance(0)
        assertEqual(sut.index, makeIndex(sut, 3))
    }

    func testAdvanceIfNextMatches() {
        var sut = makeSut("abcdefg")

        assertTrue(sut.advanceIfNext(matches: #/abc/#))
        assertEqual(sut.index, makeIndex(sut, 3))
        assertTrue(sut.advanceIfNext(matches: #/def/#))
        assertEqual(sut.index, makeIndex(sut, 6))
    }

    func testAdvanceIfNextMatches_noMatch() {
        var sut = makeSut("abcdefg")

        assertFalse(sut.advanceIfNext(matches: #/def/#))
        assertEqual(sut.index, makeIndex(sut, 0))
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
