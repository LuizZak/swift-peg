import XCTest
import Testing

@testable import SwiftPEG

struct Collection_ExtTests {
    // MARK: greatestCommonPrefixIndex

    @Test
    func greatestCommonPrefixIndex_empty() {
        let sut: [[Int]] = []

        let result = sut.greatestCommonPrefixIndex()

        assertNil(result)
    }

    @Test
    func greatestCommonPrefixIndex_empty_elements() {
        let sut: [[Int]] = [
            [],
            [],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertNil(result)
    }

    @Test
    func greatestCommonPrefixIndex_singleArray() {
        let sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertEqual(result, 3)
    }

    @Test
    func greatestCommonPrefixIndex_commonPrefix_equalLength() {
        let sut: [[Int]] = [
            [0, 1, 2],
            [0, 1, 3],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertEqual(result, 2)
    }

    @Test
    func greatestCommonPrefixIndex_commonPrefix_unequalLength() {
        let sut: [[Int]] = [
            [0, 1],
            [0, 1, 3],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertEqual(result, 2)
    }

    @Test
    func greatestCommonPrefixIndex_noCommonPrefix() {
        let sut: [[Int]] = [
            [0, 1, 2],
            [3, 0, 1],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertNil(result)
    }

    // MARK: greatestCommonIndices

    @Test
    func greatestCommonIndices_empty() {
        let sut: [[Int]] = []

        let result = sut.greatestCommonIndices()

        assertNil(result)
    }

    @Test
    func greatestCommonIndices_singleArray() {
        let sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.greatestCommonIndices()

        assertEqual(result, [[0, 1, 2]])
    }

    @Test
    func greatestCommonIndices_mismatchedElementCount() {
        let sut: [[Int]] = [
            [0, 2, 1, 1],
            [3, 0, 4, 1],
        ]

        let result = sut.greatestCommonIndices()

        assertEqual(result, [
            [0, 2],
            [1, 3],
        ])
    }

    @Test
    func greatestCommonIndices_repeatingElement() {
        let sut: [[Int]] = [
            [0, 1, 0],
            [1, 2, 0],
            [1, 3, 4, 5, 0],
        ]

        let result = sut.greatestCommonIndices()

        assertEqual(result, [
            [1, 2],
            [0, 2],
            [0, 4],
        ])
    }

    @Test
    func greatestCommonIndices_singleRepeatingElement() {
        let sut: [[Int]] = [
            [0, 1, 0],
            [1, 2, 6],
            [0, 3, 1, 5, 7],
        ]

        let result = sut.greatestCommonIndices()

        assertEqual(result, [
            [1],
            [0],
            [2],
        ])
    }

    @Test
    func greatestCommonIndices_noCommonElement() {
        let sut: [[Int]] = [
            [0, 1, 0],
            [3, 2, 6],
            [4, 7, 8, 9],
        ]

        let result = sut.greatestCommonIndices()

        assertNil(result)
    }

    @Test
    func greatestCommonIndices_commonPrefix() {
        let sut: [[Int]] = [
            [0, 1, 2],
            [0, 1],
            [0, 3, 1, 4],
        ]

        let result = sut.greatestCommonIndices()

        assertEqual(result, [
            [0, 1],
            [0, 1],
            [0, 2],
        ])
    }

    // MARK: greatestMonotoneRange

    @Test
    func greatestMonotoneRange() {
        let sut = [0, 1, 1, 2, 0, 3, 4, 5, 6]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 4..<9)
    }

    @Test
    func greatestMonotoneRange_sortedArray() {
        let sut = [0, 1, 1, 2, 3, 5, 10, 11]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 0..<8)
    }

    @Test
    func greatestMonotoneRange_noIncreasingElements() {
        let sut = [11, 10, 5, 3, 2, 1, 1, 0]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 5..<7)
    }

    @Test
    func greatestMonotoneRange_strictlyNonIncreasingElements() {
        let sut = [11, 10, 5, 3, 2, 1, 0]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 0..<1)
    }

    // MARK: factorPrefix

    @Test
    func factorPrefix_empty() {
        var sut: [[Int]] = []

        let result = sut.factorPrefix()

        assertNil(result)
        assertEqual(sut, [])
    }

    @Test
    func factorPrefix_singleArray() {
        var sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.factorPrefix()

        assertEqual(result, [0, 1, 2])
        assertEqual(sut, [[]])
    }

    @Test
    func factorPrefix_unequalLength() {
        var sut: [[Int]] = [
            [0, 1, 2],
            [0, 1, 2, 3],
            [0, 1],
        ]

        let result = sut.factorPrefix()

        assertEqual(result, [0, 1])
        assertEqual(sut, [[2], [2, 3], []])
    }

    @Test
    func factorPrefix_allEqual() {
        var sut: [[Int]] = [
            [0, 1, 2, 3],
            [0, 1, 2, 3],
            [0, 1, 2, 3],
        ]

        let result = sut.factorPrefix()

        assertEqual(result, [0, 1, 2, 3])
        assertEqual(sut, [[], [], []])
    }

    @Test
    func factorPrefix_noCommonPrefix() {
        var sut: [[Int]] = [
            [0, 1, 2, 3],
            [1, 1, 2, 3],
            [2, 1, 2, 3],
        ]

        let result = sut.factorPrefix()

        assertNil(result)
        assertEqual(sut, [[0, 1, 2, 3], [1, 1, 2, 3], [2, 1, 2, 3]])
    }

    // MARK: factorCommon

    @Test
    func factorCommon_empty() {
        var sut: [[Int]] = []

        let result = sut.factorCommon()

        assertNil(result)
        assertEqual(sut, [])
    }

    @Test
    func factorCommon_singleArray() {
        var sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.factorCommon()

        assertEqual(result, [0, 1, 2])
        assertEqual(sut, [[]])
    }

    @Test
    func factorCommon_repeatingElement() {
        var sut: [[Int]] = [
            [0, 1, 0],
            [1, 2, 0],
            [1, 3, 4, 5, 0],
        ]

        let result = sut.factorCommon()

        assertEqual(result, [1, 0])
        assertEqual(sut, [
            [0],
            [2],
            [3, 4, 5],
        ])
    }

    @Test
    func factorCommon_singleRepeatingElement() {
        var sut: [[Int]] = [
            [0, 1, 0],
            [1, 2, 6],
            [0, 3, 1, 5, 7],
        ]

        let result = sut.factorCommon()

        assertEqual(result, [1])
        assertEqual(sut, [
            [0, 0],
            [2, 6],
            [0, 3, 5, 7],
        ])
    }

    @Test
    func factorCommon_noCommonElement() {
        var sut: [[Int]] = [
            [0, 1, 0],
            [3, 2, 6],
            [4, 7, 8, 9],
        ]

        let result = sut.factorCommon()

        assertNil(result)
        assertEqual(sut, [
            [0, 1, 0],
            [3, 2, 6],
            [4, 7, 8, 9],
        ])
    }

    @Test
    func factorCommon_commonPrefix() {
        var sut: [[Int]] = [
            [0, 1, 2],
            [0, 1],
            [0, 3, 1, 4],
        ]

        let result = sut.factorCommon()

        assertEqual(result, [0, 1])
        assertEqual(sut, [
            [2],
            [],
            [3, 4],
        ])
    }
}
