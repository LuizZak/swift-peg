import XCTest

@testable import SwiftPEG

class Collection_ExtTests: XCTestCase {
    // MARK: greatestCommonPrefixIndex

    func testGreatestCommonPrefixIndex_empty() {
        let sut: [[Int]] = []

        let result = sut.greatestCommonPrefixIndex()

        assertNil(result)
    }

    func testGreatestCommonPrefixIndex_empty_elements() {
        let sut: [[Int]] = [
            [],
            [],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertNil(result)
    }

    func testGreatestCommonPrefixIndex_singleArray() {
        let sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertEqual(result, 3)
    }

    func testGreatestCommonPrefixIndex_commonPrefix_equalLength() {
        let sut: [[Int]] = [
            [0, 1, 2],
            [0, 1, 3],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertEqual(result, 2)
    }

    func testGreatestCommonPrefixIndex_commonPrefix_unequalLength() {
        let sut: [[Int]] = [
            [0, 1],
            [0, 1, 3],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertEqual(result, 2)
    }

    func testGreatestCommonPrefixIndex_noCommonPrefix() {
        let sut: [[Int]] = [
            [0, 1, 2],
            [3, 0, 1],
        ]

        let result = sut.greatestCommonPrefixIndex()

        assertNil(result)
    }

    // MARK: greatestCommonIndices

    func testGreatestCommonIndices_empty() {
        let sut: [[Int]] = []

        let result = sut.greatestCommonIndices()

        assertNil(result)
    }

    func testGreatestCommonIndices_singleArray() {
        let sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.greatestCommonIndices()

        assertEqual(result, [[0, 1, 2]])
    }

    func testGreatestCommonIndices_repeatingElement() {
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

    func testGreatestCommonIndices_singleRepeatingElement() {
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

    func testGreatestCommonIndices_noCommonElement() {
        let sut: [[Int]] = [
            [0, 1, 0],
            [3, 2, 6],
            [4, 7, 8, 9],
        ]

        let result = sut.greatestCommonIndices()

        assertNil(result)
    }

    func testGreatestCommonIndices_commonPrefix() {
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

    func testGreatestMonotoneRange() {
        let sut = [0, 1, 1, 2, 0, 3, 4, 5, 6]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 4..<9)
    }

    func testGreatestMonotoneRange_sortedArray() {
        let sut = [0, 1, 1, 2, 3, 5, 10, 11]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 0..<8)
    }

    func testGreatestMonotoneRange_noIncreasingElements() {
        let sut = [11, 10, 5, 3, 2, 1, 1, 0]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 5..<7)
    }

    func testGreatestMonotoneRange_strictlyNonIncreasingElements() {
        let sut = [11, 10, 5, 3, 2, 1, 0]

        let result = sut.greatestMonotoneRange()

        assertEqual(result, 0..<1)
    }

    // MARK: factorPrefix

    func testFactorPrefix_empty() {
        var sut: [[Int]] = []

        let result = sut.factorPrefix()

        assertNil(result)
        assertEqual(sut, [])
    }

    func testFactorPrefix_singleArray() {
        var sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.factorPrefix()

        assertEqual(result, [0, 1, 2])
        assertEqual(sut, [[]])
    }

    func testFactorPrefix_unequalLength() {
        var sut: [[Int]] = [
            [0, 1, 2],
            [0, 1, 2, 3],
            [0, 1],
        ]

        let result = sut.factorPrefix()

        assertEqual(result, [0, 1])
        assertEqual(sut, [[2], [2, 3], []])
    }

    func testFactorPrefix_allEqual() {
        var sut: [[Int]] = [
            [0, 1, 2, 3],
            [0, 1, 2, 3],
            [0, 1, 2, 3],
        ]

        let result = sut.factorPrefix()

        assertEqual(result, [0, 1, 2, 3])
        assertEqual(sut, [[], [], []])
    }

    func testFactorPrefix_noCommonPrefix() {
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

    func testFactorCommon_empty() {
        var sut: [[Int]] = []

        let result = sut.factorCommon()

        assertNil(result)
        assertEqual(sut, [])
    }

    func testFactorCommon_singleArray() {
        var sut: [[Int]] = [
            [0, 1, 2],
        ]

        let result = sut.factorCommon()

        assertEqual(result, [0, 1, 2])
        assertEqual(sut, [[]])
    }

    func testFactorCommon_repeatingElement() {
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

    func testFactorCommon_singleRepeatingElement() {
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

    func testFactorCommon_noCommonElement() {
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

    func testFactorCommon_commonPrefix() {
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
