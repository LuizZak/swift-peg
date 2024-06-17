import XCTest

@testable import SwiftPEG

class Collection_ExtTests: XCTestCase {
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