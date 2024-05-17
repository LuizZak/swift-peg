import XCTest
@testable import SwiftPEG

final class TokenizerTests_MarkRestore: XCTestCase {
    // MARK: - Marking/restoring

    func testMark_internals() throws {
        let stubRaw = stubTestRawTokenizer([
            0, 1,
        ])
        let sut = makeSut(stubRaw)

        let mark = sut.mark()

        assertEqual(mark.index, sut.tokenIndex)
        assertIdentical(mark.owner, sut)
    }

    /// Tests behavior of mark/restore calls, ensuring that token indices are
    /// correctly backtracked.
    func testMarkRestore_results() throws {
        let stubRaw = stubTestRawTokenizer([
            0, 1, 2, 3,
        ])
        let sut = makeSut(stubRaw)

        let result_0 = try sut.next()
        let mark = sut.mark()
        let result_1 = try sut.next()
        let result_2 = try sut.next()
        sut.restore(mark)
        let result_3 = try sut.next()
        let result_4 = try sut.next()
        let result_5 = try sut.next()
        let result_6 = try sut.next()

        assertEqual([
            result_0, result_1, result_2, result_3, result_4, result_5, result_6,
        ], [
            stubRaw.tokens[0],
            stubRaw.tokens[1],
            stubRaw.tokens[2],
            stubRaw.tokens[1],
            stubRaw.tokens[2],
            stubRaw.tokens[3],
            nil,
        ])
    }

    /// Tests behavior of mark/restore calls when dealing with the underlying
    /// raw tokenizer, ensuring no more calls to `next()` are made than necessary
    /// to fulfill the requests.
    func testMarkRestore_rawTokenizerBehavior() throws {
        let stubRaw = stubTestRawTokenizer([
            0, 1, 2, 3,
        ])
        let sut = makeSut(stubRaw)

        _ = try sut.next()
        let mark = sut.mark()
        _ = try sut.next()
        _ = try sut.next()
        sut.restore(mark)
        _ = try sut.next()
        _ = try sut.next()
        _ = try sut.next()
        _ = try sut.next()

        assertEqual(stubRaw.next_callCount, 5)
    }

    /// Mark/restore pairs when the tokenizer has not moved should be a noop.
    func testMarkRestore_samePosition_noop() throws {
        let stubRaw = stubTestRawTokenizer([
            0,
        ])
        let sut = makeSut(stubRaw)

        let mark = sut.mark()
        sut.restore(mark)

        assertEqual(stubRaw.next_callCount, 0)
    }
}

// MARK: - Test internals

private func makeSut<T>(_ raw: T) -> Tokenizer<T> where T: RawTokenizerType {
    return Tokenizer(rawTokenizer: raw)
}

private func stubTestRawTokenizer<T>(_ tokens: [T]) -> TestRawTokenizer<T> {
    return TestRawTokenizer(tokens: tokens)
}
