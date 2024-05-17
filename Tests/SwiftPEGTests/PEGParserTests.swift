import XCTest

@testable import SwiftPEG

class PEGParserTests: XCTestCase {
    func testExpect_success() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(0)

        assertEqual(result, 0)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.restore_callCount, 0)
        assertEqual(stubTokenizer.tokenIndex, 1)
    }

    func testExpect_failure() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(1)

        assertEqual(result, nil)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.restore_callCount, 1)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    func testExpect_failure_repeated_isCached() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(1)
        _ = try sut.expect(1)
        _ = try sut.expect(1)

        assertEqual(result, nil)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.restore_callCount, 3)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    func testExpect_failure_repeated_isCached_perToken() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(1)
        _ = try sut.expect(2)
        _ = try sut.expect(3)

        assertEqual(result, nil)
        assertEqual(stubTokenizer.next_callCount, 3)
        assertEqual(stubTokenizer.restore_callCount, 3)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }
}

// MARK: - Test internals

private func makeSut<T>(_ raw: T) -> PEGParser<T> where T: RawTokenizerType {
    return PEGParser(raw: raw)
}

private func makeSut<T>(_ tokenizer: Tokenizer<T>) -> PEGParser<T> where T: RawTokenizerType {
    return PEGParser(tokenizer: tokenizer)
}

private func stubTestTokenizer<T>(_ tokens: [T]) -> TestTokenizer<TestRawTokenizer<T>> {
    return TestTokenizer(rawTokenizer: stubTestRawTokenizer(tokens))
}

private func stubTestRawTokenizer<T>(_ tokens: [T]) -> TestRawTokenizer<T> {
    return TestRawTokenizer(tokens: tokens)
}
