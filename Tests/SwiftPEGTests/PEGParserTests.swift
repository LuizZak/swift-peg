import XCTest
import Testing

@testable import SwiftPEG

struct PEGParserTests {
    @Test
    func expect_success() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(0)

        assertEqual(result?.rawToken, 0)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.restore_callCount, 0)
        assertEqual(stubTokenizer.tokenIndex, 1)
    }

    @Test
    func expect_failure() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(1)

        assertEqual(result?.rawToken, nil)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.restore_callCount, 1)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    @Test
    func expect_failure_repeated_isNotCached() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result_0 = try sut.expect(1)
        let result_1 = try sut.expect(1)
        let result_2 = try sut.expect(1)

        assertEqual(result_0?.rawToken, nil)
        assertEqual(result_1?.rawToken, nil)
        assertEqual(result_2?.rawToken, nil)
        assertEqual(stubTokenizer.next_callCount, 3)
        assertEqual(stubTokenizer.restore_callCount, 3)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    @Test
    func expect_failure_repeated_isNotCached_perToken() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.expect(1)
        _ = try sut.expect(2)
        _ = try sut.expect(3)

        assertEqual(result?.rawToken, nil)
        assertEqual(stubTokenizer.next_callCount, 3)
        assertEqual(stubTokenizer.restore_callCount, 3)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    @Test
    func expect_restoresLocationIfSuccessful() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let mark = sut.mark()
        _ = try sut.expect(0)
        sut.restore(mark)
        let result = try sut.expect(0)

        assertEqual(result?.rawToken, 0)
        assertEqual(stubTokenizer.next_callCount, 2)
        assertEqual(stubTokenizer.tokenIndex, 1)
    }

    @Test
    func gather() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 0, 1, 0, 1, 0, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(
            try sut.gather(
                separator: {
                    try sut.expect(1)
                }, item: {
                    try sut.expect(0)
                })
        )

        assertEqual(result.map(\.rawToken), [0, 0, 0, 0])
        assertEqual(stubTokenizer.next_callCount, 8)
        assertEqual(stubTokenizer.tokenIndex, 7)
    }

    @Test
    func gather_trailingSeparator_returnsNil() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 0, 1, 0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try assertUnwrap(
            try sut.gather(
                separator: {
                    try sut.expect(1)
                }, item: {
                    try sut.expect(0)
                })
        )

        assertEqual(result.map(\.rawToken), [0, 0, 0])
        assertEqual(stubTokenizer.next_callCount, 7)
        assertEqual(stubTokenizer.tokenIndex, 5)
    }

    @Test
    func positiveLookahead_success() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.positiveLookahead { try sut.expect(0) }

        assertTrue(result)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    @Test
    func positiveLookahead_failure() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.positiveLookahead { try sut.expect(1) }

        assertFalse(result)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    @Test
    func negativeLookahead_success() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.negativeLookahead { try sut.expect(1) }

        assertTrue(result)
        assertEqual(stubTokenizer.next_callCount, 1)
        assertEqual(stubTokenizer.tokenIndex, 0)
    }

    @Test
    func negativeLookahead_failure() throws {
        let stubTokenizer = stubTestTokenizer([
            0, 1, 2,
        ])
        let sut = makeSut(stubTokenizer)

        let result = try sut.negativeLookahead { try sut.expect(0) }

        assertFalse(result)
        assertEqual(stubTokenizer.next_callCount, 1)
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
