import Testing

@testable import SwiftPEG

struct TokenizerTests {
    /// Ensure that the tokenizer doesn't probe the raw tokenizer before a request
    /// for tokens is made.
    @Test
    func ephemeral() throws {
        let rawStub = stubTestRawTokenizer([0])
        _ = makeSut(rawStub)

        assertEqual(rawStub.next_callCount, 0)
    }

    /// On initialization, `isEOF` must be `false` if the raw tokenizer's own
    /// `isEOF` is `false`.
    @Test
    func isEOF_onInit_notEof_returnsFalse() throws {
        let rawStub = stubTestRawTokenizer([
            0,
        ])
        let sut = makeSut(rawStub)

        assertFalse(sut.isEOF)
    }

    /// On initialization, `isEOF` must be `false` even if the raw tokenizer's own
    /// `isEOF` is `true` until the tokenizer attempts to parse a token.
    @Test
    func isEOF_onInit_atEof_returnsFalseUntilPeekToken() throws {
        let rawStub = stubTestRawTokenizer(Array<Int>())
        let sut = makeSut(rawStub)

        assertFalse(sut.isEOF)

        _=try sut.peekToken()

        assertTrue(sut.isEOF)
    }

    /// After `next()`, `isEOF` must be `false` if the raw tokenizer's own
    /// `isEOF` is `false`.
    @Test
    func isEOF_onNext_notEof_returnsFalse() throws {
        let rawStub = stubTestRawTokenizer([
            0, 1,
        ])
        let sut = makeSut(rawStub)

        _=try sut.next()

        assertFalse(sut.isEOF)
    }

    /// After `next()`, `isEOF` must be `true` if the raw tokenizer's own
    /// `isEOF` is `true`.
    @Test
    func isEOF_onNext_atEof_returnsTrue() throws {
        let rawStub = stubTestRawTokenizer([
            0,
        ])
        let sut = makeSut(rawStub)

        _=try sut.next()

        assertTrue(sut.isEOF)
    }

    /// Calling `Tokenizer.next()` when not at EOF should return the token from
    /// the raw tokenizer.
    @Test
    func next_nonNil_returnsToken() throws {
        let rawStub = stubTestRawTokenizer([
            0, 1, 2, 3,
        ])
        let sut = makeSut(rawStub)

        let result_0 = try sut.next()
        let result_1 = try sut.next()
        let result_2 = try sut.next()

        assertEqual(result_0?.rawToken, rawStub.tokens[0])
        assertEqual(result_1?.rawToken, rawStub.tokens[1])
        assertEqual(result_2?.rawToken, rawStub.tokens[2])
        assertEqual(rawStub.next_callCount, 3)
        assertSuccessesEqual(rawStub.next_calls, [
            .success(rawStub.tokens[0]),
            .success(rawStub.tokens[1]),
            .success(rawStub.tokens[2]),
        ])
    }

    /// Calling `Tokenizer.next()` when at EOF should return nil.
    @Test
    func next_eof_returnsNil() throws {
        let rawStub = stubTestRawTokenizer([
            0,
        ])
        let sut = makeSut(rawStub)

        _=try sut.next()
        let result = try sut.next()

        assertEqual(result?.rawToken, nil)
        assertEqual(rawStub.next_callCount, 1)
        assertSuccessesEqual(rawStub.next_calls, [
            .success(rawStub.tokens[0]),
        ])
    }

    /// Calling `Tokenizer.next()` should return any error that the raw tokenizer
    /// has raised.
    @Test
    func next_forwardsErrors() throws {
        let rawStub = stubTestRawTokenizer([
            0,
        ])
        rawStub.mockError(at: 0)
        let sut = makeSut(rawStub)

        assertThrows(sut.next)
        assertThrows(sut.next)
        assertEqual(rawStub.next_callCount, 2)
        assertFailures(rawStub.next_calls)
    }
}

// MARK: - Test internals

private func makeSut<T>(_ raw: T) -> Tokenizer<T> where T: RawTokenizerType {
    return Tokenizer(rawTokenizer: raw)
}

private func stubTestRawTokenizer<T>(_ tokens: [T]) -> TestRawTokenizer<T> {
    return TestRawTokenizer(tokens: tokens)
}
