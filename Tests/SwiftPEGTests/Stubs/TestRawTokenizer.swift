import SwiftPEG

class TestRawTokenizer<T: Hashable> {
    let tokens: [T]
    var index: Int = 0

    /// Lists errors to serve at specific indices instead of tokens.
    var errorIndices: [Int: Error] = [:]

    init(tokens: [T]) {
        self.tokens = tokens
    }

    func mockError(at index: Int, _ error: Error = GenericError()) {
        errorIndices[index] = error
    }

    var next_calls: [Result<T?, Error>] = []
    var next_callCount: Int = 0

    func next() throws -> T? {
        next_callCount += 1

        guard !isEOF else {
            next_calls.append(.success(nil))
            return nil
        }

        if let error = errorIndices[index] {
            next_calls.append(.failure(error))
            throw error
        }

        defer { index += 1 }

        next_calls.append(.success(tokens[index]))
        return tokens[index]
    }

    struct GenericError: Error { }
}

extension TestRawTokenizer: RawTokenizerType {
    typealias TokenType = T

    var isEOF: Bool { index >= tokens.count }
}
