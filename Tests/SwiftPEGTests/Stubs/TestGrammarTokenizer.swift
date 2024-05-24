import SwiftPEG

class TestGrammarTokenizer {
    let tokens: [SwiftPEGGrammar.GrammarToken]
    var index: Int = 0

    /// Lists errors to serve at specific indices instead of tokens.
    var errorIndices: [Int: Error] = [:]

    init(tokens: [SwiftPEGGrammar.GrammarToken]) {
        self.tokens = tokens
    }

    func mockError(at index: Int, _ error: Error = GenericError()) {
        errorIndices[index] = error
    }

    var next_calls: [Result<SwiftPEGGrammar.GrammarToken?, Error>] = []
    var next_callCount: Int = 0

    func next() throws -> (token: SwiftPEGGrammar.GrammarToken, location: FileSourceLocation)? {
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
        return (tokens[index], .init(line: 1, column: index))
    }

    struct GenericError: Error { }
}

extension TestGrammarTokenizer: RawTokenizerType {
    typealias Token = SwiftPEGGrammar.GrammarToken
    typealias Location = FileSourceLocation

    var isEOF: Bool { index >= tokens.count }
}
