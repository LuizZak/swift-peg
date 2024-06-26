import SwiftPEG

class TestRawTokenizer<T: RawTokenType> {
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

    func next() throws -> (rawToken: T, location: Int)? {
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
        return (tokens[index], index)
    }

    struct GenericError: Error { }
}

extension TestRawTokenizer: RawTokenizerType {
    typealias Token = T
    typealias Location = Int

    var isEOF: Bool { index >= tokens.count }
}

extension Int: RawTokenType {
    public var kind: Int {
        return self
    }

    public var string: String {
        self.description
    }

    public var length: Int {
        return 1
    }

    public static func produceDummy(_ kind: TokenKind) -> Self { kind }
}

extension Int: TokenKindType {
    public static var whitespace: Int { -999 }
}
