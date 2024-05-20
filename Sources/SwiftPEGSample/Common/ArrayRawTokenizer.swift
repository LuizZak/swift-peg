import SwiftPEG

/// Raw tokenizer that sources tokens from a backing array.
class ArrayRawTokenizer<T: TokenType> {
    let tokens: [T]
    var index: Int = 0

    init(tokens: [T]) {
        self.tokens = tokens
    }

    func next() throws -> (token: T, location: Int)? {
        guard index < tokens.count else { return nil }
        defer { index += 1 }
        return (tokens[index], index)
    }
}

extension ArrayRawTokenizer: RawTokenizerType {
    typealias Token = T
    typealias Location = Int

    var isEOF: Bool { index >= tokens.count }
}
