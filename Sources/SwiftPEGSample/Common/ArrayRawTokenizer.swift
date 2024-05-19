import SwiftPEG

/// Raw tokenizer that sources tokens from a backing array.
class ArrayRawTokenizer<T: TokenType> {
    let tokens: [T]
    var index: Int = 0

    init(tokens: [T]) {
        self.tokens = tokens
    }

    func next() throws -> T? {
        guard index < tokens.count else { return nil }
        defer { index += 1 }
        return tokens[index]
    }
}

extension ArrayRawTokenizer: RawTokenizerType {
    typealias Token = T

    var isEOF: Bool { index >= tokens.count }
}
