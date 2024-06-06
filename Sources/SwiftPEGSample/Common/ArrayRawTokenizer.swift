import SwiftPEG

/// Raw tokenizer that sources tokens from a backing array.
class ArrayRawTokenizer<T: RawTokenType> {
    let tokens: [T]
    var index: Int = 0

    init(tokens: [T]) {
        self.tokens = tokens
    }

    func next() throws -> (rawToken: T, location: FileSourceLocation)? {
        guard index < tokens.count else { return nil }
        defer { index += 1 }
        return (tokens[index], FileSourceLocation(line: 0, column: 1))
    }
}

extension ArrayRawTokenizer: RawTokenizerType {
    typealias Token = T
    typealias Location = FileSourceLocation

    var isEOF: Bool { index >= tokens.count }
}
