/// A tokenizer for SwiftPEG grammar files.
/// Reads tokens out of a character stream.
public class MetagrammarRawTokenizer: RawTokenizerType {
    public typealias Token = Metagrammar.MetagrammarToken

    private var _source: String
    private var _index: String.Index

    public var isEOF: Bool {
        _index >= _source.endIndex
    }

    public init(source: String) {
        self._source = source
        _index = source.startIndex
    }

    public func next() throws -> Token? {
        _skipWhitespace()

        guard _index < _source.endIndex else {
            return nil
        }

        guard
            let token = Token.from(string: _source[_index...]),
            token.tokenUTF8Length > 0
        else {
            throw Error.unknownToken(index: _index)
        }

        _advance(by: token.tokenUTF8Length)

        return token
    }

    private func _skipWhitespace() {
        while _index < _source.endIndex && _source[_index].isWhitespace {
            _advance(by: 1)
        }
    }

    private func _advance(by count: Int) {
        _index = _source.utf8.index(_index, offsetBy: count)
    }

    enum Error: TokenizerError {
        case unknownToken(index: String.Index)
    }
}
