/// A tokenizer for SwiftPEG grammar files.
/// Reads tokens out of a character stream.
public class MetagrammarRawTokenizer: RawTokenizerType {
    public typealias Token = Metagrammar.MetagrammarToken

    @usableFromInline
    internal var _source: String
    @usableFromInline
    internal var _index: String.Index

    @inlinable
    public var isEOF: Bool {
        _index >= _source.endIndex
    }

    @inlinable
    public init(source: String) {
        self._source = source
        _index = source.startIndex
    }

    @inlinable
    public func next() throws -> Token? {
        skipWhitespace()

        guard _index < _source.endIndex else {
            return nil
        }

        guard
            let token = Token.from(string: _source[_index...]),
            token.tokenUTF8Length > 0
        else {
            throw Error.unknownToken(index: _index)
        }

        advance(by: token.tokenUTF8Length)

        return token
    }

    @inlinable
    internal func skipWhitespace() {
        while _index < _source.endIndex && _source[_index].isWhitespace {
            advance(by: 1)
        }
    }

    @inlinable
    internal func advance(by count: Int) {
        _index = _source.utf8.index(_index, offsetBy: count)
    }

    public enum Error: TokenizerError {
        case unknownToken(index: String.Index)

        public var description: String {
            switch self {
            case .unknownToken:
                return "Unknown token"
            }
        }
    }
}
