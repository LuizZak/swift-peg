/// A tokenizer for SwiftPEG grammar files.
/// Reads tokens out of a character stream.
public class MetagrammarRawTokenizer: RawTokenizerType {
    public typealias Token = Metagrammar.MetagrammarToken
    public typealias Location = FileSourceLocation

    @usableFromInline
    internal var _source: String
    @usableFromInline
    internal var _index: String.Index

    /// Internal source location tracker
    @usableFromInline
    internal var _location: FileSourceLocation

    @inlinable
    public var isEOF: Bool {
        _index >= _source.endIndex
    }

    @inlinable
    public init(source: String) {
        self._source = source
        _index = source.startIndex
        _location = FileSourceLocation(line: 1, column: 1)
    }

    @inlinable
    public func next() throws -> (token: Token, location: Location)? {
        skipToContent()

        guard _index < _source.endIndex else {
            return nil
        }

        guard
            let token = Token.from(string: _source[_index...]),
            token.tokenUTF8Length > 0
        else {
            throw Error.unknownToken(index: _index)
        }

        defer { advance(by: token.tokenUTF8Length) }

        return (token, _location)
    }

    @inlinable
    internal func skipToContent() {
        var lastIndex = _index
        repeat {
            lastIndex = _index
            skipComments()
        } while _index < _source.endIndex && lastIndex != _index
    }

    @inlinable
    internal func skipLine() {
        while _index < _source.endIndex && _source[_index] != "\n" {
            advance(by: 1)
        }
        if _index < _source.endIndex {
            advance(by: 1) // Skip linefeed character
        }
    }

    @inlinable
    internal func skipComments() {
        while _index < _source.endIndex && _source[_index] == "#" {
            skipLine()
        }
    }

    @inlinable
    internal func advance(by count: Int) {
        for _ in 0..<count {
            if _source[_index] == "\n" {
                _location.column = 0
                _location.line += 1
            }

            _index = _source.utf8.index(after: _index)
            _location.column += 1
        }
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
