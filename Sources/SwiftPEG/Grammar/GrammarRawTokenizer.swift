/// A tokenizer for SwiftPEG grammar files.
/// Reads tokens out of a character stream.
public class GrammarRawTokenizer: RawTokenizerType {
    public typealias Token = SwiftPEGGrammar.GrammarToken
    public typealias Location = FileSourceLocation

    @usableFromInline
    var _stream: StringStream<String>

    /// Internal source location tracker
    @usableFromInline
    internal var _location: FileSourceLocation

    @inlinable
    public var isEOF: Bool {
        _stream.isEof
    }

    @inlinable
    public init(source: String) {
        self._stream = StringStream(source: source)
        _location = FileSourceLocation(line: 1, column: 1)
    }

    @inlinable
    public func next() throws -> (token: Token, location: Location)? {
        skipToContent()

        guard !_stream.isEof else {
            return nil
        }

        // TODO: Maybe backtracking like this defeats the purpose of having a
        // TODO: 'raw' stream in the first place?
        let state = _stream.save()

        guard
            let token = Token.from(stream: &_stream),
            token.length > 0
        else {
            throw Error.unknownToken(index: _stream.index, _stream.peek())
        }

        defer {
            // Backtrack and advance back so we have appropriate line/column counts
            _stream.restore(state)
            advance(by: token.length)
            _stream.markSubstringStart()
        }

        return (token, _location)
    }

    @inlinable
    internal func skipToContent() {
        var lastIndex = _stream.index
        repeat {
            lastIndex = _stream.index
            skipComments()
        } while !_stream.isEof && lastIndex != _stream.index
    }

    @inlinable
    internal func skipLine() {
        while !_stream.isEof && _stream.peek() != "\n" {
            advance(by: 1)
        }
        if !_stream.isEof {
            advance(by: 1) // Skip linefeed character
        }
    }

    @inlinable
    internal func skipComments() {
        while !_stream.isEof && _stream.isNext("#") {
            skipLine()
        }
    }

    @inlinable
    internal func advance(by count: Int) {
        for _ in 0..<count {
            if _stream.isEof { break }

            if _stream.peek() == "\n" {
                _location.column = 0
                _location.line += 1
            }

            _stream.advance()
            _location.column += 1
        }
    }

    public enum Error: TokenizerError {
        case unknownToken(index: String.Index, Character)

        public var description: String {
            switch self {
            case .unknownToken(_, let tok):
                return "Unknown token \(tok)"
            }
        }
    }
}
