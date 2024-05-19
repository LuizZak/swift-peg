/// A type of error produced by tokenizers.
public protocol TokenizerError: Error, CustomStringConvertible {
}

/// A type of error produced by parsers.
public protocol ParserError: Error, CustomStringConvertible {
}
