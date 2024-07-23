/// A type of error produced by tokenizers.
public protocol TokenizerError: Error, CustomStringConvertible {
}

/// A type of error produced by parsers.
public protocol ParserError: Error, CustomStringConvertible {
    /// Gets a human- and machine readable location diagnostic string that
    /// represents the source of this error.
    var location: String { get }
}
