/// A location for use in raw tokenizers that read contents from a file string
/// buffer that can be located in terms of line/columns.
public struct FileSourceLocation: Hashable, CustomStringConvertible {
    /// The 1-based line index for this location.
    public var line: Int

    /// The 1-based column index for this location.
    public var column: Int

    @inlinable
    public var description: String {
        "line \(line) column \(column)"
    }

    @inlinable
    public init(line: Int, column: Int) {
        self.line = line
        self.column = column
    }
}

extension FileSourceLocation: Comparable {
    /// Compares two file source locations and returns `true` if `lhs` precedes
    /// `rhs` in the file, when scanning the file line-by-line, from left to right.
    /// 
    /// Assumes that `lhs` and `rhs` are locations in the same file buffer.
    @inlinable
    public static func < (lhs: Self, rhs: Self) -> Bool {
        if lhs.line < rhs.line {
            return true
        }
        if lhs.line == rhs.line {
            return lhs.column < rhs.column
        }

        return false
    }
}
