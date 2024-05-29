/// Base class for producing C-style code string buffers during code generation.
public class CodeStringBuffer {
    var indentationMode: IndentationMode = .spaces(4)
    
    /// Current indentation level.
    var indentation: Int = 0

    var pendingPrefix: [PendingPrefix] = []

    /// The string buffer that represents the final file.
    var buffer: String = ""

    public init() {

    }

    /// Empties the buffer and resets the indentation level back to zero.
    public func resetState() {
        indentation = 0
        buffer = ""
    }

    /// Performs end-of-production changes to the buffer, like removing redundant
    /// line feeds from the end of the buffer.
    /// 
    /// Returns the contents of the buffer.
    public func finishBuffer() -> String {
        while buffer.hasSuffix("\n") {
            buffer.removeLast()
        }

        return buffer
    }

    /// Returns `true` if the last character of the buffer is a line feed (\n).
    public func isOnNewline() -> Bool {
        buffer.hasSuffix("\n")
    }

    /// Returns `true` if the last character of the buffer is a space or line
    /// feed (\n).
    /// Also returns `true` if the buffer is empty.
    public func isOnSpaceSeparator() -> Bool {
        buffer.isEmpty || isOnNewline() || buffer.hasSuffix(" ")
    }

    /// Returns `true` if the last two characters of the buffer are line feeds
    /// (\n\n).
    public func isOnDoubleNewline() -> Bool {
        buffer.hasSuffix("\n\n")
    }

    /// Returns the string form of the indentation to put on lines.
    public func indentationString() -> String {
        String(repeating: indentationMode.asString, count: indentation)
    }

    /// Creates a new conditional emitter that is monitoring changes from this
    /// point in the buffer.
    public func startConditionalEmitter() -> ConditionalEmitter {
        ConditionalEmitter(producer: self)
    }

    /// Increases current indentation level by one.
    public func indent() {
        indentation += 1
    }

    /// Decreases current indentation level by one.
    public func unindent() {
        indentation = max(0, indentation - 1)
    }

    /// Emits the given text into the buffer as-is.
    public func emitRaw(_ text: String) {
        buffer += text
    }

    /// Emits the given text into the buffer, automatically indenting text if
    /// the current line is empty and the incoming text is not prefixed by a newline
    /// of its own.
    /// 
    /// Does not emit a newline at the end.
    public func emit(_ text: String) {
        if !text.hasPrefix("\n") {
            ensureIndentation()
        }

        emitRaw(text)
    }

    /// Emits a line feed (\n) into the buffer.
    public func emitNewline() {
        emitRaw("\n")
    }

    /// Emits the given text on the current line and pushes a new line onto the
    /// buffer.
    public func emitLine(_ text: String) {
        emit(text)
        emitNewline()
    }

    /// Emits a space separator to separate the current stream of characters from
    /// an incoming stream in the buffer.
    public func emitSpaceSeparator() {
        emitRaw(" ")
    }

    /// Emits a line comment in the buffer.
    /// The comment is automatically prefixed with '// ', and a line feed is also
    /// added to the end of the line.
    public func emitComment(_ line: String) {
        emitLine("// \(line)")
    }

    /// Emits a block comment with the given contents. Automatically prefixes and
    /// suffixes the comment with the comment delimiters '/*' and '*/' and a line
    /// feed at the end.
    public func emitCommentBlock(_ lines: String) {
        emitLine("/* \(lines) */")
    }

    /// Emits a doc comment line in the buffer.
    /// The comment is automatically prefixed with '/// ', and a line feed is also
    /// added to the end of the line.
    public func emitDocComment(_ line: String) {
        emitLine("/// \(line)")
    }

    /// Emits a doc block comment with the given contents. Automatically prefixes
    /// and suffixes the comment with the comment delimiters '/**' and '*/' and
    /// a line feed at the end.
    public func emitDocCommentBlock(_ lines: String) {
        emitLine("/** \(lines) */")
    }

    /// Emits a pending prefix entry to the buffer, with a line feed at the
    /// end.
    public func emitPrefix(_ prefix: PendingPrefix) {
        switch prefix {
        case .docComment(let line):
            emitDocComment(line)
        case .lineComment(let line):
            emitComment(line)
        }
    }

    /// Emits all pending prefix lines, clearing them from the queue in the
    /// process.
    public func emitPendingPrefix() {
        pendingPrefix.forEach(emitPrefix)
        pendingPrefix.removeAll()
    }

    /// Calls a block for emitting contents into the buffer, finishing with a line
    /// feed at the end.
    /// In case a line feed was inserted by the block itself, no extra line feed
    /// is inserted.
    public func emitLineWith(_ block: () throws -> Void) rethrows {
        let bufferSizeBefore = buffer.count
        defer {
            if buffer.count > bufferSizeBefore {
                if !isOnNewline() {
                    emitNewline()
                }
            } else {
                emitNewline()
            }
        }
        
        try block()
    }

    /// Emits contents from a given sequence of items by passing them through a
    /// producer that may call other `emit-` functions, where this function
    /// automatically separates elements that appear in between with `separator`.
    ///
    /// Can be used to generate comma-separated list of syntax elements.
    public func emitWithSeparators<S: Sequence>(
        _ items: S,
        separator: String,
        _ producer: (S.Element) throws -> Void
    ) rethrows {

        var iterator = items.makeIterator()

        // Emit first item as-is
        guard let first = iterator.next() else {
            return
        }

        try producer(first)

        // Subsequent items require a separator
        while let next = iterator.next() {
            emit(separator)
            try producer(next)
        }
    }

    /// Backtracks whitespace in the buffer until a non-whitespace character is
    /// found.
    ///
    /// If called while the buffer is filled with only whitespace characters,
    /// the buffer is emptied completely.
    public func backtrackWhitespace() {
        buffer = buffer.trimmingWhitespaceTrail()
    }

    /// Ensures the last character of the buffer is a line feed (\n). If not, a
    /// line feed is pushed.
    public func ensureNewline() {
        if !isOnNewline() {
            emitRaw("\n")
        }
    }

    /// Ensures at least one space or line feed character is present at the end
    /// of the buffer, emitting one if none is found.
    public func ensureSpaceSeparator() {
        if !isOnSpaceSeparator() {
            emitSpaceSeparator()
        }
    }
    
    /// Ensures an empty line sits in the end of the buffer.
    /// If the buffer is empty, no change is made.
    public func ensureEmptyLine() {
        guard !buffer.isEmpty else { return }
        
        ensureDoubleNewline()
    }

    /// Ensures the last two characters of the buffer are line feeds (\n\n). If
    /// not, line feeds are pushed to the end of the buffer until there are
    /// at least two.
    public func ensureDoubleNewline() {
        guard isOnNewline() else {
            emitRaw("\n")
            emitRaw("\n")
            return
        }

        if !isOnDoubleNewline() {
            emitRaw("\n")
        }
    }

    /// Pre-fills the current line with indentation, if it is empty.
    public func ensureIndentation() {
        if isOnNewline() || buffer.isEmpty {
            emitRaw(indentationString())
        }
    }

    /// Queues a given prefix to the appended to the next non-empty line 
    public func queuePrefix(_ prefix: PendingPrefix) {
        pendingPrefix.append(prefix)
    }

    /// An object that watches for changes made to the buffer between points, and
    /// emits content conditionally only if changes to the buffer where made since
    /// the last point monitored.
    public class ConditionalEmitter {
        // TODO: Change the state being watched to something lighter like a simple counter integer on SwiftProducer
        typealias State = String

        private let producer: CodeStringBuffer
        private var state: State

        init(producer: CodeStringBuffer) {
            self.producer = producer
            self.state = producer.buffer
        }

        private func _recordState() {
            self.state = producer.buffer
        }

        private func _hasChanged() -> Bool {
            self.state != producer.buffer
        }

        /// Conditionally executes a given block if the buffer has been changed
        /// since this object was created, or since the last time it emitted
        /// something.
        /// 
        /// This call counts as emitting, even if the buffer has not been modified.
        public func conditional(_ block: (CodeStringBuffer) throws -> Void) rethrows {
            if _hasChanged() {
                defer {  _recordState() }
                try block(producer)
            }
        }

        /// Conditionally emits a given text if the buffer has been changed
        /// since this object was created, or since the last time it emitted
        /// something.
        public func emit(_ text: String) {
            if _hasChanged() {
                producer.emit(text)
                _recordState()
            }
        }

        /// Conditionally calls `ensureEmptyLine` on the producer if the buffer
        /// has been changed since this object was created, or since the last
        /// time it emitted something.
        public func ensureEmptyLine() {
            if _hasChanged() {
                producer.ensureEmptyLine()
                _recordState()
            }
        }
    }

    enum IndentationMode {
        case spaces(Int)
        case tabs(Int)

        var asString: String {
            switch self {
            case .spaces(let count):
                return String(repeating: " ", count: count)

            case .tabs(let count):
                return String(repeating: "\t", count: count)
            }
        }
    }

    /// Specifies a line to prefixed to the next non-empty line emitted by the
    /// producer. Used to suffix declarations with comments.
    public enum PendingPrefix {
        case lineComment(String)
        case docComment(String)
    }
}

// MARK: - Misc helpers

public extension CodeStringBuffer {
    /// Invokes the contents of the given block while temporarily indenting the
    /// producer by one.
    func indented(_ block: () throws -> Void) rethrows {
        indent()
        defer { unindent() }
        try block()
    }

    /// Emits a left brace, a newline, indents by one, invokes `block` and
    /// finally unindent before emitting a right brace on a separate line:
    ///
    /// ```
    /// <current line's contents> {
    ///     <block()>
    /// }
    /// ```
    func emitBlock(_ block: () throws -> Void) rethrows {
        emitLine("{")
        defer {
            ensureNewline()
            emitLine("}")
        }
        try indented(block)
    }

    /// Emits a left brace, a newline, indents by one, invokes `block` and
    /// finally unindent before emitting a right brace on a separate line,
    /// removing any empty line trailing the right brace:
    ///
    /// ```
    /// <current line's contents> {
    ///     <block()>
    /// }
    /// ```
    /// 
    /// Used to generate type member blocks that may generated newlines after
    /// each member.
    func emitMembersBlock(_ block: () throws -> Void) rethrows {
        emitLine("{")
        defer {
            backtrackWhitespace()
            ensureNewline()
            emitLine("}")
        }
        try indented(block)
    }

    /// Emits a left brace, a newline, indents by one, invokes `block` and
    /// finally unindents before emitting a right brace without issuing a newline.
    ///
    /// ```
    /// <current line's contents> {
    ///     <block()>
    /// }
    /// ```
    /// 
    /// Used to generate blocks within expressions or function calls.
    func emitInlinedBlock(_ block: () throws -> Void) rethrows {
        emitLine("{")
        defer {
            ensureNewline()
            emit("}")
        }
        try indented(block)
    }

    /// Emits a left brace, a newline, and a right brace on a separate line:
    ///
    /// ```
    /// <current line's contents> {
    /// }
    /// ```
    func emitEmptyBlock() {
        emitLine("{")
        ensureNewline()
        emitLine("}")
    }
}
