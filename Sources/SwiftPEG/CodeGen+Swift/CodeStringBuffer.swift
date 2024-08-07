/// Base class for producing C-style code string buffers during code generation.
public class CodeStringBuffer {
    var indentationMode: IndentationMode = .spaces(4)

    /// Current indentation level.
    var indentation: Int = 0

    var pendingPrefix: [PendingPrefix] = []

    /// The string buffer that represents the final file.
    var buffer: String

    public convenience init() {
        self.init(startingBuffer: "")
    }

    fileprivate init(startingBuffer: String) {
        self.buffer = startingBuffer
    }

    /// Copies miscellaneous state from another code string buffer object.
    ///
    /// - note: Does't copy `pendingPrefix` or `buffer` itself.
    fileprivate func copyState(from other: CodeStringBuffer) {
        self.indentationMode = other.indentationMode
        self.indentation = other.indentation
    }

    /// Empties the buffer and resets the indentation level back to zero.
    public func resetState() {
        indentation = 0
        buffer = ""
    }

    /// Performs end-of-production changes to the buffer, and optionally adds a
    /// trailing newline at the end of the buffer.
    ///
    /// If `addTrailingNewline` is `false`, any trailing newlines are instead
    /// removed from the buffer.
    ///
    /// Returns the contents of the buffer.
    public func finishBuffer(addTrailingNewline: Bool = false) -> String {
        while buffer.hasSuffix("\n") {
            buffer.removeLast()
        }
        if addTrailingNewline {
            ensureNewline()
        }

        return buffer
    }

    /// Creates a new conditional emitter that is monitoring changes from this
    /// point in the buffer.
    public func startConditionalEmitter() -> ConditionalEmitter {
        ConditionalEmitter(codeBuffer: self)
    }

    /// Creates a new insertion marker at the current end-point of the buffer.
    ///
    /// - note: Behavior of code if multiple overlapping insertion points exist
    /// at the same time, and are used to emit text interchangeably is not defined.
    public func makeInsertionMarker() -> InsertionMarker {
        InsertionMarker(codeBuffer: self)
    }

    /// Creates an insertion marker that emits code at the current position in
    /// the buffer even after text has been emitted past it. Invokes the closure
    /// only once, even if called multiple times.
    ///
    /// - note: Behavior of code is not defined if multiple overlapping insertion
    /// points exist at the same time and are used to emit text interchangeably.
    /// - seealso: ``makeInsertionMarker()``
    public func startDelayedEmission(_ block: @escaping (CodeStringBuffer) -> Void) -> DelayedEmissionMarker {
        DelayedEmissionMarker(marker: makeInsertionMarker(), block)
    }

    /// Returns `true` if the last character of the buffer is a line feed character,
    /// according to `Character.isNewline`.
    public func isOnNewline() -> Bool {
        buffer.last?.isNewline ?? false
    }

    /// Returns `true` if the last character of the buffer is a space or line
    /// feed (\n).
    /// Also returns `true` if the buffer is empty.
    public func isOnSpaceSeparator() -> Bool {
        buffer.isEmpty || (buffer.last?.isWhitespace ?? false)
    }

    /// Returns `true` if the last two characters of the buffer are line feed
    /// characters, according to `Character.isNewline`.
    public func isOnDoubleNewline() -> Bool {
        guard buffer.count > 1 else {
            return false
        }
        let lastIndex = buffer.index(buffer.indices.endIndex, offsetBy: -1)
        let secondLastIndex = buffer.index(buffer.indices.endIndex, offsetBy: -2)
        let last = buffer[lastIndex]
        let secondLast = buffer[secondLastIndex]

        return last.isNewline && secondLast.isNewline
    }

    /// Returns the string form of the indentation to put on lines.
    public func indentationString() -> String {
        String(repeating: indentationMode.asString, count: indentation)
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
    public func emitRaw(_ text: some StringProtocol) {
        buffer += text
    }

    /// Emits the given text into the buffer, automatically indenting text if
    /// the current line is empty and the incoming text is not prefixed by a newline
    /// of its own.
    ///
    /// Does not emit a newline at the end.
    ///
    /// If `text` is empty, no change to the buffer is made.
    public func emit(_ text: some StringProtocol) {
        if text.isEmpty { return }
        if !(text.first?.isNewline ?? false) {
            ensureIndentation()
        }

        emitRaw(text)
    }

    /// Emits a line feed (`\n`) into the buffer.
    public func emitNewline() {
        emitRaw("\n")
    }

    /// Emits the given text, first breaking up each line, then emitting the lines
    /// one at a time with ``emitLine(_:)``.
    ///
    /// Indentation of the incoming text is appended to the current indentation
    /// level of the buffer.
    public func emitMultiline(_ text: some StringProtocol) {
        let lines = text.split(omittingEmptySubsequences: false) {
            $0.isNewline
        }

        for line in lines {
            emitLine(line)
        }
    }

    /// Emits the given text on the current line and pushes a new line onto the
    /// buffer.
    public func emitLine(_ text: some StringProtocol) {
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
    public func emitComment(_ line: some StringProtocol) {
        emitLine("// \(line)")
    }

    /// Emits a block comment with the given contents. Automatically prefixes and
    /// suffixes the comment with the comment delimiters '/*' and '*/' and a line
    /// feed at the end.
    public func emitCommentBlock(_ lines: some StringProtocol) {
        emitLine("/* \(lines) */")
    }

    /// Emits a doc comment line in the buffer.
    /// The comment is automatically prefixed with '/// ', and a line feed is also
    /// added to the end of the line.
    public func emitDocComment(_ line: some StringProtocol) {
        emitLine("/// \(line)")
    }

    /// Emits a doc block comment with the given contents. Automatically prefixes
    /// and suffixes the comment with the comment delimiters '/**' and '*/' and
    /// a line feed at the end.
    public func emitDocCommentBlock(_ lines: some StringProtocol) {
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

    /// Emits contents from a given sequence of string items by calling `self.emit()`
    /// on each element, automatically separating elements that appear in between
    /// with `separator`.
    ///
    /// Can be used to generate comma-separated list of syntax elements.
    public func emitWithSeparators(
        _ items: some Sequence<some StringProtocol>,
        separator: some StringProtocol
    ) {

        var iterator = items.makeIterator()

        // Emit first item as-is
        guard let first = iterator.next() else {
            return
        }

        emit(first)

        // Subsequent items require a separator
        while let next = iterator.next() {
            emit(separator)
            emit(next)
        }
    }

    /// Emits contents from a given sequence of items by passing them through a
    /// producer that may call other `emit-` functions, where this function
    /// automatically separates elements that appear in between with `separator`.
    ///
    /// Can be used to generate comma-separated list of syntax elements.
    public func emitWithSeparators<S: Sequence>(
        _ items: S,
        separator: some StringProtocol,
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
    ///
    /// Unless the buffer is empty, this function behaves exactly the same as
    /// `ensureDoubleNewline()`.
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

    /// A structure that wraps a delayed code emission that may or may not be
    /// inserted in a past position of the buffer.
    ///
    /// - seealso: ``InsertionMarker``
    public struct DelayedEmissionMarker {
        let marker: InsertionMarker
        let emission: (CodeStringBuffer) -> Void

        init(marker: InsertionMarker, _ emission: @escaping (CodeStringBuffer) -> Void) {
            self.marker = marker
            self.emission = emission
        }

        /// Emits this delayed emission into the position that this marker was
        /// created at.
        ///
        /// - note: Only invokes/emits the associated production closure.
        func emit() {
            self.marker.insertOnce(emission)
        }
    }

    /// An object that serves as an insertion point into the buffer that can be
    /// used to insert text at a previous buffer position after text has been
    /// emitted past it.
    public class InsertionMarker {
        private let codeBuffer: CodeStringBuffer
        private let index: String.Index
        private var hasInserted: Bool = false

        convenience init(codeBuffer: CodeStringBuffer) {
            self.init(codeBuffer: codeBuffer, index: codeBuffer.buffer.endIndex)
        }

        init(codeBuffer: CodeStringBuffer, index: String.Index) {
            self.codeBuffer = codeBuffer
            self.index = index
        }

        /// Opens a temporary context for inserting text, invoking a given closure
        /// only if a previous insertion wasn't performed with this insertion
        /// marker.
        ///
        /// - seealso: ``insert(_:)``
        @discardableResult
        func insertOnce(_ producer: (CodeStringBuffer) throws -> Void) rethrows -> Bool {
            guard !hasInserted else {
                return false
            }

            return try insert(producer)
        }

        /// Opens a temporary context for inserting text into the buffer at the
        /// point that this insertion marker points to.
        ///
        /// The buffer passed to the closure behaves as if the text of the original
        /// buffer ended at this insertion point. If the text of the original
        /// buffer was erased past this insertion point somehow (with e.g.
        /// `backtrackWhitespace`), then the method returns `false`.
        ///
        /// When the closure finishes and (if throwable) doesn't throw, and the
        /// provided temporary buffer wasn't erased back to a smaller size, the
        /// contents that where written into the temporary buffer are inserted
        /// into the original buffer at the appropriate location, and the method
        /// returns `true`.
        @discardableResult
        func insert(_ producer: (CodeStringBuffer) throws -> Void) rethrows -> Bool {
            hasInserted = true

            guard codeBuffer.buffer.endIndex >= index else {
                return false
            }
            let temporary = CodeStringBuffer(
                startingBuffer: String(codeBuffer.buffer[..<index])
            )
            temporary.copyState(from: codeBuffer)

            try producer(temporary)
            guard temporary.buffer.endIndex > index else {
                return false
            }

            // Sanity check that the original buffer didn't change while the
            // closure was running
            guard codeBuffer.buffer.endIndex >= index else {
                return false
            }

            let insertSlice = temporary.buffer[index...]
            codeBuffer.buffer.insert(contentsOf: insertSlice, at: index)

            return true
        }
    }

    /// An object that watches for changes made to the buffer between points, and
    /// emits content conditionally only if changes to the buffer where made since
    /// the last point monitored.
    public class ConditionalEmitter {
        // TODO: Change the state being watched to something lighter like a
        // TODO: simple counter integer on CodeStringBuffer
        typealias State = String

        private let codeBuffer: CodeStringBuffer
        private var state: State

        init(codeBuffer: CodeStringBuffer) {
            self.codeBuffer = codeBuffer
            self.state = codeBuffer.buffer
        }

        private func _recordState() {
            self.state = codeBuffer.buffer
        }

        /// Returns whether the contents of the underlying buffer have changed
        /// since the last time this conditional emitter emitted something.
        private func _hasChanged() -> Bool {
            self.state != codeBuffer.buffer
        }

        /// Conditionally executes a given block if the buffer has been changed
        /// since this object was created, or since the last time it emitted
        /// something.
        ///
        /// This call counts as emitting, even if the buffer has not been modified.
        public func conditional(_ block: (CodeStringBuffer) throws -> Void) rethrows {
            if _hasChanged() {
                defer {  _recordState() }
                try block(codeBuffer)
            }
        }

        /// Conditionally emits a given text if the buffer has been changed
        /// since this object was created, or since the last time it emitted
        /// something.
        public func emit(_ text: String) {
            if _hasChanged() {
                codeBuffer.emit(text)
                _recordState()
            }
        }

        /// Conditionally calls `ensureEmptyLine` on the producer if the buffer
        /// has been changed since this object was created, or since the last
        /// time it emitted something.
        public func ensureEmptyLine() {
            if _hasChanged() {
                codeBuffer.ensureEmptyLine()
                _recordState()
            }
        }
    }

    enum IndentationMode: Hashable {
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

    /// Emits `lead`, a space if `lead` does not end with one, then a left brace,
    /// a newline, indents by one, invokes `block` and finally unindents before
    /// emitting a right brace on a separate line:
    ///
    /// ```
    /// <current line's contents><lead> {
    ///     <block()>
    /// }
    /// ```
    func emitBlock(_ lead: some StringProtocol, _ block: () throws -> Void) rethrows {
        emit(lead)
        ensureSpaceSeparator()
        emitLine("{")
        defer {
            ensureNewline()
            emitLine("}")
        }
        try indented(block)
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
