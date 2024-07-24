import XCTest

@testable import SwiftPEG

class CodeStringBufferTests: XCTestCase {
    func testEphemeral() {
        let sut = makeSut()

        sut.assertBuffer(self, #""#)
        assertEqual(sut.indentation, 0)
        assertEqual(sut.indentationMode, .spaces(4))
    }

    func testResetState() {
        let sut = makeSut()
        sut.emitLine("line")
        sut.indent()

        sut.resetState()

        sut.assertBuffer(self, #""#)
        assertEqual(sut.indentation, 0)
    }

    func testFinishBuffer_addTrailingNewline_false() {
        let sut = makeSut()
        sut.emitLine("line")
        sut.emitNewline()

        let result = sut.finishBuffer()

        diffTest(expected: """
            line
            """)
            .diff(result)
    }

    func testFinishBuffer_addTrailingNewline_true() {
        let sut = makeSut()
        sut.emitLine("line")
        sut.emitNewline()

        let result = sut.finishBuffer(addTrailingNewline: true)

        diffTest(expected: """
            line

            """)
            .diff(result)
    }

    func testStartConditionalEmitter_emit_unchanged() {
        let sut = makeSut()
        sut.emitLine("line")
        let emitter = sut.startConditionalEmitter()

        emitter.emit("conditional")

        sut.assertBuffer(self, """
            line

            """)
    }

    func testStartConditionalEmitter_emit_changed() {
        let sut = makeSut()
        sut.emitLine("line")
        let emitter = sut.startConditionalEmitter()
        sut.emitLine("another line")

        emitter.emit("conditional")

        sut.assertBuffer(self, """
            line
            another line
            conditional
            """)
    }

    func testStartConditionalEmitter_emit_idempotent() {
        let sut = makeSut()
        sut.emitLine("line")
        let emitter = sut.startConditionalEmitter()
        sut.emitLine("another line")

        emitter.emit("conditional")
        emitter.emit("conditional")
        emitter.emit("conditional")

        sut.assertBuffer(self, """
            line
            another line
            conditional
            """)
    }

    func testMakeInsertionMarker() {
        let sut = makeSut()
        sut.emit("insertion (")
        let marker = sut.makeInsertionMarker()
        sut.emitLine(")")

        marker.insert { buffer in
            buffer.emit("marker")
        }

        sut.assertBuffer(self, #"""
            insertion (marker)

            """#)
    }

    func testMakeInsertionMarker_repeated() {
        let sut = makeSut()
        sut.emit("insertion (")
        let marker = sut.makeInsertionMarker()
        sut.emitLine(")")

        marker.insert { buffer in
            buffer.emit("marker")
        }
        marker.insert { buffer in
            buffer.emit("marker")
        }

        sut.assertBuffer(self, #"""
            insertion (markermarker)

            """#)
    }

    func testMakeInsertionMarker_insertOnce_idempotent() {
        let sut = makeSut()
        sut.emit("insertion (")
        let marker = sut.makeInsertionMarker()
        sut.emitLine(")")

        marker.insertOnce { buffer in
            buffer.emit("marker")
        }
        marker.insertOnce { buffer in
            buffer.emit("marker")
        }

        sut.assertBuffer(self, #"""
            insertion (marker)

            """#)
    }

    func testStartDelayedEmission() {
        let sut = makeSut()
        sut.emit("delayed (")
        let emission = sut.startDelayedEmission { buffer in
            buffer.emit("emission")
        }
        sut.emitLine(")")

        emission.emit()

        sut.assertBuffer(self, #"""
            delayed (emission)

            """#)
    }

    func testStartDelayedEmission_idempotent() {
        let sut = makeSut()
        sut.emit("delayed (")
        let emission = sut.startDelayedEmission { buffer in
            buffer.emit("emission")
        }
        sut.emitLine(")")

        emission.emit()
        emission.emit()
        emission.emit()

        sut.assertBuffer(self, #"""
            delayed (emission)

            """#)
    }

    func testIsOnNewline() {
        func assertIsOnNewline_true(_ input: String, line: UInt = #line) {
            let sut = makeSut(input)
            assertTrue(sut.isOnNewline(), file: #file, line: line)
        }
        func assertIsOnNewline_false(_ input: String, line: UInt = #line) {
            let sut = makeSut(input)
            assertFalse(sut.isOnNewline(), file: #file, line: line)
        }

        assertIsOnNewline_false("")
        assertIsOnNewline_false("\n ")
        assertIsOnNewline_false("\n_")
        assertIsOnNewline_true("\n")
        assertIsOnNewline_true("\r")
        assertIsOnNewline_true("\r\n")
    }

    func testIsOnSpaceSeparator() {
        func assertIsOnSpaceSeparator_true(_ input: String, line: UInt = #line) {
            let sut = makeSut(input)
            assertTrue(sut.isOnSpaceSeparator(), file: #file, line: line)
        }
        func assertIsOnSpaceSeparator_false(_ input: String, line: UInt = #line) {
            let sut = makeSut(input)
            assertFalse(sut.isOnSpaceSeparator(), file: #file, line: line)
        }

        assertIsOnSpaceSeparator_false(" _")
        assertIsOnSpaceSeparator_true("")
        assertIsOnSpaceSeparator_true(" ")
        assertIsOnSpaceSeparator_true("\t")
        assertIsOnSpaceSeparator_true("\n ")
        assertIsOnSpaceSeparator_true("\n")
        assertIsOnSpaceSeparator_true("\r")
        assertIsOnSpaceSeparator_true("\r\n")
    }

    func testIsOnDoubleNewline() {
        func assertIsOnDoubleNewline_true(_ input: String, line: UInt = #line) {
            let sut = makeSut(input)
            assertTrue(sut.isOnDoubleNewline(), file: #file, line: line)
        }
        func assertIsOnDoubleNewline_false(_ input: String, line: UInt = #line) {
            let sut = makeSut(input)
            assertFalse(sut.isOnDoubleNewline(), file: #file, line: line)
        }

        assertIsOnDoubleNewline_false("")
        assertIsOnDoubleNewline_false("\n ")
        assertIsOnDoubleNewline_false("\n_")
        assertIsOnDoubleNewline_false("\n")
        assertIsOnDoubleNewline_false("\r")
        assertIsOnDoubleNewline_false("\r\n")
        assertIsOnDoubleNewline_true("\n\n")
        assertIsOnDoubleNewline_true("\n\r")
        assertIsOnDoubleNewline_true("\n\r\n")
        assertIsOnDoubleNewline_true("\r\n\n")
    }

    func testIndentationString_spaces() {
        let sut = makeSut()

        assertEqual(sut.indentationString(), "")

        sut.indent()

        assertEqual(sut.indentationString(), "    ")

        sut.indent()

        assertEqual(sut.indentationString(), "        ")
    }

    func testIndentationString_tabs() {
        let sut = makeSut()
        sut.indentationMode = .tabs(1)

        assertEqual(sut.indentationString(), "")

        sut.indent()

        assertEqual(sut.indentationString(), "\t")

        sut.indent()

        assertEqual(sut.indentationString(), "\t\t")
    }

    func testIndent() {
        let sut = makeSut()

        sut.indent()

        assertEqual(sut.indentation, 1)

        sut.indent()

        assertEqual(sut.indentation, 2)
    }

    func testUnindent() {
        let sut = makeSut()
        sut.indent()
        sut.indent()

        sut.unindent()

        assertEqual(sut.indentation, 1)

        sut.unindent()

        assertEqual(sut.indentation, 0)

        sut.unindent()

        assertEqual(sut.indentation, 0)
    }

    func testEmitRaw() {
        let sut = makeSut()
        sut.indent()

        sut.emitRaw("text")

        sut.assertBuffer(self, #"""
            text
            """#)
    }

    func testEmit() {
        let sut = makeSut()

        sut.emit("text")
        sut.emit("text")

        sut.assertBuffer(self, #"""
            texttext
            """#)
    }

    func testEmit_prefixesWithIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.emit("text")
        sut.emit("text")

        sut.assertBuffer(self, #"""
                texttext
            """#)
    }

    func testEmit_prefixesWithIndentation_skipsIfInputIsEmpty() {
        let sut = makeSut()
        sut.indent()

        sut.emit("")

        sut.assertBuffer(self, #"""
            """#)
    }

    func testEmit_prefixesWithIndentation_skipsIfTextLeadsWithNewline() {
        let sut = makeSut()
        sut.indent()

        sut.emit("\ntext")

        sut.assertBuffer(self, #"""

            text
            """#)
    }

    func testEmitNewline() {
        let sut = makeSut()
        sut.indent()

        sut.emitNewline()

        sut.assertBuffer(self, #"""


            """#)
    }

    func testEmitMultiline() {
        let sut = makeSut()

        sut.emitMultiline("""
        a
        multiline
        string
        """)

        sut.assertBuffer(self, #"""
            a
            multiline
            string

            """#)
    }

    func testEmitMultiline_splitsOnKnownNewlines() {
        let sut = makeSut()

        sut.emitMultiline("a\nmultiline\rstring")

        sut.assertBuffer(self, #"""
            a
            multiline
            string

            """#)
    }

    func testEmitMultiline_keepsEmptyLines() {
        let sut = makeSut()

        sut.emitMultiline("""
            a


            multiline

            string
            """)

        sut.assertBuffer(self, #"""
            a


            multiline

            string

            """#)
    }

    func testEmitMultiline_usesIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.emitMultiline("""
        a
        multiline
        string
        """)

        sut.assertBuffer(self, #"""
                a
                multiline
                string

            """#)
    }

    func testEmitMultiline_usesIndentation_keepsEmptyLines() {
        let sut = makeSut()

        sut.emitMultiline("""
                a


                multiline

                string
            """)

        sut.assertBuffer(self, #"""
                a


                multiline

                string

            """#)
    }

    func testEmitLine() {
        let sut = makeSut()

        sut.emitLine("a line")

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEmitLine_usesIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.emitLine("a line")

        sut.assertBuffer(self, #"""
                a line

            """#)
    }

    func testEmitSpaceSeparator() {
        let sut = makeSut()

        sut.emit("a")
        sut.emitSpaceSeparator()
        sut.emit("line")

        sut.assertBuffer(self, #"""
            a line
            """#)
    }

    func testEmitComment() {
        let sut = makeSut()

        sut.emitComment("a comment")

        sut.assertBuffer(self, #"""
            // a comment

            """#)
    }

    func testEmitCommentBlock() {
        let sut = makeSut()

        sut.emitCommentBlock("""
            a
            comment
            block
            """)

        sut.assertBuffer(self, #"""
            /* a
            comment
            block */

            """#)
    }

    func testEmitDocComment() {
        let sut = makeSut()

        sut.emitDocComment("a comment")

        sut.assertBuffer(self, #"""
            /// a comment

            """#)
    }

    func testEmitDocCommentBlock() {
        let sut = makeSut()

        sut.emitDocCommentBlock("""
            a
            comment
            block
            """)

        sut.assertBuffer(self, #"""
            /** a
            comment
            block */

            """#)
    }

    func testEmitPrefix() {
        let sut = makeSut()

        sut.emitPrefix(.lineComment("a line comment"))
        sut.emitPrefix(.docComment("a doc comment"))

        sut.assertBuffer(self, #"""
            // a line comment
            /// a doc comment

            """#)
    }

    func testEmitPendingPrefix_empty() {
        let sut = makeSut()

        sut.emitPendingPrefix()

        sut.assertBuffer(self, "")
    }

    func testEmitLineWith() {
        let sut = makeSut()

        sut.emitLineWith {
            sut.emit("a line")
        }

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEmitLineWith_emitsLineOnError() {
        let sut = makeSut()

        try? sut.emitLineWith {
            sut.emit("a line")
            throw TestError.unexpectedThrow("dummy error")
        }

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEmitLineWith_avoidEmittingExtraNewline() {
        let sut = makeSut()

        sut.emitLineWith {
            sut.emitLine("a line")
        }

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEmitWithSeparators() {
        let sut = makeSut()

        sut.emitWithSeparators(["a", "b", "c"], separator: ", ")

        sut.assertBuffer(self, #"""
            a, b, c
            """#)
    }

    func testEmitWithSeparators_emptyInput() {
        let sut = makeSut()

        sut.emitWithSeparators([] as [String], separator: ", ")

        sut.assertBuffer(self, #"""
            """#)
    }

    func testEmitWithSeparatorsProducer() {
        let sut = makeSut()

        sut.emitWithSeparators([0, 1, 2], separator: ", ") { item in
            sut.emit(item.description)
        }

        sut.assertBuffer(self, #"""
            0, 1, 2
            """#)
    }

    func testEmitWithSeparatorsProducer_emptyInput() {
        let sut = makeSut()

        sut.emitWithSeparators([] as [Int], separator: ", ") { item in
            sut.emit(item.description)
        }

        sut.assertBuffer(self, #"""
            """#)
    }

    func testBacktrackWhitespace() {
        let sut = makeSut(" \n   a line\r\n \t")

        sut.backtrackWhitespace()

        sut.assertBuffer(self, " \n   a line")
    }

    func testBacktrackWhitespace_noWhitespaceTrailing() {
        let sut = makeSut(" \n   a line")

        sut.backtrackWhitespace()

        sut.assertBuffer(self, " \n   a line")
    }

    func testBacktrackWhitespace_emptyBuffer() {
        let sut = makeSut("")

        sut.backtrackWhitespace()

        sut.assertBuffer(self, "")
    }

    func testBacktrackWhitespace_whitespaceOnlyBuffer() {
        let sut = makeSut(" \n   \r\n \t")

        sut.backtrackWhitespace()

        sut.assertBuffer(self, "")
    }

    func testEnsureNewline() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureNewline()

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEnsureNewline_idempotent() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureNewline()
        sut.ensureNewline()
        sut.ensureNewline()

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEnsureNewline_newlineTrailing() {
        let sut = makeSut(#"""
            a line

            """#)

        sut.ensureNewline()

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testEnsureNewline_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureNewline()

        sut.assertBuffer(self, "\n")
    }

    func testEnsureSpaceSeparator() {
        let sut = makeSut("text")

        sut.ensureSpaceSeparator()

        sut.assertBuffer(self, "text ")
    }

    func testEnsureSpaceSeparator_idempotent() {
        let sut = makeSut("text")

        sut.ensureSpaceSeparator()
        sut.ensureSpaceSeparator()
        sut.ensureSpaceSeparator()

        sut.assertBuffer(self, "text ")
    }

    func testEnsureSpaceSeparator_spaceTrailing() {
        let sut = makeSut("text")

        sut.ensureSpaceSeparator()

        sut.assertBuffer(self, "text ")
    }

    func testEnsureSpaceSeparator_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureSpaceSeparator()

        sut.assertBuffer(self, "")
    }

    func testEnsureEmptyLine() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureEmptyLine()

        sut.assertBuffer(self, #"""
            a line


            """#)
    }

    func testEnsureEmptyLine_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureEmptyLine()

        sut.assertBuffer(self, "")
    }

    func testEnsureEmptyLine_idempotent() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureEmptyLine()
        sut.ensureEmptyLine()
        sut.ensureEmptyLine()

        sut.assertBuffer(self, #"""
            a line


            """#)
    }

    func testEnsureEmptyLine_newlineTrailing() {
        let sut = makeSut(#"""
            a line

            """#)

        sut.ensureEmptyLine()

        sut.assertBuffer(self, #"""
            a line


            """#)
    }

    func testEnsureDoubleNewline() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureDoubleNewline()

        sut.assertBuffer(self, #"""
            a line


            """#)
    }

    func testEnsureDoubleNewline_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureDoubleNewline()

        sut.assertBuffer(self, #"""



            """#)
    }

    func testEnsureDoubleNewline_idempotent() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureDoubleNewline()
        sut.ensureDoubleNewline()
        sut.ensureDoubleNewline()

        sut.assertBuffer(self, #"""
            a line


            """#)
    }

    func testEnsureDoubleNewline_newlineTrailing() {
        let sut = makeSut(#"""
            a line

            """#)

        sut.ensureDoubleNewline()

        sut.assertBuffer(self, #"""
            a line


            """#)
    }

    func testEnsureIndentation() {
        let sut = makeSut("")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer(self, "    ")
    }

    func testEnsureIndentation_nonEmptyBuffer() {
        let sut = makeSut("text")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer(self, "text")
    }

    func testEnsureIndentation_idempotent() {
        let sut = makeSut("")
        sut.indent()

        sut.ensureIndentation()
        sut.ensureIndentation()
        sut.ensureIndentation()

        sut.assertBuffer(self, "    ")
    }

    func testEnsureIndentation_spaceTrailing() {
        let sut = makeSut(" ")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer(self, " ")
    }

    func testEnsureIndentation_newlineTrailing() {
        let sut = makeSut(" \n")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer(self, " \n    ")
    }

    func testQueuePrefix() {
        let sut = makeSut()

        sut.queuePrefix(.lineComment("a comment"))
        sut.queuePrefix(.docComment("another comment"))
        sut.emitPendingPrefix()

        sut.assertBuffer(self, #"""
            // a comment
            /// another comment

            """#)
    }

    func testQueuePrefix_usesIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.queuePrefix(.lineComment("a comment"))
        sut.queuePrefix(.docComment("another comment"))
        sut.emitPendingPrefix()

        sut.assertBuffer(self, #"""
                // a comment
                /// another comment

            """#)
    }

    func testQueuePrefix_delaysEmission() {
        let sut = makeSut()

        sut.queuePrefix(.lineComment("a comment"))
        sut.emitLine("a line")

        sut.assertBuffer(self, #"""
            a line

            """#)
    }

    func testIndented() {
        let sut = makeSut()
        sut.indent()

        sut.indented {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                    a doubly-indented line
                an indented line

            """#)
    }

    func testIndented_resetsIndentationOnError() {
        let sut = makeSut()
        sut.indent()

        try? sut.indented {
            sut.emitLine("a doubly-indented line")
            throw TestError.unexpectedThrow("dummy error")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                    a doubly-indented line
                an indented line

            """#)
    }

    func testEmitBlock() {
        let sut = makeSut()
        sut.indent()

        sut.emitBlock {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                {
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    func testEmitBlock_respectsTrailingNewlines() {
        let sut = makeSut()

        sut.emitBlock {
            sut.emitLine("an indented line")
            sut.emitNewline()
        }

        sut.assertBuffer(self, #"""
            {
                an indented line

            }

            """#)
    }

    func testEmitBlock_keepsCurrentLine() {
        let sut = makeSut()
        sut.indent()
        sut.emit("a leading")

        sut.emitBlock {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                a leading{
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    func testEmitBlock_ensuresNewlineOnClosingBrace() {
        let sut = makeSut()

        sut.emitBlock {
            sut.emit("non-newline terminated")
        }

        sut.assertBuffer(self, #"""
            {
                non-newline terminated
            }

            """#)
    }

    func testEmitBlockWithLead() {
        let sut = makeSut()
        sut.indent()

        sut.emitBlock("block") {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                block {
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    func testEmitBlockWithLead_keepsCurrentLine() {
        let sut = makeSut()
        sut.indent()
        sut.emit("a leading")

        sut.emitBlock("block") {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                a leadingblock {
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    func testEmitBlockWithLead_ensuresNewlineOnClosingBrace() {
        let sut = makeSut()

        sut.emitBlock("block") {
            sut.emit("non-newline terminated")
        }

        sut.assertBuffer(self, #"""
            block {
                non-newline terminated
            }

            """#)
    }

    func testEmitMembersBlock() {
        let sut = makeSut()
        sut.indent()
        sut.emit("a leading")

        sut.emitMembersBlock {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(self, #"""
                a leading{
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    func testEmitMembersBlock_erasesNewlineTrailing() {
        let sut = makeSut()
        sut.emit("a leading")

        sut.emitMembersBlock {
            sut.emitLine("an indented line")
            sut.emitNewline()
        }

        sut.assertBuffer(self, #"""
            a leading{
                an indented line
            }

            """#)
    }

    func testEmitInlinedBlock() {
        let sut = makeSut()
        sut.emit("a leading")

        sut.emitInlinedBlock {
            sut.emitLine("an indented line")
            sut.emitNewline()
        }
        sut.emitLine("a trailing")

        sut.assertBuffer(self, #"""
            a leading{
                an indented line

            }a trailing

            """#)
    }

    func testEmitEmptyBlock() {
        let sut = makeSut()
        sut.emit("a leading")

        sut.emitEmptyBlock()
        sut.emitLine("a trailing")

        sut.assertBuffer(self, #"""
            a leading{
            }
            a trailing

            """#)
    }
}

// MARK: - Test internals

private func makeSut(_ preBuffer: String = "") -> CodeStringBuffer {
    let sut = CodeStringBuffer()
    sut.buffer = preBuffer
    return sut
}

private extension CodeStringBuffer {
    func assertBuffer(
        _ tester: XCTestCase,
        _ expected: String,
        file: StaticString = #file,
        line: UInt = #line
    ) {
        tester
            .diffTest(expected: expected, file: file, line: line)
            .diff(buffer, file: file, line: line)
    }
}
