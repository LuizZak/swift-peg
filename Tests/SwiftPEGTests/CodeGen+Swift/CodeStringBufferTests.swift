import Testing

@testable import SwiftPEG

struct CodeStringBufferTests {
    @Test
    func ephemeral() {
        let sut = makeSut()

        sut.assertBuffer(#""#)
        assertEqual(sut.indentation, 0)
        assertEqual(sut.indentationMode, .spaces(4))
    }

    @Test
    func resetState() {
        let sut = makeSut()
        sut.emitLine("line")
        sut.indent()

        sut.resetState()

        sut.assertBuffer(#""#)
        assertEqual(sut.indentation, 0)
    }

    @Test
    func finishBuffer_addTrailingNewline_false() {
        let sut = makeSut()
        sut.emitLine("line")
        sut.emitNewline()

        let result = sut.finishBuffer()

        diffTest(expected: """
            line
            """)
            .diff(result)
    }

    @Test
    func finishBuffer_addTrailingNewline_true() {
        let sut = makeSut()
        sut.emitLine("line")
        sut.emitNewline()

        let result = sut.finishBuffer(addTrailingNewline: true)

        diffTest(expected: """
            line

            """)
            .diff(result)
    }

    @Test
    func startConditionalEmitter_emit_unchanged() {
        let sut = makeSut()
        sut.emitLine("line")
        let emitter = sut.startConditionalEmitter()

        emitter.emit("conditional")

        sut.assertBuffer("""
            line

            """)
    }

    @Test
    func startConditionalEmitter_emit_changed() {
        let sut = makeSut()
        sut.emitLine("line")
        let emitter = sut.startConditionalEmitter()
        sut.emitLine("another line")

        emitter.emit("conditional")

        sut.assertBuffer("""
            line
            another line
            conditional
            """)
    }

    @Test
    func startConditionalEmitter_emit_idempotent() {
        let sut = makeSut()
        sut.emitLine("line")
        let emitter = sut.startConditionalEmitter()
        sut.emitLine("another line")

        emitter.emit("conditional")
        emitter.emit("conditional")
        emitter.emit("conditional")

        sut.assertBuffer("""
            line
            another line
            conditional
            """)
    }

    @Test
    func makeInsertionMarker() {
        let sut = makeSut()
        sut.emit("insertion (")
        let marker = sut.makeInsertionMarker()
        sut.emitLine(")")

        marker.insert { buffer in
            buffer.emit("marker")
        }

        sut.assertBuffer(#"""
            insertion (marker)

            """#)
    }

    @Test
    func makeInsertionMarker_repeated() {
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

        sut.assertBuffer(#"""
            insertion (markermarker)

            """#)
    }

    @Test
    func makeInsertionMarker_insertOnce_idempotent() {
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

        sut.assertBuffer(#"""
            insertion (marker)

            """#)
    }

    @Test
    func startDelayedEmission() {
        let sut = makeSut()
        sut.emit("delayed (")
        let emission = sut.startDelayedEmission { buffer in
            buffer.emit("emission")
        }
        sut.emitLine(")")

        emission.emit()

        sut.assertBuffer(#"""
            delayed (emission)

            """#)
    }

    @Test
    func startDelayedEmission_idempotent() {
        let sut = makeSut()
        sut.emit("delayed (")
        let emission = sut.startDelayedEmission { buffer in
            buffer.emit("emission")
        }
        sut.emitLine(")")

        emission.emit()
        emission.emit()
        emission.emit()

        sut.assertBuffer(#"""
            delayed (emission)

            """#)
    }

    @Test
    func isOnNewline() {
        func assertIsOnNewline_true(_ input: String, sourceLocation: SourceLocation = #_sourceLocation) {
            let sut = makeSut(input)
            assertTrue(sut.isOnNewline(), sourceLocation: sourceLocation)
        }
        func assertIsOnNewline_false(_ input: String, sourceLocation: SourceLocation = #_sourceLocation) {
            let sut = makeSut(input)
            assertFalse(sut.isOnNewline(), sourceLocation: sourceLocation)
        }

        assertIsOnNewline_false("")
        assertIsOnNewline_false("\n ")
        assertIsOnNewline_false("\n_")
        assertIsOnNewline_true("\n")
        assertIsOnNewline_true("\r")
        assertIsOnNewline_true("\r\n")
    }

    @Test
    func isOnSpaceSeparator() {
        func assertIsOnSpaceSeparator_true(_ input: String, sourceLocation: SourceLocation = #_sourceLocation) {
            let sut = makeSut(input)
            assertTrue(sut.isOnSpaceSeparator(), sourceLocation: sourceLocation)
        }
        func assertIsOnSpaceSeparator_false(_ input: String, sourceLocation: SourceLocation = #_sourceLocation) {
            let sut = makeSut(input)
            assertFalse(sut.isOnSpaceSeparator(), sourceLocation: sourceLocation)
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

    @Test
    func isOnDoubleNewline() {
        func assertIsOnDoubleNewline_true(_ input: String, sourceLocation: SourceLocation = #_sourceLocation) {
            let sut = makeSut(input)
            assertTrue(sut.isOnDoubleNewline(), sourceLocation: sourceLocation)
        }
        func assertIsOnDoubleNewline_false(_ input: String, sourceLocation: SourceLocation = #_sourceLocation) {
            let sut = makeSut(input)
            assertFalse(sut.isOnDoubleNewline(), sourceLocation: sourceLocation)
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

    @Test
    func indentationString_spaces() {
        let sut = makeSut()

        assertEqual(sut.indentationString(), "")

        sut.indent()

        assertEqual(sut.indentationString(), "    ")

        sut.indent()

        assertEqual(sut.indentationString(), "        ")
    }

    @Test
    func indentationString_tabs() {
        let sut = makeSut()
        sut.indentationMode = .tabs(1)

        assertEqual(sut.indentationString(), "")

        sut.indent()

        assertEqual(sut.indentationString(), "\t")

        sut.indent()

        assertEqual(sut.indentationString(), "\t\t")
    }

    @Test
    func indent() {
        let sut = makeSut()

        sut.indent()

        assertEqual(sut.indentation, 1)

        sut.indent()

        assertEqual(sut.indentation, 2)
    }

    @Test
    func unindent() {
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

    @Test
    func emitRaw() {
        let sut = makeSut()
        sut.indent()

        sut.emitRaw("text")

        sut.assertBuffer(#"""
            text
            """#)
    }

    @Test
    func emit() {
        let sut = makeSut()

        sut.emit("text")
        sut.emit("text")

        sut.assertBuffer(#"""
            texttext
            """#)
    }

    @Test
    func emit_prefixesWithIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.emit("text")
        sut.emit("text")

        sut.assertBuffer(#"""
                texttext
            """#)
    }

    @Test
    func emit_prefixesWithIndentation_skipsIfInputIsEmpty() {
        let sut = makeSut()
        sut.indent()

        sut.emit("")

        sut.assertBuffer(#"""
            """#)
    }

    @Test
    func emit_prefixesWithIndentation_skipsIfTextLeadsWithNewline() {
        let sut = makeSut()
        sut.indent()

        sut.emit("\ntext")

        sut.assertBuffer(#"""

            text
            """#)
    }

    @Test
    func emitNewline() {
        let sut = makeSut()
        sut.indent()

        sut.emitNewline()

        sut.assertBuffer(#"""


            """#)
    }

    @Test
    func emitMultiline() {
        let sut = makeSut()

        sut.emitMultiline("""
        a
        multiline
        string
        """)

        sut.assertBuffer(#"""
            a
            multiline
            string

            """#)
    }

    @Test
    func emitMultiline_splitsOnKnownNewlines() {
        let sut = makeSut()

        sut.emitMultiline("a\nmultiline\rstring")

        sut.assertBuffer(#"""
            a
            multiline
            string

            """#)
    }

    @Test
    func emitMultiline_keepsEmptyLines() {
        let sut = makeSut()

        sut.emitMultiline("""
            a


            multiline

            string
            """)

        sut.assertBuffer(#"""
            a


            multiline

            string

            """#)
    }

    @Test
    func emitMultiline_usesIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.emitMultiline("""
        a
        multiline
        string
        """)

        sut.assertBuffer(#"""
                a
                multiline
                string

            """#)
    }

    @Test
    func emitMultiline_usesIndentation_keepsEmptyLines() {
        let sut = makeSut()

        sut.emitMultiline("""
                a


                multiline

                string
            """)

        sut.assertBuffer(#"""
                a


                multiline

                string

            """#)
    }

    @Test
    func emitLine() {
        let sut = makeSut()

        sut.emitLine("a line")

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func emitLine_usesIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.emitLine("a line")

        sut.assertBuffer(#"""
                a line

            """#)
    }

    @Test
    func emitSpaceSeparator() {
        let sut = makeSut()

        sut.emit("a")
        sut.emitSpaceSeparator()
        sut.emit("line")

        sut.assertBuffer(#"""
            a line
            """#)
    }

    @Test
    func emitComment() {
        let sut = makeSut()

        sut.emitComment("a comment")

        sut.assertBuffer(#"""
            // a comment

            """#)
    }

    @Test
    func emitCommentBlock() {
        let sut = makeSut()

        sut.emitCommentBlock("""
            a
            comment
            block
            """)

        sut.assertBuffer(#"""
            /* a
            comment
            block */

            """#)
    }

    @Test
    func emitDocComment() {
        let sut = makeSut()

        sut.emitDocComment("a comment")

        sut.assertBuffer(#"""
            /// a comment

            """#)
    }

    @Test
    func emitDocCommentBlock() {
        let sut = makeSut()

        sut.emitDocCommentBlock("""
            a
            comment
            block
            """)

        sut.assertBuffer(#"""
            /** a
            comment
            block */

            """#)
    }

    @Test
    func emitPrefix() {
        let sut = makeSut()

        sut.emitPrefix(.lineComment("a line comment"))
        sut.emitPrefix(.docComment("a doc comment"))

        sut.assertBuffer(#"""
            // a line comment
            /// a doc comment

            """#)
    }

    @Test
    func emitPendingPrefix_empty() {
        let sut = makeSut()

        sut.emitPendingPrefix()

        sut.assertBuffer("")
    }

    @Test
    func emitLineWith() {
        let sut = makeSut()

        sut.emitLineWith {
            sut.emit("a line")
        }

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func emitLineWith_emitsLineOnError() {
        let sut = makeSut()

        try? sut.emitLineWith {
            sut.emit("a line")
            throw TestError.unexpectedThrow("dummy error")
        }

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func emitLineWith_avoidEmittingExtraNewline() {
        let sut = makeSut()

        sut.emitLineWith {
            sut.emitLine("a line")
        }

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func emitWithSeparators() {
        let sut = makeSut()

        sut.emitWithSeparators(["a", "b", "c"], separator: ", ")

        sut.assertBuffer(#"""
            a, b, c
            """#)
    }

    @Test
    func emitWithSeparators_emptyInput() {
        let sut = makeSut()

        sut.emitWithSeparators([] as [String], separator: ", ")

        sut.assertBuffer(#"""
            """#)
    }

    @Test
    func emitWithSeparatorsProducer() {
        let sut = makeSut()

        sut.emitWithSeparators([0, 1, 2], separator: ", ") { item in
            sut.emit(item.description)
        }

        sut.assertBuffer(#"""
            0, 1, 2
            """#)
    }

    @Test
    func emitWithSeparatorsProducer_emptyInput() {
        let sut = makeSut()

        sut.emitWithSeparators([] as [Int], separator: ", ") { item in
            sut.emit(item.description)
        }

        sut.assertBuffer(#"""
            """#)
    }

    @Test
    func backtrackWhitespace() {
        let sut = makeSut(" \n   a line\r\n \t")

        sut.backtrackWhitespace()

        sut.assertBuffer(" \n   a line")
    }

    @Test
    func backtrackWhitespace_noWhitespaceTrailing() {
        let sut = makeSut(" \n   a line")

        sut.backtrackWhitespace()

        sut.assertBuffer(" \n   a line")
    }

    @Test
    func backtrackWhitespace_emptyBuffer() {
        let sut = makeSut("")

        sut.backtrackWhitespace()

        sut.assertBuffer("")
    }

    @Test
    func backtrackWhitespace_whitespaceOnlyBuffer() {
        let sut = makeSut(" \n   \r\n \t")

        sut.backtrackWhitespace()

        sut.assertBuffer("")
    }

    @Test
    func ensureNewline() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureNewline()

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func ensureNewline_idempotent() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureNewline()
        sut.ensureNewline()
        sut.ensureNewline()

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func ensureNewline_newlineTrailing() {
        let sut = makeSut(#"""
            a line

            """#)

        sut.ensureNewline()

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func ensureNewline_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureNewline()

        sut.assertBuffer("\n")
    }

    @Test
    func ensureSpaceSeparator() {
        let sut = makeSut("text")

        sut.ensureSpaceSeparator()

        sut.assertBuffer("text ")
    }

    @Test
    func ensureSpaceSeparator_idempotent() {
        let sut = makeSut("text")

        sut.ensureSpaceSeparator()
        sut.ensureSpaceSeparator()
        sut.ensureSpaceSeparator()

        sut.assertBuffer("text ")
    }

    @Test
    func ensureSpaceSeparator_spaceTrailing() {
        let sut = makeSut("text")

        sut.ensureSpaceSeparator()

        sut.assertBuffer("text ")
    }

    @Test
    func ensureSpaceSeparator_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureSpaceSeparator()

        sut.assertBuffer("")
    }

    @Test
    func ensureEmptyLine() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureEmptyLine()

        sut.assertBuffer(#"""
            a line


            """#)
    }

    @Test
    func ensureEmptyLine_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureEmptyLine()

        sut.assertBuffer("")
    }

    @Test
    func ensureEmptyLine_idempotent() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureEmptyLine()
        sut.ensureEmptyLine()
        sut.ensureEmptyLine()

        sut.assertBuffer(#"""
            a line


            """#)
    }

    @Test
    func ensureEmptyLine_newlineTrailing() {
        let sut = makeSut(#"""
            a line

            """#)

        sut.ensureEmptyLine()

        sut.assertBuffer(#"""
            a line


            """#)
    }

    @Test
    func ensureDoubleNewline() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureDoubleNewline()

        sut.assertBuffer(#"""
            a line


            """#)
    }

    @Test
    func ensureDoubleNewline_emptyBuffer() {
        let sut = makeSut("")

        sut.ensureDoubleNewline()

        sut.assertBuffer(#"""



            """#)
    }

    @Test
    func ensureDoubleNewline_idempotent() {
        let sut = makeSut(#"""
            a line
            """#)

        sut.ensureDoubleNewline()
        sut.ensureDoubleNewline()
        sut.ensureDoubleNewline()

        sut.assertBuffer(#"""
            a line


            """#)
    }

    @Test
    func ensureDoubleNewline_newlineTrailing() {
        let sut = makeSut(#"""
            a line

            """#)

        sut.ensureDoubleNewline()

        sut.assertBuffer(#"""
            a line


            """#)
    }

    @Test
    func ensureIndentation() {
        let sut = makeSut("")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer("    ")
    }

    @Test
    func ensureIndentation_nonEmptyBuffer() {
        let sut = makeSut("text")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer("text")
    }

    @Test
    func ensureIndentation_idempotent() {
        let sut = makeSut("")
        sut.indent()

        sut.ensureIndentation()
        sut.ensureIndentation()
        sut.ensureIndentation()

        sut.assertBuffer("    ")
    }

    @Test
    func ensureIndentation_spaceTrailing() {
        let sut = makeSut(" ")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer(" ")
    }

    @Test
    func ensureIndentation_newlineTrailing() {
        let sut = makeSut(" \n")
        sut.indent()

        sut.ensureIndentation()

        sut.assertBuffer(" \n    ")
    }

    @Test
    func queuePrefix() {
        let sut = makeSut()

        sut.queuePrefix(.lineComment("a comment"))
        sut.queuePrefix(.docComment("another comment"))
        sut.emitPendingPrefix()

        sut.assertBuffer(#"""
            // a comment
            /// another comment

            """#)
    }

    @Test
    func queuePrefix_usesIndentation() {
        let sut = makeSut()
        sut.indent()

        sut.queuePrefix(.lineComment("a comment"))
        sut.queuePrefix(.docComment("another comment"))
        sut.emitPendingPrefix()

        sut.assertBuffer(#"""
                // a comment
                /// another comment

            """#)
    }

    @Test
    func queuePrefix_delaysEmission() {
        let sut = makeSut()

        sut.queuePrefix(.lineComment("a comment"))
        sut.emitLine("a line")

        sut.assertBuffer(#"""
            a line

            """#)
    }

    @Test
    func indented() {
        let sut = makeSut()
        sut.indent()

        sut.indented {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                    a doubly-indented line
                an indented line

            """#)
    }

    @Test
    func indented_resetsIndentationOnError() {
        let sut = makeSut()
        sut.indent()

        try? sut.indented {
            sut.emitLine("a doubly-indented line")
            throw TestError.unexpectedThrow("dummy error")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                    a doubly-indented line
                an indented line

            """#)
    }

    @Test
    func emitBlock() {
        let sut = makeSut()
        sut.indent()

        sut.emitBlock {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                {
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    @Test
    func emitBlock_respectsTrailingNewlines() {
        let sut = makeSut()

        sut.emitBlock {
            sut.emitLine("an indented line")
            sut.emitNewline()
        }

        sut.assertBuffer(#"""
            {
                an indented line

            }

            """#)
    }

    @Test
    func emitBlock_keepsCurrentLine() {
        let sut = makeSut()
        sut.indent()
        sut.emit("a leading")

        sut.emitBlock {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                a leading{
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    @Test
    func emitBlock_ensuresNewlineOnClosingBrace() {
        let sut = makeSut()

        sut.emitBlock {
            sut.emit("non-newline terminated")
        }

        sut.assertBuffer(#"""
            {
                non-newline terminated
            }

            """#)
    }

    @Test
    func emitBlockWithLead() {
        let sut = makeSut()
        sut.indent()

        sut.emitBlock("block") {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                block {
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    @Test
    func emitBlockWithLead_keepsCurrentLine() {
        let sut = makeSut()
        sut.indent()
        sut.emit("a leading")

        sut.emitBlock("block") {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                a leadingblock {
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    @Test
    func emitBlockWithLead_ensuresNewlineOnClosingBrace() {
        let sut = makeSut()

        sut.emitBlock("block") {
            sut.emit("non-newline terminated")
        }

        sut.assertBuffer(#"""
            block {
                non-newline terminated
            }

            """#)
    }

    @Test
    func emitMembersBlock() {
        let sut = makeSut()
        sut.indent()
        sut.emit("a leading")

        sut.emitMembersBlock {
            sut.emitLine("a doubly-indented line")
        }
        sut.emitLine("an indented line")

        sut.assertBuffer(#"""
                a leading{
                    a doubly-indented line
                }
                an indented line

            """#)
    }

    @Test
    func emitMembersBlock_erasesNewlineTrailing() {
        let sut = makeSut()
        sut.emit("a leading")

        sut.emitMembersBlock {
            sut.emitLine("an indented line")
            sut.emitNewline()
        }

        sut.assertBuffer(#"""
            a leading{
                an indented line
            }

            """#)
    }

    @Test
    func emitInlinedBlock() {
        let sut = makeSut()
        sut.emit("a leading")

        sut.emitInlinedBlock {
            sut.emitLine("an indented line")
            sut.emitNewline()
        }
        sut.emitLine("a trailing")

        sut.assertBuffer(#"""
            a leading{
                an indented line

            }a trailing

            """#)
    }

    @Test
    func emitEmptyBlock() {
        let sut = makeSut()
        sut.emit("a leading")

        sut.emitEmptyBlock()
        sut.emitLine("a trailing")

        sut.assertBuffer(#"""
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
        _ expected: String,
        sourceLocation: SourceLocation = #_sourceLocation
    ) {
        SwiftTestingDiffTestCaseFailureReporter()
            .diffTest(expected: expected, sourceLocation: sourceLocation)
            .diff(buffer, file: sourceLocation.fileName, line: sourceLocation.line)
    }
}
