import Testing

@testable import SwiftPEG

struct StringTests {
    @Test
    func startsUppercased() {
        #expect("Abc".startsUppercased)
        #expect("A".startsUppercased)
        #expect("abc".startsUppercased == false)
        #expect("a".startsUppercased == false)
        #expect("0".startsUppercased == false)
        #expect(" ".startsUppercased == false)
        #expect("".startsUppercased == false)
    }

    @Test
    func lowercasedFirstLetter() {
        assertEqual("a", "A".lowercasedFirstLetter)
        assertEqual("abc", "Abc".lowercasedFirstLetter)
        assertEqual("abc", "abc".lowercasedFirstLetter)
        assertEqual("ábc", "Ábc".lowercasedFirstLetter)
        assertEqual("aBC", "ABC".lowercasedFirstLetter)
        assertEqual("aBc", "aBc".lowercasedFirstLetter)
        assertEqual("ábC", "ÁbC".lowercasedFirstLetter)
        assertEqual("0", "0".lowercasedFirstLetter)
        assertEqual("", "".lowercasedFirstLetter)
        assertEqual(" ", " ".lowercasedFirstLetter)
    }

    @Test
    func uppercasedFirstLetter() {
        assertEqual("A", "a".uppercasedFirstLetter)
        assertEqual("Abc", "abc".uppercasedFirstLetter)
        assertEqual("Abc", "Abc".uppercasedFirstLetter)
        assertEqual("Ábc", "ábc".uppercasedFirstLetter)
        assertEqual("ABC", "aBC".uppercasedFirstLetter)
        assertEqual("ABc", "ABc".uppercasedFirstLetter)
        assertEqual("ÁbC", "ábC".uppercasedFirstLetter)
        assertEqual("0", "0".uppercasedFirstLetter)
        assertEqual("", "".uppercasedFirstLetter)
        assertEqual(" ", " ".uppercasedFirstLetter)
    }

    @Test
    func makeDifference() {
        let str1 = """
            Abcdef
            """
        let str2 = """
            Abdef
            """

        let result = str1.makeDifferenceMarkString(against: str2)

        assertEqual(
            """
            Abcdef
            ~~^ Difference starts here
            """,
            result
        )
    }

    @Test
    func makeDifferenceBetweenLines() {
        let str1 = """
            Abc
            Def
            Ghi
            """
        let str2 = """
            Abc
            Df
            Ghi
            """

        let result = str1.makeDifferenceMarkString(against: str2)

        assertEqual(
            """
            Abc
            Def
            ~^ Difference starts here
            Ghi
            """,
            result
        )
    }

    @Test
    func makeDifferenceBetweenEqualStrings() {
        let str1 = """
            Abc
            Def
            Ghi
            """
        let str2 = """
            Abc
            Def
            Ghi
            """

        let result = str1.makeDifferenceMarkString(against: str2)

        assertEqual(
            """
            Abc
            Def
            Ghi
             ~ Strings are equal.
            """,
            result
        )
    }

    @Test
    func makeDifferenceBetweenStringsAtBeginning() {
        let str1 = """
            Abc
            """
        let str2 = """
            Zwx
            """

        let result = str1.makeDifferenceMarkString(against: str2)

        assertEqual(
            """
            Abc
            ^ Difference starts here
            """,
            result
        )
    }

    @Test
    func commentSectionRangesInEmptyString() {
        let ranges = "".cStyleCommentSectionRanges()
        assertEqual(ranges.count, 0)
    }

    @Test
    func commentSectionRanges() {
        let input = """
            // A comment!
            Not a comment.
            /*
                A multi-lined comment!
            */
            Not a comment again.
            """

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 2)
        assertEqual(ranges[0], input.range(of: "// A comment!\n"))
        assertEqual(
            ranges[1],
            input.range(
                of: """
                    /*
                        A multi-lined comment!
                    */
                    """
            )
        )
    }

    @Test
    func commentSectionRangesEntireString() {
        let input = "// A comment!"

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 1)
        assertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesEntireStringMultiLine() {
        let input = "/* A comment! \n*/"

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 1)
        assertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesOpenMultiLineComment() {
        let input = "/* A comment! \n"

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 1)
        assertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesIgnoresCommentsInStringLiterals() {
        let input = "\"A comment in a string: // etc.\""

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 0)
    }

    @Test
    func commentSectionRangesIgnoresStringLiteralsWithinSingleLineComments() {
        let input = "/* A comment! \"A string\" \n"

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 1)
        assertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesIgnoresStringLiteralsWithinMultiLineComments() {
        let input = """
            /* A comment! "An unterminated string literal
            */ "A string /* */"
            """

        let ranges = input.cStyleCommentSectionRanges()
        assertEqual(ranges.count, 1)
        assertEqual(
            ranges[0],
            input.range(
                of: """
                    /* A comment! "An unterminated string literal
                    */
                    """
            )
        )
    }

    @Test
    func lineRangesWithCountedLines() {
        let input = """
            1
            2
            3
            """

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 3)
        assertEqual(ranges[0], input.range(of: "1"))
        assertEqual(ranges[1], input.range(of: "2"))
        assertEqual(ranges[2], input.range(of: "3"))

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 3)
        assertEqual(rangesWithLineBreaks[0], input.range(of: "1\n"))
        assertEqual(rangesWithLineBreaks[1], input.range(of: "2\n"))
        assertEqual(rangesWithLineBreaks[2], input.range(of: "3"))
    }

    @Test
    func lineRangesWithEmptyLines() {
        let input = """
            1

            3
            """

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 3)
        assertEqual(ranges[0], input.range(of: "1"))
        assertEqual(
            ranges[1],
            input.range(of: "1\n")!.upperBound..<input.range(of: "\n3")!.lowerBound
        )
        assertEqual(ranges[2], input.range(of: "3"))

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 3)
        assertEqual(rangesWithLineBreaks[0], input.range(of: "1\n"))
        assertEqual(
            rangesWithLineBreaks[1],
            input.range(of: "1\n")!.upperBound..<input.range(of: "3")!.lowerBound
        )
        assertEqual(rangesWithLineBreaks[2], input.range(of: "3"))
    }

    @Test
    func lineRangesWithEmptyFinalLine() {
        let input = """
            1

            """

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 2)
        assertEqual(ranges[0], input.range(of: "1"))
        assertEqual(ranges[1], input.range(of: "1\n")!.upperBound..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 2)
        assertEqual(rangesWithLineBreaks[0], input.range(of: "1\n"))
        assertEqual(rangesWithLineBreaks[1], input.range(of: "1\n")!.upperBound..<input.endIndex)
    }

    @Test
    func lineRangesWithTwoEmptyFinalLines() {
        let input = """
            1


            """

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 3)
        assertEqual(ranges[0], input.intRange(0..<1))
        assertEqual(ranges[1], input.intRange(2..<2))
        assertEqual(ranges[2], input.intRange(3..<3))

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 3)
        assertEqual(rangesWithLineBreaks[0], input.intRange(0..<2))
        assertEqual(rangesWithLineBreaks[1], input.intRange(2..<3))
        assertEqual(rangesWithLineBreaks[2], input.intRange(3..<3))
    }

    @Test
    func lineRangesWithSingleLine() {
        let input = "123"

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 1)
        assertEqual(ranges[0], input.startIndex..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 1)
        assertEqual(rangesWithLineBreaks[0], input.startIndex..<input.endIndex)
    }

    @Test
    func lineRangesWithEmptyString() {
        let input = ""

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 1)
        assertEqual(ranges[0], input.startIndex..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 1)
        assertEqual(rangesWithLineBreaks[0], input.startIndex..<input.endIndex)
    }

    @Test
    func lineRangesWithSingleEmptyLine() {
        let input = "\n"

        let ranges = input.lineRanges()
        assertEqual(ranges.count, 2)
        assertEqual(ranges[0], input.startIndex..<input.startIndex)
        assertEqual(ranges[1], input.endIndex..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        assertEqual(rangesWithLineBreaks.count, 2)
        assertEqual(rangesWithLineBreaks[0], input.startIndex..<input.endIndex)
        assertEqual(rangesWithLineBreaks[1], input.endIndex..<input.endIndex)
    }

    @Test
    func trimWhitespace() {
        assertEqual(SwiftPEG.trimWhitespace(""), "")
        assertEqual(SwiftPEG.trimWhitespace("  "), "")
        assertEqual(SwiftPEG.trimWhitespace("a "), "a")
        assertEqual(SwiftPEG.trimWhitespace(" a"), "a")
        assertEqual(SwiftPEG.trimWhitespace("abc"), "abc")
        assertEqual(SwiftPEG.trimWhitespace("abc "), "abc")
        assertEqual(SwiftPEG.trimWhitespace(" abc"), "abc")
        assertEqual(SwiftPEG.trimWhitespace("  abc "), "abc")
        assertEqual(SwiftPEG.trimWhitespace("\nabc\n"), "abc")
        assertEqual(SwiftPEG.trimWhitespace("\n abc def \t "), "abc def")
        assertEqual(SwiftPEG.trimWhitespace("  abc def "), "abc def")
    }

    @Test
    func trimWhitespaceLead() {
        assertEqual(SwiftPEG.trimWhitespaceLead(""), "")
        assertEqual(SwiftPEG.trimWhitespaceLead("  "), "")
        assertEqual(SwiftPEG.trimWhitespaceLead("a "), "a ")
        assertEqual(SwiftPEG.trimWhitespaceLead(" a"), "a")
        assertEqual(SwiftPEG.trimWhitespaceLead("abc"), "abc")
        assertEqual(SwiftPEG.trimWhitespaceLead("abc "), "abc ")
        assertEqual(SwiftPEG.trimWhitespaceLead(" abc"), "abc")
        assertEqual(SwiftPEG.trimWhitespaceLead("  abc "), "abc ")
        assertEqual(SwiftPEG.trimWhitespaceLead("\nabc\n"), "abc\n")
        assertEqual(SwiftPEG.trimWhitespaceLead("\n abc def \t "), "abc def \t ")
        assertEqual(SwiftPEG.trimWhitespaceLead("  abc def "), "abc def ")
    }

    @Test
    func trimWhitespaceTrail() {
        assertEqual(SwiftPEG.trimWhitespaceTrail(""), "")
        assertEqual(SwiftPEG.trimWhitespaceTrail("  "), "")
        assertEqual(SwiftPEG.trimWhitespaceTrail("a "), "a")
        assertEqual(SwiftPEG.trimWhitespaceTrail(" a"), " a")
        assertEqual(SwiftPEG.trimWhitespaceTrail("abc"), "abc")
        assertEqual(SwiftPEG.trimWhitespaceTrail("abc "), "abc")
        assertEqual(SwiftPEG.trimWhitespaceTrail(" abc"), " abc")
        assertEqual(SwiftPEG.trimWhitespaceTrail("  abc "), "  abc")
        assertEqual(SwiftPEG.trimWhitespaceTrail("\nabc\n"), "\nabc")
        assertEqual(SwiftPEG.trimWhitespaceTrail("\n abc def \t "), "\n abc def")
        assertEqual(SwiftPEG.trimWhitespaceTrail("  abc def "), "  abc def")
    }
}

extension String {
    func intRange<R>(_ range: R) -> Range<Index> where R: RangeExpression, R.Bound == Int {
        let range = range.relative(to: Array(0..<count))

        let start = index(startIndex, offsetBy: range.lowerBound)
        let end = index(startIndex, offsetBy: range.upperBound)

        return start..<end
    }
}
