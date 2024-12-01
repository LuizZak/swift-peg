import XCTest
import Testing

@testable import SwiftPEG

struct StringTests {
    @Test
    func startsUppercased() {
        XCTAssert("Abc".startsUppercased)
        XCTAssert("A".startsUppercased)
        XCTAssertFalse("abc".startsUppercased)
        XCTAssertFalse("a".startsUppercased)
        XCTAssertFalse("0".startsUppercased)
        XCTAssertFalse(" ".startsUppercased)
        XCTAssertFalse("".startsUppercased)
    }

    @Test
    func lowercasedFirstLetter() {
        XCTAssertEqual("a", "A".lowercasedFirstLetter)
        XCTAssertEqual("abc", "Abc".lowercasedFirstLetter)
        XCTAssertEqual("abc", "abc".lowercasedFirstLetter)
        XCTAssertEqual("ábc", "Ábc".lowercasedFirstLetter)
        XCTAssertEqual("aBC", "ABC".lowercasedFirstLetter)
        XCTAssertEqual("aBc", "aBc".lowercasedFirstLetter)
        XCTAssertEqual("ábC", "ÁbC".lowercasedFirstLetter)
        XCTAssertEqual("0", "0".lowercasedFirstLetter)
        XCTAssertEqual("", "".lowercasedFirstLetter)
        XCTAssertEqual(" ", " ".lowercasedFirstLetter)
    }

    @Test
    func uppercasedFirstLetter() {
        XCTAssertEqual("A", "a".uppercasedFirstLetter)
        XCTAssertEqual("Abc", "abc".uppercasedFirstLetter)
        XCTAssertEqual("Abc", "Abc".uppercasedFirstLetter)
        XCTAssertEqual("Ábc", "ábc".uppercasedFirstLetter)
        XCTAssertEqual("ABC", "aBC".uppercasedFirstLetter)
        XCTAssertEqual("ABc", "ABc".uppercasedFirstLetter)
        XCTAssertEqual("ÁbC", "ábC".uppercasedFirstLetter)
        XCTAssertEqual("0", "0".uppercasedFirstLetter)
        XCTAssertEqual("", "".uppercasedFirstLetter)
        XCTAssertEqual(" ", " ".uppercasedFirstLetter)
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

        XCTAssertEqual(
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

        XCTAssertEqual(
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

        XCTAssertEqual(
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

        XCTAssertEqual(
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
        XCTAssertEqual(ranges.count, 0)
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
        XCTAssertEqual(ranges.count, 2)
        XCTAssertEqual(ranges[0], input.range(of: "// A comment!\n"))
        XCTAssertEqual(
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
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesEntireStringMultiLine() {
        let input = "/* A comment! \n*/"

        let ranges = input.cStyleCommentSectionRanges()
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesOpenMultiLineComment() {
        let input = "/* A comment! \n"

        let ranges = input.cStyleCommentSectionRanges()
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesIgnoresCommentsInStringLiterals() {
        let input = "\"A comment in a string: // etc.\""

        let ranges = input.cStyleCommentSectionRanges()
        XCTAssertEqual(ranges.count, 0)
    }

    @Test
    func commentSectionRangesIgnoresStringLiteralsWithinSingleLineComments() {
        let input = "/* A comment! \"A string\" \n"

        let ranges = input.cStyleCommentSectionRanges()
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(ranges[0], input.startIndex..<input.endIndex)
    }

    @Test
    func commentSectionRangesIgnoresStringLiteralsWithinMultiLineComments() {
        let input = """
            /* A comment! "An unterminated string literal
            */ "A string /* */"
            """

        let ranges = input.cStyleCommentSectionRanges()
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(
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
        XCTAssertEqual(ranges.count, 3)
        XCTAssertEqual(ranges[0], input.range(of: "1"))
        XCTAssertEqual(ranges[1], input.range(of: "2"))
        XCTAssertEqual(ranges[2], input.range(of: "3"))

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 3)
        XCTAssertEqual(rangesWithLineBreaks[0], input.range(of: "1\n"))
        XCTAssertEqual(rangesWithLineBreaks[1], input.range(of: "2\n"))
        XCTAssertEqual(rangesWithLineBreaks[2], input.range(of: "3"))
    }

    @Test
    func lineRangesWithEmptyLines() {
        let input = """
            1

            3
            """

        let ranges = input.lineRanges()
        XCTAssertEqual(ranges.count, 3)
        XCTAssertEqual(ranges[0], input.range(of: "1"))
        XCTAssertEqual(
            ranges[1],
            input.range(of: "1\n")!.upperBound..<input.range(of: "\n3")!.lowerBound
        )
        XCTAssertEqual(ranges[2], input.range(of: "3"))

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 3)
        XCTAssertEqual(rangesWithLineBreaks[0], input.range(of: "1\n"))
        XCTAssertEqual(
            rangesWithLineBreaks[1],
            input.range(of: "1\n")!.upperBound..<input.range(of: "3")!.lowerBound
        )
        XCTAssertEqual(rangesWithLineBreaks[2], input.range(of: "3"))
    }

    @Test
    func lineRangesWithEmptyFinalLine() {
        let input = """
            1

            """

        let ranges = input.lineRanges()
        XCTAssertEqual(ranges.count, 2)
        XCTAssertEqual(ranges[0], input.range(of: "1"))
        XCTAssertEqual(ranges[1], input.range(of: "1\n")!.upperBound..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 2)
        XCTAssertEqual(rangesWithLineBreaks[0], input.range(of: "1\n"))
        XCTAssertEqual(rangesWithLineBreaks[1], input.range(of: "1\n")!.upperBound..<input.endIndex)
    }

    @Test
    func lineRangesWithTwoEmptyFinalLines() {
        let input = """
            1


            """

        let ranges = input.lineRanges()
        XCTAssertEqual(ranges.count, 3)
        XCTAssertEqual(ranges[0], input.intRange(0..<1))
        XCTAssertEqual(ranges[1], input.intRange(2..<2))
        XCTAssertEqual(ranges[2], input.intRange(3..<3))

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 3)
        XCTAssertEqual(rangesWithLineBreaks[0], input.intRange(0..<2))
        XCTAssertEqual(rangesWithLineBreaks[1], input.intRange(2..<3))
        XCTAssertEqual(rangesWithLineBreaks[2], input.intRange(3..<3))
    }

    @Test
    func lineRangesWithSingleLine() {
        let input = "123"

        let ranges = input.lineRanges()
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(ranges[0], input.startIndex..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 1)
        XCTAssertEqual(rangesWithLineBreaks[0], input.startIndex..<input.endIndex)
    }

    @Test
    func lineRangesWithEmptyString() {
        let input = ""

        let ranges = input.lineRanges()
        XCTAssertEqual(ranges.count, 1)
        XCTAssertEqual(ranges[0], input.startIndex..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 1)
        XCTAssertEqual(rangesWithLineBreaks[0], input.startIndex..<input.endIndex)
    }

    @Test
    func lineRangesWithSingleEmptyLine() {
        let input = "\n"

        let ranges = input.lineRanges()
        XCTAssertEqual(ranges.count, 2)
        XCTAssertEqual(ranges[0], input.startIndex..<input.startIndex)
        XCTAssertEqual(ranges[1], input.endIndex..<input.endIndex)

        let rangesWithLineBreaks = input.lineRanges(includeLineBreak: true)
        XCTAssertEqual(rangesWithLineBreaks.count, 2)
        XCTAssertEqual(rangesWithLineBreaks[0], input.startIndex..<input.endIndex)
        XCTAssertEqual(rangesWithLineBreaks[1], input.endIndex..<input.endIndex)
    }

    @Test
    func trimWhitespace() {
        XCTAssertEqual(SwiftPEG.trimWhitespace(""), "")
        XCTAssertEqual(SwiftPEG.trimWhitespace("  "), "")
        XCTAssertEqual(SwiftPEG.trimWhitespace("a "), "a")
        XCTAssertEqual(SwiftPEG.trimWhitespace(" a"), "a")
        XCTAssertEqual(SwiftPEG.trimWhitespace("abc"), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespace("abc "), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespace(" abc"), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespace("  abc "), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespace("\nabc\n"), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespace("\n abc def \t "), "abc def")
        XCTAssertEqual(SwiftPEG.trimWhitespace("  abc def "), "abc def")
    }

    @Test
    func trimWhitespaceLead() {
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead(""), "")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("  "), "")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("a "), "a ")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead(" a"), "a")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("abc"), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("abc "), "abc ")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead(" abc"), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("  abc "), "abc ")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("\nabc\n"), "abc\n")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("\n abc def \t "), "abc def \t ")
        XCTAssertEqual(SwiftPEG.trimWhitespaceLead("  abc def "), "abc def ")
    }

    @Test
    func trimWhitespaceTrail() {
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail(""), "")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("  "), "")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("a "), "a")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail(" a"), " a")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("abc"), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("abc "), "abc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail(" abc"), " abc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("  abc "), "  abc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("\nabc\n"), "\nabc")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("\n abc def \t "), "\n abc def")
        XCTAssertEqual(SwiftPEG.trimWhitespaceTrail("  abc def "), "  abc def")
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
