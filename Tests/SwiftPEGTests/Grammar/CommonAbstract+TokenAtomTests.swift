import XCTest

@testable import SwiftPEG

class CommonAbstractTests_TokenAtom: XCTestCase {
    func testIsUnfulfillable_anyTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.identifier("a"), .literal("a"), .rangeLiteral("a", "Z")],
            terminal: .any
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_characterPredicate() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.identifier("a"), .literal("a"), .rangeLiteral("a", "Z")],
            terminal: .characterPredicate("a", #"a != "b""#)
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_identifierExclusion_identifierTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.identifier("a")],
            terminal: .identifier("a")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_literalExclusion_literalTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.literal("abc")],
            terminal: .literal("abc")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_literalExclusion_literalTerminal_exclusionIsPrefix() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.literal("a")],
            terminal: .literal("abc")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_literalExclusion_literalTerminal_terminalIsPrefix() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.literal("abc")],
            terminal: .literal("a")
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_rangeLiteralExclusion_literalTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "z")],
            terminal: .literal("a")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_rangeLiteralExclusion_literalTerminal_longerLiteral() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "z")],
            terminal: .literal("abc")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_rangeLiteralExclusion_rangeLiteralTerminal_overlap() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "c")],
            terminal: .rangeLiteral("b", "d")
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_rangeLiteralExclusion_rangeLiteralTerminal_exclusionContainsTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "d")],
            terminal: .rangeLiteral("b", "d")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testIsUnfulfillable_rangeLiteralExclusion_rangeLiteralTerminal_terminalContainsExclusion() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("b", "c")],
            terminal: .rangeLiteral("a", "d")
        )

        assertFalse(sut.isUnfulfillable)
    }
}
