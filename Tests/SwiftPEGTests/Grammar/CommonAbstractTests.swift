import XCTest

@testable import SwiftPEG

class CommonAbstractTests: XCTestCase {
    func testTokenAtom_isUnfulfillable_anyTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.identifier("a"), .string("a"), .rangeLiteral("a", "Z")],
            terminal: .any
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_characterPredicate() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.identifier("a"), .string("a"), .rangeLiteral("a", "Z")],
            terminal: .characterPredicate("a", #"a != "b""#)
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_identifierExclusion_identifierTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.identifier("a")],
            terminal: .identifier("a")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_literalExclusion_literalTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.string("abc")],
            terminal: .literal("abc")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_literalExclusion_literalTerminal_exclusionIsPrefix() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.string("a")],
            terminal: .literal("abc")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_literalExclusion_literalTerminal_terminalIsPrefix() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.string("abc")],
            terminal: .literal("a")
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_rangeLiteralExclusion_literalTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "z")],
            terminal: .literal("a")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_rangeLiteralExclusion_literalTerminal_longerLiteral() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "z")],
            terminal: .literal("abc")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_rangeLiteralExclusion_rangeLiteralTerminal_overlap() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "c")],
            terminal: .rangeLiteral("b", "d")
        )

        assertFalse(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_rangeLiteralExclusion_rangeLiteralTerminal_exclusionContainsTerminal() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("a", "d")],
            terminal: .rangeLiteral("b", "d")
        )

        assertTrue(sut.isUnfulfillable)
    }

    func testTokenAtom_isUnfulfillable_rangeLiteralExclusion_rangeLiteralTerminal_terminalContainsExclusion() {
        let sut = CommonAbstract.TokenAtom(
            excluded: [.rangeLiteral("b", "c")],
            terminal: .rangeLiteral("a", "d")
        )

        assertFalse(sut.isUnfulfillable)
    }
}
