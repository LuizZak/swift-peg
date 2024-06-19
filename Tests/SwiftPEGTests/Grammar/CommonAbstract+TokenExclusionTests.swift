import XCTest

@testable import SwiftPEG

class CommonAbstract_TokenExclusionTests: XCTestCase {
    func testExcludes_anyTerminal() {
        assertDoesNotExclude(.identifier("ident"), terminal: .any)
        assertDoesNotExclude(.rangeLiteral("a", "z"), terminal: .any)
        assertDoesNotExclude(.literal("literal"), terminal: .any)
    }

    func testExcludes_characterPredicate() {
        let terminal = CommonAbstract.TokenTerminal.characterPredicate("a", #"a != """#)

        assertDoesNotExclude(.identifier("ident"), terminal: terminal)
        assertDoesNotExclude(.rangeLiteral("a", "z"), terminal: terminal)
        assertDoesNotExclude(.literal("literal"), terminal: terminal)
    }

    func testExcludes_identifierExclusion() {
        assertExcludes(.identifier("ident"), terminal: .identifier("ident"))
        assertDoesNotExclude(.identifier("ident"), terminal: .identifier("b"))
        assertDoesNotExclude(.identifier("ident"), terminal: .literal("literal"))
        assertDoesNotExclude(.identifier("ident"), terminal: .rangeLiteral("a", "z"))
        assertDoesNotExclude(.identifier("ident"), terminal: .any)
    }

    func testExcludes_literalExclusion() {
        assertExcludes(.literal("a"), terminal: .literal("a"))
        assertExcludes(.literal("a"), terminal: .rangeLiteral("a", "a"))
        assertDoesNotExclude(.literal("a"), terminal: .identifier("a"))
        assertDoesNotExclude(.literal("a"), terminal: .literal("literal"))
        assertDoesNotExclude(.literal("a"), terminal: .rangeLiteral("a", "z"))
        assertDoesNotExclude(.literal("a"), terminal: .any)
    }

    func testExcludes_literalExclusion_literalTerminal_exclusionIsPrefix() {
        assertExcludes(.literal("a"), terminal: .literal("abc"))
    }

    func testExcludes_literalExclusion_literalTerminal_terminalIsPrefix() {
        assertDoesNotExclude(.literal("abc"), terminal: .literal("a"))
    }

    func testExcludes_rangeLiteralExclusion() {
        assertExcludes(.rangeLiteral("a", "d"), terminal: .rangeLiteral("a", "d"))
        assertExcludes(.rangeLiteral("a", "d"), terminal: .literal("a"))
        assertDoesNotExclude(.rangeLiteral("a", "d"), terminal: .identifier("a"))
        assertDoesNotExclude(.rangeLiteral("a", "c"), terminal: .rangeLiteral("a", "d"))
        assertDoesNotExclude(.rangeLiteral("b", "d"), terminal: .rangeLiteral("a", "d"))
    }

    func testExcludes_rangeLiteralExclusion_literalTerminal_longerLiteral() {
        assertExcludes(.rangeLiteral("a", "d"), terminal: .literal("abc"))
    }

    func testExcludes_rangeLiteralExclusion_rangeLiteralTerminal_overlap() {
        assertDoesNotExclude(.rangeLiteral("a", "d"), terminal: .rangeLiteral("b", "e"))
    }

    func testExcludes_rangeLiteralExclusion_rangeLiteralTerminal_exclusionContainsTerminal() {
        assertExcludes(.rangeLiteral("a", "d"), terminal: .rangeLiteral("a", "c"))
        assertExcludes(.rangeLiteral("a", "d"), terminal: .rangeLiteral("b", "d"))
    }

    func testExcludes_rangeLiteralExclusion_rangeLiteralTerminal_terminalContainsExclusion() {
        assertDoesNotExclude(.rangeLiteral("a", "c"), terminal: .rangeLiteral("a", "d"))
        assertDoesNotExclude(.rangeLiteral("b", "d"), terminal: .rangeLiteral("a", "d"))
    }
}

// MARK: - Test internals

func assertExcludes(
    _ sut: CommonAbstract.TokenExclusion,
    terminal: CommonAbstract.TokenTerminal,
    file: StaticString = #file,
    line: UInt = #line
) {
    assertTrue(
        sut.excludes(terminal),
        message: "Expected '\(sut)' to exclude '\(terminal)'",
        file: file,
        line: line
    )
}

func assertDoesNotExclude(
    _ sut: CommonAbstract.TokenExclusion,
    terminal: CommonAbstract.TokenTerminal,
    file: StaticString = #file,
    line: UInt = #line
) {
    assertFalse(
        sut.excludes(terminal),
        message: "Expected '\(sut)' to not exclude '\(terminal)'",
        file: file,
        line: line
    )
}
