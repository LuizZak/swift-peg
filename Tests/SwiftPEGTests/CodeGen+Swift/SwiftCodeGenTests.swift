import XCTest

@testable import SwiftPEG

class SwiftCodeGenTests: XCTestCase {
    func testGenerateParser_emptyGrammar() throws {
        let grammar = makeGrammar([])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
            }
            """).diff(result)
    }

    func testGenerateParser_altAction() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: ["b"], action: "CustomAction()")
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let mark = self.mark()

                    if
                        let b = try self.b()
                    {
                        return CustomAction()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_singleRule() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: .init(name: "ANode"), alts: [
                ["b", "c", .item(.optional("d"))],
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                @memoized("a")
                @inlinable
                public func __a() throws -> ANode? {
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        let d = try self.optional({
                            try self.d()
                        })
                    {
                        return ANode()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_groupInAtom() throws {
        let grammar = makeGrammar([
            .init(name: "a", type: .init(name: "ANode"), alts: [
                ["b", "c", .item(.atom(.group([["d", "e"]])))],
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                @memoized("a")
                @inlinable
                public func __a() throws -> ANode? {
                    let mark = self.mark()

                    if
                        let b = try self.b(),
                        let c = try self.c(),
                        let _ = try self._a__group_()
                    {
                        return ANode()
                    }

                    self.restore(mark)
                    return nil
                }

                @memoized("_a__group_")
                @inlinable
                public func ___a__group_() throws -> Node? {
                    let mark = self.mark()
                    var cut = CutFlag()

                    if
                        let d = try self.d(),
                        let e = try self.e()
                    {
                        return Node()
                    }

                    self.restore(mark)
                    return nil
                }
            }
            """).diff(result)
    }

    func testGenerateParser_generateCut() throws {
        let grammar = makeGrammar([
            .init(name: "a", alts: [
                .init(items: ["b", .lookahead(.cut), "c"], action: "CustomAction()")
            ])
        ])
        let sut = makeSut(grammar)

        let result = try sut.generateParser()

        diffTest(expected: """
            // TestParser
            extension TestParser {
                @memoized("a")
                @inlinable
                public func __a() throws -> Node? {
                    let mark = self.mark()
                    var cut = CutFlag()

                    if
                        let b = try self.b(),
                        cut.toggleOn(),
                        let c = try self.c()
                    {
                        return CustomAction()
                    }

                    self.restore(mark)

                    if cut.isOn {
                        return nil
                    }
                    return nil
                }
            }
            """).diff(result)
    }
}

// MARK: - Test internals

private func makeSut(_ grammar: CodeGen.Grammar) -> SwiftCodeGen {
    SwiftCodeGen(grammar: grammar)
}

private func makeGrammar(
    parserName: String = "TestParser",
    parserHeader: String = "// TestParser",
    _ rules: [CodeGen.Rule]
) -> CodeGen.Grammar {

    .init(
        metas: [
            .init(name: SwiftCodeGen.parserHeader, value: .string(parserHeader)),
            .init(name: SwiftCodeGen.parserName, value: .string(parserName)),
        ],
        rules: rules
    )
}
