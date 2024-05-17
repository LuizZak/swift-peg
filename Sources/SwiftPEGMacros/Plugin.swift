import SwiftCompilerPlugin
import SwiftSyntaxMacros

@main
struct Macros: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        ParserMemoizeMacro.self,
        ParserMemoizeGeneratingMacro.self,
        NodeTypeMacro.self,
    ]
}
