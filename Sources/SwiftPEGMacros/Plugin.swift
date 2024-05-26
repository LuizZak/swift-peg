import SwiftCompilerPlugin
import SwiftSyntaxMacros

@main
struct Macros: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        ParserMemoizeMacro.self,
        ParserMemoizeGeneratingMacro.self,
        ParserMemoizeLeftRecursiveMacro.self,
        NodeTypeMacro.self,
        EnumIsCaseGenerator.self,
    ]
}
