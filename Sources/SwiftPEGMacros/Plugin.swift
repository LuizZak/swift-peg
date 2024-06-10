import SwiftCompilerPlugin
import SwiftSyntaxMacros

@main
struct Macros: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        ParserMemoizeMacro.self,
        ParserMemoizeLeftRecursiveMacro.self,
        NodeTypeMacro.self,
        EnumIsCaseGenerator.self,
        EnumAsGetterGenerator.self,
    ]
}
