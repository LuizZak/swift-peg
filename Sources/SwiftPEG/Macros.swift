import SwiftPEGMacros

/// Synthesizes a memoized version of the attached function.
/// 
/// Expects that the function's return type is not `Void`, and all parameters, if
/// present, conform to `Hashable`.
@attached(peer, names: arbitrary)
public macro memoized(_ method: String, cacheTarget: String? = nil) =
    #externalMacro(module: "SwiftPEGMacros", type: "ParserMemoizeMacro")

/// Synthesizes properties wrapping existing `Node`-typed fields and `children`
/// member for node types.
/// 
/// To synthesize properties on nodes, the macro expects desired fields to be
/// wrapped with `@NodeProperty`.
@attached(member, names: arbitrary, named(children), named(init))
public macro GeneratedNodeType<T>() =
    #externalMacro(module: "SwiftPEGMacros", type: "NodeTypeMacro")
