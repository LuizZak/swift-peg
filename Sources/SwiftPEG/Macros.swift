import SwiftPEGMacros

@attached(peer, names: arbitrary)
public macro memoized(_ method: String) =
    #externalMacro(module: "SwiftPEGMacros", type: "ParserMemoizeMacro")
