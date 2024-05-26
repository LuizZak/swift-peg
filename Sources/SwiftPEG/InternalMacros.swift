/// Synthesizes a `var is<CaseName>: Bool` variable for all of an attached enum's
/// case element declarations.
@attached(member, names: arbitrary)
macro GeneratedCaseChecks(accessLevel: String = "") =
    #externalMacro(module: "SwiftPEGMacros", type: "EnumIsCaseGenerator")

/// Synthesizes a `var is<CaseName>: Bool` variable for an attached enum case's
/// element declaration.
@attached(peer, names: arbitrary)
macro GeneratedIsCase(accessLevel: String = "") =
    #externalMacro(module: "SwiftPEGMacros", type: "EnumIsCaseGenerator")
