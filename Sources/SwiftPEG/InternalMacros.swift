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

/// Synthesizes a getter for the associated values of all cases of an attached
/// enumeration declaration:
///
/// ```
/// var as<CaseName>: (<Associated Value Types>)? {
///     switch self {
///     case .<caseName>(<bindings>): return <bindings>
///     default: return nil
///     }
/// }
/// ```
///
/// If no case of the enumeration has an associated value, a warning is issued.
@attached(member, names: arbitrary)
macro generateGetters(accessLevel: String? = nil) =
    #externalMacro(module: "SwiftPEGMacros", type: "EnumAsGetterGenerator")

/// Synthesizes a getter for the associated values of an enumeration's case
/// declaration:
///
/// ```
/// var as<CaseName>: (<Associated Value Types>)? {
///     switch self {
///     case .<caseName>(<bindings>): return <bindings>
///     default: return nil
///     }
/// }
/// ```
///
/// If attached to an enum case that has no associated values, an error is issued.
@attached(peer, names: arbitrary)
macro generateGetter(accessLevel: String? = nil) =
    #externalMacro(module: "SwiftPEGMacros", type: "EnumAsGetterGenerator")
