import SwiftPEGMacros

/// Synthesizes a memoized version of the attached function.
/// 
/// Expects that the function's return type is not `Void`, and all parameters, if
/// present, conform to `Hashable`.
@attached(peer, names: arbitrary)
public macro memoized(_ method: String, cacheTarget: String? = nil) =
    #externalMacro(module: "SwiftPEGMacros", type: "ParserMemoizeMacro")

/// Synthesizes a memoized version of the attached function, with support for
/// handling left-recursive rule reentry.
/// 
/// Expects that the function's return type is not `Void`, and all parameters, if
/// present, conform to `Hashable`.
@attached(peer, names: arbitrary)
public macro memoizedLeftRecursive(_ method: String, cacheTarget: String? = nil) =
    #externalMacro(module: "SwiftPEGMacros", type: "ParserMemoizeLeftRecursiveMacro")

/// Synthesizes properties wrapping existing `Node`-typed fields and `children`
/// member for node types.
/// 
/// To synthesize properties on nodes, the macro expects desired fields to be
/// wrapped with `@NodeProperty`.
@attached(member, names: arbitrary, named(children), named(init), named(deepCopy))
public macro GeneratedNodeType<T>(overrideDeepCopyType: String? = nil) =
    #externalMacro(module: "SwiftPEGMacros", type: "NodeTypeMacro")

/// Property wrapper that is type-aliased into different constructs that are
/// detected by `NodeTypeMacro` to indicate attributes of fields of types being
/// macro-expanded.
///
/// By itself does nothing but wrap the value.
@propertyWrapper
public struct NodeMacroWrapper<T> {
    public var wrappedValue: T

    @inlinable
    public init(wrappedValue: T) {
        self.wrappedValue = wrappedValue
    }
}

/// Property wrapper for signaling fields to create node properties with when
/// used with `NodeTypeMacro`.
///
/// By itself does nothing but wrap the value.
public typealias NodeProperty<T> = NodeMacroWrapper<T>

/// Property wrapper for signaling fields that are required during the node's
/// construction, but are not node types themselves.
///
/// By itself does nothing but wrap the value.
public typealias NodeRequired<T> = NodeMacroWrapper<T>
