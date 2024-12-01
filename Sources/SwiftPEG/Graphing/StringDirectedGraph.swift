import MiniDigraph

// Note: Currently crashes compiler (https://github.com/swiftlang/swift/issues/75904)
//typealias StringDirectedGraph = CachingDirectedGraph<AbstractDirectedGraph<String, DirectedGraph<String>.Edge>>
typealias StringDirectedGraph = InternalCachingDirectedGraph<String>
