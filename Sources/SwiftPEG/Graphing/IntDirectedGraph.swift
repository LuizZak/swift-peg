import MiniDigraph

// Note: Currently crashes compiler (https://github.com/swiftlang/swift/issues/75904)
//typealias IntDirectedGraph = CachingDirectedGraph<AbstractDirectedGraph<Int, DirectedGraph<Int>.Edge>>
typealias IntDirectedGraph = InternalCachingDirectedGraph<Int>
