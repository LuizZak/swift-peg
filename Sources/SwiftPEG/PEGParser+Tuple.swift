public extension PEGParser {
    /// Shuffles an optional-tuple (`(a, b, c, ...)?`) into a tuple-of-optionals
    /// (`(a?, b?, c?, ...)`) for pattern-unbinding.
    @inlinable
    func shuffleTuple<each T>(_ tuple: (repeat each T)?) -> (repeat (each T)?) {
        if let tuple = tuple {
            return (repeat each tuple)
        }

        return (repeat nil as (each T)?)
    }

    /// Shuffles a doubly-nested optional-tuple (`(a, b, c, ...)??`) into a
    /// tuple-of-optional-optionals (`(a??, b??, c??, ...)`) for pattern-unbinding.
    @inlinable
    func shuffleTuple2<each T>(_ tuple: (repeat each T)??) -> (repeat (each T)??) {
        if case let tuple?? = tuple {
            return (repeat each tuple)
        }

        return (repeat nil as (each T)??)
    }
}
