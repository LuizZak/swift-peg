import SwiftSyntax

extension PatternBindingListSyntax {
    /// Returns all simple identifier pattern binds in this pattern binding list.
    func ext_bindings() -> [(name: TokenSyntax, type: TypeSyntax?, binding: PatternBindingSyntax)] {
        var result: [(name: TokenSyntax, type: TypeSyntax?, binding: PatternBindingSyntax)] = []

        for binding in self {
            guard let pattern = binding.pattern.as(IdentifierPatternSyntax.self) else {
                continue
            }

            result.append((
                name: pattern.identifier,
                type: binding.typeAnnotation?.type,
                binding: binding
            ))
        }

        return result
    }
}
