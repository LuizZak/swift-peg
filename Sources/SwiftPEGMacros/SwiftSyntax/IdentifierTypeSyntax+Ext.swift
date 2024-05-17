import SwiftSyntax

extension IdentifierTypeSyntax {
    /// Attempts to extract this identifier type's generic parameter, if it
    /// has one.
    /// 
    /// Optionally matches the identifier's name against `name`, if it is not
    /// nil.
    /// 
    /// The identifier must have exactly one generic argument to succeed, returning
    /// `nil`, otherwise.
    func ext_genericParameter1Exact(name: String? = nil) -> (TypeSyntax)? {
        if let name, self.name.trimmed.description != name {
            return nil
        }
        
        guard
            let arguments = genericArgumentClause?.arguments,
            arguments.count == 1,
            let firstArgument = arguments.first
        else {
            return nil
        }

        return firstArgument.argument
    }
}
