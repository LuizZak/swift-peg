import SwiftSyntax

extension DeclGroupSyntax {
    /// Attempts to fetch the access level modifier attached to this declaration.
    func ext_accessLevel() -> DeclModifierSyntax? {
        modifiers.first(where: { $0.ext_isAccessLevel() })
    }
}

extension DeclModifierSyntax {
    func ext_isAccessLevel() -> Bool {
        switch name.trimmed.description {
        case "open", "public", "internal", "fileprivate", "private":
            return true
        default:
            return false
        }
    }
}
