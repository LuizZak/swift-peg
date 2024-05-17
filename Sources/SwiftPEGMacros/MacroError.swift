import SwiftDiagnostics

/// Errors that can be thrown during macro expansions.
enum MacroError: Swift.Error, CustomStringConvertible {
    /// Generic error message.
    case message(String)

    /// Thrown error that wraps a diagnostic message.
    /// 
    /// Should be intercepted by macros and forwarded properly via macro context.
    case diagnostic(Diagnostic)

    var description: String {
        switch self {
        case .message(let message):
            return message
        case .diagnostic(let diagnostic):
            return diagnostic.message
        }
    }
}
