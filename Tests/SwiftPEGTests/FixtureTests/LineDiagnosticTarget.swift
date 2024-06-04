import Foundation

/// Protocol that wraps file/line error messaging into an interface that exposes
/// relative line offset messaging.
protocol LineDiagnosticTarget {
    /// Gets the URL for the file associated with this diagnostic target.
    var fileUrl: URL { get }

    /// Issues a message on a given line on this diagnostic target.
    func diagnoseError(message: String, line: Int)
}
