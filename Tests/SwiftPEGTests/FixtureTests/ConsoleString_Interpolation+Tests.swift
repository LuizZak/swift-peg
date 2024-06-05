import Foundation
import Console
import SwiftPEG

extension ConsoleString.Interpolation {
    /// Interpolates a number that represents a failure or other error condition,
    /// coloring the number as green if it is zero, or red when not zero.
    mutating func appendInterpolation(failureCounter: Int) {
        appendInterpolation(
            failureCounter,
            color: failureCounter == 0 ? .green : .red
        )
    }

    /// Interpolates a given meta-property's name into this console string as
    /// `@<propertyName>`.
    mutating func appendInterpolation(name meta: SwiftPEGGrammar.Meta) {
        appendInterpolation("@\(meta.name.string)", color: .magenta)
    }

    /// Interpolates a URL by appending the URL's base URL as light text, before
    /// appending the relative path of the URL as normal text, with the last path
    /// component as a cyan color.
    mutating func appendInterpolation(url: URL, includeBase: Bool = false) {
        if includeBase, let baseUrl = url.baseURL {
            appendInterpolation(baseUrl.path, format: .light)
        }

        appendInterpolation(url.deletingLastPathComponent().relativePath)
        appendInterpolation("/")
        appendInterpolation(url.lastPathComponent, color: .cyan)
    }

    /// Interpolates a URL by appending the URL's last path component as a cyan
    /// color.
    mutating func appendInterpolation(fileName: URL) {
        appendInterpolation(fileName.lastPathComponent, color: .cyan)
    }
}
