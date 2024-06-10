import Foundation
import SwiftSyntax

class MacroArgumentParser {
    /// Attempts to parse a JSON-decodable object by converting a given attribute
    /// syntax into a JSON object, and if succeeding, attempting to parse the
    /// JSON value as an instance of a given type with `JSONDecoder`.
    static func parse<T: Decodable>(
        _ type: T.Type = T.self,
        attributeSyntax: AttributeSyntax
    ) throws -> T {

        let json = try attributeSyntax._toJson().description
        guard let data = json.data(using: .utf8) else {
            throw MacroError.diagnostic(
                attributeSyntax.ext_errorDiagnostic(message: """
                Failed to convert attribute's JSON object \(json) into a String?
                """)
            )
        }

        let decoder = JSONDecoder()

        do {
            return try decoder.decode(T.self, from: data)
        } catch DecodingError.typeMismatch(let type, let ctx) {
            // Try to attach errors to the attribute syntax
            if let stringValue = ctx.codingPath.last?.stringValue {
                throw MacroError.diagnostic(
                    attributeSyntax.ext_errorDiagnostic(message: """
                    Expected value of type \(type) for attribute argument \(stringValue)
                    """)
                )
            }

            let error = DecodingError.typeMismatch(type, ctx)
            throw MacroError.diagnostic(
                attributeSyntax.ext_errorDiagnostic(message: "\(error)")
            )
        } catch {
            throw MacroError.diagnostic(
                attributeSyntax.ext_errorDiagnostic(message: "\(error)")
            )
        }
    }
}

private extension AttributeSyntax {
    /// Converts this `AttributeSyntax` into a JSON object, containing the
    /// arguments of the syntax.
    ///
    /// If the argument set of the attribute is empty or not provided, an empty
    /// JSON object (`"{}"`) is returned, instead.
    ///
    /// Throws if the arguments could not be converted to a JSON representation.
    func _toJson() throws -> JSON {
        if let arguments = self.arguments {
            switch arguments {
            case .argumentList(let list):
                return try list._toJson()
            default:
                throw MacroError.message(
                    "Unsupported attribute argument kind \(arguments.kind)"
                )
            }
        }

        return JSON.object([:])
    }
}

private extension LabeledExprListSyntax {
    /// Used by `AttributeSyntax._toJson()`.
    ///
    /// Throws if the arguments could not be converted to a JSON representation.
    ///
    /// - note: for arguments that are not labeled, the dictionary values are
    /// auto-labeled to be `"_v<index>"`, where <index> is the absolute index of
    /// the argument.
    func _toJson() throws -> JSON {
        var object: [String: JSON] = [:]

        for (i, expr) in self.enumerated() {
            guard let exprJson = try expr.expression._toJson() else {
                throw MacroError.diagnostic(
                    expr.expression.ext_errorDiagnostic(message: """
                    Failed to parse macro argument
                    """)
                )
            }

            let label = expr.label?.trimmed
            let key = label?.description ?? "_v\(i)"

            object[key] = exprJson
        }

        return .object(object)
    }
}

private extension ExprSyntax {
    /// Used by `AttributeSyntax._toJson()`.
    ///
    /// Accepted expression types and conversions:
    /// - `StringLiteralExprSyntax` -> `JSON.string`
    /// - `IntegerLiteralExprSyntax` -> `JSON.number`
    /// - `FloatLiteralExprSyntax` -> `JSON.number`
    /// - `BooleanLiteralExprSyntax` -> `JSON.boolean`
    /// - `NilLiteralExprSyntax` -> `JSON.null`
    func _toJson() throws -> JSON? {
        if let stringLiteral = self.as(StringLiteralExprSyntax.self) {
            return .string(stringLiteral.segments.description)
        }
        if let integerLiteral = self.as(IntegerLiteralExprSyntax.self)?.trimmed {
            if let value = Int(integerLiteral.description) {
                return .number(Double(value))
            }

            throw MacroError.diagnostic(
                integerLiteral.ext_errorDiagnostic(
                    message: "Failed to parse Integer literal value"
                )
            )
        }
        if let floatingLiteral = self.as(FloatLiteralExprSyntax.self)?.trimmed {
            if let value = Double(floatingLiteral.description) {
                return .number(value)
            }

            throw MacroError.diagnostic(
                floatingLiteral.ext_errorDiagnostic(
                    message: "Failed to parse Double literal value"
                )
            )
        }
        if let booleanLiteral = self.as(BooleanLiteralExprSyntax.self) {
            if booleanLiteral.literal == .keyword(.true) {
                return .boolean(true)
            }
            if booleanLiteral.literal == .keyword(.false) {
                return .boolean(false)
            }

            throw MacroError.diagnostic(
                booleanLiteral.ext_errorDiagnostic(
                    message: "Unrecognized boolean literal"
                )
            )
        }
        if self.is(NilLiteralExprSyntax.self) {
            return .null
        }

        return nil
    }
}

/// Internal JSON representation used for macro argument parsing.
private indirect enum JSON: Hashable, CustomStringConvertible {
    case null
    case string(String)
    case number(Double)
    case object([String: JSON])
    case array([JSON])
    case boolean(Bool)

    /// Converts this JSON object into a JSON-compliant string.
    var description: String {
        switch self {
        case .null:
            return "null"

        case .array(let value):
            return "[\(value.map(\.description).joined(separator: ","))]"

        case .boolean(let value):
            return value.description

        case .object(let value):
            return "{\(value.map({ "\($0.key.debugDescription): \($0.value)" }).joined(separator: ","))}"

        case .number(let value):
            return value.description

        case .string(let value):
            return value.debugDescription
        }
    }
}
