import XCTest
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport

@testable import SwiftPEGMacros

class EnumAsGetterGeneratorTests: XCTestCase {
    let testMacros: [String: Macro.Type] = [
        "GenerateAsCase": EnumAsGetterGenerator.self,
    ]
}
