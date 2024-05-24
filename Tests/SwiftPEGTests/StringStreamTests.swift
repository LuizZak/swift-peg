import XCTest

@testable import SwiftPEG

class StringStreamTests: XCTestCase {
    func testIsEof_emptyString_returnsTrue() {
        let sut = makeSut("")

        assertTrue(sut.isEof)
    }

    func testIsEof_emptySubstring_returnsTrue() {
        let sut = makeSut("a".dropFirst())

        assertTrue(sut.isEof)
    }
}

// MARK: - Test internals

private func makeSut<StringType>(_ source: StringType) -> StringStream<StringType> {
    return .init(source: source)
}
