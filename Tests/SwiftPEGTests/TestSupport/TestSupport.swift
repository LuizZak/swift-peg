import XCTest

private func _formatMessage(_ msg: String) -> String {
    if msg.isEmpty {
        return ""
    }
    return " - \(msg)"
}

// MARK: -

/// Used as a stub for code paths that indicate success.
func success() {}

/// Fails a test with a given message.
func fail(_ message: String, file: StaticString = #file, line: UInt = #line) {
    XCTFail(message, file: file, line: line)
}

/// Asserts a given Optional is not nil, returning its value, throwing if it is
/// nil.
func assertUnwrap<T>(
    _ value: T?,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) throws -> T {
    try XCTUnwrap(value, message(), file: file, line: line)
}

/// Asserts that a value of type `T` can be dynamically cast to `U`, throwing if
/// it is not. 
func assertCast<T, U>(
    _ value: T,
    to type: U.Type = U.self,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) throws -> U {

    try assertUnwrap(
        value as? U,
        message: "\(#function) failure: \(value) cannot be cast to \(U.self) \(message())",
        file: file,
        line: line
    )
}

/// Asserts a given Optional is not nil.
func assertNotNil<T>(
    _ value: T?,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {
    XCTAssertNotNil(value, message(), file: file, line: line)
}

/// Asserts a given Optional is nil.
func assertNil<T>(
    _ value: T?,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {
    XCTAssertNil(value, message(), file: file, line: line)
}

/// Asserts a given Bool is `true`.
func assertTrue(
    _ value: Bool,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {
    XCTAssertTrue(value, message(), file: file, line: line)
}

/// Asserts a given Bool is `false`.
func assertFalse(
    _ value: Bool,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {
    XCTAssertFalse(value, message(), file: file, line: line)
}

/// Asserts two reference values point to the same object.
func assertIdentical<T>(
    _ lhs: T?,
    _ rhs: T?,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: AnyObject {

    XCTAssertIdentical(lhs, rhs, message(), file: file, line: line)
}

/// Asserts two equatable values are the same.
func assertEqual<T>(
    _ lhs: T,
    _ rhs: T,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    XCTAssertEqual(lhs, rhs, message(), file: file, line: line)
}

/// Asserts two equatable values are the same. Uses debug description of the values
/// in error message.
func assertEqual<T>(
    _ lhs: T,
    _ rhs: T,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable & CustomDebugStringConvertible {

    XCTAssertEqual(
        lhs,
        rhs,
        message() + " lhs: \(lhs.debugDescription) rhs: \(rhs.debugDescription)",
        file: file,
        line: line
    )
}

/// Asserts two results with equatable Success and Failure values are the same.
func assertEqual<T, E>(
    _ lhs: Result<T, E>,
    _ rhs: Result<T, E>,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable, E: Equatable {

    XCTAssertEqual(lhs, rhs, message(), file: file, line: line)
}

/// Asserts a block does not throw an error.
func assertNoThrow<T>(
    _ block: () throws -> T,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) throws -> T {

    do {
        return try block()
    } catch {
        fail(
            "Expected no error to be thrown \(_formatMessage(message()))",
            file: file,
            line: line
        )

        throw TestError.unexpectedThrow("")
    }
}

/// Asserts a block throws an error.
func assertThrows<T>(
    _ block: () throws -> T,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {
    do {
        _=try block()
        fail(
            "Expected error to be thrown \(_formatMessage(message()))",
            file: file,
            line: line
        )
    } catch {
        success()
    }
}

/// Asserts a `Result<T, any Error>` value is a `.failure()` case.
func assertFailure<T>(
    _ value: Result<T, any Error>,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {

    switch value {
    case .success(let value):
        fail("Expected .failure() case, got: .success(\(value))", file: file, line: line)
    case .failure:
        success()
    }
}

/// Asserts two results with equatable Success values are `.success(value)` cases
/// and that `value` are equal across both.
func assertSuccessEqual<T>(
    _ lhs: Result<T, any Error>,
    _ rhs: Result<T, any Error>,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    switch (lhs, rhs) {
    case (.success(let lhs), .success(let rhs)):
        assertEqual(lhs, rhs, message: message(), file: file, line: line)

    case (.failure(let lhs), .failure(let rhs)):
        fail(
            "Expected success in lhs and rhs, got: \(lhs) \(rhs)\(_formatMessage(message()))",
            file: file,
            line: line
        )

    case (.failure(let lhs), _):
        fail(
            "Expected success in lhs, got: \(lhs)\(_formatMessage(message()))",
            file: file,
            line: line
        )

    case (_, .failure(let rhs)):
        fail(
            "Expected success in rhs, got: \(rhs)\(_formatMessage(message()))",
            file: file,
            line: line
        )
    }
}

/// Asserts two results with equatable Success values are `.success(value)` cases
/// and that `value` are equal across both.
func assertSuccessEqual<T>(
    _ lhs: Result<T?, any Error>,
    _ rhs: Result<T, any Error>,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    switch (lhs, rhs) {
    case (.success(let lhs), .success(let rhs)):
        assertEqual(lhs, rhs, message: message(), file: file, line: line)

    case (.failure(let lhs), .failure(let rhs)):
        fail(
            "Expected success in lhs and rhs, got: \(lhs) \(rhs)\(_formatMessage(message()))",
            file: file,
            line: line
        )

    case (.failure(let lhs), _):
        fail(
            "Expected success in lhs, got: \(lhs)\(_formatMessage(message()))",
            file: file,
            line: line
        )

    case (_, .failure(let rhs)):
        fail(
            "Expected success in rhs, got: \(rhs)\(_formatMessage(message()))",
            file: file,
            line: line
        )
    }
}

/// Asserts that for to sequence of `Result` values with equatable Success values,
/// two results at the same index are `.success(value)` cases and that `value`
/// are equal across both.
/// 
/// Fails if the sequences are of different lengths.
func assertSuccessesEqual<T>(
    _ lhs: some Sequence<Result<T?, any Error>>,
    _ rhs: some Sequence<Result<T?, any Error>>,
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    let lhsArray = Array(lhs)
    let rhsArray = Array(rhs)

    guard lhsArray.count == rhsArray.count else {
        fail("Expected lhs and rhs to have same count, got: lhs = \(lhsArray.count) / rhs = \(rhsArray.count)")
        return
    }

    for (i, (lhsEntry, rhsEntry)) in zip(lhsArray, rhsArray).enumerated() {
        assertSuccessEqual(
            lhsEntry,
            rhsEntry,
            message: "at index \(i)",
            file: file,
            line: line
        )
    }
}

/// Asserts that for a sequence of `Result` values with, all cases are `.failure()`.
func assertFailures<T>(
    _ value: some Sequence<Result<T?, any Error>>,
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    for (i, valueEntry) in value.enumerated() {
        assertFailure(
            valueEntry,
            message: "at index \(i)",
            file: file,
            line: line
        )
    }
}

// MARK: Collection assertions

/// Asserts a collection of items is empty.
func assertEmpty(
    _ value: some Collection,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {

    if !value.isEmpty {
        fail(
            "Collection is not empty: \(value) \(message())",
            file: file,
            line: line
        )
    }
}

/// Asserts a collection of items has a specified count.
func assertCount(
    _ value: some Collection,
    _ count: Int,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {

    if value.count != count {
        fail(
            "Expected collection to have \(count) item(s) but found \(value.count): \(value) \(message())",
            file: file,
            line: line
        )
    }
}

/// Asserts that two collection of items contains the same set of `T` values the
/// same number of times.
func assertEqualUnordered<T>(
    _ lhs: some Collection<T>,
    _ rhs: some Collection<T>,
    message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    if lhs.count != rhs.count {
        fail(
            "lhs.count != rhs.count (\(lhs.count) != \(rhs.count)) \(message())",
            file: file,
            line: line
        )
        return
    }

    let signal: (String) -> Void = {
        fail(
            "lhs != rhs (\(lhs) != \(rhs)) \($0)",
            file: file,
            line: line
        )
    }

    var remaining = Array(lhs)
    for item in rhs {
        if let nextIndex = remaining.firstIndex(of: item) {
            remaining.remove(at: nextIndex)
        } else {
            return signal(message())
        }
    }

    if !remaining.isEmpty {
        signal(message())
    }
}

// MARK: - Definitions

enum TestError: Error, CustomStringConvertible {
    case unexpectedThrow(String)
    
    var description: String {
        switch self {
        case .unexpectedThrow(let message):
            return message
        }
    }
}
