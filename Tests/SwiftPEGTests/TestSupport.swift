import XCTest

/// Used as a stub for code paths that indicate success.
func success() {}

/// Fails a test with a given message.
func fail(_ message: String, file: StaticString = #file, line: UInt = #line) {
    XCTFail(message, file: file, line: line)
}

/// Asserts two equatable values are the same.
func assertEqual<T>(
    _ lhs: T,
    _ rhs: T,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    XCTAssertEqual(lhs, rhs, message(), file: file, line: line)
}

/// Asserts two results with equatable Success and Failure values are the same.
func assertEqual<T, E>(
    _ lhs: Result<T, E>,
    _ rhs: Result<T, E>,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable, E: Equatable {

    XCTAssertEqual(lhs, rhs, message(), file: file, line: line)
}

/// Asserts a block throws an error.
func assertThrows<T>(
    _ block: () throws -> T,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) {
    do {
        _=try block()
        fail(
            "Expected error to be thrown\(_formatMessage(message()))",
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
    _ message: @autoclosure () -> String = "",
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
    _ message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    switch (lhs, rhs) {
    case (.success(let lhs), .success(let rhs)):
        assertEqual(lhs, rhs, message(), file: file, line: line)

    case (.failure(let lhs), .failure(let rhs)):
        fail("Expected success in lhs and rhs, got: \(lhs) \(rhs)\(_formatMessage(message()))")

    case (.failure(let lhs), _):
        fail("Expected success in lhs, got: \(lhs)\(_formatMessage(message()))")

    case (_, .failure(let rhs)):
        fail("Expected success in rhs, got: \(rhs)\(_formatMessage(message()))")
    }
}

/// Asserts two results with equatable Success values are `.success(value)` cases
/// and that `value` are equal across both.
func assertSuccessEqual<T>(
    _ lhs: Result<T?, any Error>,
    _ rhs: Result<T, any Error>,
    _ message: @autoclosure () -> String = "",
    file: StaticString = #file,
    line: UInt = #line
) where T: Equatable {

    switch (lhs, rhs) {
    case (.success(let lhs), .success(let rhs)):
        assertEqual(lhs, rhs, message(), file: file, line: line)

    case (.failure(let lhs), .failure(let rhs)):
        fail("Expected success in lhs and rhs, got: \(lhs) \(rhs)\(_formatMessage(message()))")

    case (.failure(let lhs), _):
        fail("Expected success in lhs, got: \(lhs)\(_formatMessage(message()))")

    case (_, .failure(let rhs)):
        fail("Expected success in rhs, got: \(rhs)\(_formatMessage(message()))")
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
            "at index \(i)",
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
            "at index \(i)",
            file: file,
            line: line
        )
    }
}

private func _formatMessage(_ msg: String) -> String {
    if msg.isEmpty {
        return ""
    }
    return " - \(msg)"
}
