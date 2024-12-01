import Testing

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
func fail(
    _ message: String,
    sourceLocation: SourceLocation = #_sourceLocation
) {

    Issue.record(Comment(stringLiteral: message), sourceLocation: sourceLocation)
}

/// Asserts a given Optional is not nil, returning its value, throwing if it is
/// nil.
func assertUnwrap<T>(
    _ value: T?,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) throws -> T {

    return try #require(value, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts that a value of type `T` can be dynamically cast to `U`, throwing if
/// it is not.
func assertCast<T, U>(
    _ value: T,
    to type: U.Type = U.self,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) throws -> U {

    try assertUnwrap(
        value as? U,
        message: "\(#function) failure: \(value) cannot be cast to \(U.self) \(message())",
        sourceLocation: sourceLocation
    )
}

/// Asserts that an optional value of type `T` can be dynamically cast to `U`,
/// throwing if it is not.
func assertCast<T, U>(
    _ value: T?,
    to type: U.Type = U.self,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) throws -> U {

    try assertUnwrap(
        value as? U,
        message: "\(#function) failure: \(String(describing: value)) cannot be cast to \(U.self) \(message())",
        sourceLocation: sourceLocation
    )
}

/// Asserts a given Optional is not nil.
func assertNotNil<T>(
    _ value: T?,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {
    #expect(value != nil, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts a given Optional is nil.
func assertNil<T>(
    _ value: T?,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {
    #expect(value == nil, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts a given Bool is `true`.
func assertTrue(
    _ value: Bool,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {
    #expect(value == true, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts a given Bool is `false`.
func assertFalse(
    _ value: Bool,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {
    #expect(value == false, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts two reference values point to the same object.
func assertIdentical<T>(
    _ lhs: T?,
    _ rhs: T?,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: AnyObject {

    #expect(lhs === rhs, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts two equatable values are the same.
func assertEqual<T>(
    _ lhs: T,
    _ rhs: T,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    #expect(lhs == rhs, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts an equatable value is equal to one of the provided array of values.
/// Uses debug description of the values in error message.
func assertEqualsOneOf<T>(
    _ lhs: T,
    _ rhs: T...,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    if !rhs.contains(lhs) {
        fail(
            "Expected \(lhs) to be contained in one of: \(rhs)",
            sourceLocation: sourceLocation
        )
    }
}

/// Asserts two equatable values are the same. Uses debug description of the values
/// in error message.
func assertEqual<T>(
    _ lhs: T,
    _ rhs: T,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable & CustomDebugStringConvertible {

    #expect(
        lhs == rhs,
        Comment(stringLiteral: message() + " lhs: \(lhs.debugDescription) rhs: \(rhs.debugDescription)"),
        sourceLocation: sourceLocation
    )
}

/// Asserts two results with equatable Success and Failure values are the same.
func assertEqual<T, E>(
    _ lhs: Result<T, E>,
    _ rhs: Result<T, E>,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable, E: Equatable {

    #expect(lhs == rhs, Comment(stringLiteral: message()), sourceLocation: sourceLocation)
}

/// Asserts a block does not throw an error.
func assertNoThrow<T>(
    _ block: () throws -> T,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) throws -> T {

    do {
        return try block()
    } catch {
        fail(
            "Expected no error to be thrown \(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )

        throw TestError.unexpectedThrow("")
    }
}

/// Asserts a block throws an error.
func assertThrows<T>(
    _ block: () throws -> T,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {
    do {
        _=try block()
        fail(
            "Expected error to be thrown \(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )
    } catch {
        success()
    }
}

/// Asserts a block throws an error, and the error is of a given type.
/// Returns the error, if successfully typecast.
@discardableResult
func assertThrows<T, E: Error>(
    errorType: E.Type,
    _ block: () throws -> T,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) -> E? {
    do {
        _=try block()
        fail(
            "Expected error to be thrown \(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )
    } catch let error as E {
        return error
    } catch {
        fail(
            "Expected thrown error to be of type \(E.self), found \(type(of: error)) \(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )
    }

    return nil
}

/// Asserts a `Result<T, any Error>` value is a `.failure()` case.
func assertFailure<T>(
    _ value: Result<T, any Error>,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {

    switch value {
    case .success(let value):
        fail("Expected .failure() case, got: .success(\(value))", sourceLocation: sourceLocation)
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
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    switch (lhs, rhs) {
    case (.success(let lhs), .success(let rhs)):
        assertEqual(lhs, rhs, message: message(), sourceLocation: sourceLocation)

    case (.failure(let lhs), .failure(let rhs)):
        fail(
            "Expected success in lhs and rhs, got: \(lhs) \(rhs)\(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )

    case (.failure(let lhs), _):
        fail(
            "Expected success in lhs, got: \(lhs)\(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )

    case (_, .failure(let rhs)):
        fail(
            "Expected success in rhs, got: \(rhs)\(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )
    }
}

/// Asserts two results with equatable Success values are `.success(value)` cases
/// and that `value` are equal across both.
func assertSuccessEqual<T>(
    _ lhs: Result<T?, any Error>,
    _ rhs: Result<T, any Error>,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    switch (lhs, rhs) {
    case (.success(let lhs), .success(let rhs)):
        assertEqual(lhs, rhs, message: message(), sourceLocation: sourceLocation)

    case (.failure(let lhs), .failure(let rhs)):
        fail(
            "Expected success in lhs and rhs, got: \(lhs) \(rhs)\(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )

    case (.failure(let lhs), _):
        fail(
            "Expected success in lhs, got: \(lhs)\(_formatMessage(message()))",
            sourceLocation: sourceLocation
        )

    case (_, .failure(let rhs)):
        fail(
            "Expected success in rhs, got: \(rhs)\(_formatMessage(message()))",
            sourceLocation: sourceLocation
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
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    let lhsArray = Array(lhs)
    let rhsArray = Array(rhs)

    guard lhsArray.count == rhsArray.count else {
        fail(
            "Expected lhs and rhs to have same count, got: lhs = \(lhsArray.count) / rhs = \(rhsArray.count)",
            sourceLocation: sourceLocation
        )
        return
    }

    for (i, (lhsEntry, rhsEntry)) in zip(lhsArray, rhsArray).enumerated() {
        assertSuccessEqual(
            lhsEntry,
            rhsEntry,
            message: "at index \(i)",
            sourceLocation: sourceLocation
        )
    }
}

/// Asserts that for a sequence of `Result` values with, all cases are `.failure()`.
func assertFailures<T>(
    _ value: some Sequence<Result<T?, any Error>>,
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    for (i, valueEntry) in value.enumerated() {
        assertFailure(
            valueEntry,
            message: "at index \(i)",
            sourceLocation: sourceLocation
        )
    }
}

// MARK: Collection assertions

/// Asserts a collection of items is empty.
func assertEmpty(
    _ value: some Collection,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {

    if !value.isEmpty {
        fail(
            "Collection is not empty: \(value) \(message())",
            sourceLocation: sourceLocation
        )
    }
}

/// Asserts a collection of items has a specified count.
func assertCount(
    _ value: some Collection,
    _ count: Int,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {

    if value.count != count {
        fail(
            "Expected collection to have \(count) item(s) but found \(value.count): \(value) \(message())",
            sourceLocation: sourceLocation
        )
    }
}

/// Asserts that two collection of items contains the same set of `T` values the
/// same number of times.
func assertEqualUnordered<T>(
    _ lhs: some Collection<T>,
    _ rhs: some Collection<T>,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) where T: Equatable {

    assertEqualUnordered(
        lhs,
        rhs,
        compare: ==,
        message: message(),
        sourceLocation: sourceLocation
    )
}

/// Asserts that two collection of items contains the same set of `T` values the
/// same number of times.
func assertEqualUnordered<T>(
    _ lhs: some Collection<T>,
    _ rhs: some Collection<T>,
    compare: (T, T) -> Bool,
    message: @autoclosure () -> String = "",
    sourceLocation: SourceLocation = #_sourceLocation
) {

    if lhs.count != rhs.count {
        fail(
            "lhs.count != rhs.count (\(lhs.count) != \(rhs.count)) lhs: \(lhs) rhs: \(rhs) \(message())",
            sourceLocation: sourceLocation
        )
        return
    }

    let signal: (String) -> Void = {
        fail(
            "lhs != rhs (\(lhs) != \(rhs)) \($0)",
            sourceLocation: sourceLocation
        )
    }

    var remaining = Array(lhs)
    for item in rhs {
        if let nextIndex = remaining.firstIndex(where: { compare($0, item) }) {
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
