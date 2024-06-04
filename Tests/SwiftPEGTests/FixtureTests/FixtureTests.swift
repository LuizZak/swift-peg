import XCTest

class FixtureTests: XCTestCase {
    func testFixtures() throws {
        let runner = FixtureTestRunner(tester: self)
        try runner.runFixtures()
    }
}
