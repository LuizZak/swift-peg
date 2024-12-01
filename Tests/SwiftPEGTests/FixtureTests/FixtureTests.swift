import XCTest
import Testing

struct FixtureTests {
    @Test
    func fixtures() throws {
        let runner = FixtureTestRunner()
        try runner.runFixtures()
    }
}
