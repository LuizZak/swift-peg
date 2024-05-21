@testable import SwiftPEG

class TestTokenizer<Raw: RawTokenizerType>: Tokenizer<Raw> {

    var mark_callCount: Int = 0
    var mark_calls: [Mark] = []
    override func mark() -> Mark {
        let result = super.mark()
        mark_callCount += 1
        mark_calls.append(result)
        return result
    }

    var next_callCount: Int = 0
    var next_calls: [TokenResult?] = []
    override func next() throws -> TokenResult? {
        let result = try super.next()
        next_callCount += 1
        next_calls.append(result)
        return result
    }

    var peekToken_callCount: Int = 0
    var peekToken_calls: [TokenResult?] = []
    override func peekToken() throws -> TokenResult? {
        let result = try super.peekToken()
        peekToken_callCount += 1
        peekToken_calls.append(result)
        return result
    }

    var restore_callCount: Int = 0
    var restore_calls: [Tokenizer<Raw>.Mark] = []
    override func restore(_ mark: Tokenizer<Raw>.Mark) {
        super.restore(mark)

        restore_callCount += 1
        restore_calls.append(mark)
    }
}
