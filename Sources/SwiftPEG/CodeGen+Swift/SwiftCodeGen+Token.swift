extension SwiftCodeGen {
    func generateTokenType() throws -> String {
        buffer.resetState()



        return buffer.finishBuffer()
    }

    func generateTokenParser(_ syntax: CommonAbstract.TokenSyntax) {
        
    }
}
