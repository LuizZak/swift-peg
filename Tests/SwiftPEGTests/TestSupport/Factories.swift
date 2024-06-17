import SwiftPEG

/// Parses a given input string as a SwiftPEG grammar, returning the raw grammar
/// nodes.
func parseGrammar(
    _ grammar: String,
    file: StaticString = #file,
    line: UInt = #line
) throws -> SwiftPEGGrammar.Grammar {

    let rawTokenizer = GrammarRawTokenizer(source: grammar)
    let tokenizer = Tokenizer(rawTokenizer: rawTokenizer)
    let parser = GrammarParser(tokenizer: tokenizer)

    guard let grammar = try parser.start(), try parser.isEOF() else {
        throw parser.makeSyntaxError()
    }

    return grammar
}
