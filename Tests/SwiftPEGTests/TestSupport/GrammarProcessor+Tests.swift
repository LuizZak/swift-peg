import SwiftPEG

extension GrammarProcessor {

    func diagnosticsCount(
        where predicate: (GrammarProcessor.GrammarProcessorDiagnostic) -> Bool
    ) -> Int {

        return diagnostics.filter(predicate).count
    }
}
