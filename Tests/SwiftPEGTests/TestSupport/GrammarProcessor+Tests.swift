import SwiftPEG

extension GrammarProcessor {

    func diagnosticsCount(
        where predicate: (GrammarProcessor.GrammarProcessorDiagnostic) -> Bool
    ) -> Int {

        return diagnostics(where: predicate).count
    }

    func diagnostics(
        where predicate: (GrammarProcessor.GrammarProcessorDiagnostic) -> Bool
    ) -> [GrammarProcessor.GrammarProcessorDiagnostic] {

        return diagnostics.filter(predicate)
    }
}
