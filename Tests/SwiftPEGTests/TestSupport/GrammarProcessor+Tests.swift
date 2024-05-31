import SwiftPEG

extension GrammarProcessor {

    func test_diagnosticsCount(
        where predicate: (GrammarProcessor.GrammarProcessorDiagnostic) -> Bool
    ) -> Int {

        return test_diagnostics(where: predicate).count
    }

    func test_diagnostics(
        where predicate: (GrammarProcessor.GrammarProcessorDiagnostic) -> Bool
    ) -> [GrammarProcessor.GrammarProcessorDiagnostic] {

        return diagnostics.filter(predicate)
    }

    func test_diagnosticMessages() -> String {
        diagnostics.map(\.description).joined(separator: "\n")
    }

    func test_errorMessages() -> String {
        errors.map(\.description).joined(separator: "\n")
    }
}
