/// Provides alt reductions capabilities, producing the shortest production of
/// non-optional elements that an alt requires.
class AltReducer {
    let alt: InternalGrammar.Alt

    init(_ alt: InternalGrammar.Alt) {
        self.alt = alt
    }

    func reduced() -> InternalGrammar.Alt {
        alt.reduced ?? .init(items: [], action: alt.action, failAction: alt.failAction)
    }
}
