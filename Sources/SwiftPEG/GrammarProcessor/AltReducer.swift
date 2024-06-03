/// Provides alt reductions capabilities, producing the shortest production of
/// non-optional elements that an alt requires.
class AltReducer {
    var _currentDepth: Int = 0
    var _depthLimit: Int = 0

    var reachedDepthLimit: Bool {
        _currentDepth >= _depthLimit
    }

    let alt: InternalGrammar.Alt

    init(_ alt: InternalGrammar.Alt) {
        self.alt = alt
    }

    func reduced() -> InternalGrammar.Alt {
        alt.reduced ?? .init(items: [], action: alt.action, failAction: alt.failAction)
    }

    /// Returns a set of permutations of the alt associated with this reducer,
    /// with each optional production sequentially omitted in all possible
    /// combinations.
    ///
    /// - parameter depthLimit: A limit on how many optional items to permute,
    /// with the limit reaching indicating that no further optional items should
    /// be permuted on. A depth limit of zero produces an array with a single alt
    /// matching the input alt. Each encountered optional item adds one to the
    /// depth, resulting in a potential output size of up to `depthLimit^2`.
    func permuted(depthLimit: Int) -> [InternalGrammar.Alt] {
        _currentDepth = 0
        _depthLimit = depthLimit

        return permuted(alt.items).map {
            InternalGrammar.Alt(items: $0, action: alt.action, failAction: alt.failAction)
        }.compactMap({ $0.flattened() })
    }

    private func permuted(_ alt: InternalGrammar.Alt) -> [InternalGrammar.Alt] {
        if reachedDepthLimit { return [alt] }

        return permuted(alt.items).map {
            InternalGrammar.Alt(items: $0, action: alt.action, failAction: alt.failAction)
        }
    }

    private func permuted(_ alts: some Collection<InternalGrammar.Alt>) -> [[InternalGrammar.Alt]] {
        if reachedDepthLimit { return [Array(alts)] }

        guard let first = alts.first else {
            return []
        }

        let variations = permuted(first)

        let next = permuted(alts.dropFirst())
        if next.isEmpty {
            return variations.map { [$0] }
        }

        return variations.flatMap { variation in
            next.map { next in
                [variation] + next
            }
        }
    }

    private func permuted(_ namedItems: some Collection<InternalGrammar.NamedItem>) -> [[InternalGrammar.NamedItem]] {
        if reachedDepthLimit { return [Array(namedItems)] }

        guard let first = namedItems.first else {
            return []
        }

        let variations = permuted(first)

        let next = permuted(namedItems.dropFirst())
        if next.isEmpty {
            return variations
        }

        return variations.flatMap { variation in
            next.map { next in
                variation + next
            }
        }
    }

    private func permuted(_ namedItem: InternalGrammar.NamedItem) -> [[InternalGrammar.NamedItem]] {
        if reachedDepthLimit { return [[namedItem]] }

        switch namedItem {
        case .item(let name, let item, let type):
            return permuted(item).map { items in
                items.map { item in
                    .item(name: name, item, type: type)
                }
            }
        case .lookahead(let lookahead):
            _currentDepth += 1

            return [[.lookahead(lookahead)], []]
        }
    }

    private func permuted(_ item: InternalGrammar.Item) -> [[InternalGrammar.Item]] {
        if reachedDepthLimit { return [[item]] }

        switch item {
        case .optional(let atom):
            _currentDepth += 1

            return permuted(atom).map { atoms in
                atoms.map(InternalGrammar.Item.atom)
            } + [[]]

        case .optionalItems(let alts):
            _currentDepth += 1

            return permuted(alts).map { alts in
                [InternalGrammar.Item.atom(.group(alts))]
            } + [[]]

        case .zeroOrMore(let atom, let repetitionMode):
            _currentDepth += 1

            return permuted(atom).map { atoms in
                atoms.map({
                    InternalGrammar.Item.oneOrMore($0, repetitionMode: repetitionMode)
                })
            } + [[]]

        case .oneOrMore:
            return [[item]]

        case .gather:
            return [[item]]

        case .atom(let atom):
            return permuted(atom).map { atoms in
                atoms.map(InternalGrammar.Item.atom)
            }
        }
    }

    private func permuted(_ atom: InternalGrammar.Atom) -> [[InternalGrammar.Atom]] {
        if reachedDepthLimit { return [[atom]] }

        switch atom {
        case .group(let alts):
            return permuted(alts).map { alts in
                let nonEmpty = alts.filter { !$0.items.isEmpty }

                return [InternalGrammar.Atom.group(nonEmpty)]
            }
        default:
            return [[atom]]
        }
    }

    enum Error: Swift.Error {
        /// Error raised when `AltReducer.permute` reaches the limit of permutations
        /// allowable.
        case permutationLimitReached
    }
}
