/// A stack-based context for defining non-overlapping variable names.
public final class DeclarationsContext {
    var _nonce: Int = 0
    var _stack: [[Declaration]] = []
    var _methods: [Declaration] = []

    var topmost: [Declaration] {
        get {
            if _stack.isEmpty { _stack.append([]) }

            return _stack[_stack.count - 1]
        }
        set {
            _stack[_stack.count - 1] = newValue
        }
    }

    /// Name deduplication based on locals on stack.
    func deduplicateLocal(_ name: String) -> String {
        if declaration(named: name, ofKind: .local) == nil {
            return name
        }

        var counter = 1

        while declaration(named: "\(name)\(counter)", ofKind: .local) != nil {
            counter += 1
        }

        return "\(name)\(counter)"
    }

    /// Method name deduplication based on methods on global namespace.
    func deduplicateMethod(_ name: String) -> String {
        if declaration(named: name, ofKind: .method) == nil {
            return name
        }

        var counter = 1

        while declaration(named: "\(name)\(counter)", ofKind: .method) != nil {
            counter += 1
        }

        return "\(name)\(counter)"
    }

    /// Resets this declarations context.
    public func reset() {
        _stack = []
        _methods = []
        _nonce = 0
    }

    /// Pushes a new declaration context.
    public func push() {
        _stack.append([])
    }

    /// Pops the topmost declaration context.
    public func pop() {
        _stack.removeLast()
    }

    /// Creates a new method with a given name on the global context.
    /// If another method shares the same name, an error is thrown.
    @discardableResult
    public func defineMethod(fixedName: String) throws -> Declaration {
        guard declaration(named: fixedName, ofKind: .method) == nil else {
            throw GrammarProcessor.GrammarProcessorError.message(
                "Attempted to create duplicate method with name \(fixedName)"
            )
        }

        let decl = Declaration(name: fixedName, type: .none, kind: .method)
        _methods.append(decl)

        return decl
    }

    /// Defines an arbitrarily-named local with a given optional type associated
    /// with it.
    ///
    /// The name of the local is guaranteed to be a valid Swift variable name,
    /// and to not be in duplicated in this declaration context.
    public func defineTemporaryLocal(type: Declaration.DeclType = .none) -> Declaration {
        let baseName = "__localTemp\(_nonce)"
        _nonce += 1

        return defineLocal(suggestedName: baseName, type: type)
    }

    /// Defines an arbitrarily-named auxiliary method.
    ///
    /// The name of the method is guaranteed to be a valid Swift method name,
    /// and to not be in duplicated in this declaration context.
    public func defineAuxiliaryMethod(type: Declaration.DeclType = .none) -> Declaration {
        let baseName = "__auxMethod\(_nonce)"
        _nonce += 1

        return defineLocal(suggestedName: baseName, type: type)
    }

    /// Creates a new local with a given suggested name on the topmost local
    /// variable context.
    ///
    /// The real name to use should be the one from the returned `Declaration`'s
    /// `name` property, as the suggested name may be altered to ensure it is
    /// unique within the current context.
    ///
    /// Suggested names may be altered with numeric suffixes.
    @discardableResult
    public func defineLocal(suggestedName: String, type: Declaration.DeclType = .none) -> Declaration {
        let name = deduplicateLocal(suggestedName)
        let local = Declaration(name: name, type: type, kind: .local)

        topmost.append(local)

        return local
    }

    /// Creates a new method with a given suggested name on the global context.
    ///
    /// The real name to use should be the one from the returned `Declaration`'s
    /// `name` property, as the suggested name may be altered to ensure it is
    /// unique within the current context.
    ///
    /// Suggested names may be altered with numeric suffixes.
    @discardableResult
    public func defineMethod(suggestedName: String) -> Declaration {
        let name = deduplicateMethod(suggestedName)
        let local = Declaration(name: name, type: .none, kind: .method)

        _methods.append(local)

        return local
    }

    /// Returns the latest declaration with a given set of parameters name, searching
    /// from the topmost local stack first.
    ///
    /// If all parameters are `nil`, the latest declaration on the top of the
    /// stack is returned.
    ///
    /// Returns `nil`, if no declaration matching the provided parameters could
    /// be found.
    public func declaration(
        named name: String? = nil,
        typed type: Declaration.DeclType? = nil,
        ofKind kind: Declaration.Kind? = nil
    ) -> Declaration? {
        let filter: (Declaration) -> Bool = { decl in
            if let name, decl.name != name {
                false
            } else if let type, decl.type != type {
                false
            } else if let kind, decl.kind != kind {
                false
            } else {
                true
            }
        }

        for level in _stack.reversed() {
            if let declaration = level.last(where: filter) {
                return declaration
            }
        }

        for method in _methods {
            if filter(method) {
                return method
            }
        }

        return nil
    }

    /// Specifies information about a locally-created symbol.
    public struct Declaration {
        /// The name of the declaration.
        public var name: String

        /// The type of the declaration, if available.
        public var type: DeclType

        /// The kind of the declaration.
        public var kind: Kind

        public enum Kind: Hashable {
            /// A local variable declaration.
            case local

            /// A method within a type context.
            case method
        }

        /// Specifies type information for a declaration
        public enum DeclType: Hashable {
            /// No type was specified.
            case none

            /// A marker type.
            case marker

            /// A cut flag type.
            case cutFlag
        }
    }
}
