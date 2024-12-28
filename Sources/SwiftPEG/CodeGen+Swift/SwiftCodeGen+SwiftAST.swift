import SwiftAST

// MARK: Generation
extension SwiftCodeGen {
    fileprivate func _generateRule(
        _ rule: InternalGrammar.Rule,
        for production: RemainingProduction
    ) -> FunctionMemberDecl {
        let name = bindingEngine.alias(for: rule)
        let memoizationMode = memoizationMode(for: production)

        let fName = memoizationMode == .none ? name : "__\(name)"
        var attributes: [DeclarationAttribute] = [
            .init(name: "inlinable")
        ]

        switch memoizationMode {
        case .memoized:
            attributes.append(
                .init(name: "memoized", arguments: [
                    .unlabeled(.constant(.string(name)))
                ])
            )

        case .memoizedLeftRecursive:
            attributes.append(
                .init(name: "memoizedLeftRecursive", arguments: [
                    .unlabeled(.constant(.string(name)))
                ])
            )

        case .none:
            break
        }

        let signature = FunctionSignature(
            attributes: [],
            name: fName
        )

        var body: CompoundStatement = []

        return .init(
            accessLevel: .public,
            signature: signature,
            body: body
        )
    }

    fileprivate func _generateAlt(
        _ alt: InternalGrammar.Alt,
        _ altIndex: Int,
        in production: RemainingProduction,
        backtrackBlock: () -> [Statement],
        cutFlagBlock: () -> [Statement]
    ) throws -> [Statement] {
        var result: [Statement] = []

        if requiresCutFlag(alt) {
            result.append(contentsOf: cutFlagBlock())
        }

        return result
    }



    private func memoizationMode(for production: RemainingProduction) -> MemoizationMode {
        switch production {
        case .rule(let rule):
            return memoizationMode(for: rule)

        case .auxiliary(_, let info), .nonStandardRepetition(let info, _):
            return info.memoizationMode
        }
    }

    private func memoizationMode(for rule: InternalGrammar.Rule) -> MemoizationMode {
        return
            if rule.isLeftRecursiveLeader {
                .memoizedLeftRecursive
            } else if !rule.isLeftRecursive {
                .memoized
            } else {
                .none
            }
    }
}

// MARK: Structures
extension SwiftCodeGen {
    struct ExtensionDecl {
        var accessLevel: AccessLevel
        var members: [MemberDecl]
    }

    enum MemberDecl {
        case variable(VariableMemberDecl)
        case `subscript`(SubscriptMemberDecl)
        case function(FunctionMemberDecl)
    }

    struct FunctionMemberDecl {
        var accessLevel: AccessLevel
        var signature: FunctionSignature
        var body: CompoundStatement
    }

    struct VariableMemberDecl {
        var accessLevel: AccessLevel
        var name: String
        var type: SwiftType
        var storage: VariableStorage

        enum VariableStorage {
            case stored
            case getter(CompoundStatement)
            case getterSetter(CompoundStatement)
        }
    }

    struct SubscriptMemberDecl {
        var accessLevel: AccessLevel
        var signature: SubscriptSignature
        var getter: CompoundStatement
        var setter: (String, CompoundStatement)?
    }
}
