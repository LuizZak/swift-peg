import SwiftAST

// MARK: SwiftType

extension SwiftASTEmitter {
    func emit(_ type: SwiftType) {
        buffer.emit(type.description)
    }
}
