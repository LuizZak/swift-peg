import SwiftAST

extension SwiftCodeGen {
    /// Generates Swift code defining the Token type of the grammar.
    public func generateTokenType(
        settings: TokenTypeGenSettings = .default
    ) throws -> String {
        let decls = try _generateTokenType(settings: settings)

        let emitter = SwiftASTEmitter()
        for decl in decls {
            emitter.emit(decl)
        }

        return emitter.finishBuffer()
        /*
        buffer.resetState()

        if let missingSyntax = tokenDefinitions.first(where: { $0.tokenSyntax == nil }) {
            throw Error.tokenDefinitionMissingSyntax(missingSyntax)
        }

        if let header = grammar.tokenTypeHeader() {
            buffer.emitLine(header)
        }

        let tokenName: String
        if let tokenTypeName = grammar.tokenTypeName() {
            tokenName = tokenTypeName
        } else {
            tokenName = "\(parserName)Token"
        }

        let sortedTokens = Self._sortTokens(tokenDefinitions)

        generateAccessLevel(settings: settings)
        try buffer.emitBlock("struct \(tokenName): RawTokenType, CustomStringConvertible") {
            try generateTokenTypeMembers(settings: settings, sortedTokens: sortedTokens)
        }

        return buffer.finishBuffer()
        */
    }

    /// Generates Swift code defining the Token type of the grammar.
    public func _generateTokenType(
        settings: TokenTypeGenSettings = .default
    ) throws -> [TopLevelDecl] {
        var result: [TopLevelDecl] = []

        buffer.resetState()

        if let missingSyntax = tokenDefinitions.first(where: { $0.tokenSyntax == nil }) {
            throw Error.tokenDefinitionMissingSyntax(missingSyntax)
        }

        if let header = grammar.tokenTypeHeader() {
            result.append(.unknown(header))
        }

        let tokenName: String
        if let tokenTypeName = grammar.tokenTypeName() {
            tokenName = tokenTypeName
        } else {
            tokenName = "\(parserName)Token"
        }

        let sortedTokens = Self._sortTokens(tokenDefinitions)

        let accessLevel = _generateAccessLevel(settings: settings)

        result.append(
            .struct(
                .init(
                    leadingComments: [],
                    accessLevel: accessLevel,
                    name: tokenName,
                    genericArguments: [],
                    inheritances: ["RawTokenType", "CustomStringConvertible"],
                    members: try _generateTokenTypeMembers(settings: settings, sortedTokens: sortedTokens)
                )
            )
        )

        return result
    }

    func generateTokenTypeMembers(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws {

        // var kind: TokenKind
        generateAccessLevel(settings: settings)
        buffer.emitLine("var kind: TokenKind")

        // var string: Substring
        generateAccessLevel(settings: settings)
        buffer.emitLine("var string: Substring")

        // var length: Int
        buffer.ensureDoubleNewline()
        try generateTokenTypeLength(settings: settings)

        // var description: String
        buffer.ensureDoubleNewline()
        try generateTokenTypeDescription(settings: settings)

        // init(kind: TokenKind, string: Substring)
        buffer.ensureDoubleNewline()
        try generateTokenTypeInitializer(settings: settings)

        // static func produceDummy(_ kind: TokenKind) -> Self
        buffer.ensureDoubleNewline()
        generateTokenTypeProduceDummy(settings: settings)

        // `static func recordTokenAttempt<StringType>(...)`
        // buffer.ensureDoubleNewline()
        // generateRecordAttempt(settings: settings)

        // func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring
        buffer.ensureDoubleNewline()
        try generateTokenTypeParser(
            settings: settings,
            sortedTokens: sortedTokens,
            modifiers: ["static"]
        )

        // enum TokenKind
        buffer.ensureDoubleNewline()
        try generateTokenKindEnum(
            settings: settings,
            sortedTokens: sortedTokens
        )

        // func consume_<TOKEN1>
        // func consume_<TOKEN2>
        //   ...
        buffer.ensureDoubleNewline()
        try generateTokenParsers(
            settings: settings,
            sortedTokens: sortedTokens
        )
    }

    func _generateTokenTypeMembers(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws -> [MemberDecl] {
        var result: [MemberDecl] = []

        let accessLevel = _generateAccessLevel(settings: settings)

        // var kind: TokenKind
        result.append(
            .variable(
                .init(leadingComments: [], attributes: [], accessLevel: accessLevel, isConstant: false, name: "kind", type: "TokenKind", storage: .stored)
            )
        )

        // var string: Substring
        result.append(
            .variable(
                .init(leadingComments: [], attributes: [], accessLevel: accessLevel, isConstant: false, name: "string", type: "Substring", storage: .stored)
            )
        )

        // var length: Int
        result.append(
            .variable(try _generateTokenTypeLength(settings: settings))
        )

        // var description: String
        result.append(
            .variable(try _generateTokenTypeDescription(settings: settings))
        )

        // init(kind: TokenKind, string: Substring)
        if let initializer = try _generateTokenTypeInitializer(settings: settings) {
            result.append(
                .initializer(initializer)
            )
        }

        // static func produceDummy(_ kind: TokenKind) -> Self
        result.append(
            .function(
                _generateTokenTypeProduceDummy(settings: settings)
            )
        )

        // `static func recordTokenAttempt<StringType>(...)`
        // buffer.ensureDoubleNewline()
        // generateRecordAttempt(settings: settings)

        // func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring
        result.append(
            .function(
                try _generateTokenTypeParser(
                    settings: settings,
                    sortedTokens: sortedTokens,
                    modifiers: ["static"]
                )
            )
        )

        // enum TokenKind
        result.append(
            .type(.enum(try _generateTokenKindEnum(
                settings: settings,
                sortedTokens: sortedTokens
            )))
        )

        // func consume_<TOKEN1>
        // func consume_<TOKEN2>
        //   ...
        result.append(contentsOf:
            try _generateTokenParsers(
                settings: settings,
                sortedTokens: sortedTokens
            ).map { .function($0) }
        )

        return result
    }

    /// `var length: Int`
    func generateTokenTypeLength(settings: TokenTypeGenSettings) throws {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitMultiline("""
        var length: Int {
            string.count
        }
        """)
    }

    /// `var length: Int`
    func _generateTokenTypeLength(settings: TokenTypeGenSettings) throws -> VariableMemberDecl {
        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        return .init(
            leadingComments: [],
            attributes: attributes,
            accessLevel: accessLevel,
            isConstant: false,
            name: "length",
            type: .int,
            storage: .getter([
                .expression(.identifier("string").dot("count"))
            ])
        )
    }

    /// `var description: String`
    func generateTokenTypeDescription(settings: TokenTypeGenSettings) throws {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitMultiline("""
        var description: String {
            String(string)
        }
        """)
    }

    /// `var description: String`
    func _generateTokenTypeDescription(settings: TokenTypeGenSettings) throws -> VariableMemberDecl {
        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        return .init(
            leadingComments: [],
            attributes: attributes,
            accessLevel: accessLevel,
            isConstant: false,
            name: "description",
            type: .string,
            storage: .getter([
                .expression(.identifier("String").call([.identifier("string")]))
            ])
        )
    }

    /// `init(kind: TokenKind, string: Substring)`
    func generateTokenTypeInitializer(settings: TokenTypeGenSettings) throws {
        // If access level is not `nil` or "internal", produce an initializer
        // for the token type
        guard settings.accessLevel != nil && settings.accessLevel != "internal" else {
            return
        }

        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitMultiline("""
        init(kind: TokenKind, string: Substring) {
            self.kind = kind
            self.string = string
        }
        """)
    }

    /// `init(kind: TokenKind, string: Substring)`
    func _generateTokenTypeInitializer(settings: TokenTypeGenSettings) throws -> InitMemberDecl? {
        // If access level is not `nil` or "internal", produce an initializer
        // for the token type
        guard settings.accessLevel != nil && settings.accessLevel != "internal" else {
            return nil
        }

        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        return .init(
            leadingComments: [],
            attributes: attributes,
            accessLevel: accessLevel,
            parameters: [.init(name: "kind", type: "TokenKind"), .init(name: "string", type: "Substring")],
            body: [
                .expression(.identifier("self").dot("kind").assignment(op: .assign, rhs: .identifier("kind"))),
                .expression(.identifier("self").dot("string").assignment(op: .assign, rhs: .identifier("string"))),
            ]
        )
    }

    /// `static func recordTokenAttempt<StringType>(...)`
    func generateRecordAttempt(settings: TokenTypeGenSettings) {
        generateInlinableAttribute(settings: settings)
        buffer.emitMultiline("""
        static func recordTokenAttempt<StringType>(
            longestAttempt: inout (Self.TokenKind, StringStream<StringType>.State)?,
            stream: inout StringStream<StringType>,
            parser: (inout StringStream<StringType>) -> Self.TokenKind?
        ) where StringType.SubSequence == Substring {

            let state = stream.save()

            stream.markSubstringStart()

            if let result = parser(&stream) {
                let newState = stream.save()

                if let longest = longestAttempt {
                    if newState.index > longest.1.index {
                        longestAttempt = (result, newState)
                    }
                } else {
                    longestAttempt = (result, newState)
                }
            }

            stream.restore(state)
        }
        """)
    }

    /// `static func recordTokenAttempt<StringType>(...)`
    func _generateRecordAttempt(settings: TokenTypeGenSettings) -> FunctionMemberDecl {
        let stringStreamType = SwiftType.generic("StringStream", parameters: ["StringType"])
        let stateType: SwiftType = .nested(.init(base: stringStreamType, nested: "State"))
        let parameters: [ParameterSignature] = [
            .init(name: "longestAttempt", type: .tuple([.nested(.init(base: "Self", nested: "TokenKind")), .nested(.init(base: stringStreamType, nested: "State"))]), modifier: .inout),
            .init(name: "stream", type: stringStreamType, modifier: .inout),
            .init(
                name: "parser",
                type: .swiftBlock(returnType: .optional(.nested(.init(base: "Self", nested: "TokenKind"))), parameters: [
                    .init(type: stringStreamType, modifier: .inout)
                ])
            )
        ]

        let body: CompoundStatement = [
            // let state = stream.save()
            .variableDeclaration(identifier: "state", type: stateType, initialization: .identifier("stream").dot("save").call()),
            // stream.markSubstringStart()
            .expression(.identifier("stream").dot("markSubstringStart").call()),
            /*
            if let result = parser(&stream) {
                let newState = stream.save()

                if let longest = longestAttempt {
                    if newState.index > longest.1.index {
                        longestAttempt = (result, newState)
                    }
                } else {
                    longestAttempt = (result, newState)
                }
            }
            */
            .ifLet(.identifier("result"), .identifier("parser").call([.unary(op: .bitwiseAnd, .identifier("stream"))]), body: [
                .variableDeclaration(identifier: "newState", type: stateType, initialization: .identifier("stream").dot("save").call()),
                .ifLet(.identifier("longest"), .identifier("longestAttempt"), body: [
                    .if(.identifier("newState").dot("index").binary(op: .greaterThan, rhs: .identifier("longest").dot("1").dot("index")), body: [
                        .expression(.assignment(lhs: .identifier("longestAttempt"), op: .assign, rhs: .tuple([.identifier("result"), .identifier("newState")])))
                    ])
                ], else: [
                    .expression(.assignment(lhs: .identifier("longestAttempt"), op: .assign, rhs: .tuple([.identifier("result"), .identifier("newState")])))
                ])
            ]),
            // stream.restore(state)
            .expression(.identifier("stream").dot("restore").call([.identifier("state")])),
        ]

        return .init(
            leadingComments: [],
            accessLevel: .internal,
            signature: .init(
                attributes: ["inlinable"],
                genericParameters: ["StringType"],
                name: "recordTokenAttempt",
                parameters: parameters,
                returnType: .void,
                genericWhereClause: [.sameTypeRequirement(.nested(.init(base: "StringType", nested: "SubSequence")), "Substring")],
                traits: [.static]
            ),
            body: body
        )
    }

    /// `static func produceDummy(_ kind: TokenKind) -> Self`
    func generateTokenTypeProduceDummy(settings: TokenTypeGenSettings) {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitMultiline("""
        static func produceDummy(_ kind: TokenKind) -> Self {
            .init(kind: kind, string: "<dummy>")
        }
        """)
    }

    /// `static func produceDummy(_ kind: TokenKind) -> Self`
    func _generateTokenTypeProduceDummy(settings: TokenTypeGenSettings) -> FunctionMemberDecl {
        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        return .init(
            leadingComments: [],
            accessLevel: accessLevel,
            signature: .init(
                attributes: attributes,
                name: "produceDummy",
                parameters: [
                    .init(label: nil, name: "kind", type: "TokenKind"),
                ],
                returnType: "Self",
                traits: [.static]
            ),
            body: [
                .expression(.implicitMember("init").call([
                    .init(label: "kind", expression: .identifier("kind")),
                    .init(label: "string", expression: .constant("<dummy>")),
                ]))
            ]
        )
    }

    /// `func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring`
    func generateTokenTypeParser(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition],
        modifiers: [String] = []
    ) throws {
        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitWithSeparators(modifiers + ["func"], separator: " ")
        try buffer.emitBlock(" from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring") {
            try generateTokenTypeParserBody(settings: settings, sortedTokens: sortedTokens)
        }
    }

    /// `func from<StringType>(stream: inout StringStream<StringType>) -> Self? where StringType.SubSequence == Substring`
    func _generateTokenTypeParser(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition],
        modifiers: [String] = []
    ) throws -> FunctionMemberDecl {
        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        let genericArg = "StringType"

        return .init(
            leadingComments: [],
            accessLevel: accessLevel,
            signature: .init(
                attributes: attributes,
                genericParameters: [.init(typeName: genericArg)],
                name: "from",
                parameters: [
                    .init(label: "stream", name: "stream", type: .generic("StringStream", parameters: [.typeName(genericArg)]), modifier: .inout),
                ],
                returnType: .optional("Self"),
                genericWhereClause: [
                    .sameTypeRequirement(.nested(.init(base: .typeName(genericArg), nested: "SubSequence")), "Substring"),
                ],
                traits: [.static]
            ),
            body: .init(statements: try _generateTokenTypeParserBody(settings: settings, sortedTokens: sortedTokens))
        )
    }

    func generateTokenTypeParserBody(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws {
        buffer.emitMultiline("""
        guard !stream.isEof else { return nil }
        stream.markSubstringStart()

        """)

        // TODO: Attempt to generate a switch over the first peeked character
        // TODO: like in SwiftPEGGrammar's old Token parser?
        var emittedTokenNames: Set<String> = []
        let nonDependants = sortedTokens.filter { token in
            !processedGrammar
                .tokenOcclusionGraph
                .edges
                .contains(where: { $0.end == token.name })
        }

        for token in nonDependants.filter(showEmitInTokenParser) {
            try generateTokenParseCheck(
                settings: settings,
                token,
                emittedTokenNames: &emittedTokenNames
            )
        }

        buffer.ensureDoubleNewline()
        buffer.emitLine("return nil")
    }

    func _generateTokenTypeParserBody(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws -> [Statement] {
        var result: [Statement] = []

        result.append(contentsOf: [
            .guard(.unary(op: .negate, .identifier("stream").dot("isEof")), else: [
                .return(.constant(.nil))
            ]),
            .expression(.identifier("stream").dot("markSubstringStart").call()),
        ])

        // TODO: Attempt to generate a switch over the first peeked character
        // TODO: like in SwiftPEGGrammar's old Token parser?
        var emittedTokenNames: Set<String> = []
        let nonDependants = sortedTokens.filter { token in
            !processedGrammar
                .tokenOcclusionGraph
                .edges
                .contains(where: { $0.end == token.name })
        }

        for token in nonDependants.filter(showEmitInTokenParser) {
            let stmt = try _generateTokenParseCheck(
                settings: settings,
                token,
                emittedTokenNames: &emittedTokenNames
            )

            result.append(stmt)
        }

        result.append(
            .return(.constant(.nil))
        )

        return result
    }

    func generateTokenParseCheck(
        settings: TokenTypeGenSettings,
        _ token: InternalGrammar.TokenDefinition,
        emittedTokenNames: inout Set<String>
    ) throws {

        func returnExpForToken(_ token: InternalGrammar.TokenDefinition) -> String {
            let tokenName = caseName(for: token)

            return ".init(kind: .\(tokenName), string: stream.substring)"
        }

        func emitDependantCases(_ dependants: [InternalGrammar.TokenDefinition]) throws {
            for dependant in dependants {
                guard let staticTerminal = dependant.tokenSyntax?.staticTerminal() else {
                    throw Error.tokenDependantIsNotStatic(token, dependant: dependant)
                }

                buffer.emitLine("case \(tok_escapeLiteral(staticTerminal)):")
                buffer.indented {
                    buffer.emitLine("return \(returnExpForToken(dependant))")
                }
            }
        }

        func emitDependantsSwitch(
            defaultReturnStmt: String,
            _ dependants: [InternalGrammar.TokenDefinition]
        ) throws {
            buffer.emitLine("switch stream.substring {")

            try emitDependantCases(dependants.sorted(by: { $0.name < $1.name }))

            buffer.emitLine("default:")
            buffer.indented {
                buffer.emitLine(defaultReturnStmt)
            }

            buffer.emitLine("}")
        }

        defer { emittedTokenNames.insert(token.name) }

        let method = parseMethodName(for: token)
        let parseInvocation = "\(method)(from: &stream)"

        let dependants = processedGrammar
            .tokenOcclusionGraph
            .edges.filter({
                $0.start == token.name && !emittedTokenNames.contains($0.end)
            }).map(\.end).compactMap({ name in
                processedGrammar.tokens.first(where: { $0.name == name })
            })

        emittedTokenNames.formUnion(dependants.map(\.name))

        try buffer.emitBlock("if \(parseInvocation)") {
            let returnStmt = "return \(returnExpForToken(token))"

            guard !dependants.isEmpty else {
                buffer.emitLine(returnStmt)
                return
            }

            guard settings.emitLengthSwitchPhaseInTokenOcclusionSwitch else {
                try emitDependantsSwitch(defaultReturnStmt: returnStmt, dependants)
                return
            }

            // Emit a switch over the stream's substring to move the result into
            // dependant's static terminals. If the number if dependants is below
            // a certain threshold, generate one switch, otherwise, generate a
            // switch over the different lengths of dependants to make the process
            // more granular.
            var byLength: [Int: [InternalGrammar.TokenDefinition]] = [:]
            for dependant in dependants {
                guard let staticTerminal = dependant.tokenSyntax?.staticTerminal() else {
                    throw Error.tokenDependantIsNotStatic(token, dependant: dependant)
                }

                byLength[staticTerminal.contents.count, default: []].append(dependant)
            }

            guard
                byLength.count >= 3,
                byLength.contains(where: { $0.value.count > 1 })
            else {
                try emitDependantsSwitch(defaultReturnStmt: returnStmt, dependants)
                return
            }

            buffer.emitLine("switch stream.substringLength {")

            for (length, dependants) in byLength.sorted(by: { $0.key > $1.key }) {
                buffer.emitLine("case \(length):")

                try buffer.indented {
                    try emitDependantsSwitch(
                        defaultReturnStmt: returnStmt,
                        dependants
                    )
                }

                buffer.ensureDoubleNewline()
            }

            buffer.emitLine("default:")
            buffer.indented {
                buffer.emitLine(returnStmt)
            }

            buffer.emitLine("}")
        }
    }

    func _generateTokenParseCheck(
        settings: TokenTypeGenSettings,
        _ token: InternalGrammar.TokenDefinition,
        emittedTokenNames: inout Set<String>
    ) throws -> Statement {

        func returnExpForToken(_ token: InternalGrammar.TokenDefinition) -> Expression {
            let tokenName = caseName(for: token)

            return .implicitMember("init").call([
                .init(label: "kind", expression: .implicitMember(tokenName)),
                .init(label: "string", expression: .identifier("stream").dot("substring")),
            ])
        }

        func emitDependantCases(_ dependants: [InternalGrammar.TokenDefinition]) throws -> [SwitchCase] {
            var result: [SwitchCase] = []

            for dependant in dependants {
                guard let staticTerminal = dependant.tokenSyntax?.staticTerminal() else {
                    throw Error.tokenDependantIsNotStatic(token, dependant: dependant)
                }

                let pattern = SwitchCase.CasePattern(pattern: .expression(_tok_escapeLiteral(staticTerminal)))
                let body: [Statement] = [
                    .return(returnExpForToken(dependant))
                ]

                result.append(
                    .init(casePatterns: [pattern], body: .init(statements: body))
                )
            }

            return result
        }

        func emitDependantsSwitch(
            defaultReturnStmt: ReturnStatement,
            _ dependants: [InternalGrammar.TokenDefinition]
        ) throws -> SwitchExpression {
            let exp = Expression.identifier("stream").dot("substring")
            let switchCases = try emitDependantCases(dependants.sorted(by: { $0.name < $1.name }))
            let defaultCase = SwitchDefaultCase(statements: [
                defaultReturnStmt.copy()
            ])

            return .init(exp: exp, cases: switchCases, defaultCase: defaultCase)
        }

        defer { emittedTokenNames.insert(token.name) }

        let method = parseMethodName(for: token)
        let parseInvocation = Expression.identifier(method).call([.init(label: "from", expression: .unary(op: .bitwiseAnd, .identifier("stream")))])

        let dependants = processedGrammar
            .tokenOcclusionGraph
            .edges.filter({
                $0.start == token.name && !emittedTokenNames.contains($0.end)
            }).map(\.end).compactMap({ name in
                processedGrammar.tokens.first(where: { $0.name == name })
            })

        emittedTokenNames.formUnion(dependants.map(\.name))

        var ifStatements: [Statement] = []

        ifBody:
        do {
            let returnStmt = ReturnStatement(exp: returnExpForToken(token))

            guard !dependants.isEmpty else {
                ifStatements.append(returnStmt)
                break ifBody
            }

            guard settings.emitLengthSwitchPhaseInTokenOcclusionSwitch else {
                let switchExpr = try emitDependantsSwitch(defaultReturnStmt: returnStmt, dependants)
                ifStatements.append(.expression(switchExpr))
                break ifBody
            }

            // Emit a switch over the stream's substring to move the result into
            // dependant's static terminals. If the number if dependants is below
            // a certain threshold, generate one switch, otherwise, generate a
            // switch over the different lengths of dependants to make the process
            // more granular.
            var byLength: [Int: [InternalGrammar.TokenDefinition]] = [:]
            for dependant in dependants {
                guard let staticTerminal = dependant.tokenSyntax?.staticTerminal() else {
                    throw Error.tokenDependantIsNotStatic(token, dependant: dependant)
                }

                byLength[staticTerminal.contents.count, default: []].append(dependant)
            }

            guard
                byLength.count >= 3,
                byLength.contains(where: { $0.value.count > 1 })
            else {
                let switchExpr = try emitDependantsSwitch(defaultReturnStmt: returnStmt, dependants)
                ifStatements.append(.expression(switchExpr))
                break ifBody
            }

            var switchCases: [SwitchCase] = []

            for (length, dependants) in byLength.sorted(by: { $0.key > $1.key }) {
                switchCases.append(
                    .init(patterns: [.expression(.constant(.int(length)))], body: [
                        .expression(
                        try emitDependantsSwitch(
                            defaultReturnStmt: returnStmt,
                            dependants
                        ))
                    ])
                )
            }
            let defaultCase: SwitchDefaultCase = .init(statements: [
                returnStmt
            ])

            ifStatements.append(
                .switch(.identifier("stream").dot("substringLength"), cases: switchCases, default: defaultCase)
            )
        }

        return .if(parseInvocation, body: .init(statements: ifStatements))
    }

    func generateTokenTypeParserBodyLongestParse(sortedTokens: [InternalGrammar.TokenDefinition]) throws {
        buffer.emitMultiline("""
        guard !stream.isEof else { return nil }
        stream.markSubstringStart()

        var longestAttempt: (Self.TokenKind, StringStream<StringType>.State)?

        """)

        // TODO: Attempt to generate a switch over the first peeked character
        // TODO: like in SwiftPEGGrammar's old Token parser?

        for token in sortedTokens.filter(showEmitInTokenParser) {
            let tokenName = caseName(for: token)
            let method = parseMethodName(for: token)
            let parseInvocation = "\(method)(from: &stream)"

            buffer.emitBlock("recordTokenAttempt(longestAttempt: &longestAttempt, stream: &stream) ") {
                buffer.backtrackWhitespace()
                buffer.emitLine(" stream in")

                buffer.emitMultiline("""
                if \(parseInvocation) {
                    return .\(tokenName)
                } else {
                    return nil
                }
                """)
            }
        }

        buffer.ensureDoubleNewline()
        buffer.emitMultiline("""
        guard let longestAttempt else {
            return nil
        }

        stream.restore(longestAttempt.1)
        return .init(kind: longestAttempt.0, string: stream.substring)
        """)
    }

    func _generateTokenTypeParserBodyLongestParse(
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws -> [Statement] {
        var result: [Statement] = []

        result.append(contentsOf: [
            .guard(.unary(op: .negate, .identifier("stream").dot("isEof")), else: [
                .return(.constant(.nil))
            ]),
            .expression(.identifier("stream").dot("markSubstringStart").call()),
            // var longestAttempt: (Self.TokenKind, StringStream<StringType>.State)?
            .variableDeclaration(identifier: "longestAttempt", type: .tuple([.nested(.init(base: "Self", nested: "TokenKind")), .nested(.init(base: .generic("StringStream", parameters: ["StringType"]), nested: "State"))]), initialization: nil)
        ])

        // TODO: Attempt to generate a switch over the first peeked character
        // TODO: like in SwiftPEGGrammar's old Token parser?

        for token in sortedTokens.filter(showEmitInTokenParser) {
            let tokenName = caseName(for: token)
            let method = parseMethodName(for: token)
            let parseInvocation = "\(method)(from: &stream)"

            buffer.emitBlock("recordTokenAttempt(longestAttempt: &longestAttempt, stream: &stream) ") {
                buffer.backtrackWhitespace()
                buffer.emitLine(" stream in")

                buffer.emitMultiline("""
                if \(parseInvocation) {
                    return .\(tokenName)
                } else {
                    return nil
                }
                """)
            }
        }

        result.append(contentsOf: [
            .guard(clauses: .init(pattern: .valueBindingPattern(constant: true, .identifier("longestAttempt")), expression: .identifier("longestAttempt")), else: [
                .return(.constant(.nil))
            ]),
            .expression(.identifier("stream").dot("restore").call([.identifier("longestAttempt").dot("1")])),
            .return(.implicitMember("init").call([
                .init(label: "kind", expression: .identifier("longestAttempt").dot("0")),
                .init(label: "string", expression: .identifier("stream").dot("substring")),
            ])),
        ])

        return result
    }

    // MARK: TokenKind generation

    func generateTokenKindEnum(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws {
        generateAccessLevel(settings: settings)
        try buffer.emitBlock("enum TokenKind: TokenKindType") {
            let emitter = buffer.startConditionalEmitter()

            for token in sortedTokens.filter(self.showEmitInTokenType) {
                try generateTokenKindEnumCase(token, prevCaseSeparator: emitter)
            }

            buffer.ensureDoubleNewline()
            try generateTokenKindDescription(
                settings: settings,
                sortedTokens: sortedTokens
            )
        }
    }

    func _generateTokenKindEnum(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws -> EnumDecl {
        let accessLevel = _generateAccessLevel(settings: settings)

        var cases: [EnumCaseDecl] = []
        for token in sortedTokens.filter(self.showEmitInTokenType) {
            cases.append(try _generateTokenKindEnumCase(token))
        }

        let tokenKindDesc = try _generateTokenKindDescription(
            settings: settings,
            sortedTokens: sortedTokens
        )

        return .init(
            leadingComments: [],
            accessLevel: accessLevel,
            name: "TokenKind",
            inheritances: ["TokenKindType"],
            cases: cases,
            members: [.variable(tokenKindDesc)]
        )
    }

    func generateTokenKindEnumCase(
        _ token: InternalGrammar.TokenDefinition,
        prevCaseSeparator: CodeStringBuffer.ConditionalEmitter
    ) throws {
        if let syntax = token.tokenSyntax {
            /// Emit doc-comment for case
            prevCaseSeparator.ensureEmptyLine()
            generateTokenDocComment(token, syntax, short: true)
        }

        let tokenName = caseName(for: token)
        buffer.emitLine("case \(escapeIdentifier(tokenName))")
    }

    func _generateTokenKindEnumCase(
        _ token: InternalGrammar.TokenDefinition
    ) throws -> EnumCaseDecl {
        var comments: [SwiftComment] = []
        if let syntax = token.tokenSyntax {
            /// Emit doc-comment for case
            comments = _generateTokenDocComment(token, syntax, short: true)
        }

        let tokenName = caseName(for: token)

        return .init(leadingComments: comments, name: escapeIdentifier(tokenName))
    }

    func generateTokenKindDescription(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws {

        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitBlock("var description: String") {
            buffer.emitLine("switch self {")

            for token in sortedTokens where showEmitInTokenType(token) {
                guard let syntax = token.tokenSyntax else {
                    continue
                }
                let name = caseName(for: token)

                if let literal = syntax.staticTerminal() {
                    buffer.emitLine("case .\(name): \(tok_escapeLiteral(literal))")
                } else {
                    buffer.emitLine(#"case .\#(name): "\#(token.name)""#)
                }
            }

            buffer.emitLine("}")
        }
    }

    func _generateTokenKindDescription(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws -> VariableMemberDecl {

        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        var switchCases: [SwitchCase] = []
        for token in sortedTokens where showEmitInTokenType(token) {
            guard let syntax = token.tokenSyntax else {
                continue
            }
            let name = caseName(for: token)

            let pattern: SwitchCase.CasePattern = .init(pattern: .expression(.implicitMember(name)))
            let body: Statement

            if let literal = syntax.staticTerminal() {
                body = .expression(_tok_escapeLiteral(literal))
            } else {
                body = .expression(.constant(.string(token.name)))
            }

            switchCases.append(
                .init(casePatterns: [pattern], body: [body])
            )
        }
        let switchExpr: SwitchExpression = .switch(
            .identifier("self"),
            cases: switchCases,
            default: nil
        )

        return .init(
            leadingComments: [],
            attributes: attributes,
            accessLevel: accessLevel,
            isConstant: false,
            name: "description",
            type: .string,
            storage: .getter([
                .expression(switchExpr)
            ])
        )
    }

    // MARK: - consume_ method generation

    func generateTokenParsers(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws {

        for token in sortedTokens {
            buffer.ensureDoubleNewline()
            try generateTokenParser(token, settings: settings, modifiers: ["static"])
        }
    }

    func _generateTokenParsers(
        settings: TokenTypeGenSettings,
        sortedTokens: [InternalGrammar.TokenDefinition]
    ) throws -> [FunctionMemberDecl] {

        return try sortedTokens.compactMap {
            try _generateTokenParser($0, settings: settings, modifiers: [.static])
        }
    }

    func generateTokenParser(
        _ token: InternalGrammar.TokenDefinition,
        settings: TokenTypeGenSettings,
        modifiers: [String] = []
    ) throws {
        guard let tokenSyntax = token.tokenSyntax else {
            return
        }

        generateTokenDocComment(token, tokenSyntax)

        generateInlinableAttribute(settings: settings)
        generateAccessLevel(settings: settings)
        buffer.emitWithSeparators(modifiers + ["func"], separator: " ")
        buffer.emit(" \(parseMethodName(for: token))<StringType>(from stream: inout StringStream<StringType>) -> Bool ")
        try buffer.emitBlock {
            try generateTokenParserBody(tokenSyntax)
        }
    }

    func _generateTokenParser(
        _ token: InternalGrammar.TokenDefinition,
        settings: TokenTypeGenSettings,
        modifiers: FunctionSignature.Traits = []
    ) throws -> FunctionMemberDecl? {
        guard let tokenSyntax = token.tokenSyntax else {
            return nil
        }

        let comments = _generateTokenDocComment(token, tokenSyntax)
        let attributes = _generateInlinableAttribute(settings: settings)
        let accessLevel = _generateAccessLevel(settings: settings)

        let methodName = parseMethodName(for: token)
        let signature = FunctionSignature(
            attributes: attributes,
            genericParameters: ["StringType"],
            name: methodName,
            parameters: [
                .init(label: "from", name: "stream", type: .generic("StringStream", parameters: ["StringType"]), modifier: .inout)
            ],
            returnType: .bool,
            traits: modifiers
        )

        return .init(
            leadingComments: comments,
            accessLevel: accessLevel,
            signature: signature,
            body: .init(statements: try _generateTokenParserBody(tokenSyntax))
        )
    }

    func generateTokenDocComment(
        _ token: InternalGrammar.TokenDefinition,
        _ tokenSyntax: CommonAbstract.TokenSyntax,
        short: Bool = false
    ) {
        // Derive a doc comment for the generated syntax
        let linePrefix = "///"

        if short && tokenSyntax.alts.count == 1 {
            buffer.emitLine("\(linePrefix) `\(tok_describe(tokenSyntax.alts[0]))`")
            return
        }

        buffer.emitLine("\(linePrefix) ```")
        buffer.emit("\(linePrefix) \(token.name)")
        if let tokenCodeReference = token.tokenCodeReference {
            buffer.emit(#"[\#(tokenCodeReference.debugDescription)]"#)
        }
        buffer.emitLine(":")
        for alt in tokenSyntax.alts {
            buffer.emitLine("\(linePrefix)     | \(tok_describe(alt))")
        }
        buffer.emitLine("\(linePrefix)     ;")
        buffer.emitLine("\(linePrefix) ```")
    }

    func _generateTokenDocComment(
        _ token: InternalGrammar.TokenDefinition,
        _ tokenSyntax: CommonAbstract.TokenSyntax,
        short: Bool = false
    ) -> [SwiftComment] {
        // Derive a doc comment for the generated syntax
        let buffer = CodeStringBuffer()

        if short && tokenSyntax.alts.count == 1 {
            buffer.emitLine(" `\(tok_describe(tokenSyntax.alts[0]))`")
            return [
                .docBlock(buffer.finishBuffer(addTrailingNewline: false))
            ]
        }

        buffer.emitLine(" ```")
        buffer.emit(" \(token.name)")
        if let tokenCodeReference = token.tokenCodeReference {
            buffer.emit(#"[\#(tokenCodeReference.debugDescription)]"#)
        }
        buffer.emitLine(":")
        for alt in tokenSyntax.alts {
            buffer.emitLine("     | \(tok_describe(alt))")
        }
        buffer.emitLine("     ;")
        buffer.emitLine(" ```")

        return [
            .docBlock(buffer.finishBuffer(addTrailingNewline: false))
        ]
    }

    func generateTokenParserBody(_ tokenSyntax: CommonAbstract.TokenSyntax) throws {
        // Simplify token definitions that consist of a single literal
        if
            let literal = tokenSyntax.staticTerminal(),
            tokenSyntax.alts.allSatisfy({ $0.trailExclusions.isEmpty })
        {
            buffer.emitLine("stream.advanceIfNext(\(tok_escapeLiteral(literal)))")
            return
        }

        buffer.emitLine("guard !stream.isEof else { return false }")
        // TODO: It may be better to create reusable methods that can detect state
        // TODO: usage in token syntaxes
        let delayedState = buffer.startDelayedEmission { buffer in
            buffer.emitLine("let state = stream.save()")
        }
        buffer.emitNewline()

        // TODO: Alternate do statement for an if statement depending on leading
        // TODO: item, and remove the nesting altogether if there is only one alt

        var bailStatement = BailStatementMonitor.break(label: "alt")
        var hasFallthroughPath: Bool {
            bailStatement.emitted
        }

        let emitter = buffer.startConditionalEmitter()
        for (i, alt) in tokenSyntax.alts.enumerated() {
            bailStatement = BailStatementMonitor.break(label: "alt")

            let canReturnAsBail = i == tokenSyntax.alts.count - 1

            emitter.emit("\n")

            buffer.emitLine("alt:")
            buffer.emit("do ")
            try buffer.emitBlock {
                try generateTokenParserAlt(
                    alt,
                    canReturnAsBail: canReturnAsBail,
                    bailStatement: bailStatement
                )
            }

            if !canReturnAsBail || hasFallthroughPath {
                delayedState.emit()

                buffer.emitNewline()
                buffer.emitLine("stream.restore(state)")
            }
        }

        if hasFallthroughPath {
            emitter.emit("\n")
            buffer.emitLine("return false")
        }
    }

    func _generateTokenParserBody(_ tokenSyntax: CommonAbstract.TokenSyntax) throws -> [Statement] {
        var result: [Statement] = []

        // Simplify token definitions that consist of a single literal
        if
            let literal = tokenSyntax.staticTerminal(),
            tokenSyntax.alts.allSatisfy({ $0.trailExclusions.isEmpty })
        {
            result.append(
                .expression(.identifier("stream").dot("advanceIfNext").call([
                    _tok_escapeLiteral(literal)
                ]))
            )

            return result
        }

        result.append(
            .guard(.unary(op: .negate, .identifier("stream").dot("isEof")), else: [
                .return(.constant(false))
            ])
        )

        var usedState = false
        let stateInsertIndex = result.count

        // TODO: Alternate do statement for an if statement depending on leading
        // TODO: item, and remove the nesting altogether if there is only one alt

        var bailStatement = BailStatementMonitor.break(label: "alt")
        var hasFallthroughPath: Bool {
            bailStatement.emitted
        }

        for (i, alt) in tokenSyntax.alts.enumerated() {
            bailStatement = BailStatementMonitor.break(label: "alt")

            let canReturnAsBail = i == tokenSyntax.alts.count - 1

            result.append(
                .do(.init(statements: try _generateTokenParserAlt(alt, canReturnAsBail: canReturnAsBail, bailStatement: bailStatement)))
                .labeled("alt")
            )

            if !canReturnAsBail || hasFallthroughPath {
                usedState = true

                result.append(
                    .expression(.identifier("stream").dot("restore").call([.identifier("state")]))
                )
            }
        }

        if hasFallthroughPath {
            result.append(.return(.constant(false)))
        }

        if usedState {
            result.insert(
                .variableDeclaration(identifier: "state", type: .nested(.init(base: .generic("StringStream", parameters: ["StringType"]), nested: "State")), isConstant: true, initialization: .identifier("stream").dot("save").call()),
                at: stateInsertIndex
            )
        }

        return result
    }

    func generateTokenParserAlt(
        _ alt: CommonAbstract.TokenAlt,
        canReturnAsBail: Bool,
        bailStatement: BailStatementMonitor
    ) throws {
        for (i, item) in alt.items.enumerated() {
            var bailStatement = bailStatement
            if canReturnAsBail && i == 0 {
                bailStatement = .return
            }

            try generateTokenParserItem(
                item,
                bailStatement: bailStatement
            )
            buffer.ensureDoubleNewline()
        }

        // Generate trailing token exclusions
        if !alt.trailExclusions.isEmpty {
            let exclusions = try alt.trailExclusions.map(tok_conditional)
            buffer.emit("guard ")
            buffer.emit(exclusions.joined(separator: ", "))
            buffer.emitBlock(" else") {
                bailStatement.emit(into: buffer)
            }
        }

        buffer.emitLine("return true")
    }

    func _generateTokenParserAlt(
        _ alt: CommonAbstract.TokenAlt,
        canReturnAsBail: Bool,
        bailStatement: BailStatementMonitor
    ) throws -> [Statement] {
        var result: [Statement] = []

        for (i, item) in alt.items.enumerated() {
            var bailStatement = bailStatement
            if canReturnAsBail && i == 0 {
                bailStatement = .return
            }

            let next = try _generateTokenParserItem(
                item,
                bailStatement: bailStatement
            )
            result.append(contentsOf: next)
        }

        // Generate trailing token exclusions
        if !alt.trailExclusions.isEmpty {
            let exclusions = try alt.trailExclusions.map(_tok_conditional).map { ConditionalClauseElement(expression: $0) }

            let guardStmt = GuardStatement.guard(clauses: .init(clauses: exclusions), else: .init(statements: bailStatement._emit()))

            result.append(guardStmt)
        }

        result.append(.return(.constant(true)))

        return result
    }

    /// Generates token parser items as sequences of checks against the input
    /// stream that either succeed and proceed forward within the same level,
    /// or fail with a `break alt` statement.
    func generateTokenParserItem(
        _ item: CommonAbstract.TokenItem,
        bailStatement: BailStatementMonitor
    ) throws {

        switch item {
        case .zeroOrMore(let alts):
            // Generate loop
            try generateAtomLoop(alts)

        case .oneOrMore(let alts):
            // Generate a first check outside the loop
            try generateAtomAlts(alts, bailStatement: bailStatement)

            // Generate loop
            try generateAtomLoop(alts)

        case .optionalGroup(let alts):
            try generateAtomAlts(alts, bailStatement: .none)

        case .group(let alts):
            // Like a one-or-more, but without a loop
            try generateAtomAlts(alts, bailStatement: bailStatement)

        case .optionalAtom(let atom):
            try generateIfAtom(atom, bailStatement: .custom(tok_advanceExpr(for: atom)))
            buffer.emitNewline()

        case .atom(let atom):
            try generateGuardAtom(atom, bailStatement: bailStatement)
            buffer.emitLine(try tok_advanceExpr(for: atom))
            buffer.emitNewline()
        }
    }

    /// Generates token parser items as sequences of checks against the input
    /// stream that either succeed and proceed forward within the same level,
    /// or fail with a `break alt` statement.
    func _generateTokenParserItem(
        _ item: CommonAbstract.TokenItem,
        bailStatement: BailStatementMonitor
    ) throws -> [Statement] {

        var result: [Statement] = []

        switch item {
        case .zeroOrMore(let alts):
            // Generate loop
            result.append(try _generateAtomLoop(alts))

        case .oneOrMore(let alts):
            // Generate a first check outside the loop
            if let expr = try _generateAtomAlts(alts, bailStatement: bailStatement) {
                result.append(.expression(expr))
            }

            // Generate loop
            result.append(try _generateAtomLoop(alts))

        case .optionalGroup(let alts):
            if let expr = try _generateAtomAlts(alts, bailStatement: .none) {
                result.append(.expression(expr))
            }

        case .group(let alts):
            // Like a one-or-more, but without a loop
            if let expr = try _generateAtomAlts(alts, bailStatement: bailStatement) {
                result.append(.expression(expr))
            }

        case .optionalAtom(let atom):
            let expr = try _generateIfAtom(atom, bailStatement: .custom(tok_advanceExpr(for: atom)))

            result.append(.expression(expr))

        case .atom(let atom):
            result.append(try _generateGuardAtom(atom, bailStatement: bailStatement))

            if let expr = try _tok_advanceExpr(for: atom) {
                result.append(
                    .expression(expr)
                )
            }
        }

        return result
    }

    /// Generates a guard statement that checks that a given atom matches
    /// on the string stream before proceeding.
    ///
    /// Within the body of the guard, the provided a bail statement is issued.
    func generateGuardAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatementMonitor
    ) throws {

        buffer.emit("guard \(try tok_conditional(for: atom)) else ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates a guard statement that checks that a given atom matches
    /// on the string stream before proceeding.
    ///
    /// Within the body of the guard, the provided a bail statement is issued.
    func _generateGuardAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatementMonitor
    ) throws -> GuardStatement {

        let clauses = try _tok_conditional(for: atom)
        let body = bailStatement._emit()

        return .guard(clauses: .init(clauses: clauses), else: .init(statements: body))
    }

    /// Generates an if- statement that checks that a given atom matches
    /// on the string stream before bailing.
    ///
    /// Within the body of the if, the provided a bail statement is issued.
    func generateIfAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatementMonitor
    ) throws {

        buffer.emit("if \(try tok_conditional(for: atom)) ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates an if- statement that checks that a given atom matches
    /// on the string stream before bailing.
    ///
    /// Within the body of the if, the provided a bail statement is issued.
    func _generateIfAtom(
        _ atom: CommonAbstract.TokenAtom,
        bailStatement: BailStatementMonitor
    ) throws -> IfExpression {

        let clauses = try _tok_conditional(for: atom)
        let body = bailStatement._emit()

        return .if(clauses: .init(clauses: clauses), body: .init(statements: body))
    }

    /// Generates a `while` loop that continually consumes the first matched
    /// atom in the provided list, returning
    func generateAtomLoop(_ alts: [CommonAbstract.TokenAtom]) throws {
        buffer.ensureDoubleNewline()
        buffer.emitLine("loop:")
        buffer.emit("while !stream.isEof ")
        try buffer.emitBlock {
            try generateAtomAlts(alts, bailStatement: .break(label: "loop"))
        }
    }

    /// Generates a `while` loop that continually consumes the first matched
    /// atom in the provided list, returning
    func _generateAtomLoop(_ alts: [CommonAbstract.TokenAtom]) throws -> WhileStatement {
        let expr: Expression = .unary(op: .negate, .identifier("stream").dot("isEof"))
        let body = try _generateAtomAlts(alts, bailStatement: .break(label: "loop")).map { Statement.expression($0) }

        if let body {
            return .while(expr, body: .init(statements: [body])).labeled("loop")
        }

        return .while(expr, body: .init(statements: [])).labeled("loop")
    }

    /// Generates a series of if-else conditions that attempt to match one of
    /// the provided terminals, falling back to a break statement if all terminals
    /// failed.
    ///
    /// Used to generate zero-or-more and one-or-more constructions.
    func generateAtomAlts(
        _ alts: [CommonAbstract.TokenAtom],
        bailStatement: BailStatementMonitor
    ) throws {
        // TODO: Perform switch statement emission

        guard !alts.isEmpty else { return }

        if try tryGenerateAsSwitch(alts, bailStatement: bailStatement) {
            return
        }

        try buffer.emitWithSeparators(alts, separator: " else ") { atom in
            buffer.emit("if ")
            buffer.emit(try tok_conditional(for: atom))
            buffer.emit(" ")
            try buffer.emitInlinedBlock {
                buffer.emit(try tok_advanceExpr(for: atom))
            }
        }

        // Final else block
        buffer.emit(" else ")
        buffer.emitBlock {
            bailStatement.emit(into: buffer)
        }
    }

    /// Generates a series of if-else conditions that attempt to match one of
    /// the provided terminals, falling back to a break statement if all terminals
    /// failed.
    ///
    /// Used to generate zero-or-more and one-or-more constructions.
    func _generateAtomAlts(
        _ alts: [CommonAbstract.TokenAtom],
        bailStatement: BailStatementMonitor
    ) throws -> Expression? {
        // TODO: Perform switch statement emission

        guard !alts.isEmpty else { return nil }

        if let switchExpr = try _tryGenerateAsSwitch(alts, bailStatement: bailStatement) {
            return switchExpr
        }

        let ifExpressions: [IfExpression] = try alts.map { atom in
            var body: [Statement] = []
            if let expr = try _tok_advanceExpr(for: atom) {
                body.append(.expression(expr))
            }

            return .if(clauses: .init(clauses: try _tok_conditional(for: atom)), body: .init(statements: body))
        }

        // [if1, if2, if3]
        // if1.else(if2.else(if3.else(bail)))
        guard var ifExpr = ifExpressions.last else {
            return nil
        }
        ifExpr.elseBody = .else(.init(statements: bailStatement._emit()))

        for next in ifExpressions.dropLast().reversed() {
            next.elseBody = .elseIf(ifExpr)
            ifExpr = next
        }

        return ifExpr
    }

    /// Generates the given set of alternating terminals as a switch statement,
    /// modifying the buffer and returning `true` if successful.
    ///
    /// If the alts cannot be simplified to a single switch statement, the buffer
    /// is untouched and `false` is returned.
    private func tryGenerateAsSwitch(
        _ alts: [CommonAbstract.TokenAtom],
        bailStatement: BailStatementMonitor
    ) throws -> Bool {

        guard canSimplifyAsSwitch(alts) else {
            return false
        }

        // Whether the given terminal can be combine with others in the same
        // switch-case
        func canCombine(_ term: CommonAbstract.TokenTerminal) -> Bool {
            switch term {
            case .identifier:
                return false
            case .rangeLiteral, .literal:
                return true
            case .characterPredicate, .any:
                return false
            }
        }

        // Whether the given atom can be combine with others in the same
        // switch-case
        func canCombine(_ atom: CommonAbstract.TokenAtom) -> Bool {
            atom.excluded.isEmpty && canCombine(atom.terminal)
        }

        /// Returns the combination of two atoms, as a list of atoms that cover
        /// the same range of inputs as the two input atoms.
        func combination(of lhs: CommonAbstract.TokenAtom, _ rhs: CommonAbstract.TokenAtom) -> [CommonAbstract.TokenAtom] {
            guard lhs.excluded.isEmpty && rhs.excluded.isEmpty else {
                return [lhs, rhs]
            }
            if lhs == rhs {
                return [lhs]
            }

            switch (lhs.terminal, rhs.terminal) {
            // Merge range literals
            case (.rangeLiteral(let lhsLow, let lhsHigh), .rangeLiteral(let rhsLow, let rhsHigh))
                where lhsHigh.contents == rhsLow.contents:

                return [.init(terminal: .rangeLiteral(lhsLow, rhsHigh))]

            // Merge literal into ranged literals that it is contained within
            case (.literal(let lhsLiteral), .rangeLiteral(let rhsLow, let rhsHigh))
                where (rhsLow.contents...rhsHigh.contents).contains(lhsLiteral.contents):

                return [rhs]
            case (.rangeLiteral(let lhsLow, let lhsHigh), .literal(let rhsLiteral))
                where (lhsLow.contents...lhsHigh.contents).contains(rhsLiteral.contents):

                return [lhs]

            default:
                return [lhs, rhs]
            }
        }

        // Produces the pattern for a switch-case for a given terminal
        func casePattern(_ term: CommonAbstract.TokenTerminal) -> String {
            switch term {
            case .characterPredicate(let ident, let predicate):
                return "let \(ident) where \(predicate.trimmingWhitespace())"
            case .rangeLiteral(let start, let end):
                return "\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))"
            case .literal(let literal):
                return "\(tok_escapeLiteral(literal))"
            case .any:
                return "_"
            default:
                return ""
            }
        }

        // Produces the pattern for a switch-case for a given atom
        func casePattern(_ atom: CommonAbstract.TokenAtom) -> String {
            casePattern(atom.terminal)
        }

        buffer.emitLine("switch stream.peek() {")

        // Indicates that an 'any' atom was found; this invalidates any further
        // cases, and to avoid warnings about unreachable cases we skip past any
        // alt after the 'any' atom.
        var stopEarly = false

        var index = 0
        while index < alts.count && !stopEarly {
            defer { index += 1 }
            let alt = alts[index]

            switch alt.terminal {
            case .any:
                buffer.emitLine("default:")
                stopEarly = true
            default:
                if canCombine(alt) {
                    var combined: [CommonAbstract.TokenAtom] = [alt]

                    var nextIndex = index + 1
                    while nextIndex < alts.count && !stopEarly {
                        let nextAlt = alts[nextIndex]
                        guard canCombine(nextAlt) && tok_length(for: alt) == tok_length(for: nextAlt) else {
                            break
                        }
                        let lastCombined = combination(of: combined[combined.count - 1], nextAlt)
                        combined[(combined.count - 1)...] = lastCombined[...]
                        nextIndex += 1
                    }

                    switch alt {
                    default:
                        let patterns = combined
                            .map(casePattern)
                            .joined(separator: ", ")
                        buffer.emitLine("case \(patterns):")
                    }

                    index = nextIndex - 1
                } else {
                    switch alt {
                    default:
                        buffer.emitLine("case \(casePattern(alt)):")
                    }
                }
            }

            try buffer.indented {
                buffer.emitLine(try tok_advanceExpr(for: alt))
            }
        }
        // Emit default block
        if !stopEarly {
            buffer.emitLine("default:")
            buffer.indented {
                bailStatement.emit(into: buffer)
            }
        }

        buffer.emitLine("}")

        return true
    }

    /// Generates the given set of alternating terminals as a switch statement,
    /// modifying the buffer and returning `true` if successful.
    ///
    /// If the alts cannot be simplified to a single switch statement, the buffer
    /// is untouched and `false` is returned.
    private func _tryGenerateAsSwitch(
        _ alts: [CommonAbstract.TokenAtom],
        bailStatement: BailStatementMonitor
    ) throws -> SwitchExpression? {

        guard canSimplifyAsSwitch(alts) else {
            return nil
        }

        // Whether the given terminal can be combine with others in the same
        // switch-case
        func canCombine(_ term: CommonAbstract.TokenTerminal) -> Bool {
            switch term {
            case .identifier:
                return false
            case .rangeLiteral, .literal:
                return true
            case .characterPredicate, .any:
                return false
            }
        }

        // Whether the given atom can be combine with others in the same
        // switch-case
        func canCombine(_ atom: CommonAbstract.TokenAtom) -> Bool {
            atom.excluded.isEmpty && canCombine(atom.terminal)
        }

        /// Returns the combination of two atoms, as a list of atoms that cover
        /// the same range of inputs as the two input atoms.
        func combination(of lhs: CommonAbstract.TokenAtom, _ rhs: CommonAbstract.TokenAtom) -> [CommonAbstract.TokenAtom] {
            guard lhs.excluded.isEmpty && rhs.excluded.isEmpty else {
                return [lhs, rhs]
            }
            if lhs == rhs {
                return [lhs]
            }

            switch (lhs.terminal, rhs.terminal) {
            // Merge range literals
            case (.rangeLiteral(let lhsLow, let lhsHigh), .rangeLiteral(let rhsLow, let rhsHigh))
                where lhsHigh.contents == rhsLow.contents:

                return [.init(terminal: .rangeLiteral(lhsLow, rhsHigh))]

            // Merge literal into ranged literals that it is contained within
            case (.literal(let lhsLiteral), .rangeLiteral(let rhsLow, let rhsHigh))
                where (rhsLow.contents...rhsHigh.contents).contains(lhsLiteral.contents):

                return [rhs]
            case (.rangeLiteral(let lhsLow, let lhsHigh), .literal(let rhsLiteral))
                where (lhsLow.contents...lhsHigh.contents).contains(rhsLiteral.contents):

                return [lhs]

            default:
                return [lhs, rhs]
            }
        }

        // Produces the pattern for a switch-case for a given terminal
        func casePattern(_ term: CommonAbstract.TokenTerminal) -> SwitchCase.CasePattern? {
            switch term {
            case .characterPredicate(let ident, let predicate):
                return .init(pattern: .valueBindingPattern(constant: true, .identifier(ident)), whereClause: .unknown(.init(context: predicate)))

            case .rangeLiteral(let start, let end):
                return .init(pattern: .expression(.binary(lhs: _tok_escapeLiteral(start), op: .closedRange, rhs: _tok_escapeLiteral(end))))

            case .literal(let literal):
                return .init(pattern: .expression(_tok_escapeLiteral(literal)))

            case .any:
                return .init(pattern: .wildcard())

            default:
                return nil
            }
        }

        // Produces the pattern for a switch-case for a given atom
        func casePattern(_ atom: CommonAbstract.TokenAtom) -> SwitchCase.CasePattern? {
            casePattern(atom.terminal)
        }

        // Indicates that an 'any' atom was found; this invalidates any further
        // cases, and to avoid warnings about unreachable cases we skip past any
        // alt after the 'any' atom.
        var stopEarly = false

        let switchExpr: Expression = .identifier("stream").dot("peek").call()
        var switchCases: [SwitchCase] = []
        var defaultCase: SwitchDefaultCase?

        var index = 0
        while index < alts.count && !stopEarly {
            defer { index += 1 }
            let alt = alts[index]

            guard let advanceExpr = try _tok_advanceExpr(for: alt) else {
                continue
            }

            switch alt.terminal {
            case .any:
                defaultCase = .init(body: [
                    .expression(advanceExpr)
                ])

                stopEarly = true

            default:
                var casePatterns: [SwitchCase.CasePattern] = []
                let statements: [Statement] = [
                    .expression(advanceExpr)
                ]

                if canCombine(alt) {
                    var combined: [CommonAbstract.TokenAtom] = [alt]

                    var nextIndex = index + 1
                    while nextIndex < alts.count && !stopEarly {
                        let nextAlt = alts[nextIndex]
                        guard canCombine(nextAlt) && tok_length(for: alt) == tok_length(for: nextAlt) else {
                            break
                        }
                        let lastCombined = combination(of: combined[combined.count - 1], nextAlt)
                        combined[(combined.count - 1)...] = lastCombined[...]
                        nextIndex += 1
                    }

                    switch alt {
                    default:
                        let patterns = combined.compactMap(casePattern)

                        casePatterns.append(contentsOf: patterns)
                    }

                    index = nextIndex - 1
                } else {
                    if let casePattern = casePattern(alt) {
                        casePatterns = [casePattern]
                    }
                }

                let switchCase = SwitchCase(casePatterns: casePatterns, body: .init(statements: statements))
                switchCases.append(switchCase)
            }
        }
        // Emit default block
        if !stopEarly {
            defaultCase = .init(statements: bailStatement._emit())
        }

        return .switch(switchExpr, cases: switchCases, default: defaultCase)
    }

    /// Whether the given set of alts can be simplified to a single switch statement
    /// that inspects a single token from the stream.
    func canSimplifyAsSwitch(_ alts: [CommonAbstract.TokenAtom]) -> Bool {
        for alt in alts {
            if !alt.excluded.isEmpty {
                return false
            }

            switch alt.terminal {
            case .identifier:
                return false

            case .characterPredicate, .rangeLiteral, .literal, .any:
                guard tok_length(for: alt) == 1 else {
                    return false
                }
            }
        }

        return true
    }

    // MARK: - Conditional checkers

    private func showEmitInTokenType(_ token: InternalGrammar.TokenDefinition) -> Bool {
        !token.isFragment
    }

    private func showEmitInTokenParser(_ token: InternalGrammar.TokenDefinition) -> Bool {
        !token.isFragment
    }

    // MARK: - Conditional emissions

    private func generateInlinableAttribute(settings: TokenTypeGenSettings) {
        if settings.emitInlinable {
            buffer.emitLine("@inlinable")
        }
    }

    private func _generateInlinableAttribute(settings: TokenTypeGenSettings) -> [DeclarationAttribute] {
        if settings.emitInlinable {
            return [.init(name: "inlinable")]
        }

        return []
    }

    private func generateAccessLevel(settings: TokenTypeGenSettings) {
        if let accessLevel = settings.accessLevel {
            buffer.emit("\(accessLevel) ")
        }
    }

    private func _generateAccessLevel(settings: TokenTypeGenSettings) -> AccessLevel {
        if let accessLevel = settings.accessLevel {
            return AccessLevel(rawValue: accessLevel) ?? .internal
        }

        return .internal
    }

    // MARK: - Static token transformations

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given atom.
    private func tok_conditional(for atom: CommonAbstract.TokenAtom) throws -> String {
        let conditionals = try atom.excluded.map(tok_conditional) + [tok_conditional(for: atom.terminal)]

        return conditionals.joined(separator: ", ")
    }

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given atom.
    private func _tok_conditional(for atom: CommonAbstract.TokenAtom) throws -> [ConditionalClauseElement] {
        let conditionals = try atom.excluded.map(_tok_conditional).map { ConditionalClauseElement(expression: $0) } + _tok_conditional(for: atom.terminal)

        return conditionals
    }

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given terminal.
    private func tok_conditional(for term: CommonAbstract.TokenTerminal) throws -> String {
        switch term {
        case .characterPredicate(let ident, let action):
            return "let \(ident) = stream.safePeek(), \(action.trimmingWhitespace())"

        case .rangeLiteral(let start, let end):
            return "!stream.isEof, stream.isNextInRange(\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end)))"

        case .literal(let literal):
            return "stream.isNext(\(tok_escapeLiteral(literal)))"

        case .identifier(let ident):
            return  "\(parseMethodName(for: ident))(from: &stream)"

        case .any:
            return "!stream.isEof"
        }
    }

    /// Returns the conditional statement that matches the current stream index
    /// of a StringStreamer called `stream` to a given terminal.
    private func _tok_conditional(for term: CommonAbstract.TokenTerminal) throws -> [ConditionalClauseElement] {
        switch term {
        case .characterPredicate(let ident, let action):
            return [
                .init(pattern: .valueBindingPattern(constant: true, .identifier(ident)), expression: .identifier("stream").dot("safePeek").call()),
                .init(expression: .unknown(.init(context: action.trimmingWhitespace())))
            ]

        case .rangeLiteral(let start, let end):
            return [
                .init(expression: .unary(op: .negate, .identifier("stream").dot("isEof"))),
                .init(expression: .identifier("stream").dot("isNextInRange").call([
                    _tok_escapeLiteral(start).binary(op: .closedRange, rhs: _tok_escapeLiteral(end))
                ]))
            ]

        case .literal(let literal):
            return [
                .init(expression: .identifier("stream").dot("isNext").call([_tok_escapeLiteral(literal)]))
            ]

        case .identifier(let ident):
            return [
                .init(expression: .identifier(parseMethodName(for: ident)).call([
                    .init(label: "from", expression: .unary(op: .bitwiseAnd, .identifier("stream")))
                ]))
            ]

        case .any:
            return [
                .init(expression: .unary(op: .negate, .identifier("stream").dot("isEof")))
            ]
        }
    }

    private func tok_conditional(for exclude: CommonAbstract.TokenExclusion) throws -> String {
        switch exclude {
        case .literal(let literal):
            return "!stream.isNext(\(tok_escapeLiteral(literal)))"

        case .identifier(let ident):
            return "stream.negativeLookahead(\(parseMethodName(for: ident))(from:))"

        case .rangeLiteral(let start, let end):
            return "!stream.isNextInRange(\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end)))"
        }
    }

    private func _tok_conditional(for exclude: CommonAbstract.TokenExclusion) throws -> Expression {
        switch exclude {
        case .literal(let literal):
            return .unary(op: .negate, .identifier("stream").dot("isNext").call([_tok_escapeLiteral(literal)]))

        case .identifier(let ident):
            return .identifier("stream").dot("negativeLookahead").call([
                .identifier(parseMethodName(for: ident), argumentNames: ["from"])
            ])

        case .rangeLiteral(let start, let end):
            return .unary(op: .negate, .identifier("stream").dot("isNextInRange").call([
                _tok_escapeLiteral(start).binary(op: .closedRange, rhs: _tok_escapeLiteral(end))
            ]))
        }
    }

    /// Returns the appropriate `StringStream.advance` call that advances the
    /// stream forward by the given atom's required length.
    private func tok_advanceExpr(for term: CommonAbstract.TokenAtom) throws -> String {
        let length = tok_length(for: term)
        if length == 0 {
            return ""
        }
        if length == 1 {
            return "stream.advance()"
        }
        return "stream.advance(\(length))"
    }

    /// Returns the appropriate `StringStream.advance` call that advances the
    /// stream forward by the given atom's required length.
    private func _tok_advanceExpr(for term: CommonAbstract.TokenAtom) throws -> Expression? {
        let length = tok_length(for: term)
        if length == 0 {
            return nil
        }
        if length == 1 {
            return .identifier("stream").dot("advance").call()
        }
        return .identifier("stream").dot("advance").call([.constant(.int(length))])
    }

    /// Returns how many extended grapheme clusters should be skipped for a given
    /// atom to match.
    private func tok_length(for atom: CommonAbstract.TokenAtom) -> Int {
        tok_length(for: atom.terminal)
    }

    /// Returns how many extended grapheme clusters should be skipped for a given
    /// terminal to match.
    private func tok_length(for term: CommonAbstract.TokenTerminal) -> Int {
        switch term {
        case .characterPredicate:
            return 1

        case .rangeLiteral:
            return 1

        case .literal(let literal):
            return literal.contents.count

        case .identifier:
            // Sub-syntaxes automatically advance the stream forward
            return 0

        case .any:
            return 1
        }
    }

    /// Escapes a given `DualString` into a Swift string literal expression.
    private func tok_escapeLiteral(_ string: CommonAbstract.DualString) -> String {
        switch string {
        case .fromSource(_, let original, _):
            // Convert single-quote into double-quote
            guard original.hasPrefix("'") else {
                return original
            }

            let terminator = "\""

            return terminator + StringEscaping.escapeTerminators(
                original.dropFirst().dropLast(),
                terminator: terminator
            ) + terminator

        case .fromCode(let contents):
            return tok_escapeLiteral(contents)
        }
    }

    /// Escapes a given `DualString` into a Swift string literal expression.
    private func _tok_escapeLiteral(_ string: CommonAbstract.DualString) -> Expression {
        .constant(.string(string.contents))
    }

    /// Escapes a given string into a Swift string literal expression.
    private func tok_escapeLiteral(_ literal: some StringProtocol) -> String {
        StringEscaping.escapeAsStringLiteral(literal)
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ alt: CommonAbstract.TokenAlt) -> String {
        (alt.items.map(tok_describe) + alt.trailExclusions.map(tok_describe)).joined(separator: " ")
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ item: CommonAbstract.TokenItem) -> String {
        switch item {
        case .zeroOrMore(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))*"

        case .oneOrMore(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))+"

        case .optionalGroup(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))?"

        case .group(let terms):
            return "(\(terms.map({ tok_describe($0) }).joined(separator: " | ")))"

        case .optionalAtom(let term):
            return "\(tok_describe(term))?"

        case .atom(let term):
            return tok_describe(term)
        }
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ atom: CommonAbstract.TokenAtom) -> String {
        let comp = atom.excluded.map({ tok_describe($0) }) + [tok_describe(atom.terminal)]
        return comp.joined(separator: " ")
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ exclude: CommonAbstract.TokenExclusion) -> String {
        switch exclude {
        case .identifier(let identifier):
            return "!\(identifier)"

        case .literal(let literal):
            return "!\(tok_escapeLiteral(literal))"

        case .rangeLiteral(let start, let end):
            return "!\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))"
        }
    }

    /// Used to generate doc comments for token parsing functions.
    private func tok_describe(_ term: CommonAbstract.TokenTerminal) -> String {
        switch term {
        case .characterPredicate(let c, let pred):
            return "\(c) {\(pred)}"

        case .rangeLiteral(let start, let end):
            return "\(tok_escapeLiteral(start))...\(tok_escapeLiteral(end))"

        case .identifier(let ident):
            return ident

        case .literal(let literal):
            return tok_escapeLiteral(literal)

        case .any:
            return "."
        }
    }

    /// Derives an identifier to use as an enumeration case label for a given
    /// token.
    ///
    /// If the token has a static token string that matches `<someIdentifier>`
    /// or `.<someIdentifier>`, returns `<someIdentifier>`, otherwise returns
    /// the token's name.
    func caseName(for token: InternalGrammar.TokenDefinition) -> String {
        guard let tokenCodeReference = token.tokenCodeReference else {
            return token.name
        }
        if tokenCodeReference.hasPrefix(".") {
            let suffix = tokenCodeReference.dropFirst()
            guard SwiftSyntaxExt.isIdentifier(suffix) else {
                return token.name
            }

            return String(suffix)
        } else {
            guard SwiftSyntaxExt.isIdentifier(tokenCodeReference) else {
                return token.name
            }

            return String(tokenCodeReference)
        }
    }

    /// Derives an identifier to use as an enumeration case label for a given
    /// token.
    ///
    /// If the token has a static token string that matches `<someIdentifier>`
    /// or `.<someIdentifier>`, returns `<someIdentifier>`, otherwise returns
    /// the token's name.
    func _caseName(for token: InternalGrammar.TokenDefinition) -> Expression {
        guard let tokenCodeReference = token.tokenCodeReference else {
            return .identifier(token.name)
        }
        if tokenCodeReference.hasPrefix(".") {
            let suffix = tokenCodeReference.dropFirst()
            guard SwiftSyntaxExt.isIdentifier(suffix) else {
                return .identifier(token.name)
            }

            return .identifier(String(suffix))
        } else {
            guard SwiftSyntaxExt.isIdentifier(tokenCodeReference) else {
                return .identifier(token.name)
            }

            return .identifier(String(tokenCodeReference))
        }
    }

    private func parseMethodName(for token: InternalGrammar.TokenDefinition) -> String {
        return parseMethodName(for: token.name)
    }

    private func parseMethodName(for identifier: String) -> String {
        return "consume_\(identifier)"
    }

    private static func _sortTokens(_ tokens: [InternalGrammar.TokenDefinition]) -> [InternalGrammar.TokenDefinition] {
        // Generate a graph of prefix-dependencies
        var byName: [String: InternalGrammar.TokenDefinition] = [:]
        var nameLookup: [Int: String] = [:]
        for (i, token) in tokens.enumerated() {
            byName[token.name] = token
            nameLookup[i] = token.name
        }

        var graph = IntDirectedGraph()
        graph.addNodes(nameLookup.keys)

        func tokenForNode(_ node: Int) -> InternalGrammar.TokenDefinition {
            tokens[node]
        }
        func nameForNode(_ node: Int) -> String {
            tokens[node].name
        }

        for (i, token) in tokens.enumerated() {
            guard let tokenSyntax = token.tokenSyntax else {
                continue
            }
            let tokenNode = i

            for (i, other) in tokens.enumerated() where token.name != other.name {
                guard let otherSyntax = other.tokenSyntax else {
                    continue
                }
                let otherNode = i

                if tokenSyntax.isPrefix(of: otherSyntax) && !otherSyntax.isPrefix(of: tokenSyntax) {
                    graph.addEdge(from: otherNode, to: tokenNode)
                } else if
                    tokenSyntax.isStatic(),
                    !otherSyntax.isStatic(),
                    !graph.hasPath(from: otherNode, to: tokenNode)
                {
                    // Add a synthetic dependency that forces static tokens to be
                    // emitted before dynamic tokens
                    graph.addEdge(from: tokenNode, to: otherNode)
                }
            }
        }

        guard var sorted = graph.topologicalSorted(breakTiesWith: { nameForNode($0) < nameForNode($1) })?.compactMap(tokenForNode) else {
            // TODO: Apply some fallback strategy
            return tokens
        }

        // Favor whitespace token to be first
        if let index = sorted.firstIndex(where: \.isWhitespace) {
            let token = sorted.remove(at: index)
            sorted.insert(token, at: 0)
        }

        return sorted
    }

    /// Monitor for bail statements and whether they where invoked.
    class BailStatementMonitor {
        private var kind: BailStatementKind

        /// Whether the `emit(into:)` method has been invoked for this bail
        /// statement monitor.
        private(set) var emitted: Bool = false

        init(kind: BailStatementKind) {
            self.kind = kind
        }

        func emit(into buffer: CodeStringBuffer) {
            self.emitted = true
            kind.emit(into: buffer)
        }

        func _emit() -> [Statement] {
            self.emitted = true
            return kind._emit()
        }

        /// Indicates that the bail statement should expand to a no-op, non
        /// control-flow-altering statement.
        ///
        /// Convenience for `BailStatementMonitor(kind: .none)`
        static var none: BailStatementMonitor {
            BailStatementMonitor(kind: .none)
        }

        /// Provides a custom expansion for the bail statement.
        ///
        /// Convenience for `BailStatementMonitor(kind: .custom(raw))`
        static func custom(_ raw: String) -> BailStatementMonitor {
            BailStatementMonitor(kind: .custom(raw))
        }

        /// Indicates that the bail statement should expand to:
        /// ```
        /// break [label]
        /// ```
        ///
        /// Convenience for `BailStatementMonitor(kind: .break(label: label))`
        static func `break`(label: String? = nil) -> BailStatementMonitor {
            BailStatementMonitor(kind: .break(label: label))
        }

        /// Indicates that the bail statement should expand to:
        /// ```
        /// return false
        /// ```
        ///
        /// Convenience for `BailStatementMonitor(kind: .`return`)`
        static var `return`: BailStatementMonitor {
            BailStatementMonitor(kind: .`return`)
        }

        /// Indicates that the bail statement should expand to:
        /// ```
        /// stream.restore(stateIdentifier)
        /// return false
        /// ```
        ///
        /// Convenience for `BailStatementMonitor(kind: .restoreAndReturn(stateIdentifier: stateIdentifier))`
        static func restoreAndReturn(stateIdentifier: String) -> BailStatementMonitor {
            BailStatementMonitor(kind: .restoreAndReturn(stateIdentifier: stateIdentifier))
        }
    }

    enum BailStatementKind {
        /// Indicates that the bail statement should expand to a no-op, non
        /// control-flow-altering statement.
        case none

        /// Provides a custom expansion for the bail statement.
        case custom(String)

        /// Indicates that the bail statement should expand to:
        /// ```
        /// break [label]
        /// ```
        case `break`(label: String? = nil)

        /// Indicates that the bail statement should expand to:
        /// ```
        /// return false
        /// ```
        case `return`

        /// Indicates that the bail statement should expand to:
        /// ```
        /// stream.restore(stateIdentifier)
        /// return false
        /// ```
        case restoreAndReturn(stateIdentifier: String)

        func emit(into buffer: CodeStringBuffer) {
            switch self {
            case .none:
                buffer.emitLine("_=()")

            case .custom(let line):
                buffer.emitLine(line)

            case .break(nil):
                buffer.emitLine("break")

            case .break(let label?):
                buffer.emitLine("break \(label)")

            case .return:
                buffer.emitLine("return false")

            case .restoreAndReturn(let stateIdentifier):
                buffer.emitLine("stream.restore(\(stateIdentifier))")
                buffer.emitLine("return false")
            }
        }

        func _emit() -> [Statement] {
            switch self {
            case .none:
                return [
                    .expression(.identifier("_").assignment(op: .assign, rhs: .voidTuple()))
                ]

            case .custom(let line):
                return [
                    .unknown(.init(context: line))
                ]

            case .break(nil):
                return [
                    .break()
                ]

            case .break(let label?):
                return [
                    .break(targetLabel: label)
                ]

            case .return:
                return [
                    .return(.constant(false))
                ]

            case .restoreAndReturn(let stateIdentifier):
                return [
                    .expression(
                        .identifier("stream").dot("restore").call([.identifier(stateIdentifier)])
                    ),
                    .return(.constant(false)),
                ]
            }
        }
    }
}

private extension InternalGrammar.TokenDefinition {
    var isWhitespace: Bool {
        self.tokenCodeReference == ".whitespace" || self.tokenCodeReference == "whitespace" || self.name == "whitespace"
    }
}
