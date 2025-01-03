// swift-tools-version: 5.10
import PackageDescription
import CompilerPluginSupport

let package = Package(
    name: "swift-peg",
    platforms: [.macOS(.v13)],
    products: [
        .library(
            name: "SwiftPEG",
            targets: ["SwiftPEG"]
        ),
        .executable(
            name: "SwiftPEGGenerator",
            targets: ["SwiftPEGGenerator"]
        ),
        .plugin(
            name: "SwiftPEGPlugin",
            targets: ["SwiftPEGPlugin"]
        )
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-syntax.git", exact: "600.0.0"),
        .package(url: "https://github.com/apple/swift-argument-parser.git", exact: "1.4.0"),
        .package(url: "https://github.com/LuizZak/Console.git", exact: "0.14.1"),
        .package(url: "https://github.com/LuizZak/MiniDigraph.git", exact: "0.8.0"),
        .package(url: "https://github.com/LuizZak/SwiftAST.git", exact: "0.13.0"),
    ],
    targets: [
        .macro(
            name: "SwiftPEGMacros",
            dependencies: [
                .product(name: "SwiftDiagnostics", package: "swift-syntax"),
                .product(name: "SwiftSyntax", package: "swift-syntax"),
                .product(name: "SwiftSyntaxBuilder", package: "swift-syntax"),
                .product(name: "SwiftParser", package: "swift-syntax"),
                .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
                .product(name: "SwiftSyntaxMacrosTestSupport", package: "swift-syntax"),
                .product(name: "SwiftCompilerPlugin", package: "swift-syntax"),
            ]
        ),
        .executableTarget(
            name: "SwiftPEGGenerator",
            dependencies: [
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                .product(name: "Console", package: "Console"),
                "SwiftPEG",
            ]
        ),
        .plugin(
            name: "SwiftPEGPlugin",
            capability: .command(
                intent: .custom(
                    verb: "generate-parser",
                    description: "Generate parser or token type file in target where .gram/.tokens file is present"
                ),
                permissions: [.writeToPackageDirectory(
                    reason: "Save generated parser or token type code"
                )]
            ),
            dependencies: [
                .target(name: "SwiftPEGGenerator"),
            ],
            path: "Plugins/SwiftPEGPlugin"
        ),
        .target(
            name: "SwiftPEG",
            dependencies: [
                "SwiftPEGMacros",
                .product(name: "MiniDigraph", package: "MiniDigraph"),
                .product(name: "SwiftAST", package: "SwiftAST"),
            ],
            resources: [
                .copy("Grammar/metagrammar.gram"),
                .copy("Grammar/metagrammar.tokens"),
            ]
        ),
        .executableTarget(
            name: "SwiftPEGSample",
            dependencies: [
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                "SwiftPEGMacros",
                "SwiftPEG",
            ]
        ),
        .testTarget(
            name: "SwiftPEGMacrosTests",
            dependencies: [
                "SwiftPEG",
                "SwiftPEGMacros",
            ]
        ),
        .testTarget(
            name: "SwiftPEGTests",
            dependencies: [
                "SwiftPEG",
                .product(name: "Console", package: "Console"),
            ],
            exclude: [
                "FixtureTests/Fixtures",
            ]
        )
    ]
)
