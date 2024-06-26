// swift-tools-version: 5.10
import PackageDescription
import CompilerPluginSupport

let package = Package(
    name: "swift-peg",
    products: [
        .library(
            name: "SwiftPEG",
            targets: ["SwiftPEG"]
        ),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-syntax.git", exact: "510.0.0"),
        .package(url: "https://github.com/apple/swift-argument-parser.git", exact: "1.4.0"),
        .package(url: "https://github.com/LuizZak/Console.git", exact: "0.13.0"),
        .package(url: "https://github.com/LuizZak/MiniDigraph.git", exact: "0.5.0"),
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
        .target(
            name: "SwiftPEG",
            dependencies: [
                "SwiftPEGMacros",
                .product(name: "MiniDigraph", package: "MiniDigraph"),
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
