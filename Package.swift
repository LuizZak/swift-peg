// swift-tools-version: 5.10
import PackageDescription
import CompilerPluginSupport

let package = Package(
    name: "SwiftPEG",
    products: [
        .library(
            name: "SwiftPEG",
            targets: ["SwiftPEG"]
        ),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-syntax.git", exact: "510.0.0"),
    ],
    targets: [
        .target(
            name: "SwiftPEG",
            dependencies: [
                "SwiftPEGMacros",
            ]
        ),
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
        .testTarget(
            name: "SwiftPEGTests",
            dependencies: ["SwiftPEG"]
        ),
        .testTarget(
            name: "SwiftPEGMacrosTests",
            dependencies: [
                "SwiftPEG",
                "SwiftPEGMacros",
            ]
        )
    ]
)
