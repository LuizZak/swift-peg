// swift-tools-version: 5.10
import PackageDescription

let package = Package(
    name: "SwiftPEG",
    products: [
        .library(
            name: "SwiftPEG",
            targets: ["SwiftPEG"]),
    ],
    targets: [
        .target(
            name: "SwiftPEG"),
        .testTarget(
            name: "SwiftPEGTests",
            dependencies: ["SwiftPEG"]),
    ]
)
