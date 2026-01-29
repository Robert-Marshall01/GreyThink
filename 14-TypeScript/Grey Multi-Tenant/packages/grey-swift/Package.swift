// swift-tools-version: 5.9
// =============================================================================
// Grey Multi-Tenant SDK - Swift
// Package.swift
//
// Swift Package Manager manifest for the Grey SDK.
// =============================================================================

import PackageDescription

let package = Package(
    name: "GreySDK",
    platforms: [
        .iOS(.v15),
        .macOS(.v12),
        .tvOS(.v15),
        .watchOS(.v8)
    ],
    products: [
        .library(
            name: "GreySDK",
            targets: ["GreySDK"]
        ),
    ],
    dependencies: [
        .package(url: "https://github.com/grpc/grpc-swift.git", from: "1.21.0"),
        .package(url: "https://github.com/apple/swift-nio.git", from: "2.62.0"),
        .package(url: "https://github.com/apple/swift-nio-ssl.git", from: "2.25.0"),
        .package(url: "https://github.com/apple/swift-nio-transport-services.git", from: "1.20.0"),
    ],
    targets: [
        .target(
            name: "GreySDK",
            dependencies: [
                .product(name: "GRPC", package: "grpc-swift"),
                .product(name: "NIO", package: "swift-nio"),
                .product(name: "NIOSSL", package: "swift-nio-ssl"),
                .product(name: "NIOTransportServices", package: "swift-nio-transport-services"),
            ],
            path: "Sources/GreySDK"
        ),
        .testTarget(
            name: "GreySDKTests",
            dependencies: ["GreySDK"],
            path: "Tests/GreySDKTests"
        ),
    ]
)
