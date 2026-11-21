// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "TolglowSensorCore",
    platforms: [.iOS("18.0"), .watchOS("11.0"), .macOS(.v13)],
    products: [.library(name: "TolglowSensorCore", targets: ["TolglowSensorCore"])],
    targets: [.target(name: "TolglowSensorCore"),
              .testTarget(name: "TolglowSensorCoreTests", dependencies: ["TolglowSensorCore"], resources: [.copy("Fixtures")])])
