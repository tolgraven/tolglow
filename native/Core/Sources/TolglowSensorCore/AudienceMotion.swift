import Foundation

/// Device-local acceleration in m/s² and angular velocity in rad/s, in wire order.
public struct AudienceMotion: Equatable {
    public static let channels = ["accel-x", "accel-y", "accel-z", "rotation-x", "rotation-y", "rotation-z"]
    let components: [Float]

    public init?(acceleration: SIMD3<Double>, rotation: SIMD3<Double>) {
        self.init([acceleration.x, acceleration.y, acceleration.z, rotation.x, rotation.y, rotation.z])
    }

    public init?(values: [String: Double]) {
        self.init(Self.channels.compactMap { values[$0] })
    }

    init?(_ values: [Double]) {
        guard values.count == Self.channels.count,
              values.allSatisfy({ $0.isFinite && abs($0) <= 256 }) else { return nil }
        components = values.map { Float($0) }
    }

    public static func mean(_ readings: [Self]) -> [String: Double] {
        guard !readings.isEmpty else { return [:] }
        return Dictionary(uniqueKeysWithValues: channels.enumerated().map { index, channel in
            let total = readings.reduce(0.0) { $0 + Double($1.components[index]) }
            return (channel, total / Double(readings.count))
        })
    }
}
