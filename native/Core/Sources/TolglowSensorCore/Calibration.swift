import Foundation

/// Quaternion in CoreMotion's x/y/z/w ordering. No browser orientation equivalence.
public struct Quaternion: Equatable, Sendable {
    public let x: Double, y: Double, z: Double, w: Double
    public init(x: Double, y: Double, z: Double, w: Double) throws {
        let norm = sqrt(x*x + y*y + z*z + w*w)
        guard norm.isFinite, norm > 1e-12 else { throw SensorError.invalid("Quaternion") }
        self.x = x/norm; self.y = y/norm; self.z = z/norm; self.w = w/norm
    }
    public func relative(to origin: Quaternion) throws -> Quaternion {
        let a = origin
        // conjugate(origin) * current. Calibration does not subtract Euler angles.
        return try Quaternion(x: a.w*x - a.x*w - a.y*z + a.z*y,
                              y: a.w*y + a.x*z - a.y*w - a.z*x,
                              z: a.w*z - a.x*y + a.y*x - a.z*w,
                              w: a.w*w + a.x*x + a.y*y + a.z*z)
    }
    public var eulerRadians: [String: Double] {
        ["roll": atan2(2*(w*x + y*z), 1 - 2*(x*x + y*y)),
         "pitch": asin(max(-1, min(1, 2*(w*y - z*x)))),
         "yaw": atan2(2*(w*z + x*y), 1 - 2*(y*y + z*z))]
    }
}

/// Immutable mapping description for host authoring. It does not dispatch effects
/// or silently pick a target: the common host sensor middleware owns mappings.
public struct AxisMapping: Codable, Equatable, Sendable {
    public let axis: String
    public let inputMinimum: Double
    public let inputMaximum: Double
    public let outputMinimum: Double
    public let outputMaximum: Double
    public let inverted: Bool
    public init(axis: String, inputMinimum: Double, inputMaximum: Double,
                outputMinimum: Double, outputMaximum: Double, inverted: Bool = false) throws {
        guard !axis.isEmpty, [inputMinimum, inputMaximum, outputMinimum, outputMaximum].allSatisfy(\.isFinite),
              inputMinimum < inputMaximum, outputMinimum <= outputMaximum,
              (inputMaximum - inputMinimum).isFinite, (outputMaximum - outputMinimum).isFinite
        else { throw SensorError.invalid("Axis mapping bounds") }
        self.axis = axis; self.inputMinimum = inputMinimum; self.inputMaximum = inputMaximum
        self.outputMinimum = outputMinimum; self.outputMaximum = outputMaximum; self.inverted = inverted
    }
    public init(from decoder: Decoder) throws {
        let c = try decoder.container(keyedBy: CodingKeys.self)
        try self.init(axis: c.decode(String.self, forKey: .axis),
                      inputMinimum: c.decode(Double.self, forKey: .inputMinimum),
                      inputMaximum: c.decode(Double.self, forKey: .inputMaximum),
                      outputMinimum: c.decode(Double.self, forKey: .outputMinimum),
                      outputMaximum: c.decode(Double.self, forKey: .outputMaximum),
                      inverted: c.decode(Bool.self, forKey: .inverted))
    }
    public func value(for input: Double) throws -> Double {
        guard input.isFinite else { throw SensorError.invalid("Axis value") }
        var unit = max(0, min(1, (input - inputMinimum)/(inputMaximum - inputMinimum)))
        if inverted { unit = 1 - unit }
        return outputMinimum + unit*(outputMaximum - outputMinimum)
    }
}
