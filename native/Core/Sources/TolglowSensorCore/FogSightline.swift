import Foundation

/// Normalized rectangles in the delivered camera image, with the origin at top left.
/// The phone draws these same rectangles on the image that it measures.
public struct SightlinePatch {
    public let x: Double, y: Double, width: Double, height: Double
    public static let bright = Self(x: 0.23, y: 0.43, width: 0.18, height: 0.14)
    public static let dark = Self(x: 0.59, y: 0.43, width: 0.18, height: 0.14)

    public func pixel(column: Int, row: Int, width w: Int, height h: Int) -> (x: Int, y: Int) {
        (Int((x + width * (Double(column) + 0.5) / 16) * Double(w)),
         Int((y + height * (Double(row) + 0.5) / 16) * Double(h)))
    }
}

public enum FogSightlineError: LocalizedError {
    case uneven, weak, unstable, clipped
    public var errorDescription: String? {
        switch self {
        case .clipped: return "The target clipped. Restart in clear air with a shorter shutter time."
        case .uneven: return "Align each box inside one evenly lit patch, away from its edges."
        case .weak: return "The clear target needs a brighter left patch and a darker right patch."
        case .unstable: return "Clear-air contrast is changing. Fix the target light and restart the baseline."
        }
    }
}

public struct FogPatchMeter {
    private var quadrants: [PhotometryFrame]
    public init(transfer: SDRTransfer) {
        quadrants = Array(repeating: PhotometryFrame(transfer: transfer), count: 4)
    }
    public mutating func sample(row: Int, column: Int, red: UInt8, green: UInt8, blue: UInt8) {
        quadrants[(row / 8) * 2 + column / 8].sample(red: red, green: green, blue: blue)
    }
    public func luminance() throws -> Double {
        let values: [Double]
        do { values = try quadrants.map { try $0.luminance() } }
        catch PhotometryError.clipped { throw FogSightlineError.clipped }
        let mean = values.reduce(0,+) / 4
        guard values.max()! - values.min()! <= max(0.012, 0.20 * mean) else { throw FogSightlineError.uneven }
        return mean
    }
}

public enum FogPhase: String, CaseIterable, Codable { case baseline, active, clearing }

public struct FogReading {
    public let bright: Double, dark: Double
    public var contrast: Double { bright - dark }
    public init(bright: Double, dark: Double) throws {
        guard (0...1).contains(bright), (0...1).contains(dark), bright > dark else {
            throw FogSightlineError.weak
        }
        self.bright = bright
        self.dark = dark
    }
}

public struct FogBaseline {
    private var contrasts: [Double] = []
    private var lowest = Double.infinity
    private var highest = 0.0
    public init() {}
    public var ready: Bool { contrasts.count >= 8 }
    public var contrast: Double? {
        let sorted = contrasts.sorted()
        return sorted.isEmpty ? nil : sorted[sorted.count / 2]
    }
    public mutating func accept(_ reading: FogReading) throws {
        guard reading.contrast > 0.04 else { throw FogSightlineError.weak }
        let low = min(lowest, reading.contrast), high = max(highest, reading.contrast)
        // Match the host's 10% span criterion across the entire baseline, including
        // observations older than the bounded display median.
        guard high - low < 0.10 * low else { throw FogSightlineError.unstable }
        lowest = low
        highest = high
        if contrasts.count == 32 { contrasts.removeFirst() }
        contrasts.append(reading.contrast)
    }
}

public struct FogPose {
    private let x: Double, y: Double, z: Double, w: Double
    public init(x: Double, y: Double, z: Double, w: Double) {
        self.x = x; self.y = y; self.z = z; self.w = w
    }
    /// Attitude quaternions are unit length. q and -q describe the same rotation.
    public func holds(_ next: Self, accelerationG: Double) -> Bool {
        let dot = abs(x * next.x + y * next.y + z * next.z + w * next.w)
        return dot >= cos(Double.pi / 180) && accelerationG <= 0.12
    }
}

public struct FogFit: Decodable {
    public let clearance: Double
    public let mixing: Double?
    public let extinction: Double?
    public let rSquared: Double?
    public let samples: Int?
    enum CodingKeys: String, CodingKey {
        case clearance = "clearance-half-life-s", mixing = "mixing-seconds", extinction = "extinction-per-m"
        case rSquared = "r-squared", samples
    }
}

public struct FogTraceSample {
    public let phase: FogPhase, reading: FogReading, elapsedMilliseconds: Int
    public init(phase: FogPhase, reading: FogReading, cameraUptime: Double, sessionStart: Double) {
        self.phase = phase
        self.reading = reading
        self.elapsedMilliseconds = Int(((cameraUptime - sessionStart) * 1000).rounded())
    }
    public var fields: [String: Any] {
        ["elapsed-ms": elapsedMilliseconds, "bright": reading.bright, "dark": reading.dark, "phase": phase.rawValue]
    }
}
