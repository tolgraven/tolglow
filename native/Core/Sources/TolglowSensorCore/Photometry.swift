import Foundation

/// Relative on-axis brightness, sampled through the same diffuser at one metre.
/// The capture adapter supplies SDR RGB with a known transfer function.
public enum PhotometryError: LocalizedError {
    case clipped, unstable, tooDark, invalid
    public var errorDescription: String? {
        switch self {
        case .clipped: return "Sensor clipped. Restart with a shorter exposure or the same denser diffuser for all fixtures."
        case .unstable: return "Light varied during capture. Hold the phone still and repeat the pair."
        case .tooDark: return "Too little light above the dark reading. Check alignment or restart with a longer exposure."
        case .invalid: return "Invalid camera reading."
        }
    }
}

public enum SDRTransfer: Hashable {
    case sRGB, rec709, linear

    func linearize(_ byte: UInt8) -> Double {
        let v = Double(byte) / 255
        switch self {
        case .linear: return v
        case .sRGB: return v <= 0.04045 ? v / 12.92 : pow((v + 0.055) / 1.055, 2.4)
        case .rec709: return v < 0.081 ? v / 4.5 : pow((v + 0.099) / 1.099, 1 / 0.45)
        }
    }
}

/// One central region of a frame. Clip detection happens in each color
/// channel before the conversion to linear Rec. 709 relative luminance.
public struct PhotometryFrame {
    private static let tables: [SDRTransfer: [Double]] = Dictionary(uniqueKeysWithValues:
        [SDRTransfer.sRGB, .rec709, .linear].map { transfer in
            (transfer, (0...255).map { transfer.linearize(UInt8($0)) })
        })
    private let linear: [Double]
    private var sum = 0.0
    private var count = 0
    private var saturated = 0

    public init(transfer: SDRTransfer) { linear = Self.tables[transfer]! }

    public mutating func sample(red: UInt8, green: UInt8, blue: UInt8) {
        if max(red, green, blue) >= 250 { saturated += 1 }
        sum += 0.2126 * linear[Int(red)] + 0.7152 * linear[Int(green)] + 0.0722 * linear[Int(blue)]
        count += 1
    }

    public func luminance() throws -> Double {
        guard count >= 64 else { throw PhotometryError.invalid }
        guard saturated * 100 <= count else { throw PhotometryError.clipped }
        return sum / Double(count)
    }

    public static func stableMedian(_ frames: [Double]) throws -> Double {
        guard frames.count >= 15, frames.allSatisfy({ $0.isFinite && (0...1).contains($0) }) else {
            throw PhotometryError.invalid
        }
        let sorted = frames.sorted()
        let median = sorted[sorted.count / 2]
        guard sorted[sorted.count * 9 / 10] - sorted[sorted.count / 10] <= max(0.003, 0.12 * median)
        else { throw PhotometryError.unstable }
        return median
    }
}

public struct BrightnessReading: Equatable {
    public let dark: Double
    public let white: Double
    public var signal: Double { white - dark }

    public init(dark: Double, white: Double) throws {
        guard dark.isFinite, white.isFinite, (0...1).contains(dark), (0...1).contains(white) else {
            throw PhotometryError.invalid
        }
        guard white - dark >= max(0.003, 0.03 * dark) else { throw PhotometryError.tooDark }
        self.dark = dark
        self.white = white
    }
}
