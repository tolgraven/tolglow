import Foundation

/// Lower-body landmarks in the image coordinates shared by all observations in one frame.
public struct DancePose {
    public enum Joint: Hashable { case leftKnee, rightKnee, leftAnkle, rightAnkle }
    public typealias Point = SIMD2<Double>

    fileprivate let center: Point
    fileprivate let scale: Double
    fileprivate let legs: [Joint: Point]

    public init?(leftHip: Point, rightHip: Point, legs: [Joint: Point]) {
        let points = [leftHip, rightHip] + Array(legs.values)
        guard points.allSatisfy({ $0.x.isFinite && $0.y.isFinite }), legs.count >= 2 else { return nil }
        // Hip spacing supplies a persistent scale even as a knee bends or disappears.
        let width = distance(leftHip, rightHip)
        guard width >= 0.035 else { return nil }
        center = (leftHip + rightHip) / 2
        scale = width
        self.legs = legs
    }

    fileprivate func motion(from previous: DancePose, elapsed: Double) -> Double {
        let changes = legs.compactMap { joint, point -> Double? in
            guard let old = previous.legs[joint] else { return nil }
            return distance(point - center, old - previous.center)
        }
        guard changes.count >= 2, let peak = changes.max() else { return 0 }
        let hipWidthsPerSecond = peak / (elapsed * (scale + previous.scale) / 2)
        return min(1, hipWidthsPerSecond * 0.25)
    }
}

private func distance(_ a: DancePose.Point, _ b: DancePose.Point) -> Double {
    hypot(a.x - b.x, a.y - b.y)
}

/// A guest's camera contributes at most one dancer. Ambiguous crossings and gaps
/// need a new baseline before another leg movement can count as a step.
public struct DancePoseTracker {
    private var anchor: DancePose?
    private var lastSeen: Double?
    private var lastFrame: Double?
    private var continuous = false

    public init() {}

    public mutating func strength(_ poses: [DancePose], at time: Double) -> Double {
        guard time.isFinite, lastFrame.map({ time > $0 }) ?? true else { return 0 }
        let elapsed = time - (lastFrame ?? time)
        lastFrame = time

        if let previous = anchor, let lastSeen, time - lastSeen < 0.5 {
            let radius = max(0.07, previous.scale * 1.2)
            let matches = poses.filter { distance($0.center, previous.center) < radius }
            guard matches.count == 1, let next = matches.first else {
                continuous = false
                return 0
            }
            let strength = continuous ? next.motion(from: previous, elapsed: elapsed) : 0
            remember(next, at: time)
            return strength
        }

        let middle = DancePose.Point(0.5, 0.5)
        if let next = poses.min(by: { distance($0.center, middle) < distance($1.center, middle) }) {
            remember(next, at: time)
        } else {
            anchor = nil
            continuous = false
        }
        return 0
    }

    private mutating func remember(_ pose: DancePose, at time: Double) {
        anchor = pose
        lastSeen = time
        continuous = true
    }
}
