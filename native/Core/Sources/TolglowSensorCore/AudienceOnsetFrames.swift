import Foundation

/// Every detected step is counted once. An OSC packet can carry eight gestures,
/// and one show frame can release four packets without monopolizing the radio queue.
public struct AudienceOnsetFrames {
    private var steps = AudienceSteps()
    private var lastFrame: Double?
    private var available = 0
    public var pending: Int { steps.pending }
    public var ready: Bool { available > 0 }
    public var needsFrame: Bool { pending > available }

    public init() {}

    public static func nextBoundary(after time: Double) -> Double {
        ((time * 40).rounded(.down) + 1) / 40
    }

    public mutating func append(_ count: Int) throws { try steps.append(count) }

    public mutating func openFrame(at time: Double) {
        guard time.isFinite, time >= 0 else { return }
        let frame = (time * 40).rounded(.down)
        guard lastFrame.map({ frame > $0 }) ?? true else { return }
        lastFrame = frame
        available = min(pending, 32)
    }

    public mutating func takeBatch() -> Int {
        let count = steps.takeBatch(upTo: available)
        available -= count
        return count
    }
}
