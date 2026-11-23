import Foundation

/// Periodic floor readings share an OSC stream with the faster onset frames.
public struct AudienceStageOutput {
    public var onsets = AudienceOnsetFrames()
    private var latest: AudienceLevels?
    private var due = false

    public init() {}

    /// Report how many accepted onsets expired so the stage can surface a stalled output.
    @discardableResult public mutating func reset() -> Int {
        let expired = onsets.pending
        self = AudienceStageOutput()
        return expired
    }

    @discardableResult public mutating func tick(_ levels: AudienceLevels, available: Bool) -> Int {
        guard available else { return reset() }
        let expired = levels.count == 0 ? reset() : 0
        latest = levels
        due = true
        return expired
    }

    public mutating func take(from floor: inout AudienceFloor, at time: Double) -> (levels: AudienceLevels, steps: Int, expired: Int)? {
        guard due || onsets.ready else { return nil }
        // An onset can arrive after an empty heartbeat, before the next level tick.
        if latest == nil || (latest?.count == 0 && onsets.ready) { latest = floor.drain(at: time) }
        guard let levels = latest else { return nil }
        due = false
        if levels.count == 0 { return (levels, 0, reset()) }
        return (levels, onsets.takeBatch(), 0)
    }
}

extension AudienceLevels {
    public func observation(steps: Int, source: String, session: String, sampleID: Int64,
                            at time: Double, token: String) throws -> Observation {
        guard (0...8).contains(steps), count > 0 || steps == 0 else { throw SensorError.invalid("Invalid floor gestures") }
        var values = ["audience-count": Double(count), "dance-effort": effort, "dance-steps": Double(steps),
                      "pulse-count": Double(pulseCount), "pulse-present": pulse == nil ? 0.0 : 1.0,
                      "motion-count": Double(motionCount)]
        values.merge(motion) { _, measured in measured }
        if let pulse { values["pulse-bpm"] = pulse }
        let gestures = (0..<steps).map { _ in Gesture(type: "dance-step") }
        return try Observation(deviceID: source, sessionID: session, sampleID: sampleID,
                               timeMS: time * 1000, values: values, gestures: gestures, pairingToken: token)
    }
}
