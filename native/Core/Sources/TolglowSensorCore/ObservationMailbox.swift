import Foundation

/// Pure bounded queue state, owned by one adapter actor. Continuous samples
/// coalesce; gesture pulses retain their order. A later gesture discards an older
/// pending continuous sample so sample IDs never run backwards at the receiver.
public struct ObservationMailbox: Sendable {
    private var latest: Observation?
    private var gestures: [Observation] = []
    public let gestureCapacity: Int
    public init(gestureCapacity: Int = 8) { self.gestureCapacity = max(1, min(32, gestureCapacity)) }
    public var count: Int { gestures.count + (latest == nil ? 0 : 1) }
    @discardableResult public mutating func offer(_ observation: Observation) throws -> Int {
        let dropped = latest == nil ? 0 : 1
        if observation.gestures.isEmpty { latest = observation }
        else {
            guard gestures.count < gestureCapacity else { throw SensorError.invalid("Gesture queue full") }
            latest = nil; gestures.append(observation)
        }
        return dropped
    }
    public mutating func take() -> Observation? {
        if !gestures.isEmpty { return gestures.removeFirst() }
        defer { latest = nil }
        return latest
    }
}
