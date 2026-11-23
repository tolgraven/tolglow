import Foundation

/// Tracks live WatchConnectivity samples only. A delayed sample cannot reopen an ended session.
public struct AudienceWatchReceipt {
    public private(set) var current: UUID?
    private var lastSerial: UInt32 = 0
    private var retired: [UUID] = []

    public init() {}

    private mutating func retire(_ id: UUID) {
        if !retired.contains(id) { retired.append(id) }
        if retired.count > 16 { retired.removeFirst() }
    }

    @discardableResult public mutating func end(session: UUID) -> Bool {
        retire(session)
        guard current == session else { return false }
        current = nil
        return true
    }

    public mutating func accept(session: UUID, sample: AudienceSample, sent: Double, now: Double) -> Bool {
        guard sent.isFinite, now.isFinite, (-0.5...2.0).contains(now - sent),
              !retired.contains(session) else { return false }
        if current == session {
            let advance = sample.serial &- lastSerial
            guard advance > 0 && advance < (1 << 31) else { return false }
        } else {
            if let current { retire(current) }
            current = session
        }
        lastSerial = sample.serial
        return true
    }
}

/// Heart sharing lasts only as long as fresh workout samples arrive. HealthKit
/// keeps read permission private, so a denied or unavailable sensor looks silent.
public struct AudiencePulse {
    private let started: Date?
    private var latest: (Int, Date)?

    public init(started: Date? = nil) { self.started = started }

    public mutating func withdraw() { self = AudiencePulse() }

    public mutating func observe(_ value: Double, measured: Date, now: Date) {
        guard let started, value.isFinite, (30...240).contains(value),
              measured >= started, (0...12).contains(now.timeIntervalSince(measured)),
              latest.map({ measured >= $0.1 }) ?? true else { return }
        latest = (Int(value.rounded()), measured)
    }

    public func bpm(at time: Date) -> Int? {
        latest.flatMap { (0...12).contains(time.timeIntervalSince($0.1)) ? $0.0 : nil }
    }

    public func interrupted(at time: Date) -> Bool {
        guard let started else { return false }
        return time.timeIntervalSince(latest?.1 ?? started) > 60
    }
}
