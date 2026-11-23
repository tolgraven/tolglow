import Foundation

/// A cumulative step counter survives coalescing and packet loss. Pulse is optional.
public struct AudienceSample: Equatable {
    public let serial: UInt32
    public let steps: UInt32
    public let effort: Double
    public let pulse: Int?

    public init?(serial: UInt32, steps: UInt32, effort: Double, pulse: Int? = nil) {
        guard effort.isFinite, (0...1).contains(effort),
              pulse.map({ (30...240).contains($0) }) ?? true else { return nil }
        self.serial = serial
        self.steps = steps
        self.effort = effort
        self.pulse = pulse
    }

    public var bytes: Data {
        var bytes: [UInt8] = [1]
        for word in [serial, steps] {
            for shift in [24, 16, 8, 0] { bytes.append(UInt8(truncatingIfNeeded: word >> shift)) }
        }
        bytes.append(UInt8((effort * 255).rounded()))
        bytes.append(UInt8(pulse ?? 0))
        return Data(bytes)
    }

    public init?(bytes: Data) {
        let b = Array(bytes)
        guard b.count == 11, b[0] == 1 else { return nil }
        func word(_ start: Int) -> UInt32 {
            b[start..<(start + 4)].reduce(0) { ($0 << 8) | UInt32($1) }
        }
        self.init(serial: word(1), steps: word(5), effort: Double(b[9]) / 255,
                  pulse: b[10] == 0 ? nil : Int(b[10]))
    }
}

/// A motion peak is a candidate dance step. A quiet interval re-arms the detector.
public struct DanceMeter {
    public private(set) var effort = 0.0
    public private(set) var steps: UInt32 = 0
    private var previous: Double?
    private var lastStep = -Double.infinity
    private var ready = true
    public init() {}

    public mutating func observe(strength: Double, at time: Double) {
        guard strength.isFinite, strength >= 0, time.isFinite,
              previous.map({time > $0}) ?? true else { return }
        let elapsed = time - (previous ?? (time - 0.05))
        if elapsed > 1 { effort = 0; ready = true }
        previous = time
        let level = min(1, strength)
        let retention = exp(-elapsed / (level > effort ? 0.12 : 0.6))
        effort = retention * effort + (1 - retention) * level
        if level < 0.22 { ready = true }
        if ready, level > 0.58, time - lastStep >= 0.25 {
            steps &+= 1
            ready = false
            lastStep = time
        }
    }
}

public struct AudienceLevels {
    public let count: Int
    public let effort: Double
    public let pulse: Double?
    public let steps: Int
}

/// The host owns receipt time. Each live phone contributes a single vote.
public struct AudienceFloor {
    private struct Guest { var sample: AudienceSample; var time: Double; var stepCredit: Double }
    private var guests: [UUID: Guest] = [:]
    private var pendingSteps = 0
    public let capacity: Int
    public let timeout: Double

    public init(capacity: Int = 128, timeout: Double = 2.5) {
        self.capacity = max(1, capacity)
        self.timeout = timeout.isFinite ? max(0.1, timeout) : 2.5
    }

    public mutating func leave(_ guest: UUID) { guests.removeValue(forKey: guest) }

    public mutating func expire(at time: Double) {
        guests = guests.filter { time - $0.value.time < timeout }
    }

    @discardableResult public mutating func receive(_ sample: AudienceSample, from id: UUID, at time: Double) -> Bool {
        guard time.isFinite else { return false }
        var credit = 2.0
        if let previous = guests[id], time - previous.time < timeout {
            let sequence = sample.serial &- previous.sample.serial
            let added = sample.steps &- previous.sample.steps
            credit = min(2.0 + 4 * timeout, previous.stepCredit + max(0, time - previous.time) * 4)
            guard time >= previous.time, sequence > 0, sequence < (1 << 31),
                  Double(added) <= credit else { return false }
            credit = min(2.0, credit - Double(added))
            pendingSteps += Int(added)
        } else {
            expire(at: time)
            guard guests.count < capacity else { return false }
        }
        guests[id] = Guest(sample: sample, time: time, stepCredit: credit)
        return true
    }

    public mutating func drain(at time: Double) -> AudienceLevels {
        expire(at: time)
        let pulses = guests.values.compactMap { $0.sample.pulse }
        let levels = AudienceLevels(count: guests.count,
            effort: guests.values.reduce(0) { $0 + $1.sample.effort } / Double(max(1, guests.count)),
            pulse: pulses.isEmpty ? nil : Double(pulses.reduce(0,+)) / Double(pulses.count),
            steps: pendingSteps)
        pendingSteps = 0
        return levels
    }
}

/// Gesture counts are never silently truncated. A failed append leaves the queue intact.
public struct AudienceSteps {
    public private(set) var pending = 0
    public let limit: Int
    public init(limit: Int = 2048) { self.limit = max(1, limit) }
    public mutating func append(_ count: Int) throws {
        guard count >= 0, count <= limit - pending else { throw SensorError.invalid("Audience gesture queue overflow") }
        pending += count
    }
    public mutating func takeBatch() -> Int {
        let batch = min(8, pending)
        pending -= batch
        return batch
    }
}
