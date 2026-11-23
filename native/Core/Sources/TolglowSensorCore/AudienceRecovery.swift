import Foundation

/// One explicit selection. Core Bluetooth identifiers locate a candidate; the
/// fingerprint binds every subsequent handshake to the stage originally joined.
public struct AudienceRecovery {
    public struct Attempt: Equatable {
        public let peripheral: UUID
        public let number: Int
        public let begin: TimeInterval
        public let end: TimeInterval
        public let id: UUID
        public let name: String
        public var status: String { "Reconnecting to \(name) · \(number) of \(AudienceRecovery.delays.count)" }
    }
    private struct Stage {
        let peripheral: UUID
        let fingerprint: String
        let name: String
    }
    private static let delays = [0.5, 1.0, 2.0]
    private var stage: Stage?
    private var expiry: TimeInterval?
    public private(set) var attempt: Attempt?
    public init() {}

    public func accepts(peripheral: UUID, fingerprint: String, invitation: String?) -> Bool {
        if let invitation, fingerprint != invitation { return false }
        guard let stage else { return true }
        return stage.peripheral == peripheral && stage.fingerprint == fingerprint
    }

    public func keepsSession(for invitation: String?, currentInvitation: String?) -> Bool {
        guard let invitation else { return false }
        return invitation == currentInvitation || invitation == stage?.fingerprint
    }

    @discardableResult public mutating func joined(_ peripheral: UUID, fingerprint: String, name: String,
                                                  invitation: String?) -> Bool {
        guard accepts(peripheral: peripheral, fingerprint: fingerprint, invitation: invitation) else { return false }
        stage = Stage(peripheral: peripheral, fingerprint: fingerprint, name: name)
        attempt = nil
        expiry = nil
        return true
    }

    /// Time is supplied by the monotonic host clock. Each attempt has a six-second
    /// handshake deadline; foreground suspension cannot extend the overall window.
    public mutating func retry(at now: TimeInterval) -> Attempt? {
        guard now.isFinite, let stage else { return nil }
        let index = attempt?.number ?? 0
        let finish = expiry ?? (now + 24)
        guard Self.delays.indices.contains(index), now + Self.delays[index] < finish else {
            cancel()
            return nil
        }
        let begin = now + Self.delays[index]
        let next = Attempt(peripheral: stage.peripheral, number: index + 1, begin: begin,
                           end: min(finish, begin + 6), id: UUID(), name: stage.name)
        attempt = next
        expiry = finish
        return next
    }

    public func canStart(_ ticket: Attempt, at now: TimeInterval) -> Bool {
        ticket == attempt && now >= ticket.begin && now < ticket.end
    }

    public mutating func cancel() {
        stage = nil
        attempt = nil
        expiry = nil
    }
}
