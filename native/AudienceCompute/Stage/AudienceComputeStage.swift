import CryptoKit
import Foundation
import Network

/// Confine this owner and BLE callbacks to the main queue. Only the stage knows
/// the localhost compute credential. Guest admission starts after an authenticated
/// BLE grant, three timed round trips, and an actual guest GPU readback.
final class AudienceComputeStage {
    private struct Lease { let grant: ComputeGrant; let guest: UUID; let deadline: TimeInterval }
    private final class Peer {
        let link: NWConnection
        var guest: UUID?
        var upstream: ComputeUpstream?
        var until: Date?
        var last = ProcessInfo.processInfo.systemUptime
        init(_ connection: NWConnection) { link = connection }
    }

    private let room: String
    private let poolPort: UInt16
    private let token: String
    private let operatorToken: String?
    private let listener: NWListener
    private var pending = [UUID: Lease]()
    private var peers = [UUID: Peer]()
    private var timer: Timer?
    private var listening = false
    var measurement: ((_ medianMs: Double, _ returnBytesPerSecond: Double, _ maxResultFloats: Int) -> Void)?

    init(room: String, poolPort: UInt16, token: String, operatorToken: String? = nil) throws {
        self.room = room; self.poolPort = poolPort; self.token = token; self.operatorToken = operatorToken
        listener = try NWListener(using: ComputeLink.parameters())
        listener.service = NWListener.Service(name: "Tolglow-" + String(room.prefix(32)), type: ComputeGrant.service)
        listener.newConnectionHandler = { [weak self] c in self?.accept(c) }
        listener.stateUpdateHandler = { [weak self] state in
            switch state {
            case .ready: self?.listening = true
            case .failed, .cancelled: self?.stop()
            default: break
            }
        }
    }

    func start() {
        listener.start(queue: .main)
        timer = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) { [weak self] _ in self?.sweep() }
    }

    func stop() {
        listening = false
        timer?.invalidate(); timer = nil
        listener.cancel()
        pending.removeAll()
        Array(peers.keys).forEach(close)
    }

    private func sweep() {
        let now = ProcessInfo.processInfo.systemUptime
        pending = pending.filter { $0.value.deadline > now }
        for (id, p) in Array(peers) {
            if (p.until ?? .distantFuture) <= Date() || now - p.last > (p.upstream == nil ? 8 : 45) { close(id) }
        }
    }

    func revoke(participant: UUID) {
        pending = pending.filter { $0.value.guest != participant }
        for (id, p) in Array(peers) where p.guest == participant { close(id) }
    }

    /// Deliver only within the already authenticated, participant-specific BLE channel.
    func issueGrant(for participant: UUID) -> Data? {
        sweep()
        revoke(participant: participant)
        guard listening, pending.count + peers.values.filter({ $0.guest != nil }).count < 4 else { return nil }
        let key = SymmetricKey(size: .bits256).withUnsafeBytes { Data($0) }
        let grant = ComputeGrant(version: 1, id: UUID(), room: room, key: key,
                                 expires: Date().addingTimeInterval(900))
        pending[grant.id] = Lease(grant: grant, guest: participant, deadline: ProcessInfo.processInfo.systemUptime + 20)
        return try? JSONEncoder().encode(grant)
    }

    private func accept(_ link: NWConnection) {
        guard listening, peers.count < 8 else { link.cancel(); return }
        let id = UUID(), peer = Peer(link)
        peers[id] = peer
        link.stateUpdateHandler = { [weak self] state in
            switch state {
            case .ready: self?.authenticate(id)
            case .failed, .cancelled: self?.close(id)
            default: break
            }
        }
        link.start(queue: .main)
    }

    private func close(_ id: UUID) {
        guard let p = peers.removeValue(forKey: id) else { return }
        p.link.stateUpdateHandler = nil; p.link.cancel(); p.upstream?.close()
    }

    private func authenticate(_ id: UUID) {
        guard let p = peers[id] else { return }
        ComputeLink.read(p.link, limit: 160) { [weak self] data in
            guard let self, self.peers[id] === p else { return }
            guard let data, data.count > 36,
                  let key = UUID(uuidString: String(decoding: data.prefix(36), as: UTF8.self)),
                  let lease = self.pending[key], lease.deadline > ProcessInfo.processInfo.systemUptime,
                  lease.grant.valid else { self.close(id); return }
            let read = ComputeRecords(lease.grant, direction: "to-stage")
            guard (try? read.open(Data(data.dropFirst(36)))) == Data("GPU-ready".utf8)
            else { self.close(id); return }
            self.pending.removeValue(forKey: key)
            p.guest = lease.guest; p.until = lease.grant.expires
            self.probe(id, read, ComputeRecords(lease.grant, direction: "to-phone"), [])
        }
    }

    private func probe(_ id: UUID, _ read: ComputeRecords, _ write: ComputeRecords, _ samples: [Double]) {
        guard let p = peers[id] else { return }
        if samples.count == 3 {
            let seconds = samples.sorted()[1]
            let rate = Double(ComputeLink.probeSize) / max(seconds, 0.000001)
            let capacity = min(8192, Int(min(32768, rate * 0.006)) / 4)
            measurement?(seconds * 1000, rate, capacity)
            guard seconds <= 0.030, capacity >= 256 else { close(id); return }
            attach(id, read, write, capacity)
            return
        }
        let challenge = Data([0]) + SymmetricKey(size: .bits256).withUnsafeBytes { Data($0) }
            + Data(repeating: 0, count: ComputeLink.probeSize - 33)
        guard let record = try? write.seal(challenge) else { close(id); return }
        let start = ProcessInfo.processInfo.systemUptime
        ComputeLink.send(record, on: p.link) { [weak self] sent in
            guard let self, self.peers[id] === p else { return }
            guard sent else { self.close(id); return }
            ComputeLink.read(p.link, limit: ComputeLink.probeSize + 16) { [weak self] data in
                guard let self, self.peers[id] === p else { return }
                guard let data, let answer = try? read.open(data), answer == challenge
                else { self.close(id); return }
                p.last = ProcessInfo.processInfo.systemUptime
                self.probe(id, read, write, samples + [p.last - start])
            }
        }
    }

    private func attach(_ id: UUID, _ read: ComputeRecords, _ write: ComputeRecords, _ capacity: Int) {
        guard let p = peers[id] else { return }
        do {
            let service = try ComputeUpstream(port: poolPort, token: token, operatorToken: operatorToken)
            p.upstream = service
            service.ended = { [weak self] in self?.close(id) }
            service.request = { [weak self, weak service] body in
                guard let self, self.peers[id] === p else { return }
                guard let record = try? write.seal(Data([1]) + body) else { self.close(id); return }
                ComputeLink.send(record, on: p.link) { [weak self] ok in
                    guard let self, self.peers[id] === p else { return }
                    guard ok else { self.close(id); return }
                    ComputeLink.read(p.link, limit: 65536 + 16) { [weak self] data in
                        guard let self, self.peers[id] === p else { return }
                        guard let data, let answer = try? read.open(data) else { self.close(id); return }
                        do { try service?.reply(answer); p.last = ProcessInfo.processInfo.systemUptime }
                        catch { self.close(id) }
                    }
                }
            }
            guard let guest = p.guest else { close(id); return }
            try service.start(floats: capacity, subject: room + "/" + guest.uuidString)
        } catch { close(id) }
    }
}
