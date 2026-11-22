import Combine
import Foundation
import Network
import TolglowSensorCore

/// Explicit iPhone UDP boundary. One in-flight send plus one latest sample.
/// Gesture samples have a separate bounded queue and are never silently replaced.
@MainActor final class OSCConnection: ObservableObject {
    @Published private(set) var ready = false
    @Published private(set) var status = "OSC disconnected"
    @Published private(set) var sent = 0
    @Published private(set) var dropped = 0
    var onFailure: (() -> Void)?
    private(set) var destination: PairingDestination?
    private var connection: NWConnection?
    private var generation = UUID()
    private var busy = false
    private var mailbox = ObservationMailbox()
    private var deadline: Task<Void, Never>?
    private let queue = DispatchQueue(label: "tolglow.osc.send")

    func connect(host: String, port: UInt16) {
        close()
        guard let audience = PairingDestination(host: host, port: port), let endpoint = NWEndpoint.Port(rawValue: port) else {
            status = "Choose host and UDP port"; return
        }
        let token = generation
        let owned = NWConnection(host: NWEndpoint.Host(audience.host), port: endpoint, using: .udp)
        destination = audience
        connection = owned
        owned.stateUpdateHandler = { [weak self] state in
            Task { @MainActor in
                guard let self, self.generation == token else { return }
                switch state {
                case .ready: self.deadline?.cancel(); self.deadline = nil; self.ready = true; self.status = "UDP ready — delivery is not acknowledged"; self.flush()
                case .failed(let error): self.fail(error.localizedDescription)
                case .waiting(let error): self.fail(error.localizedDescription)
                case .cancelled: self.ready = false
                default: break
                }
            }
        }
        owned.start(queue: queue)
        deadline = Task { [weak self] in
            do { try await Task.sleep(for: .seconds(5)) } catch { return }
            guard let self, self.generation == token, !self.ready else { return }
            self.fail("UDP connection timed out; disarmed")
        }
    }
    func send(_ observation: Observation, credential: SensorCredential) {
        guard ready else { dropped += 1; return }
        guard credential.destination == destination, credential.device == observation.deviceID else {
            fail("Pair this device with the current OSC destination"); return
        }
        do {
            let authenticated = try observation.authenticated(with: credential.token)
            _ = try SensorOSC.encode(authenticated) // validate before admission
            dropped += try mailbox.offer(authenticated)
            flush()
        } catch { fail("Observation or credential rejected") }
    }
    private func flush() {
        guard ready, !busy, let connection else { return }
        guard let item = mailbox.take() else { return }
        guard let packet = try? SensorOSC.encode(item) else { fail("Invalid queued observation"); return }
        busy = true
        let token = generation
        deadline = Task { [weak self] in
            do { try await Task.sleep(for: .seconds(2)) } catch { return }
            guard let self, self.generation == token, self.busy else { return }
            self.fail("UDP send timed out; no retry")
        }
        connection.send(content: packet, completion: .contentProcessed { [weak self] error in
            Task { @MainActor in
                guard let self, self.generation == token else { return }
                self.deadline?.cancel(); self.deadline = nil; self.busy = false
                if let error { self.fail(error.localizedDescription) }
                else { self.sent += 1; self.flush() }
            }
        })
    }
    private func fail(_ message: String) { close(); status = message; onFailure?() }
    func discardPending() { mailbox = ObservationMailbox() }
    func close() {
        generation = UUID(); deadline?.cancel(); deadline = nil; connection?.cancel(); connection = nil
        destination = nil; ready = false; busy = false; mailbox = ObservationMailbox(); status = "OSC disconnected"
    }
}
