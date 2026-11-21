import Combine
import Foundation
import TolglowSensorCore
import WatchConnectivity

/// WCSession is the supported Watch→phone transport. Normal watchOS apps cannot
/// create low-level UDP sockets (TN3135). This adapter never opens one on Watch.
@MainActor final class WatchRelay: NSObject, ObservableObject, WCSessionDelegate {
    @Published private(set) var status = "Watch relay inactive"
    @Published private(set) var reachable = false
    @Published private(set) var discoveredDeviceID = ""
    var receive: ((Observation) -> Bool)?
    var onFailure: (() -> Void)?
    private var active = false
    private var sending = false
    private var generation = UUID()
    private var mailbox = ObservationMailbox()
    private var deadline: Task<Void, Never>?

    func activate() {
        guard !active else { return }
        guard WCSession.isSupported() else { status = "WatchConnectivity unavailable"; return }
        active = true
        WCSession.default.delegate = self
        WCSession.default.activate()
    }
    func announce(deviceID: String) {
        guard active, WCSession.default.activationState == .activated, WCSession.default.isReachable else {
            status = "Open Tolglow on the paired iPhone"; return
        }
        let token = generation
        WCSession.default.sendMessage(["device-id": deviceID], replyHandler: { [weak self] _ in
            Task { @MainActor in
                guard let self, self.active, self.generation == token else { return }
                self.status = "Device ID shown on iPhone; enroll it with the host"
            }
        }, errorHandler: { [weak self] _ in
            Task { @MainActor in
                guard let self, self.active, self.generation == token else { return }
                self.fail("Pairing announcement failed")
            }
        })
    }
    func send(_ observation: Observation) {
        guard active, WCSession.default.activationState == .activated, WCSession.default.isReachable else {
            fail("iPhone unreachable; disarmed"); return
        }
        do { try mailbox.offer(observation) }
        catch { fail("Gesture queue full; disarmed"); return }
        flush()
    }
    private func flush() {
        guard active, !sending else { return }
        guard let item = mailbox.take() else { return }
        do {
            let data = try item.json()
            let token = generation
            sending = true
            deadline = Task { [weak self] in
                do { try await Task.sleep(for: .seconds(2)) } catch { return }
                guard let self, self.generation == token, self.sending else { return }
                self.fail("Relay acknowledgement timed out; no retry")
            }
            WCSession.default.sendMessageData(data, replyHandler: { [weak self] response in
                Task { @MainActor in
                    guard let self, self.generation == token else { return }
                    self.deadline?.cancel(); self.deadline = nil; self.sending = false
                    guard response == Data([1]) else { self.fail("iPhone relay is not armed for this device"); return }
                    self.status = "iPhone accepted sample; UDP/host delivery not acknowledged"
                    self.flush()
                }
            }, errorHandler: { [weak self] _ in
                Task { @MainActor in
                    guard let self, self.generation == token else { return }
                    self.fail("Watch relay failed; no retry")
                }
            })
        } catch { fail("Invalid Watch observation") }
    }
    func cancelPending() {
        generation = UUID(); deadline?.cancel(); deadline = nil
        mailbox = ObservationMailbox(); sending = false
    }
    func stop() { cancelPending(); active = false; reachable = false; status = "Relay stopped" }
    private func fail(_ message: String) { cancelPending(); status = message; onFailure?() }

    nonisolated func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error: Error?) {
        Task { @MainActor in
            guard self.active else { return }
            self.reachable = session.isReachable
            self.status = error?.localizedDescription ?? (activationState == .activated ? "Relay activated" : "Relay inactive")
        }
    }
    nonisolated func sessionReachabilityDidChange(_ session: WCSession) {
        Task { @MainActor in
            guard self.active else { return }
            self.reachable = session.isReachable
            if !session.isReachable { self.fail("Counterpart unreachable; disarmed") }
        }
    }
    nonisolated func session(_ session: WCSession, didReceiveMessage message: [String: Any], replyHandler: @escaping ([String: Any]) -> Void) {
        let device = message["device-id"] as? String
        Task { @MainActor in
            if self.active, let device, !device.isEmpty, device.utf8.count <= 128 {
                self.discoveredDeviceID = device
                replyHandler(["received": true])
            } else { replyHandler(["received": false]) }
        }
    }
    nonisolated func session(_ session: WCSession, didReceiveMessageData messageData: Data, replyHandler: @escaping (Data) -> Void) {
        Task { @MainActor in
            guard self.active, let observation = try? Observation.decode(messageData), observation.pairingToken == nil else {
                replyHandler(Data([0])); return
            }
            replyHandler(Data([self.receive?(observation) == true ? 1 : 0]))
        }
    }
    #if os(iOS)
    nonisolated func sessionDidBecomeInactive(_ session: WCSession) {
        Task { @MainActor in self.stop(); self.onFailure?() }
    }
    nonisolated func sessionDidDeactivate(_ session: WCSession) {
        Task { @MainActor in
            self.stop(); self.onFailure?()
            // A newly paired Watch requires explicit activation/enrollment again.
        }
    }
    #endif
}
