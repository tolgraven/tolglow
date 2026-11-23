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
    var phoneFloorActive: (() -> Bool)?
    var phoneFloorChanged: ((Bool) -> Void)?

    func announcePhoneFloor() {
        guard let phoneFloorActive, active, WCSession.default.isReachable else { return }
        WCSession.default.sendMessage(["phone-audience-active": phoneFloorActive()], replyHandler: nil, errorHandler: nil)
    }

    func checkPhoneFloor(_ completion: @escaping (Bool) -> Void) {
        guard active, WCSession.default.isReachable else { return }
        WCSession.default.sendMessage(["phone-audience-query": true], replyHandler: { reply in
            guard let joined = reply["phone-audience-active"] as? Bool else { return }
            Task { @MainActor in completion(joined) }
        }, errorHandler: nil)
    }

    var audienceReceive: ((AudienceSample, Double?, TimeInterval) -> Bool)?
    var audienceEnded: (() -> Void)?
    private var audienceSession: UUID?
    private var audienceSending = false
    private var audienceDeadline: Task<Void, Never>?
    private var audienceReceiver = AudienceWatchReceipt()
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
    func beginAudience() {
        endAudience()
        audienceSession = UUID()
    }

    func sendAudience(_ sample: AudienceSample, motionAge: Double) {
        guard let id = audienceSession else { return }
        guard active, WCSession.default.isReachable else { fail("Open your joined audience session on iPhone"); return }
        // The next 200 ms tick carries the newest effort and the cumulative steps.
        guard !audienceSending else { return }
        audienceSending = true
        audienceDeadline = Task { [weak self] in
            do { try await Task.sleep(for: .seconds(2)) } catch { return }
            guard let self, self.audienceSession == id, self.audienceSending else { return }
            self.fail("Audience relay timed out")
        }
        var message: [String: Any] = ["audience-session": id.uuidString,
                                      "sample": sample.bytes, "sent": Date().timeIntervalSince1970]
        if sample.motion != nil { message["motion-age"] = motionAge }
        WCSession.default.sendMessage(message, replyHandler: { [weak self] reply in
            let accepted = reply["received"] as? Bool == true
            Task { @MainActor in
                guard let self, self.audienceSession == id else { return }
                self.audienceDeadline?.cancel()
                self.audienceSending = false
                if !accepted { self.fail("Join the audience on iPhone first") }
            }
        }, errorHandler: { [weak self] _ in
            Task { @MainActor in
                guard let self, self.audienceSession == id else { return }
                self.fail("Audience relay disconnected")
            }
        })
    }

    func endAudience() {
        let session = audienceSession
        audienceSession = nil
        audienceDeadline?.cancel()
        audienceDeadline = nil
        audienceSending = false
        if let session, active, WCSession.default.isReachable {
            WCSession.default.sendMessage(["audience-end": session.uuidString], replyHandler: nil, errorHandler: nil)
        }
    }

    func cancelPending() {
        endAudience()
        generation = UUID(); deadline?.cancel(); deadline = nil
        mailbox = ObservationMailbox(); sending = false
    }
    func stop() {
        cancelPending()
        active = false
        reachable = false
        audienceReceiver = AudienceWatchReceipt()
        audienceEnded?()
        status = "Relay stopped"
    }
    private func fail(_ message: String) { cancelPending(); status = message; onFailure?() }

    nonisolated func session(_ session: WCSession, activationDidCompleteWith activationState: WCSessionActivationState, error: Error?) {
        Task { @MainActor in
            guard self.active else { return }
            self.reachable = session.isReachable
            if session.isReachable { self.announcePhoneFloor() } else { self.phoneFloorChanged?(false) }
            self.status = error?.localizedDescription ?? (activationState == .activated ? "Relay activated" : "Relay inactive")
        }
    }
    nonisolated func sessionReachabilityDidChange(_ session: WCSession) {
        Task { @MainActor in
            guard self.active else { return }
            self.reachable = session.isReachable
            if session.isReachable { self.announcePhoneFloor() } else { self.phoneFloorChanged?(false) }
            if !session.isReachable { self.fail("Counterpart unreachable; disarmed") }
        }
    }
    nonisolated func session(_ session: WCSession, didReceiveMessage message: [String: Any], replyHandler: @escaping ([String: Any]) -> Void) {
        let askingAboutFloor = message["phone-audience-query"] as? Bool == true
        let device = message["device-id"] as? String
        let audienceID = (message["audience-session"] as? String).flatMap(UUID.init(uuidString:))
        let sample = (message["sample"] as? Data).flatMap(AudienceSample.init(bytes:))
        let sent = message["sent"] as? Double
        Task { @MainActor in
            if self.active, askingAboutFloor, let active = self.phoneFloorActive {
                replyHandler(["phone-audience-active": active()])
            } else if self.active, let audienceID, let sample, let sent {
                let previous = self.audienceReceiver.current
                let received = Date().timeIntervalSince1970
                let accepted = self.audienceReceiver.accept(session: audienceID, sample: sample, sent: sent, now: received)
                let measured = AudienceMotion.wristTimestamp(ageAtSend: message["motion-age"] as? Double,
                                                             sent: sent, received: received,
                                                             uptime: ProcessInfo.processInfo.systemUptime)
                if accepted, let previous, previous != audienceID { self.audienceEnded?() }
                replyHandler(["received": accepted && self.audienceReceive?(sample, measured, sent) == true])
            } else if self.active, let device, !device.isEmpty, device.utf8.count <= 128 {
                self.discoveredDeviceID = device
                replyHandler(["received": true])
            } else { replyHandler(["received": false]) }
        }
    }
    nonisolated func session(_ session: WCSession, didReceiveMessage message: [String: Any]) {
        let phoneJoined = message["phone-audience-active"] as? Bool
        let id = (message["audience-end"] as? String).flatMap(UUID.init(uuidString:))
        Task { @MainActor in
            guard self.active else { return }
            if let phoneJoined { self.phoneFloorChanged?(phoneJoined) }
            if let id, self.audienceReceiver.end(session: id) { self.audienceEnded?() }
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
