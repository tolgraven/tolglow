import Combine
import CoreBluetooth
import CryptoKit
import TolglowSensorCore

/// One BLE connection with one acknowledged write in flight and one replaceable sample.
final class AudienceClient: NSObject, ObservableObject, CBCentralManagerDelegate, CBPeripheralDelegate {
    struct Venue: Identifiable { let id: UUID; let name: String }
    @Published private(set) var venues: [Venue] = []
    @Published private(set) var status = "Find the stage nearby"
    @Published private(set) var joined = false
    @Published private(set) var looking = false
    @Published private(set) var connecting = false
    @Published private(set) var reconnecting = false
    @Published private(set) var invalidInvitation = false
    @Published private(set) var room: String?
    var onSuspend: (() -> Void)?
    var onJoin: (() -> Void)?
    var onEnd: (() -> Void)?
    private var manager: CBCentralManager?
    private var devices: [UUID: CBPeripheral] = [:]
    private var peer: CBPeripheral?
    private var identity: CBCharacteristic?
    private var hello: CBCharacteristic?
    private var samples: CBCharacteristic?
    private var recovery = AudienceRecovery()
    private var pendingFingerprint: String?
    private var retryTimer: DispatchWorkItem?
    private var inForeground = true
    private var now: TimeInterval { ProcessInfo.processInfo.systemUptime }
    private var key: SymmetricKey?
    private var queued: AudienceSample?
    private var pending = false
    private var timeout: DispatchWorkItem?

    func discover(room: String?) {
        guard !invalidInvitation, self.room == nil || self.room == room else { return }
        leave(); self.room = room
        looking = true; status = "Looking for stages"
        if let manager { centralManagerDidUpdateState(manager) }
        else { manager = CBCentralManager(delegate: self, queue: .main) }
    }

    func receiveInvitation(_ fingerprint: String?) {
        if !recovery.keepsSession(for: fingerprint, currentInvitation: room) {
            end(fingerprint == nil ? "Invalid stage invitation" : "Find the invited stage")
        }
        room = fingerprint
        invalidInvitation = fingerprint == nil
    }

    func browseWithoutInvitation() {
        room = nil
        invalidInvitation = false
        discover(room: nil)
    }

    func setForeground(_ active: Bool) {
        inForeground = active
        if !active && reconnecting { leave() }
    }

    func centralManagerDidUpdateState(_ central: CBCentralManager) {
        guard central.state == .poweredOn else {
            if central.state == .resetting && (connecting || joined || reconnecting) {
                leave(); status = "Bluetooth restarted. Find your stage again."
            } else if central.state != .unknown && central.state != .resetting {
                leave(); status = "Allow Bluetooth and turn it on to join"
            }
            return
        }
        if looking { central.scanForPeripherals(withServices: [AudienceRadio.service]) }
    }

    func centralManager(_ central: CBCentralManager, didDiscover peripheral: CBPeripheral,
                        advertisementData: [String: Any], rssi RSSI: NSNumber) {
        guard looking, devices[peripheral.identifier] == nil, devices.count < 64 else { return }
        devices[peripheral.identifier] = peripheral
        let name = advertisementData[CBAdvertisementDataLocalNameKey] as? String ?? peripheral.name ?? "Nearby stage"
        venues.append(Venue(id: peripheral.identifier, name: name))
    }

    func stopBrowsing() {
        if manager?.isScanning == true { manager?.stopScan() }
        looking = false
    }

    @discardableResult func join(_ id: UUID) -> Bool {
        guard !invalidInvitation, inForeground, manager?.state == .poweredOn, !connecting, !joined, !reconnecting, let device = devices[id] else { return false }
        stopBrowsing()
        status = "Connecting to stage"
        connect(device)
        return true
    }

    private func connect(_ device: CBPeripheral) {
        connecting = true
        peer = device
        device.delegate = self
        manager?.connect(device)
        deadline()
    }

    private func deadline() {
        timeout?.cancel()
        let timer = DispatchWorkItem { [weak self] in self?.interrupted("Stage timed out") }
        timeout = timer
        let delay = recovery.attempt.map { max(0, $0.end - now) } ?? 8
        DispatchQueue.main.asyncAfter(deadline: .now() + delay, execute: timer)
    }

    func centralManager(_ central: CBCentralManager, didConnect peripheral: CBPeripheral) {
        if peer === peripheral, peripheral.state == .connected { peripheral.discoverServices([AudienceRadio.service]) }
    }

    func peripheral(_ peripheral: CBPeripheral, didDiscoverServices error: Error?) {
        guard peer === peripheral else { return }
        guard check(error, "Stage service unavailable") else { return }
        guard let service = peripheral.services?.first(where: { $0.uuid == AudienceRadio.service })
        else { failed("Stage service unavailable"); return }
        peripheral.discoverCharacteristics([AudienceRadio.identity, AudienceRadio.hello, AudienceRadio.samples], for: service)
    }

    func peripheral(_ peripheral: CBPeripheral, didDiscoverCharacteristicsFor service: CBService, error: Error?) {
        guard peer === peripheral else { return }
        guard check(error, "Stage characteristics unavailable") else { return }
        let list = service.characteristics ?? []
        identity = list.first { $0.uuid == AudienceRadio.identity }
        hello = list.first { $0.uuid == AudienceRadio.hello }
        samples = list.first { $0.uuid == AudienceRadio.samples }
        guard let identity, hello != nil, samples != nil else { failed("Unsupported stage"); return }
        peripheral.readValue(for: identity)
    }

    func peripheral(_ peripheral: CBPeripheral, didUpdateValueFor characteristic: CBCharacteristic, error: Error?) {
        guard peer === peripheral, characteristic.uuid == AudienceRadio.identity else { return }
        guard check(error, "Invalid stage identity") else { return }
        guard let identity = characteristic.value, identity.count == 81, let hello else { failed("Invalid stage identity"); return }
        let publicKey = Data(identity.prefix(65))
        let fingerprint = AudienceRadio.fingerprint(publicKey)
        guard recovery.accepts(peripheral: peripheral.identifier, fingerprint: fingerprint, invitation: room)
        else { failed("Stage identity changed. Check the invitation before rejoining."); return }
        pendingFingerprint = fingerprint
        do {
            let local = P256.KeyAgreement.PrivateKey()
            key = try AudienceRadio.key(local, publicKey, challenge: Data(identity.suffix(16)))
            peripheral.writeValue(local.publicKey.x963Representation, for: hello, type: .withResponse)
        } catch { failed("Invalid stage identity") }
    }

    func peripheral(_ peripheral: CBPeripheral, didWriteValueFor characteristic: CBCharacteristic, error: Error?) {
        guard peer === peripheral, characteristic === hello || characteristic === samples else { return }
        guard check(error, "Stage declined the connection or sample") else { return }
        timeout?.cancel()
        if characteristic === hello {
            guard connecting else { return }
            if let ticket = recovery.attempt, now >= ticket.end { interrupted("Stage timed out"); return }
            let id = peripheral.identifier
            let name = venues.first(where: { $0.id == id })?.name ?? "stage"
            guard let fingerprint = pendingFingerprint,
                  recovery.joined(id, fingerprint: fingerprint, name: name, invitation: room) else {
                failed("Stage identity changed. Check the invitation before rejoining.")
                return
            }
            reconnecting = false
            connecting = false; joined = true; status = "Sharing with this stage"
            // Starting input may synchronously write its first sample. Only a
            // sample acknowledgement can release that write's in-flight slot.
            onJoin?()
            return
        }
        pending = false
        flush()
    }

    func offer(_ sample: AudienceSample) {
        guard joined else { return }
        queued = sample
        flush()
    }

    private func flush() {
        guard !pending, let sample = queued, let key, let peer, let samples else { return }
        do {
            guard let packet = try AES.GCM.seal(sample.bytes, using: key).combined,
                  packet.count <= peer.maximumWriteValueLength(for: .withResponse)
            else { failed("Stage link has insufficient capacity"); return }
            queued = nil; pending = true
            peer.writeValue(packet, for: samples, type: .withResponse)
            deadline()
        } catch { failed("Failed to send sample") }
    }

    private func closeTransport() {
        timeout?.cancel(); timeout = nil
        retryTimer?.cancel(); retryTimer = nil
        let old = peer
        peer = nil; joined = false; connecting = false
        old?.delegate = nil
        if let old, old.state != .disconnected { manager?.cancelPeripheralConnection(old) }
        key = nil; hello = nil; samples = nil; identity = nil; queued = nil; pending = false
        pendingFingerprint = nil
    }

    func leave() { end("Disconnected") }

    private func end(_ message: String) {
        recovery.cancel()
        reconnecting = false
        closeTransport()
        stopBrowsing()
        venues.removeAll(); devices.removeAll()
        status = message
        onEnd?()
    }

    private func interrupted(_ message: String) {
        guard inForeground, manager?.state == .poweredOn, let ticket = recovery.retry(at: now) else {
            failed(message + ". Choose your stage to join again.")
            return
        }
        let wasJoined = joined
        reconnecting = true
        closeTransport()
        if wasJoined { onSuspend?() }
        status = ticket.status
        schedule(ticket, after: max(0, ticket.begin - now))
    }

    private func schedule(_ ticket: AudienceRecovery.Attempt, after delay: TimeInterval) {
        let timer = DispatchWorkItem { [weak self] in self?.retry(ticket) }
        retryTimer = timer
        DispatchQueue.main.asyncAfter(deadline: .now() + delay, execute: timer)
    }

    private func retry(_ ticket: AudienceRecovery.Attempt) {
        guard inForeground, recovery.attempt == ticket else { return }
        guard let device = devices[ticket.peripheral], manager?.state == .poweredOn else { leave(); return }
        let time = now
        guard time < ticket.end else { interrupted("Stage unavailable"); return }
        // A cancelled CBPeripheral may still deliver its disconnect callback.
        // Start a fresh handshake only after the previous connection has closed.
        guard recovery.canStart(ticket, at: time), device.state == .disconnected else {
            schedule(ticket, after: min(0.2, ticket.end - time))
            return
        }
        connect(device)
    }

    private func failed(_ text: String) { end(text) }

    private func check(_ error: Error?, _ message: String) -> Bool {
        guard let error else { return true }
        let e = error as NSError
        let lost: [CBError.Code] = [.notConnected, .connectionTimeout, .peripheralDisconnected, .connectionFailed]
        if e.domain == CBErrorDomain && lost.contains(where: { $0.rawValue == e.code }) { interrupted(message) }
        else { failed(message) }
        return false
    }

    func centralManager(_ central: CBCentralManager, didFailToConnect peripheral: CBPeripheral, error: Error?) {
        if peer === peripheral, peripheral.state == .disconnected { interrupted("Could not connect to stage") }
    }
    func centralManager(_ central: CBCentralManager, didDisconnectPeripheral peripheral: CBPeripheral, error: Error?) {
        if peer === peripheral, peripheral.state == .disconnected { interrupted("Stage disconnected") }
    }
}

#if DEBUG && targetEnvironment(simulator) && os(watchOS)
extension AudienceClient {
    /// Seeds the Watch UI test with a simulated transport interruption. The
    /// real recovery policy, status view and user cancellation remain in use.
    func previewInterruptedStage() -> Venue {
        let venue = Venue(id: UUID(), name: "West floor")
        recovery.joined(venue.id, fingerprint: "simulator-fixture", name: venue.name, invitation: nil)
        if let attempt = recovery.retry(at: now) {
            reconnecting = true
            status = attempt.status
        }
        return venue
    }
}
#endif
