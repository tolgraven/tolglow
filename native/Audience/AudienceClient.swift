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
    var onEnd: (() -> Void)?
    private var manager: CBCentralManager?
    private var devices: [UUID: CBPeripheral] = [:]
    private var peer: CBPeripheral?
    private var identity: CBCharacteristic?
    private var hello: CBCharacteristic?
    private var samples: CBCharacteristic?
    private var room: String?
    private var key: SymmetricKey?
    private var queued: AudienceSample?
    private var pending = false
    private var timeout: DispatchWorkItem?

    func discover(room: String?) {
        leave(); self.room = room
        looking = true; status = "Looking for stages"
        if let manager { centralManagerDidUpdateState(manager) }
        else { manager = CBCentralManager(delegate: self, queue: .main) }
    }

    func centralManagerDidUpdateState(_ central: CBCentralManager) {
        guard central.state == .poweredOn else {
            if central.state == .resetting && (connecting || joined) {
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

    func join(_ id: UUID) {
        guard looking, let device = devices[id] else { return }
        manager?.stopScan(); looking = false; connecting = true
        peer = device; device.delegate = self; manager?.connect(device)
        status = "Connecting to stage"
        deadline()
    }

    private func deadline() {
        timeout?.cancel()
        let timer = DispatchWorkItem { [weak self] in self?.failed("Stage timed out. Find it again to reconnect.") }
        timeout = timer
        DispatchQueue.main.asyncAfter(deadline: .now() + 8, execute: timer)
    }

    func centralManager(_ central: CBCentralManager, didConnect peripheral: CBPeripheral) {
        if peer === peripheral { peripheral.discoverServices([AudienceRadio.service]) }
    }

    func peripheral(_ peripheral: CBPeripheral, didDiscoverServices error: Error?) {
        guard peer === peripheral else { return }
        guard error == nil, let service = peripheral.services?.first(where: { $0.uuid == AudienceRadio.service })
        else { failed("Stage service unavailable"); return }
        peripheral.discoverCharacteristics([AudienceRadio.identity, AudienceRadio.hello, AudienceRadio.samples], for: service)
    }

    func peripheral(_ peripheral: CBPeripheral, didDiscoverCharacteristicsFor service: CBService, error: Error?) {
        guard peer === peripheral else { return }
        let list = service.characteristics ?? []
        identity = list.first { $0.uuid == AudienceRadio.identity }
        hello = list.first { $0.uuid == AudienceRadio.hello }
        samples = list.first { $0.uuid == AudienceRadio.samples }
        guard error == nil, let identity, hello != nil, samples != nil else { failed("Unsupported stage"); return }
        peripheral.readValue(for: identity)
    }

    func peripheral(_ peripheral: CBPeripheral, didUpdateValueFor characteristic: CBCharacteristic, error: Error?) {
        guard peer === peripheral, characteristic.uuid == AudienceRadio.identity else { return }
        guard error == nil, let identity = characteristic.value, identity.count == 81, let hello else { failed("Invalid stage identity"); return }
        let publicKey = Data(identity.prefix(65))
        guard
              room.map({ $0 == AudienceRadio.fingerprint(publicKey) }) ?? true
        else { failed("This stage does not match the invitation"); return }
        do {
            let local = P256.KeyAgreement.PrivateKey()
            key = try AudienceRadio.key(local, publicKey, challenge: Data(identity.suffix(16)))
            peripheral.writeValue(local.publicKey.x963Representation, for: hello, type: .withResponse)
        } catch { failed("Invalid stage identity") }
    }

    func peripheral(_ peripheral: CBPeripheral, didWriteValueFor characteristic: CBCharacteristic, error: Error?) {
        guard peer === peripheral else { return }
        guard error == nil else { failed("Stage declined the connection or sample"); return }
        timeout?.cancel()
        if characteristic.uuid == AudienceRadio.hello { connecting = false; joined = true; status = "Sharing with this stage" }
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

    func leave() {
        let old = peer
        peer = nil; joined = false; connecting = false; looking = false
        old?.delegate = nil
        if let old { manager?.cancelPeripheralConnection(old) }
        if manager?.isScanning == true { manager?.stopScan() }
        timeout?.cancel(); timeout = nil
        key = nil; hello = nil; samples = nil; identity = nil; queued = nil; pending = false
        venues.removeAll(); devices.removeAll()
        onEnd?()
        status = "Disconnected"
    }

    private func failed(_ text: String) { leave(); status = text }

    func centralManager(_ central: CBCentralManager, didFailToConnect peripheral: CBPeripheral, error: Error?) {
        if peer === peripheral { failed("Could not connect to stage") }
    }
    func centralManager(_ central: CBCentralManager, didDisconnectPeripheral peripheral: CBPeripheral, error: Error?) {
        if peer === peripheral { failed("Stage disconnected. Find it again to reconnect.") }
    }
}
