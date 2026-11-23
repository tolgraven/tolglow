import AppKit
import CoreBluetooth
import CoreImage.CIFilterBuiltins
import CryptoKit
import Network
import TolglowSensorCore

/// One bridge owns a stage key, radio and aggregate sensor credential for this run.
final class Stage: NSObject, CBPeripheralManagerDelegate {
    private let secret = P256.KeyAgreement.PrivateKey()
    private let name: String
    private var radio: CBPeripheralManager!
    private var guests: AudienceComputeStage?
    private var grants = [UUID: (Data, Double)]()
    private var floor = AudienceFloor()
    private var challenges: [UUID: (value: Data, time: Double)] = [:]
    private var peers: [UUID: (key: SymmetricKey, lastSeen: Double, serial: UInt32?)] = [:]
    private let session = UUID().uuidString
    private let connection: NWConnection?
    private let token: String?
    private let source: String
    private var sampleID: Int64 = 0
    private var lastCount = 0
    private var timer: Timer?
    private var sending = false
    private var queue = AudienceSteps()
    private var lastLevels: AudienceLevels?

    init(name: String, oscHost: String, port: NWEndpoint.Port?, token: String?, source: String) {
        self.name = name; self.source = source; self.token = token
        connection = port.map { NWConnection(host: NWEndpoint.Host(oscHost), port: $0, using: .udp) }
        super.init()
    }

    func start(origin: URL, imagePath: String) throws {
        let room = AudienceRadio.fingerprint(secret.publicKey.x963Representation)
        let env = ProcessInfo.processInfo.environment
        if let credential = env["TOLGLOW_AUDIENCE_COMPUTE_TOKEN"], (24...1024).contains(credential.utf8.count),
           let port = UInt16(env["TOLGLOW_AUDIENCE_COMPUTE_PORT"] ?? "8500") {
            guests = try AudienceComputeStage(room: room, poolPort: port, token: credential, operatorToken: env["TOLGLOW_STUDIO_TOKEN"])
            guests?.measurement = { ms, bytes, floats in
                print(String(format: "Guest compute probe · %.1f ms median · %.0f return bytes/s · %d floats", ms, bytes, floats))
            }
            guests?.start()
        }
        let url = origin.appendingPathComponent("audience").appendingPathComponent(room)
        let qr = CIFilter.qrCodeGenerator()
        qr.message = Data(url.absoluteString.utf8)
        qr.correctionLevel = "M"
        guard let raw = qr.outputImage else { throw SensorError.invalid("QR generation failed") }
        let padded = raw.extent.insetBy(dx: -4, dy: -4)
        let quietZone = CIImage(color: .white).cropped(to: padded)
        let image = raw.composited(over: quietZone).transformed(by: CGAffineTransform(scaleX: 8, y: 8))
        let context = CIContext()
        guard let output = context.createCGImage(image, from: image.extent),
              let data = NSBitmapImageRep(cgImage: output).representation(using: .png, properties: [:])
        else { throw SensorError.invalid("QR encoding failed") }
        try data.write(to: URL(fileURLWithPath: imagePath), options: .atomic)
        print("Stage invitation \(url.absoluteString)\nQR image \(imagePath)")
        connection?.start(queue: .main)
        radio = CBPeripheralManager(delegate: self, queue: .main)
        timer = Timer.scheduledTimer(withTimeInterval: 0.2, repeats: true) { [weak self] _ in self?.publish() }
    }

    func peripheralManagerDidUpdateState(_ peripheral: CBPeripheralManager) {
        guard peripheral.state == .poweredOn else {
            floor = AudienceFloor()
            peers.keys.forEach { guests?.revoke(participant: $0) }
            peers.removeAll(); grants.removeAll()
            print("Stage Bluetooth unavailable (\(peripheral.state.rawValue))")
            return
        }
        let service = CBMutableService(type: AudienceRadio.service, primary: true)
        let identity = CBMutableCharacteristic(type: AudienceRadio.identity, properties: [.read],
            value: nil, permissions: [.readable])
        func input(_ id: CBUUID) -> CBMutableCharacteristic {
            CBMutableCharacteristic(type: id, properties: [.write], value: nil, permissions: [.writeable])
        }
        service.characteristics = [identity, input(AudienceRadio.hello), input(AudienceRadio.samples)]
        if guests != nil {
            service.characteristics?.append(CBMutableCharacteristic(type: AudienceRadio.computeGrant, properties: [.read],
                                                                    value: nil, permissions: [.readable]))
        }
        peripheral.add(service)
    }
    func peripheralManager(_ peripheral: CBPeripheralManager, didAdd service: CBService, error: Error?) {
        guard error == nil else { print("Could not register stage service"); return }
        peripheral.startAdvertising([CBAdvertisementDataServiceUUIDsKey: [AudienceRadio.service],
                                    CBAdvertisementDataLocalNameKey: name])
    }
    func peripheralManagerDidStartAdvertising(_ peripheral: CBPeripheralManager, error: Error?) {
        print(error == nil ? "Stage discoverable" : "Bluetooth advertising failed")
    }

    func peripheralManager(_ peripheral: CBPeripheralManager, didReceiveRead request: CBATTRequest) {
        if request.characteristic.uuid == AudienceRadio.computeGrant {
            sendGrant(on: peripheral, request); return
        }
        guard request.characteristic.uuid == AudienceRadio.identity else {
            peripheral.respond(to: request, withResult: .readNotPermitted); return
        }
        let id = request.central.identifier
        let now = ProcessInfo.processInfo.systemUptime
        if request.offset == 0 {
            guard challenges[id] != nil || challenges.count < floor.capacity else {
                peripheral.respond(to: request, withResult: .insufficientResources); return
            }
            let nonce = SymmetricKey(size: .bits128).withUnsafeBytes { Data($0) }
            challenges[id] = (nonce, now)
        }
        guard let challenge = challenges[id], now - challenge.time < 8 else {
            peripheral.respond(to: request, withResult: .insufficientAuthentication); return
        }
        let data = secret.publicKey.x963Representation + challenge.value
        guard request.offset >= 0 && request.offset <= data.count else {
            peripheral.respond(to: request, withResult: .invalidOffset); return
        }
        request.value = Data(data.dropFirst(request.offset))
        peripheral.respond(to: request, withResult: .success)
    }
    private func sendGrant(on peripheral: CBPeripheralManager, _ request: CBATTRequest) {
        let id = request.central.identifier, now = ProcessInfo.processInfo.systemUptime
        guard let peer = peers[id], now - peer.lastSeen < 8 else {
            peripheral.respond(to: request, withResult: .insufficientAuthentication); return
        }
        if request.offset == 0 {
            guard grants[id] == nil, let grant = guests?.issueGrant(for: id),
                  let sealed = try? AES.GCM.seal(grant, using: peer.key, authenticating: Data("guest-compute-v1".utf8)),
                  let blob = sealed.combined else { peripheral.respond(to: request, withResult: .insufficientResources); return }
            grants[id] = (blob, now)
        }
        guard let (blob, time) = grants[id], now - time < 5, (0...blob.count).contains(request.offset)
        else { peripheral.respond(to: request, withResult: .invalidOffset); return }
        request.value = Data(blob.dropFirst(request.offset))
        peripheral.respond(to: request, withResult: .success)
    }

    func peripheralManager(_ peripheral: CBPeripheralManager, didReceiveWrite requests: [CBATTRequest]) {
        guard let first = requests.first else { return }
        guard requests.count == 1, first.offset == 0, let packet = first.value
        else { peripheral.respond(to: first, withResult: .invalidOffset); return }
        let id = first.central.identifier
        let time = ProcessInfo.processInfo.systemUptime
        do {
            switch first.characteristic.uuid {
            case AudienceRadio.hello:
                guard packet.count == 65, peers[id] != nil || peers.count < floor.capacity else {
                    peripheral.respond(to: first, withResult: .insufficientResources); return
                }
                guard let challenge = challenges.removeValue(forKey: id), time - challenge.time < 8 else {
                    peripheral.respond(to: first, withResult: .insufficientAuthentication); return
                }
                let key = try AudienceRadio.key(secret, packet, challenge: challenge.value)
                guests?.revoke(participant: id); grants.removeValue(forKey: id)
                peers[id] = (key, time, nil)
                floor.leave(id)
            case AudienceRadio.samples:
                guard (packet.count == 39 || packet.count == 63), let peer = peers[id] else {
                    peripheral.respond(to: first, withResult: .insufficientAuthentication); return
                }
                let data = try AES.GCM.open(AES.GCM.SealedBox(combined: packet), using: peer.key)
                guard let sample = AudienceSample(bytes: data),
                      peer.serial.map({ previous in
                          let delta = sample.serial &- previous
                          return delta > 0 && delta < (1 << 31)
                      }) ?? true, floor.receive(sample, from: id, at: time) else {
                    peripheral.respond(to: first, withResult: .unlikelyError); return
                }
                peers[id] = (peer.key, time, sample.serial)
            default: peripheral.respond(to: first, withResult: .writeNotPermitted); return
            }
            peripheral.respond(to: first, withResult: .success)
        } catch { peripheral.respond(to: first, withResult: .insufficientAuthentication) }
    }

    private func publish() {
        let time = ProcessInfo.processInfo.systemUptime
        challenges = challenges.filter { time - $0.value.time < 8 }
        for (id, peer) in peers where time - peer.lastSeen >= 8 { guests?.revoke(participant: id) }
        peers = peers.filter { time - $0.value.lastSeen < 8 }
        grants = grants.filter { time - $0.value.1 < 5 }
        let levels = floor.drain(at: time)
        lastLevels = levels
        if connection != nil {
            do { try queue.append(levels.steps) }
            catch {
                stop("Gesture queue overflow (\(queue.pending) pending, \(levels.steps) incoming).")
                return
            }
        }
        if lastCount != levels.count { print("Participants \(levels.count)") }
        let wasActive = lastCount > 0
        guard !sending else { return }
        lastCount = levels.count
        guard levels.count > 0 || wasActive || queue.pending > 0 else { return }
        send(levels, at: time)
    }

    private func send(_ levels: AudienceLevels, at time: Double) {
        guard let connection, let token else { return }
        let delivered = queue.takeBatch()
        var values = ["audience-count": Double(levels.count), "dance-effort": levels.effort,
                      "dance-steps": Double(delivered), "pulse-count": Double(levels.pulseCount), "pulse-present": levels.pulse == nil ? 0.0 : 1.0]
        values["motion-count"] = Double(levels.motionCount)
        values.merge(levels.motion) { _, measured in measured }
        if let pulse = levels.pulse { values["pulse-bpm"] = pulse }
        do {
            let gestures = (0..<delivered).map { _ in Gesture(type: "dance-step") }
            let observation = try Observation(deviceID: source, sessionID: session, sampleID: sampleID,
                timeMS: time * 1000, values: values, gestures: gestures, pairingToken: token)
            let packet = try SensorOSC.encode(observation)
            sampleID += 1; sending = true
            connection.send(content: packet, completion: .contentProcessed { [weak self] error in
                guard let self else { return }
                self.sending = false
                if error != nil { self.stop("OSC send failed with \(delivered) gestures in flight."); return }
                if self.queue.pending > 0, let latest = self.lastLevels {
                    self.send(latest, at: ProcessInfo.processInfo.systemUptime)
                }
            })
        } catch { stop("Could not encode audience observation.") }
    }

    private func stop(_ reason: String) {
        timer?.invalidate()
        radio.stopAdvertising()
        radio.removeAllServices()
        connection?.cancel()
        FileHandle.standardError.write(Data("Stage stopped. \(reason) Check the OSC receiver before restarting.\n".utf8))
        exit(75)
    }
}

// The packaged host reads status lines through a pipe. Closing the host owns
// the stage's ephemeral identity even if its JVM exits abruptly.
setlinebuf(stdout)
let arguments = CommandLine.arguments
let environment = ProcessInfo.processInfo.environment
guard arguments.count == 4, let origin = URL(string: arguments[2]), origin.scheme == "https",
      origin.host != nil, origin.user == nil, origin.password == nil, origin.query == nil,
      origin.fragment == nil, ["", "/"].contains(origin.path) else {
    print("Usage: TolglowAudienceBridge STAGE_NAME HTTPS_INVOCATION_ORIGIN QR_PNG_PATH")
    exit(64)
}
let token = environment["TOLGLOW_AUDIENCE_OSC_TOKEN"]
let portText = environment["TOLGLOW_AUDIENCE_OSC_PORT"]
let port = portText.flatMap(UInt16.init).flatMap { NWEndpoint.Port(rawValue: $0) }
guard (portText == nil && token == nil) || (port != nil && (token?.count ?? 0) >= 16) else {
    print("Set both a valid OSC port and its enrolled sensor token, or omit both for radio-only mode.")
    exit(64)
}
let stage = Stage(name: arguments[1], oscHost: environment["TOLGLOW_AUDIENCE_OSC_HOST"] ?? "127.0.0.1", port: port, token: token, source: environment["TOLGLOW_AUDIENCE_OSC_ID"] ?? "audience")
do {
    try stage.start(origin: origin, imagePath: arguments[3])
    if environment["TOLGLOW_AUDIENCE_SUPERVISED"] == "1" {
        FileHandle.standardInput.readabilityHandler = { handle in
            if handle.availableData.isEmpty { exit(0) }
        }
    }
    RunLoop.main.run()
} catch { print("Could not start stage. Check the QR output location."); exit(1) }
