import SwiftUI
import TolglowSensorCore

@MainActor final class PhoneModel: ObservableObject {
    let audience = AudienceSession()
    let motion = MotionSampler(deviceID: persistentDeviceID(prefix: "iphone"))
    let osc = OSCConnection()
    let relay = WatchRelay()
    let studio = StudioClient()
    let rigCalibration = RigCalibrationClient()
    private var phoneToken: SensorCredential?
    private var watchToken: SensorCredential?
    @Published var relayArmed = false
    @Published var pairedWatchID = UserDefaults.standard.string(forKey: "paired-watch-id") ?? ""
    @Published var status = "Enroll device IDs in the host before arming"

    init() {
        motion.onObservation = { [weak self] observation in
            guard let self, let token = self.phoneToken else { self?.motion.disarm(); return }
            self.osc.send(observation, credential: token)
        }
        osc.onFailure = { [weak self] in self?.disarmPhone(); self?.disarmRelay() }
        relay.onFailure = { [weak self] in self?.disarmRelay() }
        relay.audienceReceive = { [weak self] effort, steps, pulse in
            self?.audience.receiveWatch(effort: effort, steps: steps, pulse: pulse) ?? false
        }
        relay.audienceEnded = { [weak self] in self?.audience.endWatch() }
        relay.receive = { [weak self] observation in
            guard let self, self.relayArmed, self.osc.ready,
                  observation.deviceID == self.pairedWatchID,
                  let token = self.watchToken else { return false }
            self.osc.send(observation, credential: token)
            return self.osc.ready
        }
    }
    func savePairing(device: String, token: String, host: String, port: UInt16) {
        guard let destination = PairingDestination(host: host, port: port) else {
            status = "Choose the issuing OSC host and port first"; return
        }
        do {
            try PairingStore.save(token, for: device, destination: destination)
            if device == motion.deviceID { disarmPhone() }
            if device == pairedWatchID { disarmRelay() }
            status = "Host-issued token saved in Keychain"
        }
        catch { status = "Pairing credential could not be saved" }
    }
    func armPhone() {
        guard osc.ready, let destination = osc.destination,
              let token = PairingStore.load(for: motion.deviceID, destination: destination) else {
            status = "Connect OSC and enroll this iPhone first"; return
        }
        phoneToken = token; motion.arm()
    }
    func armRelay() {
        guard osc.ready, let destination = osc.destination, !pairedWatchID.isEmpty,
              let token = PairingStore.load(for: pairedWatchID, destination: destination) else {
            status = "Connect OSC and enroll the selected Watch first"; return
        }
        UserDefaults.standard.set(pairedWatchID, forKey: "paired-watch-id")
        watchToken = token; relayArmed = true; status = "Watch relay armed for the selected device"
    }
    func disarmPhone() { motion.disarm(); phoneToken = nil; osc.discardPending() }
    func disarmRelay() { relayArmed = false; watchToken = nil; osc.discardPending() }
    func suspend() {
        rigCalibration.suspend()
        disarmPhone(); disarmRelay(); motion.stop(); relay.cancelPending(); osc.close(); studio.disconnect()
        status = "Backgrounded: disarmed and disconnected"
    }
}

@main struct TolglowPhoneApp: App {
    @StateObject private var model = PhoneModel()
    @State private var invitation: AudienceInvite?
    @Environment(\.scenePhase) private var scenePhase
    var body: some Scene {
        WindowGroup {
            PhoneRoot(model: model)
                .preferredColorScheme(.dark)
                .onContinueUserActivity(NSUserActivityTypeBrowsingWeb) { activity in
                    if let url = activity.webpageURL, let room = AudienceRadio.room(in: url) {
                        invitation = AudienceInvite(id: room)
                    }
                }
                .sheet(item: $invitation) { invite in
                    AudienceView(session: model.audience, room: invite.id).onAppear { model.relay.activate() }
                }
                .onChange(of: scenePhase) { _, phase in if phase == .background { model.suspend() } }
        }
    }
}

private struct AudienceInvite: Identifiable { let id: String }

struct PhoneRoot: View {
    @ObservedObject var model: PhoneModel
    var body: some View {
        TabView {
            PerformView(studio: model.studio).tabItem { Label("Perform", systemImage: "square.grid.3x3.fill") }
            RigCalibrationView(client: model.rigCalibration, scanner: model.rigCalibration.scanner)
                .tabItem { Label("Scan rig", systemImage: "viewfinder") }
            AudienceView(session: model.audience).onAppear { model.relay.activate() }
                .tabItem { Label("Dance floor", systemImage: "figure.dance") }
            WebStudioView().tabItem { Label("Studio", systemImage: "globe") }
            PhoneMotionView(model: model, motion: model.motion, osc: model.osc, relay: model.relay)
                .tabItem { Label("Conduct", systemImage: "hand.wave.fill") }
            PairingView(model: model, osc: model.osc, relay: model.relay)
                .tabItem { Label("Connect", systemImage: "antenna.radiowaves.left.and.right") }
        }.tint(Color(red: 0.94, green: 0.66, blue: 0.28))
    }
}

struct PerformView: View {
    @ObservedObject var studio: StudioClient
    @AppStorage("studio-origin") private var origin = "http://tolglow.local:8500"
    @State private var master = 1.0
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(alignment: .leading, spacing: 18) {
                    TextField("Studio origin", text: $origin).textInputAutocapitalization(.never).autocorrectionDisabled().keyboardType(.URL)
                    HStack {
                        Button("Connect") { studio.connect(origin) }
                        Button("Refresh") { studio.refresh() }.disabled(studio.busy)
                    }.buttonStyle(.bordered)
                    Text(studio.status).font(.caption).foregroundStyle(.secondary)
                    if let snapshot = studio.snapshot {
                        HStack {
                            Text("Master")
                            Slider(value: $master, in: 0...1, onEditingChanged: { editing in
                                if !editing { studio.command("master/set", fields: ["value": master]) }
                            }).disabled(studio.busy)
                            Text(master, format: .percent.precision(.fractionLength(0))).monospacedDigit().frame(width: 46)
                        }
                        Button(snapshot.blackout ? "Restore output" : "Blackout") {
                            studio.command("blackout/set", fields: ["value": !snapshot.blackout])
                        }.buttonStyle(.borderedProminent).tint(snapshot.blackout ? .orange : .red).disabled(studio.busy)
                        LazyVGrid(columns: [GridItem(.adaptive(minimum: 120))], spacing: 12) {
                            ForEach(snapshot.cues) { cue in
                                let active = snapshot.instances.contains(where: { $0.cueID == cue.id })
                                Button { studio.toggle(cue) } label: {
                                    VStack(alignment: .leading, spacing: 14) {
                                        Image(systemName: active ? "stop.fill" : "play.fill")
                                        Text(cue.name ?? cue.id).font(.headline).lineLimit(2)
                                        Text(active ? "Running · tap to release" : "Tap to launch").font(.caption2)
                                    }.frame(maxWidth: .infinity, minHeight: 100, alignment: .leading).padding(12)
                                        .background(swatch(cue.swatch).opacity(active ? 0.8 : 0.28), in: RoundedRectangle(cornerRadius: 14))
                                }.buttonStyle(.plain).disabled(studio.busy)
                            }
                        }
                    }
                }.padding()
            }.navigationTitle("Tolglow")
                .onChange(of: studio.snapshot?.master) { _, value in if let value { master = value } }
        }
    }
    private func swatch(_ hex: String?) -> Color {
        guard let hex, hex.count == 7, hex.first == "#", let value = UInt32(hex.dropFirst(), radix: 16) else { return .orange }
        return Color(red: Double((value >> 16) & 255)/255, green: Double((value >> 8) & 255)/255, blue: Double(value & 255)/255)
    }
}

struct PhoneMotionView: View {
    @ObservedObject var model: PhoneModel
    @ObservedObject var motion: MotionSampler
    @ObservedObject var osc: OSCConnection
    @ObservedObject var relay: WatchRelay
    var body: some View {
        NavigationStack {
            Form {
                Section("This iPhone") {
                    Text(motion.status)
                    HStack {
                        Button("Preview") { motion.preview() }
                        Button("Calibrate") { motion.calibrate() }.disabled(motion.armed)
                    }
                    Button(motion.armed ? "Disarm iPhone" : "Arm iPhone") {
                        if motion.armed { model.disarmPhone() } else { model.armPhone() }
                    }.tint(motion.armed ? .red : .orange)
                    Button("Primary action") { motion.primaryAction() }.disabled(!motion.armed)
                    ForEach(["roll", "pitch", "yaw"], id: \.self) { key in
                        LabeledContent(key, value: String(format: "%.3f rad", motion.values[key] ?? 0))
                    }
                }
                Section("Watch relay") {
                    Text(relay.status)
                    Button("Activate WatchConnectivity") { relay.activate() }
                    Button(model.relayArmed ? "Disarm Watch relay" : "Arm enrolled Watch relay") {
                        if model.relayArmed { model.disarmRelay() } else { model.armRelay() }
                    }.tint(model.relayArmed ? .red : .orange)
                    Text("Keep this iPhone app and the Watch app foreground for this first version. Backgrounding disarms the phone relay.")
                        .font(.caption).foregroundStyle(.secondary)
                }
                Section("Delivery") {
                    Text(osc.status)
                    LabeledContent("UDP sends / coalesced", value: "\(osc.sent) / \(osc.dropped)")
                    Text(model.status).font(.caption)
                }
            }.navigationTitle("Conduct")
        }
    }
}

struct PairingView: View {
    @ObservedObject var model: PhoneModel
    @ObservedObject var osc: OSCConnection
    @ObservedObject var relay: WatchRelay
    @AppStorage("osc-host") private var host = "tolglow.local"
    @AppStorage("osc-port") private var port = "9000"
    @State private var token = ""
    @State private var forWatch = false
    var body: some View {
        NavigationStack {
            Form {
                StudioOperatorAccessSection()
                Section("OSC destination") {
                    TextField("Host", text: $host).textInputAutocapitalization(.never).autocorrectionDisabled()
                    TextField("UDP port", text: $port).keyboardType(.numberPad)
                    Button("Connect UDP") {
                        model.disarmPhone(); model.disarmRelay()
                        osc.connect(host: host, port: UInt16(port) ?? 0)
                    }
                    Text(osc.status).font(.caption)
                }
                Section("Device enrollment") {
                    Text("iPhone: \(model.motion.deviceID)").font(.caption.monospaced()).textSelection(.enabled)
                    TextField("Paired Watch device ID", text: $model.pairedWatchID)
                        .textInputAutocapitalization(.never).autocorrectionDisabled().disabled(model.relayArmed)
                    if !relay.discoveredDeviceID.isEmpty {
                        Button("Use announced Watch ID") { model.disarmRelay(); model.pairedWatchID = relay.discoveredDeviceID }
                        Text(relay.discoveredDeviceID).font(.caption.monospaced())
                    }
                    Toggle("Save token for Watch", isOn: $forWatch)
                    SecureField("Host-issued pairing token", text: $token).textInputAutocapitalization(.never).autocorrectionDisabled()
                    Button("Save pairing token") {
                        model.savePairing(device: forWatch ? model.pairedWatchID : model.motion.deviceID, token: token,
                                         host: host, port: UInt16(port) ?? 0)
                        token = ""
                    }
                    Text("Enroll this exact device in the host, then paste its token. Device IDs alone do not authenticate a controller. UDP is plaintext: use a trusted lighting LAN or an authenticated network tunnel.")
                        .font(.caption).foregroundStyle(.secondary)
                }
                Section { Text(model.status) }
            }.navigationTitle("Connect")
        }
    }
}
