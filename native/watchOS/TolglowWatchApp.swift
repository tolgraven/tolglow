import SwiftUI

@MainActor final class WatchModel: ObservableObject {
    let motion = MotionSampler(deviceID: persistentDeviceID(prefix: "watch"))
    let relay = WatchRelay()
    init() {
        motion.onObservation = { [weak self] sample in self?.relay.send(sample) }
        relay.onFailure = { [weak self] in self?.motion.stop() }
    }
    func suspend() { motion.stop(); relay.cancelPending() }
}

@main struct TolglowWatchApp: App {
    @StateObject private var model = WatchModel()
    @Environment(\.scenePhase) private var scenePhase
    var body: some Scene {
        WindowGroup {
            WatchRoot(motion: model.motion, relay: model.relay)
                .onChange(of: scenePhase) { _, phase in if phase != .active { model.suspend() } }
        }
    }
}

struct WatchRoot: View {
    @ObservedObject var motion: MotionSampler
    @ObservedObject var relay: WatchRelay
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 12) {
                    Text(motion.armed ? "CONDUCTING" : "DISARMED").font(.headline).foregroundStyle(motion.armed ? .green : .orange)
                    Button("Primary action") { motion.primaryAction() }
                        .handGestureShortcut(.primaryAction, isEnabled: motion.armed)
                        .disabled(!motion.armed)
                    Button(motion.armed ? "Disarm" : "Arm") {
                        if motion.armed { motion.disarm(); relay.cancelPending() }
                        else if relay.reachable { motion.arm() }
                    }.tint(motion.armed ? .red : .orange)
                    HStack {
                        Button("Preview") { motion.preview() }
                        Button("Neutral") { motion.calibrate() }.disabled(motion.armed)
                    }.font(.caption)
                    Text(motion.status).font(.caption2)
                    Divider()
                    Button("Connect iPhone") { relay.activate() }
                    Button("Pair / announce ID") { relay.announce(deviceID: motion.deviceID) }
                    Text(relay.status).font(.caption2)
                    DisclosureGroup("Device / arm session") {
                        Text(motion.deviceID).font(.system(size: 9, design: .monospaced))
                        Text(motion.sessionID).font(.system(size: 9, design: .monospaced))
                    }
                    Text("Double Tap activates Primary action on supported hardware/settings. No raw finger gesture stream.").font(.caption2).foregroundStyle(.secondary)
                }.padding(.horizontal, 6)
            }.navigationTitle("Tolglow")
        }
    }
}
