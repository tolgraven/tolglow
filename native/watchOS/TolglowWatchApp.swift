import SwiftUI

@MainActor final class WatchModel: ObservableObject {
    let motion = MotionSampler(deviceID: persistentDeviceID(prefix: "watch"))
    let relay = WatchRelay()
    lazy var workout = DanceWorkout(motion: motion, relay: relay)
    init() {
        motion.onObservation = { [weak self] sample in self?.relay.send(sample) }
        relay.onFailure = { [weak self] in
            guard let self else { return }
            if self.workout.active || self.workout.requesting {
                if self.workout.link.route == .pairedPhone { self.workout.stop(self.relay.status) }
            } else { self.motion.stop() }
        }
    }
    func sceneChanged(_ phase: ScenePhase) {
        if phase == .active || workout.runningWorkout { return }
        // HealthKit may present the permission sheet while the scene is inactive.
        if workout.requesting && phase == .inactive { return }
        let stage = workout.link.stage
        if workout.active || workout.requesting || (phase == .background && (stage.looking || stage.connecting || stage.joined)) {
            workout.stop("Left the app. Dance sharing stopped.")
        }
        motion.stop()
        relay.cancelPending()
    }
}

@main struct TolglowWatchApp: App {
    @StateObject private var model = WatchModel()
    @Environment(\.scenePhase) private var scenePhase
    var body: some Scene {
        WindowGroup {
            WatchRoot(motion: model.motion, relay: model.relay, workout: model.workout)
                .onChange(of: scenePhase) { _, phase in model.sceneChanged(phase) }
        }
    }
}

struct WatchRoot: View {
    @ObservedObject var motion: MotionSampler
    @ObservedObject var relay: WatchRelay
    @ObservedObject var workout: DanceWorkout
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(spacing: 12) {
                    NavigationLink("Dance floor") { DanceAudienceView(workout: workout) }.accessibilityIdentifier("dance-floor")
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
                    NavigationLink("Device / arm session") {
                        ScrollView {
                            VStack(alignment: .leading, spacing: 12) {
                                Text("Device ID").font(.caption)
                                Text(motion.deviceID).font(.system(size: 11, design: .monospaced))
                                Text("Arm session").font(.caption)
                                Text(motion.sessionID).font(.system(size: 11, design: .monospaced))
                            }.padding(.horizontal, 6)
                        }.navigationTitle("Device")
                    }
                    Text("Double Tap activates Primary action on supported hardware/settings. No raw finger gesture stream.").font(.caption2).foregroundStyle(.secondary)
                }.padding(.horizontal, 6)
            }.navigationTitle("Tolglow")
        }
    }
}
