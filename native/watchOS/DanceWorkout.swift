import Combine
import Foundation
import HealthKit
import TolglowSensorCore

@MainActor final class DanceWorkout: NSObject, ObservableObject, HKWorkoutSessionDelegate, HKLiveWorkoutBuilderDelegate {
    @Published private(set) var active = false
    @Published private(set) var requesting = false
    @Published private(set) var effort = 0.0
    @Published private(set) var steps: UInt32 = 0
    @Published private(set) var bpm: Int?
    @Published private(set) var status = "Join the floor on your iPhone first."

    @Published var shareMotionAxes = false
    private var axes: AudienceMotion?

    private let store = HKHealthStore()
    private let motion: MotionSampler
    private let relay: WatchRelay
    private var session: HKWorkoutSession?
    private var builder: HKLiveWorkoutBuilder?
    private var motionUpdates: AnyCancellable?
    private var ticker: Task<Void, Never>?
    private var generation = UUID()
    private var meter = DanceMeter()
    private var serial: UInt32 = 0
    private var pulse: AudiencePulse?
    private var lastMotion = 0.0
    var runningWorkout: Bool { active && session != nil }

    init(motion: MotionSampler, relay: WatchRelay) {
        self.motion = motion
        self.relay = relay
        super.init()
    }

    func start(withPulse: Bool) {
        guard !active, !requesting else { return }
        guard relay.reachable else { relay.activate(); status = "Connect your iPhone and join the floor, then tap again."; return }
        motion.stop()
        relay.cancelPending()
        generation = UUID()
        let token = generation
        if withPulse {
            guard HKHealthStore.isHealthDataAvailable(),
                  let heart = HKObjectType.quantityType(forIdentifier: .heartRate) else {
                status = "Heart rate is unavailable. You can join with motion only."
                return
            }
            requesting = true
            status = "Requesting dance workout and heart rate access…"
            Task {
                do {
                    try await store.requestAuthorization(toShare: [HKObjectType.workoutType()], read: [heart])
                    guard generation == token else { return }
                    guard store.authorizationStatus(for: HKObjectType.workoutType()) == .sharingAuthorized else {
                        stop("Workout permission declined. You can join with motion only.")
                        return
                    }
                    try startWorkout(token: token)
                } catch {
                    guard generation == token else { return }
                    stop("Workout unavailable. You can join with motion only.")
                }
            }
        } else {
            beginMotion()
        }
    }

    private func startWorkout(token: UUID) throws {
        let config = HKWorkoutConfiguration()
        config.activityType = .socialDance
        config.locationType = .indoor
        let workout = try HKWorkoutSession(healthStore: store, configuration: config)
        let live = workout.associatedWorkoutBuilder()
        session = workout
        builder = live
        workout.delegate = self
        live.delegate = self
        live.dataSource = HKLiveWorkoutDataSource(healthStore: store, workoutConfiguration: config)
        let now = Date()
        pulse = AudiencePulse(started: now)
        workout.startActivity(with: now)
        live.beginCollection(withStart: now) { [weak self] success, _ in
            Task { @MainActor in
                guard let self, self.generation == token else { return }
                if success { self.beginMotion() }
                else { self.stop("Workout collection unavailable. You can join with motion only.") }
            }
        }
    }

    private func beginMotion() {
        guard relay.reachable else { stop("iPhone disconnected. Rejoin when it is reachable."); return }
        requesting = false
        active = true
        meter = DanceMeter()
        effort = 0
        steps = 0
        serial = 0
        lastMotion = ProcessInfo.processInfo.systemUptime
        relay.beginAudience()
        // Skip the last cached observation from any previous conducting session.
        motionUpdates = motion.$values.dropFirst().sink { [weak self] values in
            guard let self, let x = values["accel-x"], let y = values["accel-y"], let z = values["accel-z"] else { return }
            let now = ProcessInfo.processInfo.systemUptime
            let previous = self.meter.steps
            self.meter.observe(strength: sqrt(x*x + y*y + z*z) / 9.80665, at: now)
            self.axes = self.shareMotionAxes ? AudienceMotion(values: values) : nil
            self.lastMotion = now
            if self.meter.steps != previous { self.publish(refreshLevels: false) }
        }
        // Establish the zero counter before the first onset, including on a fast rejoin.
        publish()
        motion.preview()
        status = session == nil ? "Sharing motion with the floor." : "Dance workout active. Sharing available pulse and motion."
        ticker = Task { [weak self] in
            while !Task.isCancelled {
                do { try await Task.sleep(for: .milliseconds(200)) } catch { return }
                self?.publish()
            }
        }
    }

    private func publish(refreshLevels: Bool = true) {
        guard ProcessInfo.processInfo.systemUptime - lastMotion < 2.5 else { stop("Wrist motion unavailable. Dance sharing stopped."); return }
        if refreshLevels { effort = meter.effort }
        steps = meter.steps
        let now = Date()
        if pulse?.interrupted(at: now) == true {
            stop("No heart rate available. Check permission and watch fit, or join with motion only.")
            return
        }
        if refreshLevels { bpm = pulse?.bpm(at: now) }
        let freshAxes = shareMotionAxes && ProcessInfo.processInfo.systemUptime - lastMotion <= 0.6 ? axes : nil
        guard let sample = AudienceSample(serial: serial, steps: steps, effort: effort, pulse: bpm, motion: freshAxes) else { return }
        serial &+= 1
        relay.sendAudience(sample, motionAge: max(0, ProcessInfo.processInfo.systemUptime - lastMotion))
    }

    func stop(_ message: String = "Left the floor. Sharing stopped.") {
        generation = UUID()
        ticker?.cancel()
        ticker = nil
        motionUpdates?.cancel()
        motionUpdates = nil
        axes = nil; shareMotionAxes = false
        if active || requesting {
            relay.endAudience()
            motion.stop()
        }
        active = false
        requesting = false
        session?.end()
        builder?.discardWorkout()
        builder = nil
        session = nil
        pulse = nil
        bpm = nil
        effort = 0
        steps = 0
        meter = DanceMeter()
        status = message
    }

    nonisolated func workoutSession(_ workoutSession: HKWorkoutSession, didChangeTo toState: HKWorkoutSessionState,
                                    from fromState: HKWorkoutSessionState, date: Date) {
        Task { @MainActor in
            guard self.session === workoutSession else { return }
            if toState == .ended || toState == .paused || toState == .stopped {
                self.stop("Workout ended or paused. Dance sharing stopped.")
            }
        }
    }
    nonisolated func workoutSession(_ workoutSession: HKWorkoutSession, didFailWithError error: Error) {
        Task { @MainActor in
            guard self.session === workoutSession else { return }
            self.stop("Workout interrupted. Dance sharing stopped.")
        }
    }
    nonisolated func workoutBuilderDidCollectEvent(_ workoutBuilder: HKLiveWorkoutBuilder) {}
    nonisolated func workoutBuilder(_ workoutBuilder: HKLiveWorkoutBuilder, didCollectDataOf collectedTypes: Set<HKSampleType>) {
        guard let type = HKObjectType.quantityType(forIdentifier: .heartRate), collectedTypes.contains(type),
              let stats = workoutBuilder.statistics(for: type),
              let date = stats.mostRecentQuantityDateInterval()?.end,
              let value = stats.mostRecentQuantity()?.doubleValue(for: HKUnit.count().unitDivided(by: .minute())),
              value.isFinite, (30...240).contains(value) else { return }
        Task { @MainActor in
            guard self.builder === workoutBuilder else { return }
            self.pulse?.observe(value, measured: date, now: Date())
        }
    }
}
