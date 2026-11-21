import Combine
import CoreMotion
import Foundation
import TolglowSensorCore

/// The only CoreMotion owner. Created inert; preview/arm are explicit UI actions.
/// Callbacks are delivered on main, never from a lighting frame/render worker.
@MainActor final class MotionSampler: ObservableObject {
    @Published private(set) var armed = false
    @Published private(set) var calibrated = false
    @Published private(set) var status = "Motion stopped"
    @Published private(set) var values: [String: Double] = [:]
    @Published private(set) var sessionID = "Not armed"
    let deviceID: String
    var onObservation: ((Observation) -> Void)?
    private let manager = CMMotionManager()
    private var current: Quaternion?
    private var neutral: Quaternion?
    private var calibrationID: String?
    private var sampleID: Int64 = 0
    private var timestampMS: Double = 0
    private var generation = UUID()

    init(deviceID: String) { self.deviceID = deviceID }

    func preview() {
        guard !manager.isDeviceMotionActive else { return }
        guard manager.isDeviceMotionAvailable else { status = "Device motion unavailable"; return }
        // Deliberately foreground-only. No workout/audio session invented to keep sensors alive.
        generation = UUID()
        let token = generation
        manager.deviceMotionUpdateInterval = 1.0 / 20.0
        manager.startDeviceMotionUpdates(using: .xArbitraryZVertical, to: .main) { [weak self] motion, error in
            guard let self, self.generation == token else { return }
            if let error { self.stop(); self.status = error.localizedDescription; return }
            guard let motion else { return }
            do {
                let q = motion.attitude.quaternion
                let pose = try Quaternion(x: q.x, y: q.y, z: q.z, w: q.w)
                self.current = pose
                self.timestampMS = motion.timestamp * 1000
                var values = try self.neutral.map { try pose.relative(to: $0).eulerRadians } ?? pose.eulerRadians
                let a = motion.userAcceleration, r = motion.rotationRate
                values.merge(["accel-x": a.x * 9.80665, "accel-y": a.y * 9.80665, "accel-z": a.z * 9.80665,
                              "rotation-x": r.x, "rotation-y": r.y, "rotation-z": r.z]) { _, new in new }
                self.values = values
                if self.armed { self.emit() }
            } catch { self.stop(); self.status = "Invalid motion sample" }
        }
        status = "Previewing at 20 Hz — hold neutral pose, then calibrate"
    }

    func calibrate() {
        guard !armed, let current else { status = "Preview motion before calibration; disarm to recalibrate"; return }
        neutral = current; calibrationID = UUID().uuidString; calibrated = true
        status = "Neutral pose captured"
    }

    func arm() {
        guard calibrated, manager.isDeviceMotionActive else { status = "Preview and calibrate first"; return }
        sampleID = 0; sessionID = UUID().uuidString; armed = true; status = "Armed — sending observations"
    }
    func disarm() { armed = false; status = "Disarmed" }
    func stop() {
        generation = UUID(); disarm(); manager.stopDeviceMotionUpdates()
        current = nil; neutral = nil; calibrated = false; calibrationID = nil
        status = "Motion stopped"
    }
    func primaryAction() {
        guard armed else { return }
        // Semantic SwiftUI action, not an inferred accelerometer/finger gesture.
        emit(gestures: [Gesture()])
    }
    private func emit(gestures: [Gesture] = []) {
        do {
            let observation = try Observation(deviceID: deviceID, sessionID: sessionID,
                sampleID: sampleID, timeMS: timestampMS, values: values, gestures: gestures,
                referenceFrame: "x-arbitrary-z-vertical", calibrationID: calibrationID)
            sampleID += 1
            onObservation?(observation)
        } catch { stop(); status = "Observation rejected locally" }
    }
}

func persistentDeviceID(prefix: String) -> String {
    let key = "tolglow-device-id"
    if let saved = UserDefaults.standard.string(forKey: key) { return saved }
    let created = prefix + "-" + UUID().uuidString
    UserDefaults.standard.set(created, forKey: key)
    return created
}
