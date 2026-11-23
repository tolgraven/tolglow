import CoreMotion
import Foundation
import TolglowSensorCore

/// Relative attitude and acceleration can reject a bumped tripod. Keeping the
/// same camera position and filling its sampled area still require the operator.
@MainActor final class FixedCameraStance {
    var onMoved: (() -> Void)?
    private let manager = CMMotionManager()
    private var fixed: FogPose?
    private var generation = UUID()
    private var latest: CMDeviceMotion?
    func start() throws {
        guard manager.isDeviceMotionAvailable else { throw BrightnessCaptureFailure("Device motion is unavailable.") }
        let token = generation
        manager.deviceMotionUpdateInterval = 0.1
        manager.startDeviceMotionUpdates(using: .xArbitraryZVertical, to: .main) { [weak self] reading, _ in
            Task { @MainActor in
                guard let self, self.generation == token else { return }
                self.latest = reading
                if self.fixed != nil {
                    do { try self.verify() } catch { self.onMoved?() }
                }
            }
        }
    }
    func verify(lock: Bool = false) throws {
        guard let m = latest, abs(ProcessInfo.processInfo.systemUptime - m.timestamp) < 0.5 else {
            throw BrightnessCaptureFailure("Wait for a steady camera mount and fresh motion data.")
        }
        let q = m.attitude.quaternion, a = m.userAcceleration
        let pose = FogPose(x: q.x, y: q.y, z: q.z, w: q.w)
        guard (fixed ?? pose).holds(pose, accelerationG: sqrt(a.x*a.x + a.y*a.y + a.z*a.z)) else {
            throw BrightnessCaptureFailure("The camera stance changed.")
        }
        if lock { fixed = pose }
    }
    func stop() { generation = UUID(); manager.stopDeviceMotionUpdates(); fixed = nil; latest = nil }
}
