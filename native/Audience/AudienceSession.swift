import Combine
import Foundation
import TolglowSensorCore
#if !APPCLIP
import CoreMotion
#endif

enum AudienceInput: String, CaseIterable, Identifiable {
    case touch = "Tap"
    case camera = "Camera"
    #if !APPCLIP
    case motion = "Phone motion"
    #endif
    var id: Self { self }
}

final class AudienceSession: ObservableObject {
    let client = AudienceClient()
    @Published var input = AudienceInput.touch
    @Published private(set) var effort = 0.0
    @Published private(set) var totalSteps: UInt32 = 0
    @Published private(set) var message = ""
    private var meter = DanceMeter()
    private var timer: Timer?
    private var pose: PoseMeter?
    private var serial: UInt32 = 0
    private var taps: UInt32 = 0
    private var tapTime = -Double.infinity
    private var lastPhoneSteps: UInt32?
    private var watch: (sample: AudienceSample, time: Double, motionTime: Double?)?
    private var phoneMotion: AudienceMotion?
    @Published var shareMotionAxes = false
    private var lastWatchSteps: UInt32?
    private var lastPulse: Int?
    private var lastLevelTime = -Double.infinity
    @Published var allowLockedWatch = false
    private(set) var watchOnlyInBackground = false
    private var generation = UUID()
    private var lastInputTime = -Double.infinity
    #if !APPCLIP
    private let motion = CMMotionManager()
    #endif

    init() { client.onEnd = { [weak self] in self?.stopInputs() } }

    func startInputs() {
        guard client.joined, timer == nil else { return }
        resetInput()
        timer = Timer.scheduledTimer(withTimeInterval: 0.2, repeats: true) { [weak self] _ in self?.publish() }
        publish()
    }

    func resetInput() {
        let token = UUID()
        generation = token
        pose?.stop(); pose = nil
        #if !APPCLIP
        motion.stopDeviceMotionUpdates()
        #endif
        phoneMotion = nil
        meter = DanceMeter(); lastInputTime = -.infinity; taps = 0; tapTime = -.infinity
        lastPhoneSteps = 0
        guard client.joined else { return }
        message = ""
        let ingest: (Double, Double) -> Void = { [weak self] strength, time in
            guard let self, self.generation == token else { return }
            let before = self.meter.steps
            self.meter.observe(strength: strength, at: time)
            self.lastInputTime = ProcessInfo.processInfo.systemUptime
            if self.meter.steps != before { self.phoneOnset() }
        }
        switch input {
        case .touch: break
        case .camera:
            let capture = PoseMeter(sample: ingest, unavailable: { [weak self] in
                guard let self, self.generation == token else { return }
                self.message = "Camera unavailable. Tap along or allow camera access in Settings."
            })
            pose = capture
            capture.start()
        #if !APPCLIP
        case .motion:
            guard motion.isDeviceMotionAvailable else { message = "Motion unavailable. Tap along instead."; return }
            motion.deviceMotionUpdateInterval = 1 / 25
            motion.startDeviceMotionUpdates(to: .main) { [weak self] sample, _ in
                guard let self, self.generation == token, let sample else { return }
                let a = sample.userAcceleration
                if self.shareMotionAxes {
                    let r = sample.rotationRate
                    self.phoneMotion = AudienceMotion(acceleration: SIMD3(a.x, a.y, a.z) * 9.80665,
                                                      rotation: SIMD3(r.x, r.y, r.z))
                } else { self.phoneMotion = nil }
                ingest(sqrt(a.x*a.x + a.y*a.y + a.z*a.z) / 1.3, sample.timestamp)
            }
        #endif
        }
    }

    func tap() {
        let now = ProcessInfo.processInfo.systemUptime
        guard client.joined, input == .touch, now - tapTime >= 0.25 else { return }
        taps &+= 1; tapTime = now
        phoneOnset()
    }

    private func phoneOnset() {
        // The wrist owns the person's counter while its stream is fresh.
        if let wrist = watch {
            if ProcessInfo.processInfo.systemUptime - wrist.time <= 2 {
                lastPhoneSteps = input == .touch ? taps : meter.steps
                return
            }
            endWatch()
        }
        publish(refreshLevels: false)
    }

    @discardableResult func receiveWatch(_ sample: AudienceSample, motionTime: Double?) -> Bool {
        guard client.joined else { return false }
        let time = ProcessInfo.processInfo.systemUptime
        if time - (watch?.time ?? -.infinity) > 2 { lastWatchSteps = sample.steps }
        watch = (sample, time, motionTime)
        let refresh = watchOnlyInBackground && time - lastLevelTime >= 0.2
        if refresh || sample.steps != lastWatchSteps { publish(refreshLevels: refresh) }
        return true
    }
    func endWatch() {
        watch = nil; lastWatchSteps = nil; lastPulse = nil
        if watchOnlyInBackground { client.leave() }
    }

    func enterBackground() {
        guard !watchOnlyInBackground else { return }
        let now = ProcessInfo.processInfo.systemUptime
        guard client.joined, allowLockedWatch, let watch, now - watch.time <= 2 else { client.leave(); return }
        watchOnlyInBackground = true
        stopSampling()
    }

    func foreground() {
        guard watchOnlyInBackground else { return }
        watchOnlyInBackground = false
        startInputs()
    }

    private func publish(refreshLevels: Bool = true) {
        let now = ProcessInfo.processInfo.systemUptime
        let phoneSteps = input == .touch ? taps : meter.steps
        let phoneEffort = input == .touch ? exp(-3 * (now - tapTime)) : (now - lastInputTime <= 0.6 ? meter.effort : 0)
        if let data = watch, now - data.time > 2 { endWatch(); lastPhoneSteps = phoneSteps }
        // A paired dancer gets one vote and one step counter even if both devices detect the same step.
        if let data = watch?.sample {
            let delta = data.steps &- (lastWatchSteps ?? data.steps)
            if delta <= 8 { totalSteps &+= delta }
            lastWatchSteps = data.steps
            if refreshLevels { effort = max(data.effort, phoneEffort) }
        } else {
            totalSteps &+= phoneSteps &- (lastPhoneSteps ?? phoneSteps)
            if refreshLevels { effort = phoneEffort }
        }
        lastPhoneSteps = phoneSteps
        if refreshLevels { lastPulse = watch?.sample.pulse; lastLevelTime = now }
        let phoneAxes = shareMotionAxes && now - lastInputTime <= 0.6 ? phoneMotion : nil
        let wristAxes = watch.flatMap { $0.sample.motion?.fresh(measured: $0.motionTime, at: now) }
        if let packet = AudienceSample(serial: serial, steps: totalSteps, effort: effort,
                                       pulse: lastPulse, motion: wristAxes ?? phoneAxes) {
            client.offer(packet); serial &+= 1
        }
    }

    private func stopSampling() {
        generation = UUID(); pose?.stop(); pose = nil
        #if !APPCLIP
        motion.stopDeviceMotionUpdates()
        #endif
        timer?.invalidate(); timer = nil; meter = DanceMeter()
        lastInputTime = -.infinity; tapTime = -.infinity
        lastLevelTime = -.infinity
    }

    func stopInputs() {
        watchOnlyInBackground = false; allowLockedWatch = false; shareMotionAxes = false
        stopSampling()
        serial = 0; totalSteps = 0; effort = 0; lastPhoneSteps = nil; endWatch()
    }
}
