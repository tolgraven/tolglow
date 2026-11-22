import Combine
import CoreGraphics
import CoreMotion
import Foundation
import TolglowSensorCore

let fogPathMetres = 0.1...1000.0

struct FogFixture: Decodable, Identifiable {
    let id: String
    let name: String?
}
private struct FogHost: Decodable {
    let runtime: String
    let epoch: Int
    let fixtures: [FogFixture]
    enum CodingKeys: String, CodingKey { case runtime = "runtime-id", epoch, fixtures }
}
private struct FogReply: Decodable { let status: String; let message: String?; let fit: FogFit? }

private struct FogSession {
    let endpoint: URL
    let host: FogHost
    let id = UUID().uuidString
    let http: URLSession
    func send(_ op: String, _ fields: [String: Any] = [:]) async throws -> FogReply {
        var req = URLRequest(url: endpoint)
        req.httpMethod = "POST"
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        var body: [String: Any] = ["runtime-id": host.runtime, "expected-epoch": host.epoch, "session-id": id, "op": op]
        body.merge(fields) { _, new in new }
        req.httpBody = try JSONSerialization.data(withJSONObject: body)
        let result = try await calibrationJSON(FogReply.self, req, http)
        guard result.status == "accepted" else {
            throw BrightnessCaptureFailure(result.message ?? "The host rejected the fog measurement.")
        }
        return result
    }
}

@MainActor final class FogCalibrationClient: ObservableObject {
    private let camera = PhotometryCamera()
    private let motion = CMMotionManager()
    @Published private(set) var fixtures: [FogFixture] = []
    @Published private(set) var prepared = false
    @Published private(set) var busy = false
    @Published private(set) var recording = false
    @Published private(set) var posting = false
    @Published private(set) var preview: CGImage?
    @Published private(set) var phase: FogPhase = .baseline
    @Published private(set) var counts: [FogPhase: Int] = [:]
    @Published private(set) var transmission: Double?
    @Published private(set) var lastReading: FogReading?
    @Published private(set) var fit: FogFit?
    @Published private(set) var status = "Connect to select a fog machine."
    @Published private(set) var quality = ""
    var baselineReady: Bool { baseline.ready }
    var connected: Bool { host != nil }

    private let http: URLSession = {
        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 4
        config.timeoutIntervalForResource = 6
        return URLSession(configuration: config)
    }()
    private var host: FogHost?
    private var endpoint: URL?
    private var session: FogSession?
    private var cameraTask: Task<Void, Never>?
    private var watchdog: Task<Void, Never>?
    private var command: Task<Void, Never>?
    private var generation = UUID()
    private var baseline = FogBaseline()
    private var pose: FogPose?
    private var latestMotion: CMDeviceMotion?
    private var startTime = 0.0
    private var lastTime = 0.0
    private var lastFrame = 0.0
    private var uptime: Double { ProcessInfo.processInfo.systemUptime }

    init() {
        camera.onInterruption = { [weak self] in
            Task { @MainActor in self?.stop("Camera interrupted. Start a new sightline.") }
        }
    }

    func resetHost() {
        guard !prepared, !busy, !recording else { return }
        fixtures = []; host = nil; endpoint = nil
    }

    func connect(_ address: String) {
        resetHost()
        guard !busy, !prepared, let url = URL(string: address), ["http","https"].contains(url.scheme ?? ""),
              url.host != nil, url.user == nil, url.password == nil, url.query == nil,
              url.fragment == nil, ["","/"].contains(url.path) else {
            status = "Enter an HTTP(S) Studio origin."; return
        }
        let endpoint = url.appendingPathComponent("api/fog-calibration")
        perform {
            let host = try await calibrationJSON(FogHost.self, URLRequest(url: endpoint), self.http)
            try Task.checkCancellation()
            self.host = host; self.endpoint = endpoint; self.fixtures = host.fixtures
            self.status = "Select the source, mount the phone and align both patches."
        }
    }

    func prepare(shutter: Int32) {
        guard !busy, !prepared, host != nil, motion.isDeviceMotionAvailable else {
            status = "A rear camera and device motion are required."; return
        }
        perform {
            let feed = FogCameraFeed()
            try await self.camera.start(shutter: shutter, fogFeed: feed)
            try Task.checkCancellation()
            self.motion.deviceMotionUpdateInterval = 0.1
            self.motion.startDeviceMotionUpdates(using: .xArbitraryZVertical, to: .main) { [weak self] reading, _ in
                Task { @MainActor in self?.observeMotion(reading) }
            }
            self.prepared = true
            self.lastFrame = self.uptime
            self.status = "Align each patch in its box and keep the phone fixed."
            self.cameraTask = Task { [weak self] in
                for await frame in feed.frames {
                    guard let self, !Task.isCancelled else { break }
                    await self.receive(frame)
                }
            }
            self.watchdog = Task { [weak self] in
                while !Task.isCancelled {
                    try? await Task.sleep(for: .seconds(2))
                    guard !Task.isCancelled, let self else { return }
                    if self.prepared && self.fit == nil && !self.posting && self.uptime - self.lastFrame > 6 {
                        self.stop("Camera frames stopped arriving with locked exposure. Start a new sightline.")
                        return
                    }
                }
            }
        }
    }

    private func observeMotion(_ reading: CMDeviceMotion?) {
        latestMotion = reading
        if recording, let pose {
            guard let (current, acceleration) = motionReading(), pose.holds(current, accelerationG: acceleration) else {
                stop("The phone moved or motion data was interrupted. Realign and start a new sightline."); return
            }
        }
    }

    private func motionReading() -> (FogPose, Double)? {
        guard let reading = latestMotion, abs(uptime - reading.timestamp) <= 1 else { return nil }
        let q = reading.attitude.quaternion, a = reading.userAcceleration
        return (FogPose(x: q.x, y: q.y, z: q.z, w: q.w), sqrt(a.x*a.x + a.y*a.y + a.z*a.z))
    }

    func start(fixture: String, path: Double) {
        guard prepared, !busy, !recording, let host, let endpoint,
              (fixture.isEmpty || host.fixtures.contains(where: { $0.id == fixture })), fogPathMetres.contains(path),
              let reading = lastReading, let (fixed, acceleration) = motionReading(),
              fixed.holds(fixed, accelerationG: acceleration) else {
            status = "Enter the measured sightline length and steady the phone."; return
        }
        do { var trial = FogBaseline(); try trial.accept(reading) }
        catch { quality = error.localizedDescription; return }
        let session = FogSession(endpoint: endpoint, host: host, http: http)
        self.session = session
        perform {
            let before = self.uptime
            var fields: [String: Any] = ["path-m": path]
            if !fixture.isEmpty { fields["fixture-id"] = fixture }
            _ = try await session.send("start", fields)
            let after = self.uptime
            try Task.checkCancellation()
            guard after - before <= 1 else { throw BrightnessCaptureFailure("Host round-trip exceeded one second. Reconnect on the local network.") }
            self.startTime = (after + before) / 2
            self.pose = fixed; self.lastTime = 0; self.baseline = FogBaseline()
            self.phase = .baseline; self.counts = [:]; self.transmission = nil; self.fit = nil
            self.recording = true
            self.status = "Recording clear air. Hold the target illumination constant."
        }
    }

    private func receive(_ frame: FogFrame) async {
        guard prepared else { return }
        lastFrame = uptime
        preview = frame.preview
        if recording, let pose {
            guard let (current, acceleration) = motionReading(), pose.holds(current, accelerationG: acceleration) else {
                stop("The phone moved or motion data was interrupted. Realign and start a new sightline."); return
            }
        }
        do { lastReading = try frame.reading.get(); quality = "" }
        catch { lastReading = nil; quality = error.localizedDescription; return }
        guard recording, !busy, let session, let reading = lastReading,
              frame.uptime >= startTime, frame.uptime - lastTime >= 0.95 else { return }
        let currentPhase = phase
        var nextBaseline = baseline
        if currentPhase == .baseline {
            do { try nextBaseline.accept(reading) }
            catch { quality = error.localizedDescription; return }
        }
        let sample = FogTraceSample(phase: currentPhase, reading: reading, cameraUptime: frame.uptime, sessionStart: startTime)
        lastTime = frame.uptime; posting = true
        let token = generation
        do {
            _ = try await session.send("samples", ["samples": [sample.fields]])
            guard token == generation else { return }
            if currentPhase == .baseline { baseline = nextBaseline }
            counts[currentPhase, default: 0] += 1
            transmission = baseline.contrast.map { reading.contrast / $0 }
            posting = false
        } catch {
            if token == generation { stop(error.localizedDescription) }
        }
    }

    func advance() {
        guard recording, !busy else { return }
        if phase == .baseline, baselineReady {
            phase = .active; status = "Operate the fogger with its usual controls. Mark clearing after release."
        } else if phase == .active, counts[.active, default: 0] >= 3 {
            phase = .clearing; status = "Leave output off and observe the contrast recovering."
        }
    }

    func fitSeries() {
        guard recording, !busy, !posting, phase == .clearing, let session else { return }
        perform {
            do {
                let reply = try await session.send("fit")
                try Task.checkCancellation()
                guard let fit = reply.fit else { throw BrightnessCaptureFailure("The host returned no fitted values.") }
                self.fit = fit; self.recording = false
                self.releaseCamera()
                self.status = "Review the measured values before applying them to this show."
            } catch {
                try Task.checkCancellation()
                self.status = error.localizedDescription
            }
        }
    }

    func apply() {
        guard !busy, fit != nil, let session else { return }
        perform {
            _ = try await session.send("apply")
            try Task.checkCancellation()
            self.session = nil
            self.stop("Fog calibration saved to the show.")
        }
    }

    private func perform(_ work: @escaping () async throws -> Void) {
        guard !busy else { return }
        let token = generation
        busy = true
        command = Task {
            defer { if token == self.generation { self.busy = false } }
            do { try await work() }
            catch { if token == self.generation { self.stop(error.localizedDescription) } }
        }
    }

    private func releaseCamera() {
        cameraTask?.cancel(); cameraTask = nil; watchdog?.cancel(); watchdog = nil
        motion.stopDeviceMotionUpdates(); latestMotion = nil; camera.stop(); prepared = false
        preview = nil; lastReading = nil
    }

    func stop(_ message: String = "Fog measurement stopped.") {
        let previous = session
        generation = UUID()
        command?.cancel(); command = nil
        releaseCamera()
        session = nil; recording = false; busy = false; posting = false; fit = nil
        counts = [:]; baseline = FogBaseline(); transmission = nil; quality = ""; pose = nil
        status = message
        if let previous { Task { _ = try? await previous.send("cancel") } }
    }
}
