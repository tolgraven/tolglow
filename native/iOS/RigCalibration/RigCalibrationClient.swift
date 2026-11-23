import Combine
import Foundation

/// Ring is the control/scan boundary; this is not a second OSC or lighting engine.
/// Mutations are never retried after an ambiguous network failure. The host lease
/// releases owned calibration cues if foreground heartbeats stop for 15 seconds.
@MainActor final class RigCalibrationClient: ObservableObject {
    let scanner = RigScanner()
    @Published private(set) var status = "Connect to Studio, map origin/+Z, then explicitly arm a scan"
    @Published private(set) var fixtures: [[String: Any]] = []
    @Published private(set) var scan: [String: Any]?
    @Published private(set) var busy = false
    @Published private(set) var armed = false
    @Published private(set) var outputCapable = false
    @Published private(set) var connected = false
    @Published private(set) var surfaceRevisions: [String: String] = [:]
    private var identity: [String: Any] = [:]
    private var baseURL: URL?
    private var polling: Task<Void, Never>?
    private var generation = UUID()
    private var finishRequested = false
    private var stopCameraRequested = false
    private struct HostRejection: LocalizedError {
        let message: String
        var errorDescription: String? { message }
    }
    private var ownedSessionID: String?
    private let http: URLSession = {
        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 4; config.timeoutIntervalForResource = 6
        config.urlCredentialStorage = nil
        config.httpShouldSetCookies = false
        return URLSession(configuration: config)
    }()
    init() {
        scanner.onInterrupted = { [weak self] in Task { @MainActor in self?.finish() } }
    }
    private func request(_ body: [String: Any]? = nil, path: String = "api/calibration") async throws -> [String: Any] {
        let requestGeneration = generation
        guard let baseURL else { throw URLError(.notConnectedToInternet) }
        var request = URLRequest(url: baseURL.appendingPathComponent(path))
        if let body {
            request.httpMethod = "POST"; request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try JSONSerialization.data(withJSONObject: identity.merging(body) { _, new in new })
        }
        let (bytes, response) = try await http.bytes(for: request, delegate: StudioRedirectPolicy.shared)
        try StudioOperatorAccess.checkSignIn(response)
        var data = Data()
        for try await byte in bytes {
            guard data.count < 2_097_152 else { throw URLError(.dataLengthExceedsMaximum) }
            data.append(byte)
        }
        let result = (try JSONSerialization.jsonObject(with: data)) as? [String: Any] ?? [:]
        guard requestGeneration == generation else { throw CancellationError() }
        if result["status"] as? String == "rejected" {
            throw HostRejection(message: result["message"] as? String ?? "Host rejected calibration request")
        }
        guard let response = response as? HTTPURLResponse, (200..<300).contains(response.statusCode) else {
            throw URLError(.badServerResponse)
        }
        return result
    }
    private func refresh() async throws {
        let result = try await request()
        identity = ["runtime-id": result["runtime-id"] ?? "", "expected-epoch": result["epoch"] ?? -1]
        connected = true
        surfaceRevisions = [:]
        for row in result["saved-surfaces"] as? [[String: String]] ?? [] {
            if let name = row["name"], let revision = row["revision"] { surfaceRevisions[name] = revision }
        }
        fixtures = result["fixtures"] as? [[String: Any]] ?? []
        scan = result["session"] as? [String: Any]
        outputCapable = result["output-capable?"] as? Bool ?? false
        armed = (scan?["armed?"] as? Bool ?? false) && (scan?["session-id"] as? String) == ownedSessionID
        scanner.captureEnabled = armed && !finishRequested
    }
    func connect(_ address: String) {
        guard !armed, !busy, let url = URL(string: address), ["http", "https"].contains(url.scheme ?? ""),
              url.host != nil, url.user == nil, url.password == nil, url.query == nil, url.fragment == nil,
              url.path.isEmpty || url.path == "/" else { status = "Finish the scan first and enter an HTTP(S) Studio origin"; return }
        connected = false; identity = [:]; outputCapable = false
        surfaceRevisions = [:]; scanner.clearSurface()
        baseURL = url
        perform { try await self.refresh(); self.status = self.outputCapable ? "Host connected; output needs explicit scan arming" : "Preview host: no physical output sink enabled" }
    }
    private func perform(_ operation: @escaping () async throws -> Void) {
        guard !busy else { return }
        busy = true; let token = generation
        Task {
            defer {
                if self.generation == token {
                    self.busy = false
                    if self.finishRequested { self.finish() }
                }
            }
            do { try await operation() }
            catch {
                if self.generation == token {
                    self.status = error.localizedDescription
                    self.scanner.captureEnabled = false
                    // Stop heartbeats; don't silently keep lighting after a failed control request.
                    self.polling?.cancel(); self.polling = nil
                }
            }
        }
    }
    func saveSurface(named name: String) {
        guard connected, !armed, let surface = scanner.surface, scanner.running else { return }
        var payload = surface.geometry
        payload["surface-name"] = name
        payload["expected-surface-revision"] = surfaceRevisions[name]
        perform {
            let receipt = try await self.request(payload, path: "api/venue-surface")
            guard receipt["status"] as? String == "accepted" else {
                throw HostRejection(message: "Host has not confirmed this measurement. Reconnect to check the saved surfaces.")
            }
            self.scanner.clearSurface()
            try await self.refresh()
            self.status = "Saved scan-\(name) to the show rig."
        }
    }
    func arm(selectedFixtures: [String]) {
        guard let alignment = scanner.worldToRig, !selectedFixtures.isEmpty, !armed else { return }
        finishRequested = false
        perform {
            // Bracket host clock with AR frame timestamps; a verified <=50 ms
            // frame age contributes 25 ms to this conservative offset interval.
            let before = try self.scanner.clockSample()
            let result = try await self.request()
            let after = try self.scanner.clockSample()
            guard let host = result["physical-now-ms"] as? Double else { throw URLError(.cannotParseResponse) }
            let lower = host - after - 50, upper = host - before
            let uncertainty = (upper - lower) / 2
            guard uncertainty >= 0, uncertainty <= 100 else {
                throw NSError(domain: "TolglowScan", code: 3, userInfo: [NSLocalizedDescriptionKey: "Clock uncertainty too high; improve local connection and tracking"])
            }
            let sid = UUID().uuidString
            _ = try await self.request(["op": "start", "session-id": sid, "fixture-ids": selectedFixtures,
                                        "world-id": self.scanner.worldID, "world-to-rig": alignment, "armed?": true,
                                        "clock": ["offset-ms": (lower + upper) / 2, "uncertainty-ms": uncertainty,
                                                  "device-clock": "arkit-frame-timestamp", "frame-age-bound-ms": 50]])
            self.ownedSessionID = sid
            try await self.refresh(); self.startPolling()
            self.status = "Armed. Hold each color for at least two beats; Next applies the first pattern."
        }
    }
    private func startPolling() {
        polling?.cancel()
        polling = Task {
            while !Task.isCancelled {
                try? await Task.sleep(for: .seconds(1))
                guard !Task.isCancelled, !busy, armed, let sid = scan?["session-id"] as? String else { continue }
                busy = true
                do {
                    let observations = scanner.takePending()
                    if !observations.isEmpty { _ = try await request(["op": "observe", "session-id": sid, "observations": observations]) }
                    if finishRequested { busy = false; finish(); return }
                    try Task.checkCancellation()
                    _ = try await request(["op": "heartbeat", "session-id": sid])
                    try await refresh()
                } catch {
                    status = "Capture delivery failed; host releases within 15 seconds. \(error.localizedDescription)"
                    scanner.captureEnabled = false; busy = false
                    if finishRequested { finish() }
                    return
                }
                busy = false
                if finishRequested { finish() }
            }
        }
    }
    func nextStep() {
        guard armed, let sid = scan?["session-id"] as? String else { return }
        perform {
            let previous = self.scan?["step-index"] as? Int ?? -1
            do {
                _ = try await self.request(["op": "step", "session-id": sid, "step-index": previous + 1])
            } catch let rejection as HostRejection {
                // The host did not advance. Keep the lease and capture only if this session still owns it.
                try await self.refresh()
                self.status = rejection.localizedDescription
                return
            }
            if (previous + 1) % 3 == 0 { self.scanner.clearBeamTrack() }
            try await self.refresh()
            self.status = "Pattern accepted. Receipt evidence begins only after physical publication and motor settling."
        }
    }
    func finish(stopCamera: Bool = false) {
        stopCameraRequested = stopCameraRequested || stopCamera
        scanner.captureEnabled = false
        finishRequested = true
        guard !busy else { return } // Allow the current observation POST to finish before draining the tail.
        polling?.cancel(); polling = nil
        finishRequested = false
        let shouldStopCamera = stopCameraRequested
        stopCameraRequested = false
        guard let sid = ownedSessionID, (scan?["session-id"] as? String) == sid else {
            if shouldStopCamera { scanner.stop() }
            return
        }
        perform {
            defer { if shouldStopCamera { self.scanner.stop() } }
            var observations = self.scanner.takePending()
            while !observations.isEmpty {
                _ = try await self.request(["op": "observe", "session-id": sid, "observations": observations])
                observations = self.scanner.takePending()
            }
            _ = try await self.request(["op": "cancel", "session-id": sid])
            try await self.refresh(); self.status = "Calibration overrides released. Captured evidence is retained for fitting."
        }
    }
    func solve(verifiedAxes: Bool) {
        guard !armed, let sid = scan?["session-id"] as? String else { return }
        perform {
            _ = try await self.request(["op": "solve", "session-id": sid,
                                        "verified-axis-fixture-ids": verifiedAxes ? (self.scan?["fixture-ids"] ?? []) : []])
            try await self.refresh(); self.status = "Review positions, residuals and mounting uncertainty before applying."
        }
    }
    func apply(_ fixtureID: String) {
        guard !armed, let sid = scan?["session-id"] as? String else { return }
        perform {
            _ = try await self.request(["op": "apply", "session-id": sid, "fixture-ids": [fixtureID]])
            try await self.refresh(); self.status = "Reviewed rig placement applied through validated admission."
        }
    }
    func suspend() {
        finishRequested = true
        scanner.captureEnabled = false; polling?.cancel(); polling = nil
        if let sid = scan?["session-id"] as? String, armed {
            // Best effort foreground cancellation; never rely on completion after suspension.
            Task { _ = try? await self.request(["op": "cancel", "session-id": sid]) }
        }
        generation = UUID(); busy = false; scanner.stop()
        status = "Backgrounded. Capture stopped; any unreachable host calibration lease expires within 15 seconds."
    }
}
