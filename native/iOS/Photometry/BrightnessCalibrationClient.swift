import Combine
import Foundation
import TolglowSensorCore

struct PhotometryFixture: Decodable, Identifiable {
    let id: String
    let type: String
    let name: String?
}

private struct PhotometryHost: Decodable {
    let runtime: String
    let epoch: Int
    let output: Bool
    let fixtures: [PhotometryFixture]
    enum CodingKeys: String, CodingKey {
        case runtime = "runtime-id", epoch, output = "output-capable?", fixtures
    }
}

private struct BrightnessReply: Decodable {
    let status: String?
    let message: String?
    let phase: String?
    let fixture: String?
    let presented: Bool?
    enum CodingKeys: String, CodingKey {
        case status, message, phase, fixture = "fixture-id", presented = "presented?"
    }
}

/// Immutable destination and show identity keep a late cancel confined to its
/// original session even after the operator reconnects to another host.
private struct BrightnessLease {
    let url: URL
    let runtime: String
    let epoch: Int
    let session = UUID().uuidString
    let ids: [String]
    let http: URLSession

    func send(_ op: String, _ fields: [String: Any] = [:]) async throws -> BrightnessReply {
        var req = URLRequest(url: url)
        req.httpMethod = "POST"
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        var body: [String: Any] = ["runtime-id": runtime, "expected-epoch": epoch, "session-id": session, "op": op]
        body.merge(fields) { _, new in new }
        req.httpBody = try JSONSerialization.data(withJSONObject: body)
        let reply = try await brightnessJSON(BrightnessReply.self, req, http)
        guard reply.status == "accepted" else { throw BrightnessCaptureFailure(reply.message ?? "Show rejected the calibration step.") }
        return reply
    }
}

private func brightnessJSON<T: Decodable>(_ type: T.Type, _ request: URLRequest, _ http: URLSession) async throws -> T {
    let (bytes, response) = try await http.bytes(for: request)
    var data = Data()
    for try await byte in bytes {
        guard data.count < 512 * 1024 else { throw URLError(.dataLengthExceedsMaximum) }
        data.append(byte)
    }
    guard let code = (response as? HTTPURLResponse)?.statusCode, (200..<300).contains(code) else {
        let error = try? JSONDecoder().decode(BrightnessReply.self, from: data)
        throw BrightnessCaptureFailure(error?.message ?? "Show could not confirm calibration.")
    }
    return try JSONDecoder().decode(type, from: data)
}

@MainActor final class BrightnessCalibrationClient: ObservableObject {
    let camera = PhotometryCamera()
    @Published private(set) var fixtures: [PhotometryFixture] = []
    @Published private(set) var illuminated: String?
    @Published private(set) var running = false
    @Published private(set) var busy = false
    @Published private(set) var outputAvailable = false
    @Published private(set) var status = "Connect to the lighting host to choose fixture types."
    @Published private(set) var readings: [String: BrightnessReading] = [:]
    private var host: PhotometryHost?
    private var url: URL?
    private var lease: BrightnessLease?
    private var action: Task<Void, Never>?
    private var renewal: Task<Void, Never>?
    private var generation = UUID()
    private let http: URLSession = {
        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 4
        config.timeoutIntervalForResource = 6
        return URLSession(configuration: config)
    }()

    init() {
        camera.onInterruption = { [weak self] in
            Task { @MainActor in self?.stop("Camera interrupted. Start a fresh calibration.") }
        }
    }

    func connect(_ address: String) {
        guard !busy, !running else { return }
        host = nil; url = nil; fixtures = []; readings = [:]; outputAvailable = false
        guard let origin = URL(string: address), ["http", "https"].contains(origin.scheme ?? ""),
              origin.host != nil, origin.user == nil, origin.password == nil, origin.query == nil,
              origin.fragment == nil, ["", "/"].contains(origin.path) else {
            status = "Enter an HTTP(S) Studio origin."; return
        }
        let endpoint = origin.appendingPathComponent("api/photometry")
        perform {
            let result = try await brightnessJSON(PhotometryHost.self, URLRequest(url: endpoint), self.http)
            try Task.checkCancellation()
            self.host = result; self.url = endpoint
            self.fixtures = result.fixtures
            self.outputAvailable = result.output
            self.status = result.output ? "Select one fixture per type and choose your reference." : "Host has no physical output enabled."
        }
    }

    func originChanged() {
        guard !busy, !running else { return }
        host = nil; url = nil; fixtures = []; readings = [:]; outputAvailable = false
        status = "Connect to this Studio origin before arming output."
    }

    func start(ids: [String], shutter: Int32) {
        guard !busy, !running, let host, let url, host.output, !ids.isEmpty,
              Set(ids).count == ids.count else { return }
        let selected = ids.compactMap { id in fixtures.first(where: { $0.id == id }) }
        guard selected.count == ids.count, Set(selected.map(\.type)).count == ids.count else {
            status = "Choose one representative per fixture type."; return
        }
        let session = BrightnessLease(url: url, runtime: host.runtime, epoch: host.epoch, ids: ids, http: http)
        readings = [:]
        perform {
            self.status = "Locking camera exposure, focus and white balance…"
            try await self.camera.start(shutter: shutter)
            try Task.checkCancellation()
            self.lease = session
            _ = try await session.send("start", ["fixture-ids": ids, "armed?": true])
            try Task.checkCancellation()
            self.running = true
            self.renew(session)
            try await self.light(ids[0], session)
        }
    }

    private func renew(_ session: BrightnessLease) {
        renewal?.cancel()
        renewal = Task {
            do {
                while !Task.isCancelled {
                    try await Task.sleep(for: .seconds(2))
                    _ = try await session.send("heartbeat")
                }
            } catch {
                if !Task.isCancelled, lease?.session == session.session {
                    stop("Lost the calibration lease. " + error.localizedDescription)
                }
            }
        }
    }

    private func setPhase(_ phase: String, _ id: String, _ session: BrightnessLease) async throws {
        var reply = try await session.send("step", ["fixture-id": id, "phase": phase])
        let clock = ContinuousClock(), deadline = ContinuousClock.now.advanced(by: .seconds(8))
        while reply.fixture != id || reply.phase != phase || reply.presented != true {
            guard clock.now < deadline else { throw BrightnessCaptureFailure("Host did not confirm physical output.") }
            try await Task.sleep(for: .milliseconds(250))
            reply = try await session.send("heartbeat")
        }
        try Task.checkCancellation()
        illuminated = phase == "white" ? id : nil
    }

    private func light(_ id: String, _ session: BrightnessLease) async throws {
        try await setPhase("white", id, session)
        status = "Full white · " + id + ". Center the diffuser one metre from its lens, then capture."
    }

    func select(_ id: String) {
        guard !busy, running, let lease, lease.ids.contains(id) else { return }
        perform { try await self.light(id, lease) }
    }

    private func measure(_ phase: String, _ id: String, _ session: BrightnessLease) async throws -> Double {
        try await setPhase(phase, id, session)
        let reading = try await camera.measure()
        try Task.checkCancellation()
        let check = try await session.send("heartbeat")
        guard check.presented == true, check.fixture == id, check.phase == phase else {
            throw BrightnessCaptureFailure("The host output changed during capture.")
        }
        return reading
    }

    func capture(_ id: String) {
        guard !busy, running, let lease, lease.ids.contains(id) else { return }
        perform {
            do {
                self.status = "Measuring background · " + id
                let dark = try await self.measure("dark", id, lease)
                self.status = "Measuring full white · " + id
                let white = try await self.measure("white", id, lease)
                let pair = try BrightnessReading(dark: dark, white: white)
                self.readings[id] = pair
                if let next = lease.ids.first(where: { self.readings[$0] == nil }) {
                    try await self.light(next, lease)
                } else {
                    try await self.setPhase("dark", id, lease)
                    self.status = "All readings captured. Review the ratios and save."
                }
            } catch let error as PhotometryError {
                // A failed repeat preserves the previous complete dark/white pair.
                try await self.setPhase("dark", id, lease)
                self.status = error.localizedDescription
            }
        }
    }

    func apply() {
        guard !busy, running, let lease, lease.ids.allSatisfy({ readings[$0] != nil }) else { return }
        let rows: [[String: Any]] = lease.ids.compactMap { id in
            guard let reading = readings[id] else { return nil }
            return ["fixture-id": id, "dark": reading.dark, "white": reading.white]
        }
        renewal?.cancel(); renewal = nil
        perform {
            _ = try await lease.send("apply", ["readings": rows, "distance-m": 1.0])
            try Task.checkCancellation()
            self.lease = nil
            self.stop("Relative brightness saved to the show.")
        }
    }

    private func perform(_ work: @escaping () async throws -> Void) {
        guard !busy else { return }
        let token = generation
        busy = true
        action = Task {
            defer { if self.generation == token { self.busy = false } }
            do { try await work() }
            catch { if self.generation == token { self.stop(error.localizedDescription) } }
        }
    }

    func stop(_ message: String = "Calibration stopped.") {
        let previous = lease
        generation = UUID()
        action?.cancel(); action = nil
        renewal?.cancel(); renewal = nil
        running = false; busy = false; illuminated = nil; lease = nil; camera.stop()
        status = message
        if let previous {
            // The host lease also expires when a backgrounded or disconnected
            // iPhone cannot deliver cancellation.
            Task { _ = try? await previous.send("cancel") }
        }
    }
}
