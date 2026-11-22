import Combine
import Foundation

struct StudioCue: Decodable, Identifiable {
    let id: String
    let name: String?
    let swatch: String?
}
struct StudioInstance: Decodable, Identifiable {
    let id: String
    let cueID: String
    enum CodingKeys: String, CodingKey { case id; case cueID = "cue-id" }
}
struct StudioSnapshot: Decodable {
    let runtimeID: String
    let showID: String
    let epoch: Int
    let master: Double
    let blackout: Bool
    let cues: [StudioCue]
    let instances: [StudioInstance]
    enum CodingKeys: String, CodingKey {
        case runtimeID = "runtime-id", showID = "show-id", blackout = "blackout?"
        case epoch, master, cues, instances
    }
}

/// Uses the same semantic Ring command API as the re-frame studio. No second
/// cue engine or optimistic lifecycle. Commands are explicit, guarded, not retried.
@MainActor final class StudioClient: ObservableObject {
    @Published private(set) var snapshot: StudioSnapshot?
    @Published private(set) var status = "Studio disconnected"
    @Published private(set) var busy = false
    private var baseURL: URL?
    private var generation = UUID()
    private var task: Task<Void, Never>?
    private let sessionID = "ios-" + UUID().uuidString
    private let http: URLSession = {
        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 5; config.timeoutIntervalForResource = 8
        config.urlCredentialStorage = nil
        config.httpShouldSetCookies = false
        return URLSession(configuration: config)
    }()

    func connect(_ address: String) {
        disconnect()
        guard let url = URL(string: address), ["http", "https"].contains(url.scheme ?? ""),
              url.host != nil, url.user == nil, url.password == nil, url.query == nil, url.fragment == nil,
              url.path.isEmpty || url.path == "/"
        else { status = "Enter an HTTP(S) studio origin"; return }
        baseURL = url; refresh()
    }
    func disconnect() {
        generation = UUID(); task?.cancel(); task = nil; baseURL = nil
        snapshot = nil; busy = false; status = "Studio disconnected"
    }
    private func request(_ path: String, body: [String: Any]? = nil) async throws -> Data {
        guard let baseURL else { throw URLError(.notConnectedToInternet) }
        var request = URLRequest(url: baseURL.appendingPathComponent(path))
        if let body {
            request.httpMethod = "POST"; request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try JSONSerialization.data(withJSONObject: body)
        }
        let (bytes, response) = try await http.bytes(for: request, delegate: StudioRedirectPolicy.shared)
        guard let response = response as? HTTPURLResponse, (200..<300).contains(response.statusCode) else {
            throw URLError(.badServerResponse)
        }
        var data = Data()
        for try await byte in bytes {
            guard data.count < 4_194_304 else { throw URLError(.dataLengthExceedsMaximum) }
            data.append(byte)
        }
        return data
    }
    func refresh() {
        guard !busy, baseURL != nil else { return }
        busy = true
        let token = generation
        task = Task {
            defer { if generation == token { busy = false } }
            do {
                let current = try JSONDecoder().decode(StudioSnapshot.self, from: await request("api/state"))
                guard generation == token else { return }
                snapshot = current; status = "Connected — refresh to observe external changes"
            } catch { if generation == token { status = error.localizedDescription } }
        }
    }
    func command(_ op: String, fields: [String: Any] = [:]) {
        guard !busy, let snapshot else { return }
        busy = true
        let token = generation
        var body = fields
        body.merge(["op": op, "runtime-id": snapshot.runtimeID, "expected-epoch": snapshot.epoch,
                    "show-id": snapshot.showID, "epoch": snapshot.epoch,
                    "command-id": UUID().uuidString,
                    "source": ["adapter": "native-ios", "session-id": sessionID]]) { _, new in new }
        task = Task {
            defer { if generation == token { busy = false } }
            do {
                let data = try await request("api/command", body: body)
                let response = try JSONSerialization.jsonObject(with: data) as? [String: Any]
                let ack = (response?["ack"] as? [String: Any]) ?? response
                guard ack?["status"] as? String == "accepted" else { throw URLError(.cannotParseResponse) }
                let current = try JSONDecoder().decode(StudioSnapshot.self, from: await request("api/state"))
                guard generation == token else { return }
                self.snapshot = current; status = "Command accepted"
            } catch { if generation == token { status = "Not confirmed: " + error.localizedDescription } }
        }
    }
    func toggle(_ cue: StudioCue) {
        if let instance = snapshot?.instances.first(where: { $0.cueID == cue.id }) {
            command("instance/end", fields: ["instance-id": instance.id])
        } else { command("cue/launch", fields: ["cue-id": cue.id, "launch": ["mode": "persistent"]]) }
    }
}
