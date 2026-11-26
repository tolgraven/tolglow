import Foundation
import WebKit

/// Explicit, bounded observation of production shader translation. This is not
/// a benchmark: querying compile/link results can synchronise with the driver.
@MainActor
final class ShaderQualification: NSObject, WKScriptMessageHandler {
    private let destination: URL
    private let script: String
    private var events: [[String: Any]] = []
    private var eventBytes = 0
    private var truncated = false
    private var task: Task<Void, Never>?
    private(set) var result: String?
    var expectedRoute: String?

    init(destination: URL, script: String) {
        self.destination = destination
        self.script = script
    }

    static func fromArguments() -> ShaderQualification? {
        let args = CommandLine.arguments
        guard let index = args.firstIndex(of: "--qualify-metal"), args.indices.contains(index + 1) else { return nil }
        do {
            guard let source = Bundle.main.url(forResource: "ShaderQualification", withExtension: "js") else {
                throw NSError(domain: "ShaderQualification", code: 1,
                              userInfo: [NSLocalizedDescriptionKey: "Missing bundled shader qualification script"])
            }
            return ShaderQualification(destination: URL(fileURLWithPath: args[index + 1]),
                                       script: try String(contentsOf: source, encoding: .utf8))
        } catch {
            fputs("Shader qualification could not start: \(error)\n", stderr)
            return nil
        }
    }

    func configure(_ configuration: WKWebViewConfiguration) {
        configuration.userContentController.add(self, name: "shaderQualification")
        configuration.userContentController.addUserScript(WKUserScript(source: script,
            injectionTime: .atDocumentStart, forMainFrameOnly: true))
    }

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        guard result == nil, message.frameInfo.isMainFrame, let event = message.body as? [String: Any],
              let data = try? JSONSerialization.data(withJSONObject: event) else { return }
        guard events.count < 160, eventBytes + data.count <= 16 * 1024 * 1024 else { truncated = true; return }
        if event["type"] as? String == "limit"
            || (event["shaders"] as? [[String: Any]] ?? []).contains(where: { $0["truncated"] as? Bool == true }) {
            truncated = true
        }
        events.append(event)
        eventBytes += data.count
    }

    func start(_ view: WKWebView, revealRoom: Bool = false) {
        let snapshotScript = (revealRoom ? "window.__tolglowShaderQualificationRevealRoom?.(); " : "")
            + "window.__tolglowShaderQualificationSnapshot?.()"
        task = Task { [weak self, weak view] in
            var snapshot: [String: Any] = [:]
            var readySamples = 0
            let deadline = ContinuousClock.now.advanced(by: .seconds(60))
            while ContinuousClock.now < deadline {
                do { try await Task.sleep(nanoseconds: 500_000_000) } catch { return }
                guard let self, let view else { return }
                if revealRoom, (snapshot["documentFocused"] as? Bool) != true,
                   let window = view.window, window.isKeyWindow {
                    window.makeFirstResponder(view)
                }
                if let value = await Self.evaluate(view, snapshotScript) as? [String: Any] {
                    snapshot = value
                }
                guard !Task.isCancelled else { return }
                let previews = snapshot["previews"] as? [[String: Any]] ?? []
                let ready = previews.contains { ($0["ready"] as? Bool == true) && ($0["presented"] as? Bool == true) }
                readySamples = ready ? readySamples + 1 : 0
                if readySamples >= 4 { self.finish(snapshot, reason: "rendered"); return }
            }
            self?.finish(snapshot, reason: "timeout")
        }
    }

    static func evaluate(_ view: WKWebView, _ script: String) async -> Any? {
        // A failed WebContent process may never invoke its JS completion.
        await withCheckedContinuation { continuation in
            var finished = false
            let resolve: (Any?) -> Void = { value in
                guard !finished else { return }
                finished = true
                continuation.resume(returning: value)
            }
            view.evaluateJavaScript(script) { value, _ in
                resolve(value)
            }
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) { resolve(nil) }
        }
    }

    private func finish(_ snapshot: [String: Any], reason: String) {
        guard result == nil else { return }
        let programs = events.filter { $0["type"] as? String == "program" }
        let linked = programs.filter { $0["linked"] as? Bool == true }
        let shaders = linked.flatMap { $0["shaders"] as? [[String: Any]] ?? [] }
        let metal = shaders.filter {
            guard $0["compiled"] as? Bool == true, let text = $0["translated"] as? String else { return false }
            return text.contains("#include <metal_stdlib>") || text.contains("using namespace metal;")
        }
        let drew = events.contains { $0["type"] as? String == "draw" && $0["contextLost"] as? Bool == false }
        let previews = snapshot["previews"] as? [[String: Any]] ?? []
        let routePreserved = expectedRoute == nil || previews.contains {
            $0["route"] as? String == expectedRoute && $0["ready"] as? Bool == true && $0["presented"] as? Bool == true
        }
        let qualified = reason == "rendered" && drew && !linked.isEmpty && !metal.isEmpty
            && linked.count == programs.count && !truncated && routePreserved
        let status = qualified ? "qualified" : "unqualified"
        let receipt: [String: Any] = ["schema": 1, "issue": 493, "status": status,
            "reason": reason, "backendEvidence": metal.isEmpty ? "MSL not observable" : "translated MSL",
            "os": ProcessInfo.processInfo.operatingSystemVersionString,
            "webKitVersion": Bundle(for: WKWebView.self).object(forInfoDictionaryKey: "CFBundleVersion") as? String ?? "unavailable",
            "applicationBundle": Bundle.main.bundleIdentifier ?? "standalone native qualification runner",
            "date": ISO8601DateFormatter().string(from: Date()), "snapshot": snapshot,
            "linkedPrograms": linked.count, "metalShaders": metal.count, "drawObserved": drew,
            "expectedRoute": expectedRoute ?? "not compared", "routePreserved": routePreserved,
            "truncated": truncated, "events": events]
        do {
            let data = try JSONSerialization.data(withJSONObject: receipt, options: [.prettyPrinted, .sortedKeys])
            try data.write(to: destination, options: .atomic)
            result = status
            print("Studio Metal qualification: \(status); \(linked.count) linked programs, \(metal.count) MSL shaders. \(destination.path)")
        } catch {
            result = "error"
            fputs("Shader qualification receipt failed: \(error)\n", stderr)
        }
    }

    func stop(_ view: WKWebView) {
        task?.cancel()
        view.configuration.userContentController.removeScriptMessageHandler(forName: "shaderQualification")
    }
}

/// Shared by the shipped window and its native verification runner.
@MainActor
func studioWebView(origin: URL, navigation: StudioNavigation, qualification: ShaderQualification?,
                   rooms: RoomWindows? = nil, configure: ((WKWebViewConfiguration) -> Void)? = nil) -> WKWebView {
    let configuration = WKWebViewConfiguration()
    configuration.allowsAirPlayForMediaPlayback = true
    qualification?.configure(configuration)
    rooms?.configure(configuration)
    configure?(configuration)
    let view = StudioContentView(frame: .zero, configuration: configuration)
    view.navigationDelegate = navigation
    rooms?.attach(view)
    view.load(URLRequest(url: origin))
    qualification?.start(view, revealRoom: true)
    return view
}
