import Foundation
import WebKit

/// An application-bundled script runs with a local canvas in an ephemeral
/// WKWebView. Messages pass as data arguments, never executable source strings.
@MainActor final class GuestGPU {
    let view: WKWebView

    init() {
        let config = WKWebViewConfiguration()
        config.websiteDataStore = .nonPersistent()
        view = WKWebView(frame: .zero, configuration: config)
    }

    func probe() async throws -> Bool {
        guard let file = Bundle.main.url(forResource: "audience-compute", withExtension: "js")
        else { return false }
        let library = try String(contentsOf: file, encoding: .utf8)
        try await view.evaluateJavaScript(library)
        return try await view.callAsyncJavaScript(
            "globalThis.workerGPU = AudienceGPU.start(document.createElement('canvas')); return await workerGPU.probe()",
            arguments: [:], in: nil, contentWorld: .page) as? Bool == true
    }

    func run(_ bytes: Data) async throws -> Data {
        guard let packet = String(data: bytes, encoding: .utf8) else { throw ComputeFailure.malformed }
        guard let replies = try await view.callAsyncJavaScript("return await workerGPU.compute(packet)",
            arguments: ["packet":packet], in: nil, contentWorld: .page) as? [String] else { throw ComputeFailure.unavailable }
        return try JSONSerialization.data(withJSONObject: replies)
    }

    func stop() { view.evaluateJavaScript("globalThis.workerGPU?.stop()"); view.stopLoading() }
}
