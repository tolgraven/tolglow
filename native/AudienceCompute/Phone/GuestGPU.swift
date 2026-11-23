import Foundation
import WebKit

/// An application-bundled script runs with a local canvas in an ephemeral
/// WKWebView. Messages pass as data arguments, never executable source strings.
@MainActor final class GuestGPU: NSObject, WKNavigationDelegate {
    let view: WKWebView
    private var document: CheckedContinuation<Void, Error>?

    override init() {
        let config = WKWebViewConfiguration()
        config.websiteDataStore = .nonPersistent()
        view = WKWebView(frame: .zero, configuration: config)
        super.init()
        view.navigationDelegate = self
    }

    func probe() async throws -> Bool {
        guard let file = Bundle.main.url(forResource: "audience-compute", withExtension: "js")
        else { return false }
        try await withCheckedThrowingContinuation { (ready: CheckedContinuation<Void, Error>) in
            document = ready
            view.load(URLRequest(url: URL(string: "about:blank")!))
        }
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

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        let ready = document; document = nil; ready?.resume()
    }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        failDocument(error)
    }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        failDocument(error)
    }
    private func failDocument(_ error: Error) {
        let waiting = document; document = nil; waiting?.resume(throwing: error)
    }
    func stop() {
        failDocument(ComputeFailure.unavailable)
        view.navigationDelegate = nil
        view.evaluateJavaScript("globalThis.workerGPU?.stop()")
        view.stopLoading()
    }
}
