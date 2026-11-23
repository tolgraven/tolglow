import SwiftUI
import WebKit

/// Embed the existing re-frame application, with no parallel feature engine or
/// privileged native JavaScript bridge. Loading is an explicit user operation.
struct WebStudioView: View {
    @AppStorage("studio-origin") private var origin = "http://tolglow.local:8500"
    #if TOLGLOW_EMBEDDED_HOST
    @State private var localStudio = false
    #endif
    @State private var loadedURL: URL?
    @State private var loadID = UUID()
    @State private var status = "Open the configured studio to use its complete web interface"
    var body: some View {
        VStack(spacing: 8) {
            HStack {
                TextField("Studio origin", text: $origin)
                    .keyboardType(.URL).textInputAutocapitalization(.never).autocorrectionDisabled()
                Button("Open") {
                    guard let url = URL(string: origin), ["http", "https"].contains(url.scheme ?? ""),
                          url.host != nil, url.user == nil, url.password == nil,
                          url.query == nil, url.fragment == nil, url.path.isEmpty || url.path == "/" else { status = "Enter an HTTP(S) studio origin"; return }
                    loadID = UUID()
                    loadedURL = url; status = "Loading studio"
                }
                #if TOLGLOW_EMBEDDED_HOST
                Button("On this device") { localStudio = true }
                #endif
                if loadedURL != nil { Button("Close") { loadedURL = nil; status = "Studio closed" } }
            }.padding(.horizontal)
            Text(status).font(.caption2).foregroundStyle(.secondary).padding(.horizontal)
            if let loadedURL {
                OwnedStudioWebView(url: loadedURL, status: $status).id(loadID)
            } else {
                ContentUnavailableView("Web studio", systemImage: "globe", description: Text("Use the same re-frame cues, parameters, editors and tools served by your Tolglow host."))
            }
        }.padding(.top, 8)
        #if TOLGLOW_EMBEDDED_HOST
        .sheet(isPresented: $localStudio) { OnDeviceStudio() }
        #endif
    }
}

struct OwnedStudioWebView: UIViewRepresentable {
    let url: URL
    @Binding var status: String
    func makeCoordinator() -> Coordinator { Coordinator(origin: url, status: $status) }
    func makeUIView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.allowsInlineMediaPlayback = true
        configuration.allowsPictureInPictureMediaPlayback = true
        configuration.allowsAirPlayForMediaPlayback = true
        configuration.defaultWebpagePreferences.allowsContentJavaScript = true
        let view = WKWebView(frame: .zero, configuration: configuration)
        view.navigationDelegate = context.coordinator
        view.allowsBackForwardNavigationGestures = true
        view.load(URLRequest(url: url))
        return view
    }
    func updateUIView(_ uiView: WKWebView, context: Context) {}
    static func dismantleUIView(_ uiView: WKWebView, coordinator: Coordinator) {
        coordinator.active = false
        uiView.stopLoading(); uiView.navigationDelegate = nil
        // No native script handlers, motion services or retained webview singleton.
    }

    final class Coordinator: NSObject, WKNavigationDelegate {
        let origin: URL
        var status: Binding<String>
        var active = true
        init(origin: URL, status: Binding<String>) { self.origin = origin; self.status = status }
        private func sameOrigin(_ url: URL) -> Bool {
            func port(_ value: URL) -> Int { value.port ?? (value.scheme == "https" ? 443 : 80) }
            return url.scheme == origin.scheme && url.host == origin.host && port(url) == port(origin)
        }
        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                     decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
            guard active, let destination = navigationAction.request.url else { decisionHandler(.cancel); return }
            if sameOrigin(destination) { decisionHandler(.allow) }
            else {
                decisionHandler(.cancel)
                status.wrappedValue = "Navigation outside the configured studio was blocked"
            }
        }
        func webView(_ webView: WKWebView, didReceive challenge: URLAuthenticationChallenge,
                     completionHandler: @escaping (URLSession.AuthChallengeDisposition, URLCredential?) -> Void) {
            guard active else { completionHandler(.cancelAuthenticationChallenge, nil); return }
            StudioOperatorAccess.respond(to: challenge, requestURL: origin) { disposition, credential in
                if disposition == .rejectProtectionSpace {
                    self.status.wrappedValue = "Enter the password for this Studio in Connect"
                }
                completionHandler(disposition, credential)
            }
        }

        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            if active { status.wrappedValue = "Connected to web studio" }
        }
        func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
            if active { status.wrappedValue = error.localizedDescription }
        }
        func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
            if active { status.wrappedValue = error.localizedDescription }
        }
    }
}
