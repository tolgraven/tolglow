import AppKit
import WebKit

/// The Studio page in one show window: binds it to its show's runtime and
/// carries its view tabs (see docs/studio-ui.md, Windows and tabs).
///
/// Views of one show are tabs inside Studio, not native window tabs: a native
/// tab would be another WKWebView with its own state connection and preview
/// engine. So New Tab, Close Tab and tab cycling are menu items that ask the
/// page, and the page reports its active view and tab count for the title.
@MainActor
final class StudioPage: NSObject, ObservableObject, WKScriptMessageHandler {
    static let messageName = "tolglowStudioWindow"
    private static let tabsDefaultsKey = "studio-view-tabs"

    /// What the page reported: its active view's label and how many tabs it has.
    @Published private(set) var viewLabel: String?
    @Published private(set) var tabCount = 0
    weak var view: WKWebView?
    private var folderPath: String?
    private var origin: URL?

    /// Install the binding before the page's first script runs. `runtimeId`
    /// is the show this window started; Studio sends it with every command and
    /// refuses another runtime's state. Saved tabs come back per show folder,
    /// because the loopback port, and with it the page's storage, changes on
    /// every launch.
    func configure(_ configuration: WKWebViewConfiguration, origin: URL, runtimeId: String, show: String, folder: URL) {
        self.origin = origin
        folderPath = folder.standardizedFileURL.path
        var binding: [String: Any] = ["runtimeId": runtimeId, "show": show]
        if let folderPath, let tabs = (UserDefaults.standard.dictionary(forKey: Self.tabsDefaultsKey) as? [String: String])?[folderPath] {
            binding["tabs"] = tabs
        }
        let controller = configuration.userContentController
        controller.add(PageMessageHandler(self), name: Self.messageName)
        if let data = try? JSONSerialization.data(withJSONObject: binding),
           let json = String(data: data, encoding: .utf8) {
            controller.addUserScript(WKUserScript(source: "window.tolglowStudioWindow = Object.freeze(\(json));",
                                                  injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
    }

    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard message.frameInfo.isMainFrame, let origin,
              message.frameInfo.request.url.map({ $0.scheme == origin.scheme && $0.host == origin.host && $0.port == origin.port }) == true,
              let body = message.body as? [String: Any], body["type"] as? String == "tabs" else { return }
        viewLabel = body["label"] as? String
        tabCount = body["count"] as? Int ?? 0
        if let folderPath, let tabs = body["tabs"] as? String, tabs.utf8.count <= 4096 {
            var saved = UserDefaults.standard.dictionary(forKey: Self.tabsDefaultsKey) as? [String: String] ?? [:]
            saved[folderPath] = tabs
            UserDefaults.standard.set(saved, forKey: Self.tabsDefaultsKey)
        }
    }

    /// A tab menu action: new, close, reopen, next or previous.
    func send(_ action: String) {
        view?.evaluateJavaScript(
            "window.dispatchEvent(new CustomEvent('tolglow-view-tabs', {detail: {action: '\(action)'}}))")
    }

    func detach() {
        view = nil
        viewLabel = nil
        tabCount = 0
    }
}

/// WKUserContentController retains its handlers; keep the page collectable.
private final class PageMessageHandler: NSObject, WKScriptMessageHandler {
    weak var target: StudioPage?
    init(_ target: StudioPage) { self.target = target }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        MainActor.assumeIsolated { target?.userContentController(controller, didReceive: message) }
    }
}
