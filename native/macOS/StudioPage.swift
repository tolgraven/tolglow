import AppKit
import WebKit

/// The Studio page in one show window: binds it to its show's runtime and
/// carries its view tabs and Perform panel layouts (see docs/studio-ui.md,
/// Windows and tabs).
///
/// Views of one show are tabs inside Studio, not native window tabs: a native
/// tab would be another WKWebView with its own state connection and preview
/// engine. So New Tab, Close Tab and tab cycling are menu items that ask the
/// page, and the page reports its active view and tab count for the title.
@MainActor
final class StudioPage: NSObject, ObservableObject, WKScriptMessageHandler {
    static let messageName = "tolglowStudioWindow"

    /// What the page reported: its active view's label and how many tabs it has.
    @Published private(set) var viewLabel: String?
    @Published private(set) var tabCount = 0
    weak var view: WKWebView?
    private var folderPath: String?
    private var origin: URL?

    /// Install the binding before the page's first script runs. `runtimeId`
    /// is the show this window started; Studio sends it with every command and
    /// refuses another runtime's state. Saved tabs and panel layouts come back
    /// per show folder, because the loopback port, and with it the page's
    /// storage, changes on every launch.
    func configure(_ configuration: WKWebViewConfiguration, origin: URL, runtimeId: String, show: String, folder: URL) {
        self.origin = origin
        folderPath = folder.standardizedFileURL.path
        var binding: [String: Any] = ["runtimeId": runtimeId, "show": show]
        if let folderPath {
            if let tabs = FolderStore.tabs.value(for: folderPath) { binding["tabs"] = tabs }
            if let layouts = FolderStore.layouts.value(for: folderPath) { binding["layouts"] = layouts }
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
              let body = message.body as? [String: Any] else { return }
        switch body["type"] as? String {
        case "tabs":
            viewLabel = body["label"] as? String
            tabCount = body["count"] as? Int ?? 0
            if let folderPath, let tabs = body["tabs"] as? String { FolderStore.tabs.save(tabs, for: folderPath) }
        case "layouts":
            // Panel layouts (#668): the page posts its saved JSON on every save and
            // counts the save as kept only once the app says it kept its copy.
            let kept = folderPath.flatMap { folder in
                (body["layouts"] as? String).map { FolderStore.layouts.save($0, for: folder) }
            } ?? false
            if let request = body["request"] as? Int,
               let json = Self.layoutsReply(request: request, kept: kept) {
                message.webView?.evaluateJavaScript(
                    "window.dispatchEvent(new CustomEvent('tolglow-studio-window', {detail: \(json)}))")
            }
        default:
            return
        }
    }

    /// The page's answer to a layouts message: whether the copy was kept (a copy
    /// over the store's limit is refused). Nil when it cannot be encoded.
    static func layoutsReply(request: Int, kept: Bool) -> String? {
        let detail: [String: Any] = ["type": "layouts", "request": request, "kept": kept]
        guard let data = try? JSONSerialization.data(withJSONObject: detail) else { return nil }
        return String(data: data, encoding: .utf8)
    }

    /// A tab menu action (new, close, reopen, next or previous), or
    /// "checkpoints" to open the page's Show checkpoints dialog.
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
