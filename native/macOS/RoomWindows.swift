import AppKit
import WebKit

/// Hosts the Room window that Studio opens with `window.open` and places it on
/// another display. WebKit creates the window's web view from Studio's own
/// configuration, so it shares Studio's process and page group: Studio still
/// owns the single preview engine and portals the Room into that window (see
/// docs/studio-ui.md, Room in its own window). WKWebView has no Window
/// Management API, so placement is a script message answered with a
/// `tolglow-room-window` event on Studio's window.
@MainActor
final class RoomWindows: NSObject, WKUIDelegate, WKNavigationDelegate, WKScriptMessageHandler, NSWindowDelegate {
    static let messageName = "tolglowRoomWindow"
    private let origin: URL
    private weak var studio: WKWebView?
    private var room: (window: NSWindow, view: WKWebView)?
    private var screensObserver: NSObjectProtocol?

    init(origin: URL) {
        self.origin = origin
        super.init()
        screensObserver = NotificationCenter.default.addObserver(
            forName: NSApplication.didChangeScreenParametersNotification, object: nil, queue: .main) { [weak self] _ in
            MainActor.assumeIsolated { self?.screensChanged() }
        }
    }

    deinit {
        if let screensObserver { NotificationCenter.default.removeObserver(screensObserver) }
    }

    /// Install on Studio's configuration before its web view is created.
    func configure(_ configuration: WKWebViewConfiguration) {
        configuration.userContentController.add(WeakMessageHandler(self), name: Self.messageName)
        // The Room's Fullscreen action fills the display it was moved to.
        configuration.preferences.isElementFullscreenEnabled = true
    }

    func attach(_ view: WKWebView) {
        studio = view
        view.uiDelegate = self
    }

    // MARK: Window lifetime

    func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration,
                 for action: WKNavigationAction, windowFeatures: WKWindowFeatures) -> WKWebView? {
        // Only Studio's main frame may open the Room window, and only as an
        // empty same-origin page; one Room window at a time.
        guard webView === studio, action.sourceFrame.isMainFrame, sameOrigin(action.sourceFrame.request.url),
              action.request.url.map({ $0.absoluteString.isEmpty || $0.absoluteString == "about:blank" }) ?? true
        else { return nil }
        close()
        let size = NSSize(width: windowFeatures.width?.doubleValue ?? 960, height: windowFeatures.height?.doubleValue ?? 640)
        let view = WKWebView(frame: NSRect(origin: .zero, size: size), configuration: configuration)
        view.uiDelegate = self
        view.navigationDelegate = self
        let window = NSWindow(contentRect: NSRect(origin: .zero, size: size),
                              styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
        window.title = "Tolglow Room"
        window.isReleasedWhenClosed = false
        window.collectionBehavior.insert(.fullScreenPrimary)
        window.contentView = view
        window.delegate = self
        if let parent = studio?.window { window.setFrameOrigin(NSPoint(x: parent.frame.maxX - size.width, y: parent.frame.minY)) }
        window.makeKeyAndOrderFront(nil)
        room = (window, view)
        return view
    }

    /// Studio called `close()` on the Room window.
    func webViewDidClose(_ webView: WKWebView) {
        if webView === room?.view { close() }
    }

    /// The operator closed the Room window: let Studio redock.
    func windowWillClose(_ notification: Notification) {
        guard let current = room, notification.object as? NSWindow === current.window else { return }
        room = nil
        current.view.stopLoading()
        current.window.contentView = nil
        reply(["status": "closed"])
    }

    /// The Room window never outlives Studio's web view.
    func close() {
        guard let current = room else { return }
        room = nil
        current.window.delegate = nil
        current.view.stopLoading()
        current.window.contentView = nil
        current.window.close()
    }

    /// The Room window shows Studio's empty page and nothing else.
    func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        let url = action.request.url
        decisionHandler(url == nil || url?.absoluteString == "about:blank" || sameOrigin(url) ? .allow : .cancel)
    }

    // MARK: Placement

    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard message.frameInfo.isMainFrame, sameOrigin(message.frameInfo.request.url),
              let body = message.body as? [String: Any], body["action"] as? String == "move-to-display" else { return }
        moveToDisplay()
    }

    private func moveToDisplay() {
        guard let window = room?.window else { return }
        let screens = NSScreen.screens
        let studioScreen = studio?.window?.screen.flatMap { screens.firstIndex(of: $0) }
        let roomScreen = window.screen.flatMap { screens.firstIndex(of: $0) }
        guard let target = Self.target(count: screens.count, studio: studioScreen, room: roomScreen) else {
            reply(["status": "single"])
            return
        }
        let screen = screens[target]
        if window.styleMask.contains(.fullScreen) { window.toggleFullScreen(nil) }
        window.setFrame(screen.visibleFrame, display: true)
        if window.screen == screen {
            reply(["status": "placed", "label": screen.localizedName])
        } else {
            reply(["status": "manual"])
        }
    }

    /// A display other than Studio's; the next free one when the Room is
    /// already on a free display, so repeated requests cycle.
    static func target(count: Int, studio: Int?, room: Int?) -> Int? {
        let free = (0..<count).filter { $0 != studio }
        guard !free.isEmpty else { return nil }
        if let room, let at = free.firstIndex(of: room) { return free[(at + 1) % free.count] }
        return free.first
    }

    private func screensChanged() {
        guard room != nil else { return }
        reply(["status": "screens", "count": NSScreen.screens.count])
    }

    // MARK: Helpers

    private func sameOrigin(_ url: URL?) -> Bool {
        url?.scheme == origin.scheme && url?.host == origin.host && url?.port == origin.port
    }

    private func reply(_ detail: [String: Any]) {
        guard let studio, let data = try? JSONSerialization.data(withJSONObject: detail),
              let json = String(data: data, encoding: .utf8) else { return }
        studio.evaluateJavaScript("window.dispatchEvent(new CustomEvent('tolglow-room-window', {detail: \(json)}))")
    }
}

/// WKUserContentController retains its handlers; keep the owner collectable.
private final class WeakMessageHandler: NSObject, WKScriptMessageHandler {
    weak var target: (any WKScriptMessageHandler)?
    init(_ target: any WKScriptMessageHandler) { self.target = target }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        target?.userContentController(controller, didReceive: message)
    }
}
