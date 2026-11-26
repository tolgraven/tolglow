import AppKit
import WebKit

/// Hosts the windows Studio opens with `window.open`: the Room's (#665) and
/// any detached Perform panel's (#668), and places the Room on another
/// display. WebKit creates each window's web view from Studio's own
/// configuration, so it shares Studio's process and page group: Studio still
/// owns the state socket and the single preview engine, and portals the panel
/// into that window (see docs/studio-ui.md, Room in its own window and Panel
/// layouts). WKWebView has no Window Management API, so placement and closing
/// are script messages answered with a `tolglow-room-window` event on Studio's
/// window.
///
/// Studio identifies each window right after opening it with
/// `{action: "identify", role: "room" | "panel", id}`, posted from the new
/// window's own document (or, failing that, from Studio's, which names the
/// window opened last). Only the Room's window is placed, and closing a window
/// reports its role and id so the right panel redocks.
@MainActor
final class RoomWindows: NSObject, WKUIDelegate, WKNavigationDelegate, WKScriptMessageHandler, NSWindowDelegate {
    static let messageName = "tolglowRoomWindow"

    /// One window Studio opened.
    private final class Hosted {
        let window: NSWindow
        let view: WKWebView
        var role: String?
        var id: String?
        var title: NSKeyValueObservation?
        init(window: NSWindow, view: WKWebView) {
            self.window = window
            self.view = view
        }
    }

    private let origin: URL
    private weak var studio: WKWebView?
    /// In opening order.
    private var hosted: [Hosted] = []
    /// The window an identification from Studio's own document refers to.
    private weak var lastOpened: WKWebView?
    private var screensObserver: NSObjectProtocol?
    /// A move requested while the Room window was fullscreen waits for the exit animation.
    private var placeAfterFullScreen = false

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

    private var room: Hosted? { hosted.first { $0.role == "room" } }

    private func entry(for view: WKWebView) -> Hosted? { hosted.first { $0.view === view } }

    private func entry(for window: NSWindow) -> Hosted? { hosted.first { $0.window === window } }

    // MARK: Window lifetime

    func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration,
                 for action: WKNavigationAction, windowFeatures: WKWindowFeatures) -> WKWebView? {
        // Only Studio's main frame may open a window, and only as an empty
        // same-origin page. Each panel has at most one window; Studio enforces it.
        guard webView === studio, action.sourceFrame.isMainFrame, sameOrigin(action.sourceFrame.request.url),
              action.request.url.map({ $0.absoluteString.isEmpty || $0.absoluteString == "about:blank" }) ?? true
        else { return nil }
        let size = NSSize(width: windowFeatures.width?.doubleValue ?? 960, height: windowFeatures.height?.doubleValue ?? 640)
        let view = WKWebView(frame: NSRect(origin: .zero, size: size), configuration: configuration)
        view.uiDelegate = self
        view.navigationDelegate = self
        let window = NSWindow(contentRect: NSRect(origin: .zero, size: size),
                              styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
        window.title = "Tolglow"
        window.isReleasedWhenClosed = false
        window.collectionBehavior.insert(.fullScreenPrimary)
        window.contentView = view
        window.delegate = self
        let entry = Hosted(window: window, view: view)
        // Studio titles each window ("Tolglow · Live") once it has prepared it.
        entry.title = view.observe(\.title, options: [.new]) { [weak window] view, _ in
            MainActor.assumeIsolated {
                if let title = view.title, !title.isEmpty { window?.title = title }
            }
        }
        if let last = hosted.last?.window {
            // Below and to the right of the window opened before it.
            window.setFrameTopLeftPoint(NSPoint(x: last.frame.minX + 24, y: last.frame.maxY - 24))
        } else if let parent = studio?.window {
            window.setFrameOrigin(NSPoint(x: parent.frame.maxX - size.width, y: parent.frame.minY))
        }
        window.makeKeyAndOrderFront(nil)
        hosted.append(entry)
        lastOpened = view
        return view
    }

    /// Studio called `close()` on one of its windows.
    func webViewDidClose(_ webView: WKWebView) {
        if let entry = entry(for: webView) { close(entry) }
    }

    /// The operator closed a window: let Studio redock that panel.
    func windowWillClose(_ notification: Notification) {
        guard let window = notification.object as? NSWindow, let entry = entry(for: window) else { return }
        release(entry)
        reply(Self.closedReply(role: entry.role, id: entry.id))
    }

    /// Every window shares Studio's web content process. If that process ends,
    /// Studio reloads docked and the windows would be left showing nothing.
    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        if entry(for: webView) != nil { close() }
    }

    /// No window outlives Studio's web view.
    func close() {
        for entry in hosted { close(entry) }
    }

    private func close(_ entry: Hosted) {
        release(entry)
        entry.window.delegate = nil
        entry.window.close()
    }

    private func release(_ entry: Hosted) {
        hosted.removeAll { $0 === entry }
        if entry.role == "room" { placeAfterFullScreen = false }
        entry.title?.invalidate()
        entry.title = nil
        entry.view.stopLoading()
        entry.window.contentView = nil
    }

    /// A Studio window shows Studio's empty page and nothing else.
    func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        let url = action.request.url
        decisionHandler(url == nil || url?.absoluteString == "about:blank" || sameOrigin(url) ? .allow : .cancel)
    }

    // MARK: Messages

    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        let origin = message.frameInfo.securityOrigin
        guard message.frameInfo.isMainFrame, origin.protocol == self.origin.scheme, origin.host == self.origin.host,
              origin.port == (self.origin.port ?? 0), let body = message.body as? [String: Any] else { return }
        let fromStudio = message.webView === studio
        switch body["action"] as? String {
        case "identify":
            // From the window's own document, or from Studio's about the window it opened last.
            let target = fromStudio ? lastOpened : message.webView
            guard let target, let entry = entry(for: target),
                  let role = Self.role(body["role"]) else { return }
            if role == "room" { for other in hosted where other !== entry && other.role == "room" { other.role = nil } }
            entry.role = role
            entry.id = body["id"] as? String
        case "move-to-display" where fromStudio:
            moveToDisplay()
        default:
            return
        }
    }

    /// The roles a window can have; anything else is ignored.
    static func role(_ value: Any?) -> String? {
        guard let role = value as? String, role == "room" || role == "panel" else { return nil }
        return role
    }

    /// What Studio hears when a window closes: a Room window's close is the
    /// Room's, a panel's carries its id, and an unidentified window's nothing.
    static func closedReply(role: String?, id: String?) -> [String: Any] {
        var detail: [String: Any] = ["status": "closed"]
        if let role { detail["role"] = role }
        if let id { detail["id"] = id }
        return detail
    }

    // MARK: Placement

    private func moveToDisplay() {
        // A move already waiting for the fullscreen exit places and replies;
        // a second toggle or frame during that animation would undo or lose it.
        guard let window = room?.window, !placeAfterFullScreen else { return }
        let screens = NSScreen.screens
        let studioScreen = studio?.window?.screen.flatMap { screens.firstIndex(of: $0) }
        let roomScreen = window.screen.flatMap { screens.firstIndex(of: $0) }
        guard let target = Self.target(count: screens.count, studio: studioScreen, room: roomScreen) else {
            reply(["status": "single", "role": "room"])
            return
        }
        let screen = screens[target]
        // Leaving fullscreen is animated, and a frame set during it is lost.
        // Place the window once it has left fullscreen.
        if window.styleMask.contains(.fullScreen) {
            placeAfterFullScreen = true
            window.toggleFullScreen(nil)
            return
        }
        window.setFrame(screen.visibleFrame, display: true)
        if window.screen == screen {
            reply(["status": "placed", "role": "room", "label": screen.localizedName])
        } else {
            reply(["status": "manual", "role": "room"])
        }
    }

    func windowDidExitFullScreen(_ notification: Notification) {
        guard placeAfterFullScreen, notification.object as? NSWindow === room?.window else { return }
        placeAfterFullScreen = false
        moveToDisplay()
    }

    func windowDidFailToExitFullScreen(_ window: NSWindow) {
        guard placeAfterFullScreen, window === room?.window else { return }
        placeAfterFullScreen = false
        reply(["status": "manual", "role": "room"])
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
        reply(["status": "screens", "role": "room", "count": NSScreen.screens.count])
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
