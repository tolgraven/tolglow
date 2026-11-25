import AppKit
import WebKit

final class StudioContentView: WKWebView {
    private var needsInitialFocus = true

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        let center = NotificationCenter.default
        center.removeObserver(self, name: NSWindow.didBecomeKeyNotification, object: nil)
        guard needsInitialFocus, let window else { return }
        center.addObserver(self, selector: #selector(focusWhenKey(_:)), name: NSWindow.didBecomeKeyNotification, object: window)
        focusWhenKey()
    }

    @objc private func focusWhenKey(_ notification: Notification? = nil) {
        guard needsInitialFocus, let window, window.isKeyWindow else { return }
        // SwiftUI attaches the show view after startup. Wait for the attachment
        // to settle before giving WebKit its initial keyboard focus.
        DispatchQueue.main.async { [weak self, weak window] in
            guard let self, let window, self.window === window, window.isKeyWindow, self.needsInitialFocus else { return }
            if window.makeFirstResponder(self) {
                self.needsInitialFocus = false
                NotificationCenter.default.removeObserver(self, name: NSWindow.didBecomeKeyNotification, object: window)
            }
        }
    }
}
