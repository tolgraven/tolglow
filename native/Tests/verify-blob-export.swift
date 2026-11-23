import AppKit
import WebKit

/// Exercise WebKit's actual blob navigation export and the same native download
/// delegate used by the app. Inject only the destination normally chosen by the operator.
@main
struct VerifyExport {
    @MainActor static func main() {
        let args = CommandLine.arguments
        let origin = URL(string: args[1])!
        let target = URL(fileURLWithPath: args[2])
        let expected = Data("fixture-export".utf8)
        let app = NSApplication.shared
        app.setActivationPolicy(.accessory)
        let view = WKWebView(frame: NSRect(x: 0, y: 0, width: 300, height: 180))
        let window = NSWindow(contentRect: view.frame, styleMask: [.titled], backing: .buffered, defer: false)
        window.contentView = view
        window.orderFrontRegardless()
        let navigation = StudioNavigation(origin: origin) { _, _, done in done(target) }
        view.navigationDelegate = navigation
        try! Data("old backup".utf8).write(to: target)
        view.load(URLRequest(url: origin))
        let deadline = Date().addingTimeInterval(25)
        var clicked = false
        withExtendedLifetime(navigation) {
            while Date() < deadline {
                RunLoop.current.run(until: Date().addingTimeInterval(0.1))
                if !clicked, view.url == origin, !view.isLoading {
                    clicked = true
                    view.evaluateJavaScript("""
                      const link = document.createElement("a");
                      link.href = URL.createObjectURL(new Blob(["fixture-export"], {type:"application/octet-stream"}));
                      link.download = "show.transit"; document.body.append(link); link.click();
                    """)
                }
                if (try? Data(contentsOf: target)) == expected { exit(0) }
            }
        }
        fatalError("WebKit did not complete the Studio blob download")
    }
}
