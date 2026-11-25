import AppKit
import WebKit

/// Runs the shipped WKWebView setup against an already running Studio origin.
/// Exit 2 means unqualified (e.g. a hidden debug extension), not product failure.
@main
struct VerifyMetal {
    @MainActor static func main() throws {
        guard CommandLine.arguments.count == 4,
              let origin = URL(string: CommandLine.arguments[1]),
              ["127.0.0.1", "localhost", "::1"].contains(origin.host ?? "") else {
            fputs("usage: verify-metal <loopback Studio URL> <receipt.json> <ShaderQualification.js>\n", stderr)
            exit(1)
        }
        let qualification = ShaderQualification(destination: URL(fileURLWithPath: CommandLine.arguments[2]),
            script: try String(contentsOfFile: CommandLine.arguments[3], encoding: .utf8))
        let app = NSApplication.shared
        app.setActivationPolicy(.accessory)
        let navigation = StudioNavigation(origin: origin)
        // Establish that the unmodified production renderer starts first. Then
        // reload this same window with observation enabled; no extra renderer,
        // forced visibility or worker fallback is used to make the check pass.
        let view = studioWebView(origin: origin, navigation: navigation, qualification: nil)
        view.frame = NSRect(x: 0, y: 0, width: 1280, height: 800)
        let window = NSWindow(contentRect: view.frame, styleMask: [.titled], backing: .buffered, defer: false)
        window.title = "Tolglow shader qualification"
        window.contentView = view
        window.makeKeyAndOrderFront(nil)
        app.activate(ignoringOtherApps: true)
        Task { @MainActor in
            let deadline = ContinuousClock.now.advanced(by: .seconds(45))
            var control: [String: Any] = [:]
            while ContinuousClock.now < deadline {
                if let value = await ShaderQualification.evaluate(view, """
                    JSON.parse(JSON.stringify((() => {
                      const canvases = [...document.querySelectorAll('canvas')].filter(c => c.tolglowPreview);
                      return {hidden: document.hidden, ready: canvases.some(c => c.tolglowPreview.ready),
                        previews: canvases.map(c => ({width: c.width, height: c.height,
                          telemetry: c.tolglowPreview.getTelemetry?.() ?? {}}))};
                    })()))
                    """) as? [String: Any] { control = value }
                if control["ready"] as? Bool == true { break }
                try? await Task.sleep(nanoseconds: 500_000_000)
            }
            let output = URL(fileURLWithPath: CommandLine.arguments[2]).deletingLastPathComponent()
            do {
                let data = try JSONSerialization.data(withJSONObject: control, options: [.prettyPrinted, .sortedKeys])
                try data.write(to: output.appendingPathComponent("native-control.json"), options: .atomic)
            } catch { fputs("Could not save native control: \(error)\n", stderr); exit(1) }
            guard control["ready"] as? Bool == true else {
                fputs("Uninstrumented Studio preview did not become ready; see native-control.json\n", stderr)
                window.orderOut(nil); exit(1)
            }
            let first = (control["previews"] as? [[String: Any]])?.first
            qualification.expectedRoute = (first?["telemetry"] as? [String: Any])?["thread"] as? String ?? "main"
            qualification.configure(view.configuration)
            qualification.start(view)
            view.reload()
            while qualification.result == nil { try? await Task.sleep(nanoseconds: 100_000_000) }
            qualification.stop(view)
            view.stopLoading()
            view.navigationDelegate = nil
            window.orderOut(nil)
            let code: Int32 = qualification.result == "qualified" ? 0 : qualification.result == "unqualified" ? 2 : 1
            withExtendedLifetime((window, navigation)) {}
            exit(code)
        }
        // WebKit page visibility follows real AppKit window events. An async
        // CLI main alone does not run this event loop and leaves workers idle.
        app.run()
    }
}
