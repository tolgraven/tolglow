import AppKit
import Vision
import WebKit

@main
struct CaptureStage {
    enum Failure: Error { case timeout(String, visibility: String), badImage, refusedHTTP(Int), staleInvitation }
    @MainActor static func wait(_ view: WKWebView, for description: String, until ready: () async throws -> Bool) async throws {
        let clock = ContinuousClock()
        let deadline = clock.now + .seconds(30)
        while clock.now < deadline {
            if try await ready() { return }
            try await Task.sleep(nanoseconds: 100_000_000)
        }
        let visibility = (try? await view.evaluateJavaScript("document.visibilityState")) as? String
        throw Failure.timeout(description, visibility: visibility ?? "unknown")
    }

    @MainActor static func waitForStageButton(_ view: WKWebView, _ label: String, click: Bool = false) async throws {
        try await wait(view, for: "enabled \(label) button") {
            try await view.evaluateJavaScript("""
              (() => {
                const button = document.querySelector('.stage-invitation button:not(:disabled)');
                if (document.hidden || button?.textContent.trim() !== '\(label)') return false;
                \(click ? "button.click();" : "")
                return true;
              })()
            """) as? Bool == true
        }
    }

    @MainActor static func show(_ window: NSWindow) {
        NSApplication.shared.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    @MainActor static func report(_ phase: String, view: WKWebView, qr: URL) async {
        let state = (try? await view.evaluateJavaScript("""
          (() => {
            const panels = [...document.querySelectorAll('.stage-invitation')];
            const buttons = [...document.querySelectorAll('.stage-invitation button')];
            return JSON.stringify({
              url: location.origin + location.pathname,
              readyState: document.readyState,
              visibility: document.visibilityState,
              connections: document.querySelectorAll('#view-connections').length,
              connectionBadge: document.querySelector('#connection-label')?.textContent,
              stagePanels: panels.length,
              stageButtons: buttons.map(button => ({text: button.textContent, disabled: button.disabled})),
              stageBadge: panels[0]?.querySelector('[role=status]')?.textContent,
              stageQRs: document.querySelectorAll('.stage-invitation-qr').length,
              editableOrigins: document.querySelectorAll('.stage-invitation input[type=url]:not([readonly]):not(:disabled)').length,
              events: window.stageCapturePageEvents
            });
          })()
        """)) as? String
        let request = URLRequest(url: qr, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 3)
        let response = try? await URLSession.shared.data(for: request)
        let status = (response?.1 as? HTTPURLResponse)?.statusCode ?? 0
        FileHandle.standardError.write(Data("Stage \(phase), QR HTTP \(status): \(state ?? "JS unavailable")\n".utf8))
    }

    @MainActor static func capture(_ view: WKWebView, _ path: URL) async throws {
        _ = try await view.evaluateJavaScript("document.querySelector('.stage-invitation').scrollIntoView({block:'center'})")
        let image = try await view.takeSnapshot(configuration: nil)
        guard let tiff = image.tiffRepresentation, let bitmap = NSBitmapImageRep(data: tiff),
              let png = bitmap.representation(using: .png, properties: [:]) else { throw Failure.badImage }
        try png.write(to: path)
    }

    static func post(_ endpoint: URL, _ values: [String: Any]) async throws {
        var request = URLRequest(url: endpoint)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONSerialization.data(withJSONObject: values)
        let (_, response) = try await URLSession.shared.data(for: request)
        let code = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard code == 200 else { throw Failure.refusedHTTP(code) }
    }

    @MainActor static func main() async throws {
        let origin = URL(string: CommandLine.arguments[1])!
        let folder = URL(fileURLWithPath: CommandLine.arguments[2])
        try await post(origin.appendingPathComponent("api/sensors/pair"), ["device-id": "audience", "label": "Dance floor"])
        let mapping = "{:version 1 :channels [{:id :audience-count :input :audience-count} {:id :dance-effort :input :dance-effort}]}"
        try await post(origin.appendingPathComponent("api/sensors/configure"), ["device-id": "audience", "source": mapping])
        try await post(origin.appendingPathComponent("api/sensors/osc/start"), ["port": 0])
        let application = NSApplication.shared
        application.setActivationPolicy(.regular)
        let configuration = WKWebViewConfiguration()
        configuration.userContentController.addUserScript(WKUserScript(source: """
          window.stageCapturePageEvents = [];
          for (const name of ['visibilitychange', 'pagehide', 'pageshow']) {
            window.addEventListener(name, event => {
              const events = window.stageCapturePageEvents;
              if (events.length === 16) events.shift();
              events.push([name, document.visibilityState, event.persisted ?? null]);
            });
          }
        """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        let view = WKWebView(frame: NSRect(x: 0, y: 0, width: 1050, height: 760), configuration: configuration)
        let window = NSWindow(contentRect: view.frame, styleMask: [.titled], backing: .buffered, defer: false)
        window.contentView = view
        // Studio subscribes to connection state only while its document is visible.
        show(window)
        let destination = folder.appendingPathComponent("invitation.png")
        let navigation = StudioNavigation(origin: origin) { _, _, done in done(destination) }
        view.navigationDelegate = navigation
        view.load(URLRequest(url: origin.appendingPathComponent("connections")))
        let editableOrigin = ".stage-invitation input[type=url]:not([readonly]):not(:disabled)"
        let setupIsVisible = "!document.hidden && !!document.querySelector('\(editableOrigin)')"
        try await wait(view, for: "editable invocation origin") {
            (try? await view.evaluateJavaScript(setupIsVisible)) as? Bool == true
        }
        _ = try await view.evaluateJavaScript("""
          const field = document.querySelector('\(editableOrigin)');
          Object.getOwnPropertyDescriptor(HTMLInputElement.prototype,'value').set.call(field,'https://lights.example.org');
          field.dispatchEvent(new Event('input', {bubbles:true}));
        """)
        try await waitForStageButton(view, "Start stage")
        try await capture(view, folder.appendingPathComponent("stage-ready.png"))
        try await waitForStageButton(view, "Start stage", click: true)
        try await wait(view, for: "loaded stage QR") {
            try await view.evaluateJavaScript("""
              (() => {
                const image = document.querySelector('.stage-invitation-qr img');
                return !!image && image.complete && image.naturalWidth > 48;
              })()
            """) as? Bool == true
        }
        let invitation = try await view.evaluateJavaScript("document.querySelector('.stage-invitation-qr input').value") as! String
        let imageSource = try await view.evaluateJavaScript("document.querySelector('.stage-invitation-qr img').src") as! String
        let imageURL = URL(string: imageSource)!
        try await capture(view, folder.appendingPathComponent("stage-invitation.png"))
        await report("before Save QR", view: view, qr: imageURL)
        _ = try await view.evaluateJavaScript("document.querySelector('.stage-invitation a[download]').click()")
        for _ in 0..<100 where !FileManager.default.fileExists(atPath: destination.path) {
            try await Task.sleep(nanoseconds: 100_000_000)
        }
        let qr = try Data(contentsOf: destination)
        guard qr.starts(with: [137,80,78,71,13,10,26,10]) else { throw Failure.badImage }
        let decoder = VNDetectBarcodesRequest()
        decoder.symbologies = [.qr]
        try VNImageRequestHandler(url: destination, options: [:]).perform([decoder])
        guard decoder.results?.map(\.payloadStringValue) == [invitation],
              invitation.range(of: #"^https://lights\.example\.org/audience/[0-9a-f]{64}$"#,
                               options: .regularExpression) != nil else { throw Failure.badImage }
        FileHandle.standardError.write(Data("Decoded the saved stage QR and verified its public HTTPS invitation.\n".utf8))
        show(window)
        await report("after Save QR", view: view, qr: imageURL)
        do {
            try await waitForStageButton(view, "Stop stage", click: true)
        } catch {
            await report("at Stop failure", view: view, qr: imageURL)
            throw error
        }
        try await wait(view, for: "editable invocation origin after stopping") {
            try await view.evaluateJavaScript(setupIsVisible) as? Bool == true
        }
        try await capture(view, folder.appendingPathComponent("stage-closed.png"))
        let (_, old) = try await URLSession.shared.data(from: imageURL)
        guard (old as? HTTPURLResponse)?.statusCode == 404 else { throw Failure.staleInvitation }
        print("Stopped the stage, restored its editable setup and verified that the old QR returns HTTP 404.")
        withExtendedLifetime((window, navigation)) {}
    }
}
