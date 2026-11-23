import AppKit
import Vision
import WebKit

@main
struct CaptureStage {
    enum Failure: Error { case timeout(String), badImage, refusedHTTP(Int), staleInvitation }
    @MainActor static func wait(_ view: WKWebView, _ condition: String) async throws {
        let deadline = Date().addingTimeInterval(30)
        while Date() < deadline {
            if (try? await view.evaluateJavaScript(condition)) as? Bool == true { return }
            try await Task.sleep(nanoseconds: 100_000_000)
        }
        throw Failure.timeout(condition)
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
        try await post(origin.appendingPathComponent("api/sensors/osc/start"), ["port": 0])
        let application = NSApplication.shared
        application.setActivationPolicy(.accessory)
        let view = WKWebView(frame: NSRect(x: 0, y: 0, width: 1050, height: 760))
        let window = NSWindow(contentRect: view.frame, styleMask: [.titled], backing: .buffered, defer: false)
        window.contentView = view
        window.orderFrontRegardless()
        let destination = folder.appendingPathComponent("invitation.png")
        let navigation = StudioNavigation(origin: origin) { _, _, done in done(destination) }
        view.navigationDelegate = navigation
        view.load(URLRequest(url: origin.appendingPathComponent("connections")))
        try await wait(view, "!!document.querySelector('.stage-invitation input[type=url]')")
        _ = try await view.evaluateJavaScript("""
          const field = document.querySelector('.stage-invitation input[type=url]');
          Object.getOwnPropertyDescriptor(HTMLInputElement.prototype,'value').set.call(field,'https://lights.example.org');
          field.dispatchEvent(new Event('input', {bubbles:true}));
        """)
        try await wait(view, "!!document.querySelector('.stage-invitation button:not(:disabled)')")
        try await capture(view, folder.appendingPathComponent("stage-ready.png"))
        _ = try await view.evaluateJavaScript("document.querySelector('.stage-invitation button').click()")
        try await wait(view, """
          (()=>{const image=document.querySelector('.stage-invitation-qr img');
            return !!image && image.complete && image.naturalWidth > 48})()
        """)
        let invitation = try await view.evaluateJavaScript("document.querySelector('.stage-invitation-qr input').value") as! String
        let imageURL = try await view.evaluateJavaScript("document.querySelector('.stage-invitation-qr img').src") as! String
        try await capture(view, folder.appendingPathComponent("stage-invitation.png"))
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
        print("Decoded the saved stage QR and verified its public HTTPS invitation.")
        _ = try await view.evaluateJavaScript("document.querySelector('.stage-invitation button').click()")
        try await wait(view, "!!document.querySelector('.stage-invitation input[type=url]')")
        try await capture(view, folder.appendingPathComponent("stage-closed.png"))
        let (_, old) = try await URLSession.shared.data(from: URL(string: imageURL)!)
        guard (old as? HTTPURLResponse)?.statusCode == 404 else { throw Failure.staleInvitation }
        withExtendedLifetime((window, navigation)) {}
    }
}
