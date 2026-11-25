import AppKit
import WebKit

// Scratch recorder using the production window and Room worker. It observes
// after startup and leaves focus acquisition to StudioContentView.
@MainActor final class ShaderQualification: NSObject, WKScriptMessageHandler {
    let destination: URL
    let source: String

    init(destination: URL, source: String) {
        self.destination = destination
        self.source = source
    }

    static func fromArguments() -> ShaderQualification? {
        let arguments = CommandLine.arguments
        guard let index = arguments.firstIndex(of: "--record-room"),
              arguments.indices.contains(index + 1),
              let script = Bundle.main.url(forResource: "RoomCadence", withExtension: "js"),
              let source = try? String(contentsOf: script, encoding: .utf8) else { return nil }
        let expanded = arguments.contains("--room-expanded") ? "true" : "false"
        return ShaderQualification(destination: URL(fileURLWithPath: arguments[index + 1]),
                                   source: "globalThis.roomCadenceExpanded = " + expanded + ";\n" + source)
    }

    func configure(_ configuration: WKWebViewConfiguration) {
        let controller = configuration.userContentController
        controller.add(self, name: "recordRoomCadence")
        controller.addUserScript(WKUserScript(source: source, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
    }

    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard message.frameInfo.isMainFrame, let payload = message.body as? String,
              var receipt = (try? JSONSerialization.jsonObject(with: Data(payload.utf8))) as? [String: Any],
              receipt["type"] as? String == "room-cadence" else { return }
        receipt["bundleID"] = Bundle.main.bundleIdentifier
        receipt["webKit"] = Bundle(identifier: "com.apple.WebKit")?.infoDictionary?["CFBundleVersion"]
        do {
            let bytes = try JSONSerialization.data(withJSONObject: receipt, options: [.prettyPrinted, .sortedKeys])
            try bytes.write(to: destination, options: .atomic)
        } catch {
            fputs("Room cadence receipt write failed: \(error)\n", stderr)
        }
    }

    func stop(_ view: WKWebView) {
        view.configuration.userContentController.removeScriptMessageHandler(forName: "recordRoomCadence")
    }
}

@MainActor
func studioWebView(origin: URL, navigation: StudioNavigation, qualification: ShaderQualification?) -> WKWebView {
    let configuration = WKWebViewConfiguration()
    configuration.allowsAirPlayForMediaPlayback = true
    qualification?.configure(configuration)
    let view = StudioContentView(frame: .zero, configuration: configuration)
    view.navigationDelegate = navigation
    view.load(URLRequest(url: origin))
    return view
}
