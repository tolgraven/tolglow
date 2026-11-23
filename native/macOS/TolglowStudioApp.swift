import AppKit
import SwiftUI
import WebKit

private class Lifetime: NSObject, NSApplicationDelegate {
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

@main
struct TolglowStudioApp: App {
    @NSApplicationDelegateAdaptor(Lifetime.self) private var lifetime
    @StateObject private var show = LocalShow()
    var body: some Scene {
        Window("Tolglow Studio", id: "studio") {
            Group {
                if let url = show.origin {
                    StudioWindow(url: url).id(url)
                } else {
                    VStack(spacing: 16) {
                        Image(systemName: "light.stage").font(.system(size: 48)).foregroundStyle(.secondary)
                        Text("Tolglow Studio").font(.largeTitle)
                        Text(show.message).foregroundStyle(.secondary)
                        if !show.running {
                            Button("Open Show Folder…", action: show.choose).controlSize(.large)
                            Button(show.sourcesInstalled ? "Reveal Application Sources" : "Set Up Editable Application Sources",
                                   action: show.prepareSources).disabled(show.installingSources)
                        }
                    }.frame(maxWidth: .infinity, maxHeight: .infinity)
                }
            }
            .toolbar {
                if show.running { Button("Stop Show", action: show.stop) }
            }
            .frame(minWidth: 720, minHeight: 480)
            .onReceive(NotificationCenter.default.publisher(for: NSApplication.willTerminateNotification)) { _ in show.stop() }
            .onAppear {
                let args = CommandLine.arguments
                if let index = args.firstIndex(of: "--open-show"), args.indices.contains(index + 1) {
                    show.start(URL(fileURLWithPath: args[index + 1], isDirectory: true))
                }
            }
        }
        .defaultSize(width: 1280, height: 800)
        .commands {
            CommandGroup(replacing: .newItem) {
                Button("Open Show Folder…", action: show.choose).keyboardShortcut("o").disabled(show.running)
            }
        }
    }
}

private struct StudioWindow: NSViewRepresentable {
    let url: URL
    func makeCoordinator() -> Navigation { Navigation(origin: url) }
    func makeNSView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.allowsAirPlayForMediaPlayback = true
        let view = WKWebView(frame: .zero, configuration: configuration)
        view.navigationDelegate = context.coordinator
        view.load(URLRequest(url: url))
        return view
    }
    func updateNSView(_ view: WKWebView, context: Context) {}
    static func dismantleNSView(_ view: WKWebView, coordinator: Navigation) {
        view.stopLoading()
        view.navigationDelegate = nil
    }
    final class Navigation: NSObject, WKNavigationDelegate {
        let origin: URL
        init(origin: URL) { self.origin = origin }
        func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                     decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
            let url = navigationAction.request.url
            let local = url?.scheme == origin.scheme && url?.host == origin.host && url?.port == origin.port
            decisionHandler(local ? .allow : .cancel)
        }
    }
}
