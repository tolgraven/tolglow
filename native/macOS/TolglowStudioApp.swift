import AppKit
import SwiftUI
import WebKit

private class Lifetime: NSObject, NSApplicationDelegate {
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

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
                            if show.nativeHost != nil {
                                Picker("Show runtime", selection: $show.engine) {
                                    ForEach(LocalShow.Engine.allCases, id: \.self) { value in
                                        Text(value.rawValue).tag(value)
                                    }
                                }
                                .pickerStyle(.segmented)
                                .frame(maxWidth: 320)
                            }
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
                if let index = args.firstIndex(of: "--compiled-show"), args.indices.contains(index + 1), show.nativeHost != nil {
                    show.start(URL(fileURLWithPath: args[index + 1], isDirectory: true), use: .native)
                } else if let index = args.firstIndex(of: "--open-show"), args.indices.contains(index + 1) {
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
    @MainActor final class Coordinator {
        let navigation: StudioNavigation
        let qualification = ShaderQualification.fromArguments()
        init(url: URL) { navigation = StudioNavigation(origin: url) }
    }
    func makeCoordinator() -> Coordinator { Coordinator(url: url) }
    func makeNSView(context: Context) -> WKWebView {
        studioWebView(origin: url, navigation: context.coordinator.navigation,
                      qualification: context.coordinator.qualification)
    }
    func updateNSView(_ view: WKWebView, context: Context) {}
    static func dismantleNSView(_ view: WKWebView, coordinator: Coordinator) {
        coordinator.qualification?.stop(view)
        view.stopLoading()
        view.navigationDelegate = nil
    }
}
