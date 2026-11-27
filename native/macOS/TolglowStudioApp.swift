import AppKit
import SwiftUI
import WebKit

private class Lifetime: NSObject, NSApplicationDelegate {
    func applicationWillFinishLaunching(_ notification: Notification) {
        // A window is one show. Views of that show are tabs inside Studio, which
        // share its connection and preview engine; merging two shows' windows
        // into one native tab bar would mix their controls.
        NSWindow.allowsAutomaticWindowTabbing = false
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

@main
struct TolglowStudioApp: App {
    @NSApplicationDelegateAdaptor(Lifetime.self) private var lifetime
    var body: some Scene {
        // File › New Window (⌘N) opens another show window with its own process.
        WindowGroup("Tolglow Studio", id: "show") {
            ShowWindow()
        }
        .defaultSize(width: 1280, height: 800)
        .commands { StudioCommands() }
    }
}

private struct ShowWindow: View {
    @StateObject private var show = LocalShow()
    @StateObject private var page = StudioPage()

    private var title: String { show.folder?.lastPathComponent ?? "Tolglow Studio" }
    private var subtitle: String {
        guard show.running else { return "" }
        let view = page.viewLabel ?? ""
        return show.ownsDevices ? view : [view, "Preview only"].filter { !$0.isEmpty }.joined(separator: " · ")
    }

    var body: some View {
        Group {
            if let url = show.origin, let runtime = show.runtimeId, let folder = show.folder {
                VStack(spacing: 0) {
                    if !show.ownsDevices {
                        Text(show.message).font(.callout).foregroundStyle(.secondary)
                            .frame(maxWidth: .infinity).padding(6).background(.bar)
                    }
                    StudioWindow(url: url, runtimeId: runtime, folder: folder, page: page).id(runtime)
                }
            } else {
                VStack(spacing: 16) {
                    Image(systemName: "light.stage").font(.system(size: 48)).foregroundStyle(.secondary)
                    Text("Tolglow Studio").font(.largeTitle)
                    Text(show.message).foregroundStyle(.secondary).multilineTextAlignment(.center)
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
                }.frame(maxWidth: .infinity, maxHeight: .infinity).padding()
            }
        }
        .toolbar {
            if show.running { Button("Stop Show", action: show.stop) }
        }
        .navigationTitle(title)
        .navigationSubtitle(subtitle)
        .frame(minWidth: 720, minHeight: 480)
        .background(WindowReader { show.window = $0 })
        .focusedSceneObject(show)
        .focusedSceneObject(page)
        .onReceive(NotificationCenter.default.publisher(for: NSApplication.willTerminateNotification)) { _ in show.stop() }
        .onAppear { show.startFromArguments() }
        // Closing a window stops its show; other windows keep theirs.
        .onDisappear { show.close() }
    }
}

/// The standard menus act on the key window's show and Studio page.
private struct StudioCommands: Commands {
    @FocusedObject private var show: LocalShow?
    @FocusedObject private var page: StudioPage?

    var body: some Commands {
        CommandGroup(after: .newItem) {
            Button("New Tab") { page?.send("new") }
                .keyboardShortcut("t").disabled(page?.view == nil)
            Button("Reopen Closed Tab") { page?.send("reopen") }
                .keyboardShortcut("t", modifiers: [.command, .shift]).disabled(page?.view == nil)
            Button("Open Show Folder…") { show?.choose() }
                .keyboardShortcut("o").disabled(show == nil || show?.running == true)
            // The page's Show checkpoints dialog: the show folder's identity
            // file and recovery, including a checkpoint from before the update (#742).
            Button("Show Folder and Checkpoints…") { page?.send("checkpoints") }
                .disabled(page?.view == nil)
            Divider()
            Button("Close Tab") { page?.send("close") }
                .keyboardShortcut("w", modifiers: [.command, .option]).disabled((page?.tabCount ?? 0) < 2)
        }
        CommandGroup(before: .windowArrangement) {
            Button("Show Next Tab") { page?.send("next") }
                .keyboardShortcut(.tab, modifiers: .control).disabled((page?.tabCount ?? 0) < 2)
            Button("Show Previous Tab") { page?.send("previous") }
                .keyboardShortcut(.tab, modifiers: [.control, .shift]).disabled((page?.tabCount ?? 0) < 2)
            Divider()
        }
    }
}

/// Reports the NSWindow hosting a SwiftUI view.
private struct WindowReader: NSViewRepresentable {
    let found: (NSWindow?) -> Void
    func makeNSView(context: Context) -> NSView {
        let view = NSView()
        DispatchQueue.main.async { [weak view] in found(view?.window) }
        return view
    }
    func updateNSView(_ view: NSView, context: Context) {
        DispatchQueue.main.async { [weak view] in if let window = view?.window { found(window) } }
    }
}

private struct StudioWindow: NSViewRepresentable {
    let url: URL
    let runtimeId: String
    let folder: URL
    let page: StudioPage
    @MainActor final class Coordinator {
        let navigation: StudioNavigation
        let qualification = ShaderQualification.fromArguments()
        let rooms: RoomWindows
        weak var page: StudioPage?
        init(url: URL, page: StudioPage) {
            navigation = StudioNavigation(origin: url)
            rooms = RoomWindows(origin: url)
            self.page = page
        }
    }
    func makeCoordinator() -> Coordinator { Coordinator(url: url, page: page) }
    func makeNSView(context: Context) -> WKWebView {
        let view = studioWebView(origin: url, navigation: context.coordinator.navigation,
                                 qualification: context.coordinator.qualification, rooms: context.coordinator.rooms) {
            page.configure($0, origin: url, runtimeId: runtimeId, show: folder.lastPathComponent, folder: folder)
        }
        page.view = view
        return view
    }
    func updateNSView(_ view: WKWebView, context: Context) {}
    static func dismantleNSView(_ view: WKWebView, coordinator: Coordinator) {
        coordinator.qualification?.stop(view)
        view.stopLoading()
        view.navigationDelegate = nil
        view.uiDelegate = nil
        view.configuration.userContentController.removeScriptMessageHandler(forName: StudioPage.messageName)
        coordinator.rooms.close()
        if coordinator.page?.view === view { coordinator.page?.detach() }
    }
}
