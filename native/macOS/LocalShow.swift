import AppKit
import Foundation

/// One owned child per window. The private readiness file belongs to this
/// launch, so an unrelated listener cannot be mistaken for a running show.
/// Windows claim their folder in `ShowRegistry` first: a folder runs in one
/// window only, and only the first running show gets devices and output.
@MainActor
final class LocalShow: ObservableObject {
    enum Engine: String, CaseIterable {
        case native = "Compiled Clojure"
        case jvm = "Full JVM Clojure"
    }

    /// This window's identity in `ShowRegistry`.
    let id = UUID()
    /// Live windows, so a duplicate open can select the window already running the folder.
    private static var windows: [UUID: () -> NSWindow?] = [:]
    /// Command-line launch options apply to the first window only.
    private static var consumedArguments = false

    @Published var engine = Engine(rawValue: UserDefaults.standard.string(forKey: "show-engine") ?? "") ?? .jvm {
        didSet { UserDefaults.standard.set(engine.rawValue, forKey: "show-engine") }
    }
    var nativeHost: URL? {
        guard let executable = Bundle.main.resourceURL?.deletingLastPathComponent()
            .appendingPathComponent("Helpers/TolglowNativeHost"),
              FileManager.default.isExecutableFile(atPath: executable.path) else { return nil }
        return executable
    }
    @Published private(set) var origin: URL?
    /// The runtime this window's page is bound to, read from `/api/identity`.
    @Published private(set) var runtimeId: String?
    @Published private(set) var folder: URL?
    /// False when another window's show owns the devices.
    @Published private(set) var ownsDevices = true
    @Published private(set) var message = "Open a folder containing tolglow.edn or show.clj."
    @Published private(set) var running = false
    @Published private(set) var installingSources = false
    @Published private(set) var sourcesInstalled = ApplicationSources()?.installed ?? false
    weak var window: NSWindow? {
        didSet { let id = id; Self.windows[id] = { [weak window] in window } }
    }

    func prepareSources() {
        guard !installingSources, let sources = ApplicationSources() else { return }
        if sources.installed {
            NSWorkspace.shared.open(sources.directory)
            return
        }
        installingSources = true
        Task {
            do {
                try await Task.detached(priority: .utility) { try sources.install() }.value
                sourcesInstalled = true
                message = "Application sources are ready for Studio workspaces."
            } catch { message = error.localizedDescription }
            installingSources = false
        }
    }
    private var process: Process?
    private var control: Pipe?
    private var readiness: Task<Void, Never>?

    /// Start the folder named on the command line, once per app launch.
    func startFromArguments() {
        guard !Self.consumedArguments else { return }
        Self.consumedArguments = true
        let args = CommandLine.arguments
        if let index = args.firstIndex(of: "--compiled-show"), args.indices.contains(index + 1), nativeHost != nil {
            start(URL(fileURLWithPath: args[index + 1], isDirectory: true), use: .native)
        } else if let index = args.firstIndex(of: "--open-show"), args.indices.contains(index + 1) {
            start(URL(fileURLWithPath: args[index + 1], isDirectory: true))
        }
    }

    func choose() {
        let panel = NSOpenPanel()
        panel.canChooseDirectories = true
        panel.canChooseFiles = false
        panel.prompt = "Run Show"
        panel.message = "Run the Clojure code and enabled modules in this folder."
        guard panel.runModal() == .OK, let url = panel.url else { return }
        start(url)
    }

    func start(_ folder: URL, use requestedEngine: Engine? = nil) {
        guard !running, let resources = Bundle.main.resourceURL else { return }
        let devices: Bool
        switch ShowRegistry.shared.claim(id, folder: folder) {
        case .duplicate(let other):
            message = "\(folder.lastPathComponent) is already open in another window."
            Self.windows[other]?()?.makeKeyAndOrderFront(nil)
            return
        case .previewOnly(let owner):
            devices = false
            message = "\(owner) owns this Mac's devices and output, so \(folder.lastPathComponent) opens preview-only."
        case .devices:
            devices = true
        }
        let files = FileManager.default
        let directory = files.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        let ready = directory.appendingPathComponent("port")
        let child = Process()
        let pipe = Pipe()
        do {
            try files.createDirectory(at: directory, withIntermediateDirectories: false,
                                      attributes: [.posixPermissions: 0o700])
            child.currentDirectoryURL = folder
            var environment = ProcessInfo.processInfo.environment
            let helper = Bundle.main.bundleURL.appendingPathComponent("Contents/Helpers/TolglowAudienceBridge")
            environment["TOLGLOW_AUDIENCE_BRIDGE"] = helper.path
            if let sources = ApplicationSources(), sources.installed {
                environment["TOLGLOW_APPLICATION_SOURCES"] = sources.directory.path
            }
            environment["TOLGLOW_DESKTOP_DEVICES"] = devices ? nil : "none"
            child.environment = environment
            if (requestedEngine ?? engine) == .native, let nativeHost {
                child.executableURL = nativeHost
                child.arguments = [folder.path, ready.path]
            } else {
                child.executableURL = resources.appendingPathComponent("runtime/bin/java")
                child.arguments = ["-XX:+UseZGC", "-Djava.awt.headless=true",
                                   "-cp", resources.appendingPathComponent("studio.jar").path,
                                   "clojure.main", "-m", "tolglow.desktop", folder.path, ready.path]
            }
            child.standardInput = pipe
            let id = id
            child.terminationHandler = { [weak self] finished in
                try? FileManager.default.removeItem(at: directory)
                Task { @MainActor in
                    // A closed window's LocalShow is usually gone before its
                    // process exits; the folder and the devices must still be
                    // released, or no window could open that folder again.
                    guard let self else { ShowRegistry.shared.release(id); return }
                    guard self.process === finished else { return }
                    ShowRegistry.shared.release(id)
                    self.readiness?.cancel()
                    self.process = nil
                    self.control = nil
                    self.origin = nil
                    self.runtimeId = nil
                    self.running = false
                    self.message = finished.terminationStatus == 0 ? "Show stopped." : "Show exited (\(finished.terminationStatus)). Check the show configuration and the app log in Console."
                }
            }
            try child.run()
            process = child
            control = pipe
            running = true
            ownsDevices = devices
            self.folder = folder
            if devices { message = "Starting \(folder.lastPathComponent)…" }
            readiness = Task {
                while !Task.isCancelled && child.isRunning {
                    if let data = try? String(contentsOf: ready, encoding: .utf8),
                       let port = UInt16(data), port > 0, let origin = URL(string: "http://127.0.0.1:\(port)/") {
                        // Bind the window to the runtime that answers on this
                        // port before Studio loads (see StudioPage).
                        if let runtime = await Self.identity(origin) {
                            guard !Task.isCancelled, child.isRunning else { return }
                            runtimeId = runtime
                            self.origin = origin
                            message = folder.lastPathComponent
                            return
                        }
                    }
                    try? await Task.sleep(for: .milliseconds(100))
                }
            }
        } catch {
            ShowRegistry.shared.release(id)
            try? files.removeItem(at: directory)
            message = error.localizedDescription
        }
    }

    /// The runtime ID a show reports at `/api/identity`, or nil while it cannot answer.
    static func identity(_ origin: URL) async -> String? {
        var request = URLRequest(url: origin.appendingPathComponent("api/identity"), timeoutInterval: 5)
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        guard let (data, response) = try? await URLSession.shared.data(for: request),
              (response as? HTTPURLResponse)?.statusCode == 200,
              let body = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else { return nil }
        return (body["runtime-id"] as? String).flatMap { $0.isEmpty ? nil : $0 }
    }

    func stop() {
        readiness?.cancel()
        origin = nil
        runtimeId = nil
        guard let control else { return }
        self.control = nil
        try? control.fileHandleForWriting.close()
        message = "Stopping show…"
    }

    /// The window closed: stop its show and forget the window.
    func close() {
        stop()
        Self.windows[id] = nil
    }
}
