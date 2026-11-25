import AppKit
import Foundation

/// One owned child per window. The private readiness file belongs to this
/// launch, so an unrelated listener cannot be mistaken for a running show.
@MainActor
final class LocalShow: ObservableObject {
    enum Engine: String, CaseIterable {
        case native = "Compiled Clojure"
        case jvm = "Full JVM Clojure"
    }

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
    @Published private(set) var message = "Open a folder containing tolglow.edn or show.clj."
    @Published private(set) var running = false
    @Published private(set) var installingSources = false
    @Published private(set) var sourcesInstalled = ApplicationSources()?.installed ?? false

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
            child.terminationHandler = { [weak self] finished in
                try? FileManager.default.removeItem(at: directory)
                Task { @MainActor in
                    guard let self, self.process === finished else { return }
                    self.readiness?.cancel()
                    self.process = nil
                    self.control = nil
                    self.origin = nil
                    self.running = false
                    self.message = finished.terminationStatus == 0 ? "Show stopped." : "Show exited (\(finished.terminationStatus)). Check the show configuration and the app log in Console."
                }
            }
            try child.run()
            process = child
            control = pipe
            running = true
            message = "Starting \(folder.lastPathComponent)…"
            readiness = Task {
                while !Task.isCancelled && child.isRunning {
                    if let data = try? String(contentsOf: ready, encoding: .utf8),
                       let port = UInt16(data), port > 0 {
                        origin = URL(string: "http://127.0.0.1:\(port)/")
                        message = folder.lastPathComponent
                        return
                    }
                    try? await Task.sleep(for: .milliseconds(100))
                }
            }
        } catch {
            try? files.removeItem(at: directory)
            message = error.localizedDescription
        }
    }

    func stop() {
        readiness?.cancel()
        origin = nil
        guard let control else { return }
        self.control = nil
        try? control.fileHandleForWriting.close()
        message = "Stopping show…"
    }
}
