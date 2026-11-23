#if TOLGLOW_EMBEDDED_HOST
import SwiftUI
import TolglowRuntime
import UniformTypeIdentifiers

/// Each isolate belongs to one native thread from creation through teardown.
private final class ShowThread {
    private let stopSignal = DispatchSemaphore(value: 0)
    func stop() { stopSignal.signal() }

    func run(home: URL, folder: URL?, ready: @escaping (Int) -> Void, ended: @escaping (String?) -> Void) {
        let worker = Thread { [stopSignal] in
            let scoped = folder?.startAccessingSecurityScopedResource() ?? false
            defer { if scoped { folder?.stopAccessingSecurityScopedResource() } }
            var isolate: OpaquePointer?
            var thread: OpaquePointer?
            guard graal_create_isolate(nil, &isolate, &thread) == 0, let thread else {
                ended("Could not initialize the local runtime.")
                return
            }
            let port = tolglow_open_local(thread, home.path, folder?.path ?? "")
            if port > 0 {
                ready(Int(port))
                stopSignal.wait()
                _ = tolglow_close_local(thread)
            }
            let closed = graal_tear_down_isolate(thread)
            if port <= 0 { ended("Could not start this show. Check its configuration in tolglow.edn or show.clj.") }
            else if closed != 0 { ended("The local runtime could not finish shutting down.") }
            else { ended(nil) }
        }
        worker.name = "Tolglow on-device show"
        worker.start()
    }
}

@MainActor
private final class OnDeviceShow: ObservableObject {
    @Published var origin: URL?
    @Published var status = "Choose a show folder or open the included show."
    @Published private(set) var busy = false
    private var owner: ShowThread?
    private var stopping = false

    func start(folder: URL?) {
        guard owner == nil else { return }
        let manager = FileManager.default
        do {
            let support = try manager.url(for: .applicationSupportDirectory, in: .userDomainMask,
                                          appropriateFor: nil, create: true)
            let home = support.appendingPathComponent("Local Studio", isDirectory: true)
            try manager.createDirectory(at: home, withIntermediateDirectories: true)
            let session = ShowThread()
            owner = session
            busy = true
            stopping = false
            status = "Starting local Studio…"
            session.run(home: home, folder: folder) { [weak self] port in
                Task { @MainActor in
                    guard let self, self.owner === session, !self.stopping else { return }
                    self.origin = URL(string: "http://127.0.0.1:\(port)")
                    self.status = "Local Studio"
                }
            } ended: { [weak self] error in
                Task { @MainActor in
                    guard let self, self.owner === session else { return }
                    self.origin = nil
                    self.owner = nil
                    self.busy = false
                    self.status = error ?? "Local show stopped."
                }
            }
        } catch { status = error.localizedDescription }
    }

    func stop() {
        guard let owner, !stopping else { return }
        stopping = true
        origin = nil
        status = "Stopping local show…"
        owner.stop()
    }
}

struct OnDeviceStudio: View {
    @Environment(\.scenePhase) private var phase
    @Environment(\.dismiss) private var dismiss
    @StateObject private var show = OnDeviceShow()
    @State private var choosing = false
    @State private var webStatus = ""

    var body: some View {
        NavigationStack {
            VStack(alignment: .leading, spacing: 12) {
                if let origin = show.origin {
                    OwnedStudioWebView(url: origin, status: $webStatus)
                    Text(webStatus).font(.caption).padding(.horizontal)
                } else {
                    Text(show.status).padding(.horizontal)
                    if !show.busy {
                        Button("Open included show") { show.start(folder: nil) }
                        Button("Choose show folder") { choosing = true }
                        Text("Choose a folder containing tolglow.edn or show.clj. This device hosts the show while Studio is open.")
                            .font(.footnote).foregroundStyle(.secondary)
                    }
                    Spacer()
                }
            }
            .padding(.horizontal, show.origin == nil ? 16 : 0)
            .buttonStyle(.bordered)
            .navigationTitle("On-device Studio")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) { Button("Done") { show.stop(); dismiss() } }
                if show.busy { ToolbarItem(placement: .primaryAction) { Button("Stop show") { show.stop() } } }
            }
        }
        .fileImporter(isPresented: $choosing, allowedContentTypes: [.folder]) { result in
            switch result {
            case .success(let folder): show.start(folder: folder)
            case .failure(let error): show.status = error.localizedDescription
            }
        }
        .onChange(of: phase) { _, value in if value == .background { show.stop() } }
        .onDisappear { show.stop() }
    }
}
#endif
