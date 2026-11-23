import SwiftUI
import WebKit

private struct ComputeSurface: UIViewRepresentable {
    let gpu: GuestGPU
    func makeUIView(context: Context) -> WKWebView { gpu.view }
    func updateUIView(_ view: WKWebView, context: Context) {}
}

struct GuestComputeControl: View {
    @ObservedObject var client: AudienceClient
    let room: String
    @StateObject private var worker = AudienceComputeGuest()
    @State private var request: UUID?
    @State private var denied = false

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            Button(worker.active ? "Stop sharing GPU" : "Share GPU with this stage") {
                if worker.active { worker.stop(); return }
                let id = UUID(); request = id; denied = false
                client.requestComputeGrant { grant in
                    guard request == id, client.joined else { return }
                    request = nil
                    guard let grant else { denied = true; return }
                    worker.start(grant: grant, pinnedRoom: room)
                }
            }.disabled(request != nil)
            Text(denied ? "Stage unavailable. You can keep dancing." : worker.message)
                .font(.caption).foregroundStyle(.secondary)
            if let gpu = worker.gpu { ComputeSurface(gpu: gpu).frame(width: 2, height: 2).accessibilityHidden(true) }
        }
        .onReceive(client.$joined) { connected in if !connected { stop() } }
        .onDisappear(perform: stop)
    }

    private func stop() {
        request = nil
        worker.stop()
    }
}
