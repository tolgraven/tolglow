import SwiftUI

struct SurfaceReflectanceView: View {
    @StateObject private var client = BrightnessCalibrationClient()
    @Environment(\.scenePhase) private var phase
    @AppStorage("studio-origin") private var origin = "http://tolglow.local:8500"
    @State private var surfaceID = ""
    @State private var lamp = ""
    @State private var nominal: Double? = 0.18
    @State private var tolerance: Double?
    @State private var shutter: Int32 = 125
    private var receiver: MeterReceiver? { client.receivers.first { $0.id == surfaceID } }
    private var locked: Bool { client.busy || client.running }
    private var validCard: Bool {
        guard let nominal, let tolerance else { return false }
        return tolerance > 0 && nominal - tolerance > 0 && nominal + tolerance < 1
    }

    var body: some View {
        Form {
            Section("Room and fixture") {
                TextField("Studio origin", text: $origin).textInputAutocapitalization(.never)
                    .autocorrectionDisabled().keyboardType(.URL)
                Button("Connect") { client.connect(origin) }
                Picker("Receiver", selection: $surfaceID) {
                    Text("Choose surface").tag("")
                    ForEach(client.receivers) { r in Text(r.id).tag(r.id) }
                }
                if let p = receiver?.point, p.count == 3 {
                    Text(String(format: "Measure at rig center (%.2f, %.2f, %.2f) m", p[0], p[1], p[2]))
                }
                Picker("Calibrated fixed fixture", selection: $lamp) {
                    Text("Choose fixture").tag("")
                    ForEach(client.fixtures.filter { $0.reflectanceCapable == true }) { f in Text(f.name ?? f.id).tag(f.id) }
                }
            }.disabled(locked)

            Section("Reference card") {
                Text("Aim the fixed light at the receiver center. Mount the phone facing that point. Place a matte neutral card flush against the surface. Keep both free of glare, shadows and fog.")
                HStack { Text("Rated fraction"); Spacer(); TextField("0.18", value: $nominal, format: .number).keyboardType(.decimalPad).multilineTextAlignment(.trailing) }
                HStack { Text("Rated ± tolerance"); Spacer(); TextField("e.g. 0.01", value: $tolerance, format: .number).keyboardType(.decimalPad).multilineTextAlignment(.trailing) }
                Picker("Locked shutter", selection: $shutter) {
                    ForEach([Int32(50), 60, 125, 250, 500, 1000], id: \.self) { Text("1/\($0) s").tag($0) }
                }
                Text("Arming turns the selected fixture full white and darkens other patched emitters.")
                Button("Lock camera and arm reference light") {
                    if let receiver, let nominal, let tolerance {
                        client.start(ids: [lamp], shutter: shutter, surface: .init(receiver: receiver, fraction: nominal, tolerance: tolerance))
                    }
                }.disabled(!client.outputAvailable || receiver == nil || lamp.isEmpty || !validCard)
            }.disabled(locked)

            if client.running {
                Section("Fixed camera") {
                    MeterPreview(session: client.camera.session, showsSamplingArea: true).frame(height: 220)
                    switch client.pairCount {
                    case 0: Text("Cover the entire outlined sampling area with the card. Lock the tripod in this position.")
                    case 1: Text("Remove the card. The outline must now cover only the original matte receiver.")
                    case 2: Text("Replace the same card, flush at the original point, to check for drift.")
                    default: EmptyView()
                    }
                    if client.pairCount < 3 {
                        Button(["Measure reference", "Measure uncovered surface", "Verify reference"][min(client.pairCount, 2)]) {
                            client.captureSurface()
                        }.disabled(client.busy)
                    }
                }
                if let fit = client.surfaceFit, fit.range.count == 2 {
                    Section("Relative diffuse return") {
                        LabeledContent("Compared with card", value: String(format: "%.2f×", fit.ratio))
                        LabeledContent("Estimated reflectance", value: fit.estimate.formatted(.percent.precision(.fractionLength(1))))
                        Text(String(format: "Observed range %.1f–%.1f%% including card tolerance and frame variation",
                                    fit.range[0] * 100, fit.range[1] * 100))
                        Button("Save to receiver") { client.applySurface() }.disabled(client.busy)
                    }
                }
            }
            if locked { Button("Stop and release lights", role: .destructive) { client.stop() } }
            Text(client.status).font(.callout).textSelection(.enabled)
        }
        .navigationTitle("Surface reflectance").onDisappear { client.stop() }
        .onChange(of: origin) { _, _ in surfaceID = ""; lamp = ""; client.originChanged() }
        .onChange(of: phase) { _, new in if new == .background { client.stop() } }
    }
}
