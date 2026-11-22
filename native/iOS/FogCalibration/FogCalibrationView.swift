import SwiftUI
import TolglowSensorCore

struct FogCalibrationView: View {
    @StateObject private var client = FogCalibrationClient()
    @AppStorage("studio-origin") private var origin = "http://tolglow.local:8500"
    @State private var previousIdleTimer: Bool?
    @State private var fixture = ""
    @State private var path: Double?
    @State private var shutter: Int32 = 60
    @Environment(\.scenePhase) private var scenePhase
    private var locked: Bool { client.busy || client.prepared || client.fit != nil }

    var body: some View {
        Form {
            Section("Lighting host") {
                TextField("Studio origin", text: $origin).keyboardType(.URL)
                    .textInputAutocapitalization(.never).autocorrectionDisabled().disabled(locked)
                Button("Connect") { client.connect(origin) }.disabled(locked)
                if client.connected {
                    Picker("Fog source", selection: $fixture) {
                        Text("Unmetered · manual burst").tag("")
                        ForEach(client.fixtures) { f in Text(f.name ?? f.id).tag(f.id) }
                    }.disabled(locked)
                }
            }
            if client.connected && client.fit == nil {
                Section("Fixed sightline") {
                    Text("Mount the phone on a stand. At the far end, place adjacent matte white and black patches under one steady light. Keep their lighting, distance and the phone position fixed for the entire measurement.")
                    HStack {
                        Text("Camera to target")
                        Spacer()
                        TextField("0.1–1000", value: $path, format: .number).keyboardType(.decimalPad)
                            .multilineTextAlignment(.trailing)
                        Text("m")
                    }.disabled(locked)
                    Picker("Locked shutter", selection: $shutter) {
                        ForEach([Int32(30), 50, 60, 100, 125, 250, 500], id: \.self) { n in Text("1/\(n) s").tag(n) }
                    }.disabled(locked)
                    if !client.prepared {
                        Button("Lock camera and align") { client.prepare(shutter: shutter) }
                            .disabled(locked || !fogPathMetres.contains(path ?? 0))
                    }
                    if let image = client.preview { SightlinePreview(image: image) }
                    if client.prepared && !client.recording {
                        Button("Start clear-air baseline") { client.start(fixture: fixture, path: path ?? 0) }
                            .disabled(client.busy || client.lastReading == nil)
                    }
                }
            }
            if client.recording {
                Section("Capture") {
                    LabeledContent("Baseline / active / clearing",
                                   value: FogPhase.allCases.map { String(client.counts[$0, default: 0]) }.joined(separator: " / "))
                    if let transmission = client.transmission {
                        LabeledContent("Contrast relative to clear air", value: transmission.formatted(.percent.precision(.fractionLength(1))))
                    }
                    switch client.phase {
                    case .baseline:
                        Button("Mark fog active") { client.advance() }.disabled(!client.baselineReady || client.busy)
                    case .active:
                        Button("Mark clearing") { client.advance() }.disabled(client.counts[.active, default: 0] < 4 || client.busy)
                    case .clearing:
                        Button("Fit measurements") { client.fitSeries() }
                            .disabled(client.counts[.clearing, default: 0] < 8 || client.posting || client.busy)
                    }
                }
            }
            if let fit = client.fit {
                Section("Fitted values") {
                    LabeledContent("Clearance half-life", value: String(format: "%.1f s", fit.clearance))
                    if let mixing = fit.mixing { LabeledContent("Plume mixing time", value: String(format: "%.1f s", mixing)) }
                    if let extinction = fit.extinction { LabeledContent("Extinction", value: String(format: "%.3f /m", extinction)) }
                    if let r = fit.rSquared { LabeledContent("Fit R²", value: String(format: "%.3f", r)) }
                    if let n = fit.samples { LabeledContent("Samples", value: String(n)) }
                    Button("Apply to show") { client.apply() }.disabled(client.busy)
                }
            }
            Section {
                Text(client.status)
                if !client.quality.isEmpty { Text(client.quality).foregroundStyle(.orange) }
                if locked { Button("Cancel measurement", role: .destructive) { client.stop() } }
            }
        }
        .navigationTitle("Room fog")
        .onChange(of: client.fixtures.map(\.id)) { _, ids in
            if !ids.contains(fixture) { fixture = "" }
        }
        .onChange(of: origin) { _, _ in client.resetHost() }
        .onChange(of: client.prepared) { _, prepared in
            if prepared {
                previousIdleTimer = UIApplication.shared.isIdleTimerDisabled
                UIApplication.shared.isIdleTimerDisabled = true
            } else { restoreIdleTimer() }
        }
        .onDisappear { client.stop(); restoreIdleTimer() }
        .onChange(of: scenePhase) { _, phase in
            if phase == .background || (phase == .inactive && (client.prepared || client.recording || client.fit != nil)) {
                client.stop("Fog measurement ended when the app left the foreground.")
            }
        }
    }
    private func restoreIdleTimer() {
        if let previousIdleTimer { UIApplication.shared.isIdleTimerDisabled = previousIdleTimer }
        previousIdleTimer = nil
    }
}

private struct SightlinePreview: View {
    let image: CGImage

    var body: some View {
        Image(decorative: image, scale: 1).resizable().aspectRatio(contentMode: .fit)
            .overlay { GeometryReader { g in
                box(.bright, "White", size: g.size)
                box(.dark, "Black", size: g.size)
            } }
            .accessibilityLabel("Camera sightline. Align the white target in the left box and black target in the right.")
    }

    private func box(_ patch: SightlinePatch, _ label: String, size: CGSize) -> some View {
        Rectangle().strokeBorder(.yellow, lineWidth: 2)
            .overlay(alignment: .bottomLeading) {
                Text(label).font(.caption2.bold()).foregroundStyle(.black).padding(.horizontal, 2).background(.yellow)
            }
            .frame(width: size.width * patch.width, height: size.height * patch.height)
            .position(x: size.width * (patch.x + patch.width / 2), y: size.height * (patch.y + patch.height / 2))
    }
}
