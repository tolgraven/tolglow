import AVFoundation
import SwiftUI

private final class MeterPreviewView: UIView {
    override class var layerClass: AnyClass { AVCaptureVideoPreviewLayer.self }
    var preview: AVCaptureVideoPreviewLayer { layer as! AVCaptureVideoPreviewLayer }
}

private struct MeterPreview: UIViewRepresentable {
    let session: AVCaptureSession
    func makeUIView(context: Context) -> MeterPreviewView {
        let v = MeterPreviewView()
        v.preview.videoGravity = .resizeAspect
        v.preview.session = session
        return v
    }
    func updateUIView(_ uiView: MeterPreviewView, context: Context) {}
}

struct BrightnessCalibrationView: View {
    @StateObject private var client = BrightnessCalibrationClient()
    @AppStorage("studio-origin") private var origin = "http://tolglow.local:8500"
    @State private var selected: [String] = []
    @State private var shutter: Int32 = 250
    @Environment(\.scenePhase) private var scenePhase
    private var locked: Bool { client.running || client.busy }

    var body: some View {
        Form {
            Section("Lighting host") {
                TextField("Studio origin", text: $origin).textInputAutocapitalization(.never)
                    .autocorrectionDisabled().keyboardType(.URL).disabled(locked)
                Button("Connect") { selected = []; client.connect(origin) }.disabled(locked)
            }
            if !client.fixtures.isEmpty {
                Section("Fixture types") {
                    Text("Choose one representative of each type. The first selected fixture sets the 1.00× reference.")
                    ForEach(client.fixtures) { fixture in
                        let isSelected = selected.contains(fixture.id)
                        let sameType = client.fixtures.contains { selected.contains($0.id) && $0.type == fixture.type }
                        Toggle(isOn: Binding(get: { isSelected }, set: { on in
                            if on { selected.append(fixture.id) } else { selected.removeAll { $0 == fixture.id } }
                        })) {
                            Text(fixture.name ?? fixture.id)
                            Text(fixture.type + (selected.first == fixture.id ? " · Reference" : ""))
                        }.disabled(locked || (!isSelected && sameType))
                    }
                }
                Section("Camera and output") {
                    Text("Cover the rear wide camera with a neutral diffuser. For every reading, hold the same diffuser 1.00 m from the lens of the fixture, centered on its beam and facing it squarely. Keep room lighting steady.")
                    Picker("Locked shutter", selection: $shutter) {
                        ForEach([Int32(50), 60, 125, 250, 500, 1000, 2000], id: \.self) { v in
                            Text("1/\(v) s").tag(v)
                        }
                    }.disabled(locked)
                    Button("Lock camera and arm selected fixtures") { client.start(ids: selected, shutter: shutter) }
                        .disabled(locked || !client.outputAvailable || selected.isEmpty)
                }
            }
            if client.running {
                Section("Locked camera") {
                    MeterPreview(session: client.camera.session).frame(height: 160)
                        .overlay { Image(systemName: "plus").foregroundStyle(.white).shadow(radius: 2) }
                    Text("Keep the diffuser evenly lit across the center of the image. Capture briefly switches the selected fixture from dark to full white, then back to dark.")
                }
            }
            if client.running || !client.readings.isEmpty {
                Section("Dark and full-white pairs") {
                    ForEach(selected, id: \.self) { id in
                        HStack {
                            VStack(alignment: .leading) {
                                Text(client.fixtures.first(where: { $0.id == id })?.name ?? id)
                                if let value = client.readings[id], let ref = selected.first.flatMap({ client.readings[$0] }) {
                                    Text(String(format: "%.2f× reference", value.signal / ref.signal)).font(.caption.monospacedDigit())
                                }
                            }
                            Spacer()
                            if client.running {
                                Button(client.readings[id] == nil ? "Capture" : "Repeat") { client.capture(id) }
                                    .buttonStyle(.bordered).disabled(client.busy)
                            }
                        }
                    }
                    if client.running {
                        Button("Save relative brightness") { client.apply() }
                            .disabled(client.busy || selected.contains(where: { client.readings[$0] == nil }))
                    }
                }
            }
            if locked {
                Button("Stop and release lights", role: .destructive) { client.stop() }
            }
            Section { Text(client.status).font(.callout).textSelection(.enabled) }
        }
        .navigationTitle("Fixture brightness")
        .onDisappear { client.stop() }
        .onChange(of: scenePhase) { _, phase in
            if phase == .background { client.stop("Calibration stopped when the app left the foreground.") }
        }
    }
}
