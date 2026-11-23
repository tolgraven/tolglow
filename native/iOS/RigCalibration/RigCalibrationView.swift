import SwiftUI

struct RigCalibrationView: View {
    @ObservedObject var client: RigCalibrationClient
    @ObservedObject var scanner: RigScanner
    @AppStorage("studio-origin") private var origin = "http://tolglow.local:8500"
    @State private var selected: Set<String> = []
    @State private var surfaceName = ""
    @State private var verifiedAxes = false
    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(alignment: .leading, spacing: 16) {
                    NavigationLink {
                        FogCalibrationView()
                    } label: {
                        Label("Measure room fog", systemImage: "cloud.fog")
                    }.disabled(scanner.running || client.busy || client.armed)
                    NavigationLink {
                        BrightnessCalibrationView()
                    } label: {
                        Label("Measure fixture brightness", systemImage: "sun.max")
                    }.disabled(scanner.running || client.busy || client.armed)
                    Text("Scan your rig").font(.largeTitle.bold())
                    Text("LiDAR measures visible surfaces. Tap the actual lens for placement and a solid beam landing spot for aim. Fog, reflections and hidden fixtures need manual correction or another view.")
                        .font(.callout).foregroundStyle(.secondary)
                    TextField("Studio origin", text: $origin).textInputAutocapitalization(.never).autocorrectionDisabled().keyboardType(.URL)
                    HStack {
                        Button("Connect") { client.connect(origin) }.disabled(client.busy || client.armed)
                        Button(scanner.running ? "Stop camera" : "Start LiDAR camera") {
                            if scanner.running { client.finish(stopCamera: true) } else { scanner.start() }
                        }
                    }.buttonStyle(.bordered).controlSize(.large)
                    if scanner.running {
                        RigCameraView(scanner: scanner).frame(minHeight: 320, idealHeight: 420).clipShape(RoundedRectangle(cornerRadius: 18))
                        Picker("Tap identifies", selection: $scanner.selectedKind) {
                            Text("Floor origin").tag("origin"); Text("Floor +Z").tag("heading")
                            Text("Lens").tag("lens"); Text("Beam spot").tag("beam")
                            Text("Room surface").tag("surface")
                        }.pickerStyle(.menu)
                        Text(scanner.worldToRig == nil ? "First tap the rig floor origin, then a floor point in its +Z direction (at least 0.5 m apart)." : "Gravity sets +Y. Floor origin and +Z align the scan to Studio coordinates.")
                            .font(.caption).foregroundStyle(.secondary)
                        if scanner.selectedKind == "surface" { surfaceControls }
                        Text(scanner.status).font(.callout)
                    }
                    VStack(alignment: .leading, spacing: 10) {
                        Text("Fixtures to identify").font(.headline)
                        ForEach(client.fixtures.compactMap { $0["id"] as? String }, id: \.self) { id in
                            Toggle(id, isOn: Binding(get: { selected.contains(id) }, set: { if $0 { selected.insert(id) } else { selected.remove(id) } }))
                                .disabled(client.armed)
                        }
                    }.padding().background(.thinMaterial, in: RoundedRectangle(cornerRadius: 16))
                    if let scan = client.scan, let plan = scan["plan"] as? [[String: Any]] {
                        let index = scan["step-index"] as? Int ?? -1
                        if index >= 0, index < plan.count {
                            let step = plan[index]
                            Text("Step \(index + 1)/\(plan.count) · \(step["fixture-id"] as? String ?? "") · pose \((step["pose"] as? Int ?? 0) + 1)").font(.headline)
                            Text("Hold for at least two beats. For each new pose, tap the beam landing spot. Keep the lens visible through all three colors.").font(.caption)
                        }
                        Text("\(scan["observation-count"] as? Int ?? 0) uploaded observations · \(scan["receipt-count"] as? Int ?? 0) physical receipts").font(.caption.monospaced())
                    }
                    HStack {
                        Button("Arm calibration output") { client.arm(selectedFixtures: Array(selected).sorted()) }
                            .disabled(client.busy || client.armed || !client.outputCapable || selected.isEmpty || scanner.worldToRig == nil)
                        Button("Next pattern") { client.nextStep() }.disabled(client.busy || !client.armed)
                    }.buttonStyle(.borderedProminent).controlSize(.large)
                    Button("Finish / release calibration lights") { client.finish() }
                        .buttonStyle(.bordered).tint(.red).controlSize(.large).disabled(!client.armed)
                    Text("Starting applies only the selected fixtures. Other human cues keep running. Leaving the app stops capture and requests release; the host also expires calibration after 15 seconds without a heartbeat.")
                        .font(.caption).foregroundStyle(.secondary)
                    Toggle("I verified the fixture definition's pan/tilt ranges and axis directions", isOn: $verifiedAxes)
                    Text("Without that check, mounting orientation remains unknown. This version does not infer motor direction and upside-down mounting simultaneously.").font(.caption)
                    Button("Fit captured evidence") { client.solve(verifiedAxes: verifiedAxes) }
                        .buttonStyle(.borderedProminent).disabled(client.busy || client.armed || client.scan == nil)
                    if let candidates = client.scan?["candidates"] as? [[String: Any]] {
                        ForEach(candidates.indices, id: \.self) { index in
                            candidate(candidates[index])
                        }
                    }
                    Text(client.status).font(.callout).textSelection(.enabled)
                    Text("Tap-assisted capture is available here. Automatic lens/blob detection, mesh export and validated hardware calibration accuracy remain development work.")
                        .font(.caption).foregroundStyle(.secondary)
                }.padding()
            }.navigationTitle("Rig scan").navigationBarTitleDisplayMode(.inline)
        }
    }
    @ViewBuilder private var surfaceControls: some View {
        Text("Walk along a wall, floor or platform, then tap it. The outline encloses the detected patch and can extend across gaps.")
            .font(.caption)
        if let surface = scanner.surface {
            Text(String(format: "Receiver %.2f × %.2f m · center (%.2f, %.2f, %.2f) m",
                        surface.size.x, surface.size.y, surface.point.x, surface.point.y, surface.point.z))
                .monospacedDigit()
            TextField("Surface name (e.g. booth-wall)", text: $surfaceName)
                .textInputAutocapitalization(.never).autocorrectionDisabled().textFieldStyle(.roundedBorder)
            if !client.surfaceRevisions.isEmpty {
                Menu("Remeasure a saved surface") {
                    ForEach(client.surfaceRevisions.keys.sorted(), id: \.self) { name in
                        Button(name) { surfaceName = name }
                    }
                }
            }
            let replacing = client.surfaceRevisions[surfaceName] != nil
            Button(replacing ? "Replace saved measurement" : "Add receiver to rig") {
                client.saveSurface(named: surfaceName)
            }
            .buttonStyle(.borderedProminent)
            .disabled(!client.connected || client.busy || client.armed ||
                      surfaceName.range(of: #"\A[a-z0-9][a-z0-9-]{0,47}\z"#, options: .regularExpression) == nil)
        }
    }
    @ViewBuilder private func candidate(_ value: [String: Any]) -> some View {
        let id = value["fixture-id"] as? String ?? "Unknown fixture"
        let position = value["position"] as? [String: Any]
        let aim = value["aim"] as? [String: Any]
        VStack(alignment: .leading, spacing: 10) {
            Text(id).font(.headline)
            if let xyz = position?["position"] as? [Double], xyz.count == 3 {
                Text(String(format: "Position %.2f, %.2f, %.2f m", xyz[0], xyz[1], xyz[2])).monospacedDigit()
                Text(String(format: "Position residual %.1f cm · %d inliers", (position?["rms-m"] as? Double ?? 0) * 100, position?["inliers"] as? Int ?? 0)).font(.caption)
            }
            Text("Mounting: \(aim?["mounting"] as? String ?? "unresolved")")
            if let rms = aim?["rms-deg"] as? Double { Text(String(format: "Aim residual %.2f°", rms)).font(.caption) }
            Text("Review the result in Studio's rig view. Applying preserves patch addresses and fixture definitions.").font(.caption)
            Button("Apply reviewed \(id)") { client.apply(id) }
                .buttonStyle(.borderedProminent).disabled(client.busy || value["reviewable?"] as? Bool != true)
        }.padding().frame(maxWidth: .infinity, alignment: .leading).background(.thinMaterial, in: RoundedRectangle(cornerRadius: 16))
    }
}
