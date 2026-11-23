import ARKit
import AVFoundation
import Combine
import SceneKit
import SwiftUI

/// IO boundary only. Captures real camera/depth observations; never guesses a
/// fixture identity or sends lighting output. Lens and beam points are separate.
final class RigScanner: NSObject, ObservableObject, ARSessionDelegate {
    let session = ARSession()
    @Published private(set) var status = "Start camera, then tap a visible fixture lens"
    @Published private(set) var running = false
    @Published private(set) var captured = 0
    @Published private(set) var worldID = UUID().uuidString
    @Published var selectedKind = "origin"
    @Published private(set) var worldToRig: [Float]?
    @Published private(set) var surface: VenueSurface?
    private var alignmentOrigin: SIMD3<Float>?
    private var alignmentHeading: SIMD3<Float>?
    private var tracks: [String: (id: String, point: SIMD3<Float>)] = [:]
    private var pending: [[String: Any]] = []
    private var lastSample = 0.0
    private var lastFrameArrival = 0.0
    private var latestTimestamp = 0.0
    private var goodFrameInterval = false
    private var permissionGeneration = UUID()
    var onInterrupted: (() -> Void)?
    var captureEnabled = false

    override init() {
        super.init()
        session.delegate = self; session.delegateQueue = .main
    }
    func replayPlane() {
        surface = VenueSurface(boundary: [SIMD3(-2,0,-1),SIMD3(2,0,-1),SIMD3(2,0,1),SIMD3(-2,0,1)],
                               transform: SimulatorEvidence.transform, identifier: UUID(), worldID: worldID, alignment: SimulatorEvidence.axes)
        status = "Review the outlined rectangle and its dimensions before saving."
    }
    func start() {
        if SimulatorEvidence.scene == "plane" {
            worldToRig = SimulatorEvidence.axes; selectedKind = "surface"; running = true
            status = "Tap inside the tracked plane to freeze its measurement."
            return
        }
        let token = permissionGeneration
        guard ARWorldTrackingConfiguration.isSupported,
              ARWorldTrackingConfiguration.supportsFrameSemantics(.sceneDepth) else {
            status = "This scan needs an iPhone or iPad with LiDAR. Use manual Studio placement on this device."; return
        }
        switch AVCaptureDevice.authorizationStatus(for: .video) {
        case .authorized: run()
        case .notDetermined:
            AVCaptureDevice.requestAccess(for: .video) { [weak self] allowed in
                DispatchQueue.main.async { guard self?.permissionGeneration == token else { return }; if allowed { self?.run() } else { self?.status = "Camera permission denied" } }
            }
        default: status = "Enable camera access for Tolglow in Settings"
        }
    }
    private func run() {
        guard !running else { return }
        let configuration = ARWorldTrackingConfiguration()
        configuration.worldAlignment = .gravity
        configuration.frameSemantics.insert(.sceneDepth)
        configuration.planeDetection = [.horizontal, .vertical]
        if ARWorldTrackingConfiguration.supportsSceneReconstruction(.mesh) { configuration.sceneReconstruction = .mesh }
        worldID = UUID().uuidString; tracks.removeAll(); pending.removeAll(); captured = 0
        latestTimestamp = 0; goodFrameInterval = false; captureEnabled = false
        alignmentOrigin = nil; alignmentHeading = nil; worldToRig = nil; surface = nil
        session.run(configuration, options: [.resetTracking, .removeExistingAnchors])
        running = true; status = "Move slowly to map the room. Tap a visible lens; depth must reach the fixture, not foreground haze."
    }
    func stop() {
        permissionGeneration = UUID()
        worldToRig = nil; surface = nil
        captureEnabled = false; session.pause(); running = false; tracks.removeAll(); pending.removeAll()
        status = "Camera stopped. A new scan needs a new world alignment."
    }
    /// Timestamp sampled in ARFrame's clock domain, with a conservative 50 ms
    /// frame age bound. Refuse stale/slow tracking instead of inventing alignment.
    func clockSample() throws -> Double {
        guard running, goodFrameInterval, CACurrentMediaTime() - lastFrameArrival < 0.05 else {
            throw NSError(domain: "TolglowScan", code: 1, userInfo: [NSLocalizedDescriptionKey: "Wait for stable AR tracking before clock alignment"])
        }
        return latestTimestamp * 1000
    }
    func takePending() -> [[String: Any]] {
        let batch = Array(pending.prefix(64)); pending.removeFirst(batch.count); return batch
    }
    func clearBeamTrack() { tracks.removeValue(forKey: "beam") }
    func clearSurface() { surface = nil }
    func selectSurface(at point: CGPoint, in view: ARSCNView) {
        surface = nil
        guard running, !captureEnabled, let worldToRig, let frame = session.currentFrame,
              case .normal = frame.camera.trackingState else {
            status = "Set the floor origin and heading with stable tracking before measuring a surface."
            return
        }
        guard let query = view.raycastQuery(from: point, allowing: .existingPlaneGeometry, alignment: .any),
              let plane = session.raycast(query).first?.anchor as? ARPlaneAnchor,
              let result = VenueSurface(anchor: plane, worldID: worldID, alignment: worldToRig) else {
            status = "Walk along the surface, then tap inside its tracked area."
            return
        }
        surface = result
        status = "Review the outlined rectangle and its dimensions before saving."
    }
    func tap(_ point: CGPoint, viewport: CGSize, orientation: UIInterfaceOrientation) {
        guard let frame = session.currentFrame, case .normal = frame.camera.trackingState else { return }
        let normalized = CGPoint(x: point.x / viewport.width, y: point.y / viewport.height)
            .applying(frame.displayTransform(for: orientation, viewportSize: viewport).inverted())
        let pixel = SIMD2<Float>(Float(normalized.x) * Float(CVPixelBufferGetWidth(frame.capturedImage)),
                                 Float(normalized.y) * Float(CVPixelBufferGetHeight(frame.capturedImage)))
        guard let sample = sample(frame, pixel: pixel), let coordinates = sample["world-point"] as? [Float] else {
            status = "No confident depth here. Rescan closer or tap an unobscured surface."; return
        }
        let world = SIMD3(coordinates[0], coordinates[1], coordinates[2])
        if selectedKind == "origin" || selectedKind == "heading" {
            guard !captureEnabled else { status = "Finish the current scan before changing alignment"; return }
            worldToRig = nil; surface = nil
            if selectedKind == "origin" { alignmentOrigin = world } else { alignmentHeading = world }
            if let origin = alignmentOrigin, let heading = alignmentHeading {
                var z = heading - origin
                guard abs(z.y) < 0.15 else { status = "Tap two points on the same level floor"; return }
                z.y = 0
                guard simd_length(z) >= 0.5 else { status = "Choose a heading point at least half a metre from origin"; return }
                z = simd_normalize(z)
                let up = SIMD3<Float>(0, 1, 0), x = simd_cross(up, z)
                worldToRig = [x.x, x.y, x.z, -simd_dot(x, origin),
                              0, 1, 0, -origin.y, z.x, z.y, z.z, -simd_dot(z, origin), 0, 0, 0, 1]
                status = "Rig origin and +Z heading set; now select Lens or Beam and tap its surface."
            } else { status = "Now tap a floor point in the rig's positive Z direction" }
            return
        }
        tracks[selectedKind] = (UUID().uuidString, world)
        status = "Tracking \(selectedKind). Keep it visible through red, green and blue steps."
    }
    func session(_ session: ARSession, didUpdate frame: ARFrame) {
        let interval = frame.timestamp - latestTimestamp
        goodFrameInterval = interval > 0 && interval <= 0.05
        latestTimestamp = frame.timestamp; lastFrameArrival = CACurrentMediaTime()
        guard captureEnabled, frame.timestamp - lastSample >= 0.1, case .normal = frame.camera.trackingState else { return }
        lastSample = frame.timestamp
        for (kind, track) in tracks {
            let camera = simd_inverse(frame.camera.transform) * SIMD4(track.point, 1)
            guard camera.z < -0.05 else { continue }
            let k = frame.camera.intrinsics
            let pixel = SIMD2(k[0][0] * camera.x / -camera.z + k[2][0],
                              k[1][1] * -camera.y / -camera.z + k[2][1])
            guard var observation = sample(frame, pixel: pixel), let p = observation.removeValue(forKey: "world-point") as? [Float],
                  simd_distance(track.point, SIMD3(p[0], p[1], p[2])) < 0.15 else { continue }
            observation["observation-id"] = UUID().uuidString
            observation["world-id"] = worldID; observation["kind"] = kind; observation["track-id"] = track.id
            if pending.count >= 128 {
                captureEnabled = false; status = "Upload queue full; capture paused. Reconnect or finish the scan."; onInterrupted?(); return
            }
            pending.append(observation); captured += 1
        }
    }
    private func sample(_ frame: ARFrame, pixel: SIMD2<Float>) -> [String: Any]? {
        guard let depth = frame.sceneDepth, let confidence = depth.confidenceMap else { return nil }
        let image = frame.capturedImage
        let width = CVPixelBufferGetWidth(image), height = CVPixelBufferGetHeight(image)
        let u = Int(pixel.x), v = Int(pixel.y)
        guard u >= 0, v >= 0, u < width, v < height else { return nil }
        let depthMap = depth.depthMap
        let dx = u * CVPixelBufferGetWidth(depthMap) / width, dy = v * CVPixelBufferGetHeight(depthMap) / height
        for buffer in [depthMap, confidence, image] { CVPixelBufferLockBaseAddress(buffer, .readOnly) }
        defer { for buffer in [depthMap, confidence, image] { CVPixelBufferUnlockBaseAddress(buffer, .readOnly) } }
        guard let depthBase = CVPixelBufferGetBaseAddress(depthMap), let confidenceBase = CVPixelBufferGetBaseAddress(confidence),
              CVPixelBufferGetPlaneCount(image) == 2,
              let yBase = CVPixelBufferGetBaseAddressOfPlane(image, 0), let uvBase = CVPixelBufferGetBaseAddressOfPlane(image, 1) else { return nil }
        let z = depthBase.advanced(by: dy * CVPixelBufferGetBytesPerRow(depthMap)).assumingMemoryBound(to: Float.self)[dx]
        let quality = confidenceBase.advanced(by: dy * CVPixelBufferGetBytesPerRow(confidence)).assumingMemoryBound(to: UInt8.self)[dx]
        guard z.isFinite, z > 0.05, z < 30, quality >= 1 else { return nil }
        let y = Float(yBase.advanced(by: v * CVPixelBufferGetBytesPerRowOfPlane(image, 0)).assumingMemoryBound(to: UInt8.self)[u]) / 255
        let uv = uvBase.advanced(by: (v / 2) * CVPixelBufferGetBytesPerRowOfPlane(image, 1)).assumingMemoryBound(to: UInt8.self)
        let cb = Float(uv[(u / 2) * 2]) / 255 - 0.5, cr = Float(uv[(u / 2) * 2 + 1]) / 255 - 0.5
        let rgb = [y + 1.402 * cr, y - 0.344136 * cb - 0.714136 * cr, y + 1.772 * cb].map { min(1, max(0, $0)) }
        let k = frame.camera.intrinsics
        let local = SIMD4((pixel.x - k[2][0]) / k[0][0] * z, -(pixel.y - k[2][1]) / k[1][1] * z, -z, 1)
        let world = frame.camera.transform * local
        return ["capture-ms": frame.timestamp * 1000, "pixel": [pixel.x, pixel.y], "depth-m": z,
                "intrinsics": [k[0][0], k[1][1], k[2][0], k[2][1]],
                "camera-to-world": (0..<4).flatMap { row in (0..<4).map { column in frame.camera.transform[column][row] } },
                "confidence": Int(quality), "rgb": rgb, "world-point": [world.x, world.y, world.z]]
    }
    private func invalidateWorld() {
        captureEnabled = false; surface = nil; worldToRig = nil
        alignmentOrigin = nil; alignmentHeading = nil
        session.pause(); running = false
    }
    func sessionWasInterrupted(_ session: ARSession) {
        invalidateWorld(); status = "Tracking interrupted; finish and start a new scan/world."; onInterrupted?()
    }
    func session(_ session: ARSession, didFailWithError error: Error) {
        invalidateWorld(); status = "AR tracking failed: \(error.localizedDescription)"; onInterrupted?()
    }
}

struct RigCameraView: UIViewRepresentable {
    let scanner: RigScanner
    let surface: VenueSurface?
    func makeCoordinator() -> Coordinator { Coordinator(scanner: scanner) }
    func makeUIView(context: Context) -> ARSCNView {
        let view = ARSCNView()
        if SimulatorEvidence.scene == "plane" { SimulatorEvidence.room(view) } else { view.session = scanner.session }
        view.addGestureRecognizer(UITapGestureRecognizer(target: context.coordinator, action: #selector(Coordinator.tap(_:))))
        return view
    }
    func updateUIView(_ view: ARSCNView, context: Context) {
        context.coordinator.updateOutline(surface, in: view)
    }
    final class Coordinator: NSObject {
        let scanner: RigScanner
        init(scanner: RigScanner) { self.scanner = scanner }
        private var drawnCorners: [SIMD3<Float>] = []
        private var outline: SCNNode?
        func updateOutline(_ surface: VenueSurface?, in view: ARSCNView) {
            let corners = surface?.worldCorners.map { SIMD3($0.x, $0.y, $0.z) } ?? []
            guard corners != drawnCorners else { return }
            drawnCorners = corners
            outline?.removeFromParentNode()
            outline = surface?.outline()
            if let outline { view.scene.rootNode.addChildNode(outline) }
        }
        @objc func tap(_ gesture: UITapGestureRecognizer) {
            guard let view = gesture.view as? ARSCNView else { return }
            if scanner.selectedKind == "surface" {
                scanner.selectSurface(at: gesture.location(in: view), in: view)
                return
            }
            scanner.tap(gesture.location(in: view), viewport: view.bounds.size,
                        orientation: view.window?.windowScene?.interfaceOrientation ?? .portrait)
        }
    }
}
