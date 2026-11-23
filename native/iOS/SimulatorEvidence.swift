import SwiftUI
import SceneKit
import ARKit
import Combine

// Disposable CI fixture, on this capture branch only. The shipping app has no replay switch.
enum SimulatorEvidence {
    static var scene: String { ProcessInfo.processInfo.environment["TOLGLOW_CAPTURE"] ?? "" }
    static var active: Bool { !scene.isEmpty }
    static let timer = Timer.publish(every: 0.18, on: .main, in: .common).autoconnect()
    static let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    static func next() -> String? {
        let path = dir.appendingPathComponent("stage")
        guard let text = try? String(contentsOf: path, encoding: .utf8) else { return nil }
        try? FileManager.default.removeItem(at: path)
        return text.trimmingCharacters(in: .whitespacesAndNewlines)
    }
    static func note(_ text: String) {
        try? text.write(to: dir.appendingPathComponent("observed"), atomically: true, encoding: .utf8)
    }
    static func badge(_ text: String) -> some View {
        HStack { Image(systemName: "iphone"); Text(text).font(.caption.weight(.medium)); Spacer() }
            .foregroundStyle(.secondary).padding(.horizontal, 18).padding(.vertical, 6).background(.thinMaterial)
    }
    static let axes: [Float] = [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]
    static let transform = simd_float4x4(columns: (SIMD4(1,0,0,0),SIMD4(0,0,1,0),SIMD4(0,-1,0,0),SIMD4(0,1,-3,1)))

    // Meter-scale simulated receiver supplies a camera background and the same boundary as the incoming anchor.
    static func room(_ view: ARSCNView) {
        let scene = SCNScene(); view.scene = scene
        scene.background.contents = UIColor(red: 0.045, green: 0.054, blue: 0.065, alpha: 1)
        func panel(_ size: SCNVector3, _ position: SCNVector3, _ color: UIColor) {
            let block = SCNBox(width: CGFloat(size.x), height: CGFloat(size.y), length: CGFloat(size.z), chamferRadius: 0.01)
            let material = SCNMaterial(); material.diffuse.contents = color; material.roughness.contents = 0.9
            block.materials = [material]
            let node = SCNNode(geometry: block); node.position = position; scene.rootNode.addChildNode(node)
        }
        panel(SCNVector3(7,0.06,7), SCNVector3(0,-0.035,-2), UIColor(white: 0.24, alpha: 1))
        panel(SCNVector3(4,2,0.1), SCNVector3(0,1,-3.055), UIColor(red: 0.35,green: 0.36,blue: 0.38,alpha: 1))
        for offset in [-1.5,-0.5,0.5,1.5] {
            panel(SCNVector3(0.006,1.99,0.005), SCNVector3(Float(offset),1,-3.001), UIColor(white: 0.22,alpha: 1))
        }
        let key = SCNNode(); key.light = SCNLight(); key.light?.type = .omni; key.light?.intensity = 1300
        key.position = SCNVector3(2,3.5,0.4); scene.rootNode.addChildNode(key)
        let ambient = SCNNode(); ambient.light = SCNLight(); ambient.light?.type = .ambient; ambient.light?.intensity = 320
        scene.rootNode.addChildNode(ambient)
        let camera = SCNNode(); camera.camera = SCNCamera(); camera.camera?.fieldOfView = 58
        camera.position = SCNVector3(0.9,1.65,1.55); scene.rootNode.addChildNode(camera)
        camera.look(at: SCNVector3(0,0.95,-3)); view.pointOfView = camera
        view.isPlaying = true
    }
}
