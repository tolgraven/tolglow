import ARKit
import SceneKit
import TolglowSensorCore

struct VenueSurface {
    let worldID: String
    let anchorID: UUID
    let point: SIMD3<Float>
    let normal: SIMD3<Float>
    let widthAxis: SIMD3<Float>
    let size: SIMD2<Float>
    let worldCorners: [SCNVector3]

    init?(anchor: ARPlaneAnchor, worldID: String, alignment: [Float]) {
        guard alignment.count == 16,
              let plane = ReceiverPlane(boundary: anchor.geometry.boundaryVertices.map { SIMD2($0.x, $0.z) })
        else { return nil }
        let rows = stride(from: 0, to: 16, by: 4).map { SIMD4(alignment[$0], alignment[$0 + 1], alignment[$0 + 2], alignment[$0 + 3]) }
        let localToRig = simd_float4x4(rows: rows) * anchor.transform
        func rigVector(_ value: SIMD4<Float>) -> SIMD3<Float> {
            let v = localToRig * value
            return SIMD3(v.x, v.y, v.z)
        }
        self.worldID = worldID
        anchorID = anchor.identifier
        point = rigVector(SIMD4(plane.center.x, 0, plane.center.y, 1))
        normal = rigVector(SIMD4(0, 1, 0, 0))
        widthAxis = rigVector(SIMD4(plane.widthAxis.x, 0, plane.widthAxis.y, 0))
        size = plane.size
        worldCorners = plane.corners.map { corner in
            let v = anchor.transform * SIMD4(corner.x, 0, corner.y, 1)
            return SCNVector3(v.x, v.y, v.z)
        }
    }

    var geometry: [String: Any] {
        ["world-id": worldID, "anchor-id": anchorID.uuidString,
         "point": [point.x, point.y, point.z], "normal": [normal.x, normal.y, normal.z],
         "u-axis": [widthAxis.x, widthAxis.y, widthAxis.z], "size": [size.x, size.y]]
    }

    func outline() -> SCNNode {
        let root = SCNNode()
        let material = SCNMaterial()
        material.lightingModel = .constant
        material.diffuse.contents = UIColor.systemYellow
        material.readsFromDepthBuffer = false
        material.writesToDepthBuffer = false
        // Give the selection a finite width in metres. A one-pixel line
        // coincident with the tracked plane can disappear on Retina displays.
        let radius = min(0.018, max(0.002, 0.003 * min(size.x, size.y)))
        for i in worldCorners.indices {
            let start = worldCorners[i], end = worldCorners[(i + 1) % worldCorners.count]
            let a = SIMD3<Float>(start.x, start.y, start.z), b = SIMD3<Float>(end.x, end.y, end.z)
            let length = simd_distance(a, b)
            guard length > 0 else { continue }
            let tube = SCNCylinder(radius: CGFloat(radius), height: CGFloat(length))
            tube.radialSegmentCount = 6
            tube.materials = [material]
            let edge = SCNNode(geometry: tube)
            edge.simdPosition = (a + b) / 2
            edge.simdOrientation = simd_quatf(from: SIMD3<Float>(0, 1, 0), to: (b - a) / length)
            edge.renderingOrder = 100
            root.addChildNode(edge)
        }
        return root
    }
}
