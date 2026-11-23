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
        let vertices = SCNGeometrySource(vertices: worldCorners)
        let edges = SCNGeometryElement(indices: [Int32(0), 1, 1, 2, 2, 3, 3, 0], primitiveType: .line)
        let geometry = SCNGeometry(sources: [vertices], elements: [edges])
        let material = SCNMaterial()
        material.lightingModel = .constant
        material.diffuse.contents = UIColor.systemYellow
        geometry.materials = [material]
        return SCNNode(geometry: geometry)
    }
}
