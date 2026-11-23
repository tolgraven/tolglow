import Foundation

/// A finite receiver fitted to the convex boundary supplied by ARKit.
/// Boundary points use the anchor's local X/Z plane, in metres.
public struct ReceiverPlane {
    public let center: SIMD2<Float>
    public let widthAxis: SIMD2<Float>
    public let size: SIMD2<Float>

    public init?(boundary: [SIMD2<Float>]) {
        guard boundary.count >= 3, boundary.allSatisfy({ $0.x.isFinite && $0.y.isFinite }) else { return nil }
        // A smallest enclosing rectangle shares an edge direction with the convex hull.
        // Measurement happens once per tap. No fitting runs on the AR frame callback.
        var bestArea = Float.infinity
        var best: (SIMD2<Float>, SIMD2<Float>, SIMD2<Float>)?
        var previous = boundary[boundary.count - 1]
        for next in boundary {
            let edge = next - previous
            previous = next
            let length = hypot(edge.x, edge.y)
            guard length > 0.0001 else { continue }
            let u = edge / length
            let v = SIMD2(-u.y, u.x)
            var minU = Float.infinity, maxU = -Float.infinity
            var minV = Float.infinity, maxV = -Float.infinity
            for point in boundary {
                let pu = point.x * u.x + point.y * u.y, pv = point.x * v.x + point.y * v.y
                minU = min(minU, pu); maxU = max(maxU, pu)
                minV = min(minV, pv); maxV = max(maxV, pv)
            }
            let span = SIMD2(maxU - minU, maxV - minV)
            let area = span.x * span.y
            if area < bestArea {
                bestArea = area
                best = (u * ((minU + maxU) / 2) + v * ((minV + maxV) / 2), u, span)
            }
        }
        guard let (center, axis, size) = best, size.x >= 0.05, size.y >= 0.05 else { return nil }
        self.center = center; self.widthAxis = axis; self.size = size
    }

    public var corners: [SIMD2<Float>] {
        let du = widthAxis * (size.x / 2), dv = SIMD2(-widthAxis.y, widthAxis.x) * (size.y / 2)
        return [center - du - dv, center + du - dv, center + du + dv, center - du + dv]
    }
}
