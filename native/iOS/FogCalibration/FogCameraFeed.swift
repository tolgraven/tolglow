import AVFoundation
import CoreImage
import TolglowSensorCore

struct FogFrame {
    let uptime: TimeInterval
    let preview: CGImage
    let reading: Result<FogReading, Error>
}

/// Sampling and thumbnail generation run on PhotometryCamera's serial capture queue.
/// Only the newest frame survives a slow network request.
final class FogCameraFeed {
    let frames: AsyncStream<FogFrame>
    private let continuation: AsyncStream<FogFrame>.Continuation
    private let context = CIContext(options: [.cacheIntermediates: false])
    private var nextFrame = 0.0

    init() {
        let stream = AsyncStream<FogFrame>.makeStream(bufferingPolicy: .bufferingNewest(1))
        frames = stream.stream
        continuation = stream.continuation
    }

    func capture(_ buffer: CVPixelBuffer, transfer: SDRTransfer, uptime: TimeInterval) {
        guard uptime >= nextFrame else { return }
        nextFrame = uptime + 0.3
        let source = CIImage(cvPixelBuffer: buffer)
        let scale = min(1, 480 / max(source.extent.width, source.extent.height))
        let thumb = source.transformed(by: .init(scaleX: scale, y: scale))
        guard let image = context.createCGImage(thumb, from: thumb.extent) else { return }
        let reading = Result { try Self.measure(buffer, transfer) }
        continuation.yield(FogFrame(uptime: uptime, preview: image, reading: reading))
    }

    private static func measure(_ buffer: CVPixelBuffer, _ transfer: SDRTransfer) throws -> FogReading {
        guard CVPixelBufferGetPixelFormatType(buffer) == kCVPixelFormatType_32BGRA,
              CVPixelBufferLockBaseAddress(buffer, .readOnly) == kCVReturnSuccess else { throw PhotometryError.invalid }
        defer { CVPixelBufferUnlockBaseAddress(buffer, .readOnly) }
        guard let bytes = CVPixelBufferGetBaseAddress(buffer)?.assumingMemoryBound(to: UInt8.self) else {
            throw PhotometryError.invalid
        }
        let w = CVPixelBufferGetWidth(buffer), h = CVPixelBufferGetHeight(buffer), stride = CVPixelBufferGetBytesPerRow(buffer)
        func read(_ patch: SightlinePatch) throws -> Double {
            var meter = FogPatchMeter(transfer: transfer)
            for row in 0..<16 {
                for column in 0..<16 {
                    let p = patch.pixel(column: column, row: row, width: w, height: h)
                    let i = p.y * stride + p.x * 4
                    meter.sample(row: row, column: column, red: bytes[i+2], green: bytes[i+1], blue: bytes[i])
                }
            }
            return try meter.luminance()
        }
        return try FogReading(bright: read(.bright), dark: read(.dark))
    }

    func finish() { continuation.finish() }
}
