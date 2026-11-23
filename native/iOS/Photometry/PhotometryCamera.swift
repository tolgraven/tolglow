import AVFoundation
import CoreVideo
import Foundation
import TolglowSensorCore

struct BrightnessCaptureFailure: LocalizedError {
    let message: String
    init(_ message: String) { self.message = message }
    var errorDescription: String? { message }
}

/// One physical lens, one SDR format and one set of manual camera controls for
/// a whole run. Session configuration, frame delivery and cleanup share a queue.
final class PhotometryCamera: NSObject, AVCaptureVideoDataOutputSampleBufferDelegate {
    let session = AVCaptureSession()
    var onInterruption: (() -> Void)?
    private let queue = DispatchQueue(label: "org.tolglow.fixture-photometry")
    private var device: AVCaptureDevice?
    private var observers: [NSObjectProtocol] = []
    private var exposureSeconds = 0.0
    private var iso: Float = 0
    private var pending: CheckedContinuation<[Double], Error>?
    private var requestID = UUID()
    private var settleUntil = 0.0
    private var values: [Double] = []
    private var runTransfer: SDRTransfer?

    private var fogFeed: FogCameraFeed?

    func start(shutter: Int32, fogFeed: FogCameraFeed? = nil) async throws {
        guard await AVCaptureDevice.requestAccess(for: .video) else {
            throw BrightnessCaptureFailure("Allow camera access in iPhone Settings to measure light.")
        }
        try Task.checkCancellation()
        try await withCheckedThrowingContinuation { (resume: CheckedContinuation<Void, Error>) in
            queue.async {
                do {
                    self.fogFeed = fogFeed
                    try self.configure(shutter: shutter)
                    resume.resume()
                }
                catch { self.cleanup(); resume.resume(throwing: error) }
            }
        }
    }

    private func configure(shutter: Int32) throws {
        guard let camera = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: .back),
              camera.isExposureModeSupported(.custom), camera.isWhiteBalanceModeSupported(.locked),
              camera.isFocusModeSupported(.locked) else {
            throw BrightnessCaptureFailure("This camera cannot lock exposure, white balance and focus.")
        }
        let input = try AVCaptureDeviceInput(device: camera)
        let output = AVCaptureVideoDataOutput()
        output.videoSettings = [kCVPixelBufferPixelFormatTypeKey as String: kCVPixelFormatType_32BGRA]
        output.alwaysDiscardsLateVideoFrames = true
        output.setSampleBufferDelegate(self, queue: queue)
        session.beginConfiguration()
        session.automaticallyConfiguresCaptureDeviceForWideColor = false
        if session.canSetSessionPreset(.vga640x480) { session.sessionPreset = .vga640x480 }
        guard session.canAddInput(input) else { session.commitConfiguration(); throw BrightnessCaptureFailure("Camera input unavailable.") }
        session.addInput(input)
        guard session.canAddOutput(output) else { session.commitConfiguration(); throw BrightnessCaptureFailure("Camera output unavailable.") }
        session.addOutput(output)
        if let connection = output.connection(with: .video), connection.isVideoStabilizationSupported {
            connection.preferredVideoStabilizationMode = .off
        }
        if fogFeed != nil, let connection = output.connection(with: .video),
           connection.isVideoRotationAngleSupported(90) { connection.videoRotationAngle = 90 }
        session.commitConfiguration()
        device = camera
        for name in [AVCaptureSession.wasInterruptedNotification, AVCaptureSession.runtimeErrorNotification] {
            observers.append(NotificationCenter.default.addObserver(forName: name, object: session, queue: nil) { [weak self] _ in
                self?.onInterruption?()
            })
        }
        session.startRunning()
        try camera.lockForConfiguration()
        defer { camera.unlockForConfiguration() }
        let duration = CMTime(value: 1, timescale: shutter)
        guard camera.activeFormat.supportedColorSpaces.contains(.sRGB),
              CMTimeCompare(duration, camera.activeFormat.minExposureDuration) >= 0,
              CMTimeCompare(duration, camera.activeFormat.maxExposureDuration) <= 0 else {
            throw BrightnessCaptureFailure("This camera format does not support the selected shutter speed in SDR.")
        }
        camera.activeColorSpace = .sRGB
        if camera.activeFormat.isVideoHDRSupported {
            camera.automaticallyAdjustsVideoHDREnabled = false
            camera.isVideoHDREnabled = false
        }
        if camera.isLowLightBoostSupported { camera.automaticallyEnablesLowLightBoostWhenAvailable = false }
        camera.videoZoomFactor = 1
        // Both focus and a single fixed white balance are retained when changing fixtures.
        camera.focusMode = .locked
        var gains = camera.deviceWhiteBalanceGains(for: .init(temperature: 5500, tint: 0))
        func supported(_ gain: Float) -> Float { min(camera.maxWhiteBalanceGain, max(1, gain)) }
        gains.redGain = supported(gains.redGain)
        gains.greenGain = supported(gains.greenGain)
        gains.blueGain = supported(gains.blueGain)
        camera.setWhiteBalanceModeLocked(with: gains, completionHandler: nil)
        iso = camera.activeFormat.minISO
        exposureSeconds = CMTimeGetSeconds(duration)
        camera.setExposureModeCustom(duration: duration, iso: iso, completionHandler: nil)
    }

    func measure() async throws -> Double {
        try PhotometryFrame.stableMedian(await measureFrames())
    }

    func measureFrames() async throws -> [Double] {
        try await withCheckedThrowingContinuation { resume in
            queue.async {
                guard self.pending == nil, self.session.isRunning else {
                    resume.resume(throwing: BrightnessCaptureFailure("Camera is unavailable.")); return
                }
                self.requestID = UUID()
                let id = self.requestID
                self.pending = resume
                self.values.removeAll(keepingCapacity: true)
                self.settleUntil = ProcessInfo.processInfo.systemUptime + 0.45
                self.queue.asyncAfter(deadline: .now() + 4) {
                    if self.requestID == id { self.finish(.failure(BrightnessCaptureFailure("No stable locked camera frames arrived."))) }
                }
            }
        }
    }

    private func finish(_ result: Result<[Double], Error>) {
        let prior = pending
        pending = nil
        prior?.resume(with: result)
    }

    func captureOutput(_ output: AVCaptureOutput, didOutput sample: CMSampleBuffer, from connection: AVCaptureConnection) {
        let uptime = ProcessInfo.processInfo.systemUptime
        guard pending != nil || fogFeed != nil, uptime >= settleUntil,
              let device, device.exposureMode == .custom, device.focusMode == .locked,
              device.whiteBalanceMode == .locked, abs(device.iso - iso) <= max(0.2, iso * 0.01),
              abs(CMTimeGetSeconds(device.exposureDuration) - exposureSeconds) < exposureSeconds * 0.025,
              let buffer = CMSampleBufferGetImageBuffer(sample), let format = CMSampleBufferGetFormatDescription(sample) else { return }
        do {
            let transfer = try Self.transfer(buffer, format)
            if let prior = runTransfer, prior != transfer { throw BrightnessCaptureFailure("Camera transfer function changed. Restart calibration.") }
            runTransfer = transfer
            if let fogFeed {
                fogFeed.capture(buffer, transfer: transfer, uptime: uptime)
                return
            }
            values.append(try Self.frameLuminance(buffer, transfer: transfer))
            if values.count == 15 { finish(.success(values)) }
        } catch {
            if fogFeed != nil { onInterruption?() }
            else { finish(.failure(error)) }
        }
    }

    private static func transfer(_ buffer: CVPixelBuffer, _ format: CMFormatDescription) throws -> SDRTransfer {
        let attachments = CVBufferCopyAttachments(buffer, .shouldPropagate) as? [String: Any]
        let value = (attachments?[kCVImageBufferTransferFunctionKey as String] as? String)
            ?? (CMFormatDescriptionGetExtension(format, extensionKey: kCMFormatDescriptionExtension_TransferFunction) as? String)
        if value == (kCVImageBufferTransferFunction_sRGB as String) { return .sRGB }
        if value == (kCVImageBufferTransferFunction_ITU_R_709_2 as String) { return .rec709 }
        if value == (kCVImageBufferTransferFunction_Linear as String) { return .linear }
        throw BrightnessCaptureFailure("Camera frame has no supported SDR transfer function.")
    }

    private static func frameLuminance(_ buffer: CVPixelBuffer, transfer: SDRTransfer) throws -> Double {
        guard CVPixelBufferGetPixelFormatType(buffer) == kCVPixelFormatType_32BGRA,
              CVPixelBufferLockBaseAddress(buffer, .readOnly) == kCVReturnSuccess else { throw PhotometryError.invalid }
        defer { CVPixelBufferUnlockBaseAddress(buffer, .readOnly) }
        guard let bytes = CVPixelBufferGetBaseAddress(buffer)?.assumingMemoryBound(to: UInt8.self) else { throw PhotometryError.invalid }
        let w = CVPixelBufferGetWidth(buffer), h = CVPixelBufferGetHeight(buffer), stride = CVPixelBufferGetBytesPerRow(buffer)
        guard w >= 32, h >= 32 else { throw PhotometryError.invalid }
        // Uniform 16x16 grid in the central quarter of the delivered image.
        var frame = PhotometryFrame(transfer: transfer)
        for row in 0..<16 {
            let y = 3 * h / 8 + (2 * row + 1) * h / 128
            for column in 0..<16 {
                let x = 3 * w / 8 + (2 * column + 1) * w / 128
                let i = y * stride + x * 4
                frame.sample(red: bytes[i+2], green: bytes[i+1], blue: bytes[i])
            }
        }
        return try frame.luminance()
    }

    func stop() { queue.async { self.cleanup() } }
    private func cleanup() {
        finish(.failure(CancellationError()))
        fogFeed?.finish()
        fogFeed = nil
        for observer in observers { NotificationCenter.default.removeObserver(observer) }
        observers.removeAll()
        if session.isRunning { session.stopRunning() }
        session.beginConfiguration()
        for output in session.outputs { session.removeOutput(output) }
        for input in session.inputs { session.removeInput(input) }
        session.commitConfiguration()
        device = nil
        runTransfer = nil
    }
}
