import AVFoundation
import Vision

/// Only joint positions survive each camera callback. No video, photos or audio are saved.
final class PoseMeter: NSObject, AVCaptureVideoDataOutputSampleBufferDelegate {
    private let session = AVCaptureSession()
    private let queue = DispatchQueue(label: "org.tolglow.audience.pose")
    private let request = VNDetectHumanBodyPoseRequest()
    private var previous: [VNHumanBodyPoseObservation.JointName: CGPoint] = [:]
    private var previousTime: Double?
    private var active = false
    private let sample: (Double, Double) -> Void
    private let unavailable: () -> Void
    init(sample: @escaping (Double, Double) -> Void, unavailable: @escaping () -> Void) {
        self.sample = sample; self.unavailable = unavailable
    }
    func start() {
        queue.async {
            self.active = true
            AVCaptureDevice.requestAccess(for: .video) { allowed in
                self.queue.async {
                    guard self.active else { return }
                    guard allowed, self.configure() else { DispatchQueue.main.async(execute: self.unavailable); return }
                    self.session.startRunning()
                }
            }
        }
    }
    private func configure() -> Bool {
        if !session.inputs.isEmpty { return true }
        guard let camera = AVCaptureDevice.default(.builtInWideAngleCamera, for: .video, position: .front),
              let input = try? AVCaptureDeviceInput(device: camera) else { return false }
        let output = AVCaptureVideoDataOutput()
        output.alwaysDiscardsLateVideoFrames = true
        output.setSampleBufferDelegate(self, queue: queue)
        session.beginConfiguration()
        defer { session.commitConfiguration() }
        session.sessionPreset = .vga640x480
        guard session.canAddInput(input), session.canAddOutput(output) else { return false }
        session.addInput(input); session.addOutput(output)
        return true
    }
    func stop() {
        queue.async {
            self.active = false
            if self.session.isRunning { self.session.stopRunning() }
            self.previous = [:]; self.previousTime = nil
        }
    }
    func captureOutput(_ output: AVCaptureOutput, didOutput buffer: CMSampleBuffer, from connection: AVCaptureConnection) {
        let time = CMSampleBufferGetPresentationTimeStamp(buffer).seconds
        guard active, time.isFinite, time - (previousTime ?? (time - 1)) >= 0.09 else { return }
        let dt = time - (previousTime ?? time)
        previousTime = time
        let handler = VNImageRequestHandler(cmSampleBuffer: buffer, orientation: .leftMirrored)
        let joints: [VNHumanBodyPoseObservation.JointName] = [.leftAnkle, .rightAnkle, .leftKnee, .rightKnee, .leftHip, .rightHip]
        var points: [VNHumanBodyPoseObservation.JointName: CGPoint] = [:]
        if (try? handler.perform([request])) != nil, let person = request.results?.first {
            for joint in joints {
                if let point = try? person.recognizedPoint(joint), point.confidence >= 0.45 { points[joint] = point.location }
            }
        }
        let distances = points.compactMap { joint, p in previous[joint].map { hypot(p.x - $0.x, p.y - $0.y) } }
        previous = points
        let strength = dt > 0 && dt < 0.5 && distances.count >= 2 ? min(1, Double(distances.max() ?? 0) / dt * 1.8) : 0
        DispatchQueue.main.async { self.sample(strength, time) }
    }
}
