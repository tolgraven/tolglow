import AVFoundation
import Vision
import TolglowSensorCore

/// Only joint positions survive each camera callback. No video, photos or audio are saved.
final class PoseMeter: NSObject, AVCaptureVideoDataOutputSampleBufferDelegate {
    private let session = AVCaptureSession()
    private let queue = DispatchQueue(label: "org.tolglow.audience.pose")
    private let request = VNDetectHumanBodyPoseRequest()
    private var tracker = DancePoseTracker()
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
            self.tracker = DancePoseTracker(); self.previousTime = nil
        }
    }
    func captureOutput(_ output: AVCaptureOutput, didOutput buffer: CMSampleBuffer, from connection: AVCaptureConnection) {
        let time = CMSampleBufferGetPresentationTimeStamp(buffer).seconds
        guard active, time.isFinite, time - (previousTime ?? (time - 1)) >= 0.09 else { return }
        previousTime = time
        let handler = VNImageRequestHandler(cmSampleBuffer: buffer, orientation: .leftMirrored)
        let poses: [DancePose]
        do {
            try handler.perform([request])
            poses = (request.results ?? []).compactMap(Self.pose)
        } catch {
            poses = []
        }
        let strength = tracker.strength(poses, at: time)
        DispatchQueue.main.async { self.sample(strength, time) }
    }

    private static func pose(_ person: VNHumanBodyPoseObservation) -> DancePose? {
        func point(_ joint: VNHumanBodyPoseObservation.JointName) -> DancePose.Point? {
            guard let p = try? person.recognizedPoint(joint), p.confidence >= 0.45 else { return nil }
            return DancePose.Point(Double(p.location.x), Double(p.location.y))
        }
        guard let left = point(.leftHip), let right = point(.rightHip) else { return nil }
        let legs: [DancePose.Joint: DancePose.Point?] = [
            .leftAnkle: point(.leftAnkle), .rightAnkle: point(.rightAnkle),
            .leftKnee: point(.leftKnee), .rightKnee: point(.rightKnee)
        ]
        return DancePose(leftHip: left, rightHip: right, legs: legs.compactMapValues { $0 })
    }
}
