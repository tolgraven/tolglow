import Combine
import Foundation
import Network
import UIKit

/// The app creates this object for the full audience view. Starting requires a
/// separate button action and an authenticated, QR-pinned BLE grant.
@MainActor final class AudienceComputeGuest: ObservableObject {
    @Published private(set) var message = "GPU sharing off"
    @Published private(set) var active = false
    @Published private(set) var gpu: GuestGPU?
    private var link: NWConnection?
    private var deadline: Timer?
    private var generation = UUID()
    private var last = ProcessInfo.processInfo.systemUptime
    private var monitors: [NSObjectProtocol] = []

    func start(grant data: Data, pinnedRoom: String) {
        stop()
        guard let g = try? JSONDecoder().decode(ComputeGrant.self, from: data),
              g.valid, g.room == pinnedRoom, eligible else {
            message = "GPU unavailable for this stage or power state"; return
        }
        let epoch = generation
        let executor = GuestGPU()
        gpu = executor
        active = true; message = "Checking GPU and nearby link"
        for name in [UIApplication.didEnterBackgroundNotification, ProcessInfo.thermalStateDidChangeNotification,
                     Notification.Name.NSProcessInfoPowerStateDidChange] {
            monitors.append(NotificationCenter.default.addObserver(forName: name, object: nil, queue: .main) { [weak self] _ in
                Task { @MainActor in self?.checkConditions() }
            })
        }
        last = ProcessInfo.processInfo.systemUptime
        deadline = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) { [weak self] _ in
            Task { @MainActor in
                guard let self, self.generation == epoch else { return }
                if Date() >= g.expires || ProcessInfo.processInfo.systemUptime - self.last > 45 {
                    self.stop("Session expired or idle")
                }
            }
        }
        Task {
            do {
                guard try await executor.probe(), generation == epoch else {
                    if generation == epoch { stop("GPU readback unavailable") }; return
                }
                open(g, epoch, executor)
            } catch { if generation == epoch { stop("GPU readback unavailable") } }
        }
    }

    private var eligible: Bool {
        let process = ProcessInfo.processInfo
        return UIApplication.shared.applicationState == .active && !process.isLowPowerModeEnabled &&
            [.nominal, .fair].contains(process.thermalState)
    }

    private func checkConditions() { if !eligible { stop("Paused by app, temperature or low power") } }

    private func open(_ grant: ComputeGrant, _ epoch: UUID, _ gpu: GuestGPU) {
        let endpoint = NWEndpoint.service(name: grant.serviceName, type: ComputeGrant.service,
                                          domain: "local.", interface: nil)
        let c = NWConnection(to: endpoint, using: ComputeLink.parameters())
        let read = ComputeRecords(grant, direction: "to-phone"), write = ComputeRecords(grant, direction: "to-stage")
        link = c
        c.stateUpdateHandler = { [weak self] state in
            DispatchQueue.main.async {
                guard let self, self.generation == epoch else { return }
                switch state {
                case .ready:
                    guard let proof = try? write.seal(Data("GPU-ready".utf8)) else { self.stop(); return }
                    ComputeLink.send(Data(grant.id.uuidString.utf8) + proof, on: c) { ok in
                        guard self.generation == epoch else { return }
                        if ok { self.next(c, read, write, gpu, epoch) } else { self.stop() }
                    }
                case .failed, .cancelled: self.stop("Stage disconnected")
                default: break
                }
            }
        }
        c.start(queue: .main)
    }

    private func next(_ c: NWConnection, _ read: ComputeRecords, _ write: ComputeRecords, _ gpu: GuestGPU, _ epoch: UUID) {
        ComputeLink.read(c) { [weak self] data in
            Task { @MainActor in
                guard let self, self.generation == epoch else { return }
                do {
                    guard let data, self.eligible else { self.stop("Unavailable or disconnected"); return }
                    let clear = try read.open(data)
                    let reply: Data
                    switch clear.first {
                    case 0 where clear.count == ComputeLink.probeSize: reply = clear
                    case 1:
                        self.message = "Sharing GPU with this stage"
                        reply = try await gpu.run(Data(clear.dropFirst()))
                    default: throw ComputeFailure.malformed
                    }
                    guard self.generation == epoch, self.eligible else { return }
                    self.last = ProcessInfo.processInfo.systemUptime
                    ComputeLink.send(try write.seal(reply), on: c) { ok in
                        guard self.generation == epoch else { return }
                        if ok { self.next(c, read, write, gpu, epoch) } else { self.stop() }
                    }
                } catch { if self.generation == epoch { self.stop("Unusable compute request") } }
            }
        }
    }

    func stop(_ reason: String = "GPU sharing off") {
        generation = UUID()
        deadline?.invalidate(); deadline = nil
        monitors.forEach { NotificationCenter.default.removeObserver($0) }; monitors.removeAll()
        link?.stateUpdateHandler = nil; link?.cancel(); link = nil
        gpu?.stop(); gpu = nil
        active = false; message = reason
    }
}
