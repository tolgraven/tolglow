import CryptoKit
import Foundation
import Network

@main struct Loopback {
    static func main() throws {
        let grant = ComputeGrant(version: 1, id: UUID(), room: String(repeating: "a", count: 64),
            key: SymmetricKey(size: .bits256).withUnsafeBytes { Data($0) }, expires: Date().addingTimeInterval(120))
        let sample = Data([0, 255, 128])
        let write = ComputeRecords(grant, direction: "to-phone")
        let read = ComputeRecords(grant, direction: "to-phone")
        let packed = try write.seal(sample)
        let first = try read.open(packed)
        precondition(first == sample && (try? read.open(packed)) == nil)
        precondition((try? ComputeRecords(grant, direction: "to-stage").open(packed)) == nil)
        final class Probe {
            let listener: NWListener
            var connections: [NWConnection] = []
            let group = DispatchSemaphore(value: 0)
            let queue = DispatchQueue(label: "loopback")
            let sender: ComputeRecords, receiver: ComputeRecords
            let back: ComputeRecords, returned: ComputeRecords
            var times: [Double] = []
            let packet = Data(repeating: 63, count: ComputeLink.probeSize)
            init(_ g: ComputeGrant) throws {
                listener = try NWListener(using: ComputeLink.parameters())
                sender = ComputeRecords(g, direction: "to-phone"); receiver = ComputeRecords(g, direction: "to-phone")
                back = ComputeRecords(g, direction: "to-stage"); returned = ComputeRecords(g, direction: "to-stage")
            }
            func start() {
                listener.newConnectionHandler = { c in
                    self.connections.append(c); c.start(queue: self.queue); self.echo(c)
                }
                listener.stateUpdateHandler = { state in
                    if case .ready = state, let port = self.listener.port {
                        let c = NWConnection(host: "127.0.0.1", port: port, using: ComputeLink.parameters())
                        self.connections.append(c)
                        c.stateUpdateHandler = { status in if case .ready = status { self.run(c) } }
                        c.start(queue: self.queue)
                    }
                }
                listener.start(queue: queue)
            }
            func echo(_ c: NWConnection) {
                ComputeLink.read(c) { frame in
                    guard let frame else { return }
                    let value = try! self.receiver.open(frame)
                    ComputeLink.send(try! self.back.seal(value), on: c) { ok in if ok { self.echo(c) } }
                }
            }
            func run(_ c: NWConnection) {
                let start = ProcessInfo.processInfo.systemUptime
                ComputeLink.send(try! sender.seal(packet), on: c) { sent in
                    precondition(sent)
                    ComputeLink.read(c) { data in
                        let result = try! self.returned.open(data!)
                        precondition(result == self.packet)
                        self.times.append(ProcessInfo.processInfo.systemUptime - start)
                        if self.times.count < 12 { self.run(c); return }
                        let middle = self.times.sorted()[6]
                        print(String(format: "Encrypted macOS loopback · %d round trips × %d bytes · %.3f median ms · %.0f return bytes/s",
                                     self.times.count, self.packet.count, middle * 1000, Double(self.packet.count) / middle))
                        self.listener.cancel(); self.connections.forEach { $0.cancel() }; self.group.signal()
                    }
                }
            }
        }
        let probe = try Probe(grant); probe.start()
        precondition(probe.group.wait(timeout: .now() + 15) == .success)
    }
}
