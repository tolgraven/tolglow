import Foundation

private final class LoopbackOnly: NSObject, URLSessionTaskDelegate {
    func urlSession(_ session: URLSession, task: URLSessionTask, willPerformHTTPRedirection response: HTTPURLResponse,
                    newRequest request: URLRequest, completionHandler: @escaping (URLRequest?) -> Void) {
        completionHandler(nil)
    }
}

/// One fixed loopback compute endpoint; guests cannot choose a URL, supply a
/// header, enter Studio, launch a cue, or forward their BLE credentials.
final class ComputeUpstream {
    private let session: URLSession
    private let socket: URLSessionWebSocketTask
    private let token: String
    private var open = false
    private var verifiedHost = false
    var request: ((Data) -> Void)?
    var ended: (() -> Void)?

    init(port: UInt16, token: String, operatorToken: String?) throws {
        guard port > 0, (24...1024).contains(token.utf8.count),
              let url = URL(string: "ws://127.0.0.1:\(port)/api/compute/stream") else { throw ComputeFailure.malformed }
        var req = URLRequest(url: url)
        req.setValue("http://127.0.0.1:\(port)", forHTTPHeaderField: "Origin")
        if let operatorToken { req.setValue("Bearer " + operatorToken, forHTTPHeaderField: "Authorization") }
        let conf = URLSessionConfiguration.ephemeral
        conf.httpShouldSetCookies = false
        conf.urlCache = nil; conf.urlCredentialStorage = nil
        session = URLSession(configuration: conf, delegate: LoopbackOnly(), delegateQueue: nil)
        socket = session.webSocketTask(with: req)
        socket.maximumMessageSize = ComputeLink.maximumFrame - 32
        self.token = token
    }

    func start(floats: Int, subject: String) throws {
        let hello = try ComputeLink.hello(token, floats: floats, subject: subject)
        open = true
        socket.resume()
        send(hello)
        next()
    }

    func reply(_ data: Data) throws {
        guard data.count <= 65536,
              let replies = try JSONSerialization.jsonObject(with: data) as? [String], (1...2).contains(replies.count)
        else { throw ComputeFailure.malformed }
        for text in replies {
            guard let op = ComputeLink.field(text, "op"), ["~:result","~:ready","~:error"].contains(op)
            else { throw ComputeFailure.malformed }
        }
        for text in replies { send(text) }
        next()
    }

    private func send(_ text: String) {
        socket.send(.string(text)) { [weak self] error in
            if error != nil { DispatchQueue.main.async { self?.fail() } }
        }
    }

    private func next() {
        socket.receive { [weak self] result in
            DispatchQueue.main.async {
                guard let self, self.open else { return }
                guard case .success(.string(let text)) = result, let op = ComputeLink.field(text, "op")
                else { self.fail(); return }
                switch op {
                case "~:welcome":
                    guard !self.verifiedHost, let mode = ComputeLink.field(text, "verification"),
                          ["~:full", "~:redundant"].contains(mode) else { self.fail(); return }
                    self.verifiedHost = true
                    self.next()
                case "~:prepare", "~:run":
                    guard self.verifiedHost else { self.fail(); return }
                    self.request?(Data(text.utf8))
                default: self.fail()
                }
            }
        }
    }

    private func fail() {
        guard open else { return }
        let notify = ended
        close()
        notify?()
    }

    func close() {
        open = false; request = nil; ended = nil
        socket.cancel(with: .goingAway, reason: nil); session.invalidateAndCancel()
    }
}
