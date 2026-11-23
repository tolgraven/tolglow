import CryptoKit
import Foundation
import Network

struct ComputeGrant: Codable {
    let version: UInt8
    let id: UUID
    let room: String
    let key: Data
    let expires: Date

    static let service = "_tolglow-cmp._tcp"
    var serviceName: String { "Tolglow-" + String(room.prefix(32)) }
    var valid: Bool {
        version == 1 && key.count == 32 && room.utf8.count == 64 &&
            room.utf8.allSatisfy { (48...57).contains($0) || (97...102).contains($0) } &&
            expires.timeIntervalSinceNow > 0 && expires.timeIntervalSinceNow <= 905
    }
}

enum ComputeFailure: Error { case malformed, unavailable, expired }

/// Independent directions prevent nonce reuse. Each frame is authenticated
/// against this grant and its ordered counter. A gap or replay ends the session.
final class ComputeRecords {
    private let key: SymmetricKey
    private let binding: Data
    private var sequence: UInt64 = 0

    init(_ grant: ComputeGrant, direction: String) {
        binding = Data(("Tolglow/guest-compute/v1/" + grant.id.uuidString + "/" + direction).utf8)
        key = HKDF<SHA256>.deriveKey(inputKeyMaterial: SymmetricKey(data: grant.key),
            salt: Data(grant.room.utf8), info: binding, outputByteCount: 32)
    }

    private func nonce() throws -> AES.GCM.Nonce {
        guard sequence < UInt64.max else { throw ComputeFailure.expired }
        var n = sequence.bigEndian
        return try AES.GCM.Nonce(data: Data(repeating: 0, count: 4) + withUnsafeBytes(of: &n) { Data($0) })
    }

    func seal(_ data: Data) throws -> Data {
        let box = try AES.GCM.seal(data, using: key, nonce: nonce(), authenticating: binding)
        sequence += 1
        return box.ciphertext + box.tag
    }

    func open(_ record: Data) throws -> Data {
        guard record.count >= 16 else { throw ComputeFailure.malformed }
        let box = try AES.GCM.SealedBox(nonce: nonce(), ciphertext: record.dropLast(16), tag: record.suffix(16))
        let bytes = try AES.GCM.open(box, using: key, authenticating: binding)
        sequence += 1
        return bytes
    }
}

enum ComputeLink {
    static let maximumFrame = 1024 * 1024
    static let probeSize = 16 * 1024

    static func parameters() -> NWParameters {
        let params = NWParameters.tcp
        params.includePeerToPeer = true
        params.prohibitedInterfaceTypes = [.cellular]
        return params
    }

    static func send(_ body: Data, on connection: NWConnection, then: @escaping (Bool) -> Void) {
        guard !body.isEmpty, body.count <= maximumFrame else { then(false); return }
        var size = UInt32(body.count).bigEndian
        let data = withUnsafeBytes(of: &size) { Data($0) } + body
        connection.send(content: data, completion: .contentProcessed { error in then(error == nil) })
    }

    static func read(_ connection: NWConnection, limit: Int = maximumFrame, then: @escaping (Data?) -> Void) {
        connection.receive(minimumIncompleteLength: 4, maximumLength: 4) { header, _, _, error in
            guard error == nil, let header, header.count == 4 else { then(nil); return }
            let size = header.reduce(0) { ($0 << 8) | Int($1) }
            guard (1...limit).contains(size) else { then(nil); return }
            connection.receive(minimumIncompleteLength: size, maximumLength: size) { bytes, _, _, error in
                then(error == nil && bytes?.count == size ? bytes : nil)
            }
        }
    }

    static func field(_ text: String, _ key: String) -> String? {
        guard let fields = try? JSONSerialization.jsonObject(with: Data(text.utf8)) as? [Any],
              fields.first as? String == "^ ", fields.count % 2 == 1 else { return nil }
        for i in stride(from: 1, to: fields.count - 1, by: 2) where fields[i] as? String == ("~:" + key) {
            return fields[i + 1] as? String
        }
        return nil
    }

    static func hello(_ token: String, floats: Int, subject: String) throws -> String {
        let fields: [Any] = ["^ ", "~:op", "~:hello", "~:token", token, "~:caps", ["^ ", "~:max-result-floats", floats, "~:guest?", true, "~:subject", subject] as [Any]]
        return String(decoding: try JSONSerialization.data(withJSONObject: fields), as: UTF8.self)
    }
}
