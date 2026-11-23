import CoreBluetooth
import CryptoKit
import Foundation

enum AudienceRadio {
    static let service = CBUUID(string: "7D609C70-47EC-4457-8568-D443C43B9701")
    static let identity = CBUUID(string: "7D609C70-47EC-4457-8568-D443C43B9702")
    static let hello = CBUUID(string: "7D609C70-47EC-4457-8568-D443C43B9703")
    static let samples = CBUUID(string: "7D609C70-47EC-4457-8568-D443C43B9704")

    static let computeGrant = CBUUID(string: "7D609C70-47EC-4457-8568-D443C43B9705")

    static func key(_ local: P256.KeyAgreement.PrivateKey, _ remote: Data, challenge: Data) throws -> SymmetricKey {
        guard challenge.count == 16 else { throw CocoaError(.coderInvalidValue) }
        let peer = try P256.KeyAgreement.PublicKey(x963Representation: remote)
        let shared = try local.sharedSecretFromKeyAgreement(with: peer)
        return shared.hkdfDerivedSymmetricKey(using: SHA256.self, salt: Data("Tolglow audience v1".utf8),
                                               sharedInfo: challenge, outputByteCount: 32)
    }

    static func fingerprint(_ publicKey: Data) -> String {
        SHA256.hash(data: publicKey).map { String(format: "%02x", $0) }.joined()
    }

    static func room(in url: URL) -> String? {
        guard url.scheme == "https", url.user == nil, url.password == nil,
              url.host != nil, url.pathComponents.count == 3, url.pathComponents[1] == "audience",
              let room = url.pathComponents.last, room.utf8.count == 64,
              room.utf8.allSatisfy({ (48...57).contains($0) || (97...102).contains($0) }) else { return nil }
        return room
    }
}
