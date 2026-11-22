import Foundation

/// The explicitly selected UDP host spelling and port form a credential audience.
/// DNS aliases and a different port each require their own enrollment.
public struct PairingDestination: Equatable, Sendable {
    public let host: String
    public let port: UInt16

    public init?(host: String, port: UInt16) {
        let value = host.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !value.isEmpty, value.utf8.count <= 255, port > 0,
              !value.unicodeScalars.contains(where: { CharacterSet.controlCharacters.contains($0) })
        else { return nil }
        self.host = value
        self.port = port
    }

    public var keychainService: String {
        "org.tolglow.sensor-pairing.endpoint." + Data(host.utf8).base64EncodedString() + "." + String(port)
    }
}
