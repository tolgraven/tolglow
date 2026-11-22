import Foundation

/// Identity used for an operator credential. Paths on one HTTPS origin may
/// share a credential. Changing the scheme, host or effective port ends that scope.
public struct StudioOrigin: Equatable, Sendable {
    public let scheme: String
    public let host: String
    public let port: Int

    public init?(scheme: String?, host: String?, port: Int?) {
        guard let scheme = scheme?.lowercased(), let host = host?.lowercased(), !host.isEmpty else { return nil }
        let loopback = ["localhost", "127.0.0.1", "::1", "[::1]"].contains(host)
        guard scheme == "https" || (scheme == "http" && loopback) else { return nil }
        let effectivePort = port ?? (scheme == "https" ? 443 : 80)
        guard (1...65535).contains(effectivePort) else { return nil }
        self.scheme = scheme
        self.host = host.trimmingCharacters(in: CharacterSet(charactersIn: "[]"))
        self.port = effectivePort
    }

    public init?(url: URL) {
        guard url.user == nil, url.password == nil else { return nil }
        self.init(scheme: url.scheme, host: url.host, port: url.port)
    }

    public func acceptsOperatorChallenge(scheme: String?, host: String, port: Int,
                                         realm: String?, isProxy: Bool, previousFailures: Int) -> Bool {
        let destination = StudioOrigin(scheme: scheme, host: host, port: port == 0 ? nil : port)
        return self == destination && realm == "Tolglow Studio" && !isProxy && previousFailures == 0
    }

    public var keychainAccount: String {
        "\(scheme)/\(host)/\(port)"
    }
}
