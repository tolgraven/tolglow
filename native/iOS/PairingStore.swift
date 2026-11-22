import Foundation
import Security
import TolglowSensorCore

struct SensorCredential {
    let device: String
    let destination: PairingDestination
    let token: String
}

/// Per-device host-issued tokens stay in this app's Keychain; never UserDefaults.
enum PairingStore {
    static func save(_ token: String, for device: String, destination: PairingDestination) throws {
        guard !device.isEmpty, token.count >= 16, token.utf8.count <= 512 else { throw NSError(domain: "TolglowPairing", code: 1) }
        try LocalPassword.save(token, service: destination.keychainService, account: device)
    }
    static func load(for device: String, destination: PairingDestination) -> SensorCredential? {
        guard let token = LocalPassword.read(service: destination.keychainService, account: device) else { return nil }
        return SensorCredential(device: device, destination: destination, token: token)
    }
}

enum LocalPassword {
    private static func query(_ service: String, _ account: String) -> [String: Any] {
        [kSecClass as String: kSecClassGenericPassword, kSecAttrService as String: service,
         kSecAttrAccount as String: account]
    }

    static func save(_ password: String, service: String, account: String) throws {
        let query = Self.query(service, account)
        let status = SecItemUpdate(query as CFDictionary, [kSecValueData as String: Data(password.utf8)] as CFDictionary)
        if status == errSecItemNotFound {
            var insert = query
            insert[kSecValueData as String] = Data(password.utf8)
            insert[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
            guard SecItemAdd(insert as CFDictionary, nil) == errSecSuccess else { throw CocoaError(.fileWriteUnknown) }
        } else if status != errSecSuccess { throw CocoaError(.fileWriteUnknown) }
    }

    static func read(service: String, account: String) -> String? {
        var query = Self.query(service, account)
        query[kSecReturnData as String] = true
        query[kSecMatchLimit as String] = kSecMatchLimitOne
        var item: CFTypeRef?
        guard SecItemCopyMatching(query as CFDictionary, &item) == errSecSuccess, let bytes = item as? Data else { return nil }
        return String(data: bytes, encoding: .utf8)
    }

    static func remove(service: String, account: String) throws {
        let status = SecItemDelete(query(service, account) as CFDictionary)
        guard status == errSecSuccess || status == errSecItemNotFound else { throw CocoaError(.fileWriteUnknown) }
    }
}
