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
        let query: [String: Any] = [kSecClass as String: kSecClassGenericPassword,
                                   kSecAttrService as String: destination.keychainService, kSecAttrAccount as String: device]
        let status = SecItemUpdate(query as CFDictionary, [kSecValueData as String: Data(token.utf8)] as CFDictionary)
        if status == errSecItemNotFound {
            var insert = query
            insert[kSecValueData as String] = Data(token.utf8)
            insert[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
            guard SecItemAdd(insert as CFDictionary, nil) == errSecSuccess else { throw CocoaError(.fileWriteUnknown) }
        } else if status != errSecSuccess { throw CocoaError(.fileWriteUnknown) }
    }
    static func load(for device: String, destination: PairingDestination) -> SensorCredential? {
        let query: [String: Any] = [kSecClass as String: kSecClassGenericPassword,
                                   kSecAttrService as String: destination.keychainService, kSecAttrAccount as String: device,
                                   kSecReturnData as String: true, kSecMatchLimit as String: kSecMatchLimitOne]
        var item: CFTypeRef?
        guard SecItemCopyMatching(query as CFDictionary, &item) == errSecSuccess,
              let data = item as? Data, let token = String(data: data, encoding: .utf8) else { return nil }
        return SensorCredential(device: device, destination: destination, token: token)
    }
}
