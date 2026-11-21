import Foundation

/// OSC 1.0: one fixed address, one UTF-8 JSON string, no bundle/timetag.
/// One datagram preserves the sample's atomic values/gesture identity.
public enum SensorOSC {
    public static let address = "/tolglow/sensor/v1"
    public static let maximumBytes = 8192

    private static func string(_ value: String) throws -> Data {
        guard !value.utf8.contains(0) else { throw SensorError.invalid("OSC NUL") }
        var result = Data(value.utf8)
        result.append(0)
        while result.count % 4 != 0 { result.append(0) }
        return result
    }

    public static func encode(_ observation: Observation) throws -> Data {
        let json = try observation.json()
        guard let text = String(data: json, encoding: .utf8) else { throw SensorError.invalid("UTF-8") }
        var result = try string(address)
        result.append(try string(",s"))
        result.append(try string(text))
        guard result.count <= maximumBytes else { throw SensorError.packetTooLarge }
        return result
    }

    public static func decode(_ packet: Data) throws -> Observation {
        guard packet.count <= maximumBytes, packet.count % 4 == 0 else { throw SensorError.invalid("OSC size") }
        let bytes = [UInt8](packet)
        var offset = 0
        func readString() throws -> String {
            guard offset < bytes.count, let zero = bytes[offset...].firstIndex(of: 0),
                  let result = String(bytes: bytes[offset..<zero], encoding: .utf8)
            else { throw SensorError.invalid("OSC string") }
            let end = (zero + 4) & ~3
            guard end <= bytes.count, bytes[zero..<end].allSatisfy({ $0 == 0 })
            else { throw SensorError.invalid("OSC padding") }
            offset = end
            return result
        }
        guard try readString() == address, try readString() == ",s" else { throw SensorError.invalid("OSC address/types") }
        let text = try readString()
        guard offset == bytes.count else { throw SensorError.invalid("OSC trailing data") }
        return try Observation.decode(Data(text.utf8))
    }
}
