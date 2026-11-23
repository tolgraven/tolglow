import Foundation

/// One Core Bluetooth write callback is one ATT transaction. The central can
/// split a large value across successive offsets and receives a single response.
public struct AudienceGATTWrite {
    public enum Field: Equatable { case hello, samples }
    public enum Failure: Error, Equatable { case unsupported, mixed, offset, length }
    public struct Fragment {
        public let central: UUID
        public let field: Field?
        public let offset: Int
        public let value: Data?
        public init(central: UUID, field: Field?, offset: Int, value: Data?) {
            self.central = central; self.field = field; self.offset = offset; self.value = value
        }
    }

    public let central: UUID
    public let field: Field
    public let value: Data

    public static func assemble<T>(_ requests: [T], inspect: (T) -> Fragment) throws -> Self {
        guard let first = requests.first, requests.count <= 65 else { throw Failure.length }
        let identity = inspect(first)
        guard let field = identity.field else { throw Failure.unsupported }
        let maximum = field == .hello ? 65 : 63
        guard identity.offset == 0 else { throw Failure.offset }
        guard var bytes = identity.value, !bytes.isEmpty, bytes.count <= maximum, requests.count <= maximum
        else { throw Failure.length }
        // Data retains its storage for the normal one-fragment write.
        for request in requests.dropFirst() {
            let fragment = inspect(request)
            guard fragment.central == identity.central, fragment.field == field else { throw Failure.mixed }
            guard fragment.offset == bytes.count else { throw Failure.offset }
            guard let value = fragment.value, !value.isEmpty, value.count <= maximum - bytes.count
            else { throw Failure.length }
            bytes.append(value)
        }
        guard field == .hello ? bytes.count == 65 : (bytes.count == 39 || bytes.count == 63)
        else { throw Failure.length }
        return Self(central: identity.central, field: field, value: bytes)
    }
}
