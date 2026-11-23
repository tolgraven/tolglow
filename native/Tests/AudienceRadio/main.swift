import CryptoKit
import Foundation

enum Check: Error { case failed(String) }
func expect(_ ok: Bool, _ message: String) throws {
    if !ok { throw Check.failed(message) }
}

let stage = P256.KeyAgreement.PrivateKey()
let guest = P256.KeyAgreement.PrivateKey()
let stranger = P256.KeyAgreement.PrivateKey()
let publicStage = stage.publicKey.x963Representation
let challenge = Data(repeating: 1, count: 16)
let guestKey = try AudienceRadio.key(guest, publicStage, challenge: challenge)
let stageKey = try AudienceRadio.key(stage, guest.publicKey.x963Representation, challenge: challenge)
let oldKey = try AudienceRadio.key(stage, guest.publicKey.x963Representation, challenge: Data(repeating: 2, count: 16))
let otherKey = try AudienceRadio.key(stranger, publicStage, challenge: challenge)
let source = Data([1,0,0,0,1,0,0,0,2,85,92])
let encrypted = try AES.GCM.seal(source, using: guestKey)
let opened = try AES.GCM.open(encrypted, using: stageKey)
try expect(opened == source, "joined guest")
try expect((try? AES.GCM.open(encrypted, using: oldKey)) == nil, "old challenge")
try expect((try? AES.GCM.open(encrypted, using: otherKey)) == nil, "other participant")
var altered = encrypted.combined!
altered[altered.count-1] ^= 1
let invalid = try AES.GCM.SealedBox(combined: altered)
try expect((try? AES.GCM.open(invalid, using: stageKey)) == nil, "corrupted packet")
let room = AudienceRadio.fingerprint(publicStage)
let url = URL(string: "https://stage.example/audience/" + room)!
try expect(AudienceRadio.room(in: url) == room, "QR identity")
try expect(AudienceRadio.room(in: URL(string: "http://stage.example/audience/" + room)!) == nil, "insecure invitation")
print("Stage key agreement, independent guests, expired challenge, integrity and invite checks passed.")

let centralID = UUID()
func transferred(_ data: Data, as field: AudienceGATTWrite.Field) throws -> Data {
    let parts = stride(from: 0, to: data.count, by: 17).map { offset in
        AudienceGATTWrite.Fragment(central: centralID, field: field, offset: offset,
                                   value: data.subdata(in: offset..<min(offset + 17, data.count)))
    }
    return try AudienceGATTWrite.assemble(parts, inspect: { $0 }).value
}
let assembledKey = try transferred(guest.publicKey.x963Representation, as: .hello)
let longWriteKey = try AudienceRadio.key(stage, assembledKey, challenge: challenge)
for length in [11, 35] {
    let cleartext = Data(repeating: UInt8(length), count: length)
    let payload = try AES.GCM.seal(cleartext, using: guestKey).combined!
    let assembled = try transferred(payload, as: .samples)
    let verified = try AES.GCM.open(AES.GCM.SealedBox(combined: assembled), using: longWriteKey)
    try expect(verified == cleartext, "segmented authenticated payload")
    print("Authenticated \(payload.count)-byte sample reconstructed across 17-byte value fragments.")
}
