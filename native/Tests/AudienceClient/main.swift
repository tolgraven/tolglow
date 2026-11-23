import Foundation

enum Check: Error { case failed(String) }
func expect(_ value: Bool, _ message: String) throws {
    if !value { throw Check.failed(message) }
}

// The real client owns incoming invitations. Simulate only the dropped BLE
// connection so this executable neither scans nor needs a physical peripheral.
let client = AudienceClient()
var ended = 0
client.onEnd = { ended += 1 }
_ = client.previewInterruptedStage()
try expect(client.reconnecting, "initial recovery")
client.receiveInvitation("simulator-fixture")
client.receiveInvitation("simulator-fixture")
try expect(ended == 0 && client.reconnecting, "same pinned invitation kept recovery")
try expect(client.room == "simulator-fixture" && !client.invalidInvitation, "browse became pinned")
client.receiveInvitation("second-floor")
try expect(ended == 1 && !client.joined && !client.reconnecting, "changed room ended the old session")
try expect(client.room == "second-floor", "new room selected")
client.receiveInvitation("second-floor")
try expect(ended == 1, "repeated invitation kept pending selection")
client.receiveInvitation(nil)
try expect(ended == 2 && client.invalidInvitation && client.room == nil, "invalid invocation withdrew selection")
client.receiveInvitation("third-floor")
try expect(!client.invalidInvitation && client.room == "third-floor" && !client.joined, "new scan required")
print("Radio owner preserved the matching invitation and withdrew changed or invalid sessions without scanning.")
