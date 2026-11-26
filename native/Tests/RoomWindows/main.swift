// Studio windows in the Mac app (#665, #743): which roles a window can have,
// what Studio hears when one closes, and where the Room goes. The window and
// web view lifetime itself needs the running app.
//   xcrun --sdk macosx swiftc -swift-version 5 native/macOS/RoomWindows.swift native/Tests/RoomWindows/main.swift -o /tmp/room-windows && /tmp/room-windows
import Foundation

enum Check: Error { case failed(String) }
func expect(_ ok: Bool, _ message: String) throws {
    if !ok { throw Check.failed(message) }
}

try MainActor.assumeIsolated {
    try expect(RoomWindows.role("room") == "room", "the Room's window")
    try expect(RoomWindows.role("panel") == "panel", "a panel's window")
    try expect(RoomWindows.role("studio") == nil, "an unknown role is ignored")
    try expect(RoomWindows.role(nil) == nil && RoomWindows.role(3) == nil, "a missing or non-text role is ignored")

    let room = RoomWindows.closedReply(role: "room", id: nil)
    try expect(room["status"] as? String == "closed" && room["role"] as? String == "room" && room["id"] == nil,
               "closing the Room reports the Room")
    let panel = RoomWindows.closedReply(role: "panel", id: "live")
    try expect(panel["role"] as? String == "panel" && panel["id"] as? String == "live",
               "closing a panel's window names the panel, so only it redocks")
    let unknown = RoomWindows.closedReply(role: nil, id: nil)
    try expect(unknown.count == 1 && unknown["status"] as? String == "closed",
               "an unidentified window's close names no role, so nothing redocks")

    try expect(RoomWindows.target(count: 1, studio: 0, room: 0) == nil, "one display: nowhere to go")
    try expect(RoomWindows.target(count: 2, studio: 0, room: 0) == 1, "the other display")
    try expect(RoomWindows.target(count: 3, studio: 0, room: 1) == 2, "repeated requests cycle the free displays")
    try expect(RoomWindows.target(count: 3, studio: 0, room: 2) == 1, "and wrap around")
}
print("room-windows: all checks passed")
