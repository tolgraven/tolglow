// Studio windows in the Mac app (#665, #743): which roles a window can have,
// what Studio hears when one closes, where the Room goes, and the page's answer
// to a layouts save. The window and web view lifetime itself needs the running app.
//   xcrun --sdk macosx swiftc -swift-version 5 native/macOS/RoomWindows.swift native/macOS/StudioPage.swift native/macOS/FolderStore.swift native/Tests/RoomWindows/main.swift -o /tmp/room-windows && /tmp/room-windows
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

    func reply(_ json: String?) throws -> [String: Any] {
        guard let data = json?.data(using: .utf8),
              let object = try JSONSerialization.jsonObject(with: data) as? [String: Any] else { throw Check.failed("reply is JSON") }
        return object
    }
    let kept = try reply(StudioPage.layoutsReply(request: 7, kept: true))
    try expect(kept["type"] as? String == "layouts" && kept["request"] as? Int == 7 && kept["kept"] as? Bool == true,
               "a kept layouts copy is answered with its request")
    let refused = try reply(StudioPage.layoutsReply(request: 8, kept: false))
    try expect(refused["kept"] as? Bool == false && refused["request"] as? Int == 8, "a refused copy says so")
    try expect(!FolderStore(defaults: UserDefaults(suiteName: "room-windows-check")!, key: "layouts", limit: FolderStore.layouts.limit)
                .save(String(repeating: "x", count: FolderStore.layouts.limit + 1), for: "/show"),
               "a copy over the layouts limit is refused, so the page warns")
}
print("room-windows: all checks passed")
