// Show windows on one Mac (#669): one window per folder, one device owner.
//   xcrun --sdk macosx swiftc native/macOS/ShowRegistry.swift native/Tests/ShowWindows/main.swift -o /tmp/show-windows && /tmp/show-windows
import Foundation

enum Check: Error { case failed(String) }
func expect(_ ok: Bool, _ message: String) throws {
    if !ok { throw Check.failed(message) }
}

let files = FileManager.default
let root = files.temporaryDirectory.appendingPathComponent("show-windows-\(UUID().uuidString)")
let showA = root.appendingPathComponent("Club night")
let showB = root.appendingPathComponent("Rehearsal")
let link = root.appendingPathComponent("club-link")
try files.createDirectory(at: showA, withIntermediateDirectories: true)
try files.createDirectory(at: showB, withIntermediateDirectories: true)
try files.createSymbolicLink(at: link, withDestinationURL: showA)
defer { try? files.removeItem(at: root) }

let registry = ShowRegistry()
let one = UUID(), two = UUID(), three = UUID()

try expect(ShowRegistry.key(for: showA) == ShowRegistry.key(for: link), "a symlink names the same folder")
try expect(ShowRegistry.key(for: showA) == ShowRegistry.key(for: URL(fileURLWithPath: showA.path + "/")), "a trailing slash names the same folder")
try expect(ShowRegistry.key(for: showA) != ShowRegistry.key(for: showB), "two folders differ")

// The first show owns the devices; the second opens preview-only.
try expect(registry.claim(one, folder: showA) == .devices, "first show owns devices")
try expect(registry.claim(two, folder: showB) == .previewOnly(owner: "Club night"), "second show is preview-only")
try expect(registry.deviceOwner == "Club night", "owner named")

// The same folder in another window selects the running window.
try expect(registry.claim(three, folder: link) == .duplicate(window: one), "duplicate folder refused")
try expect(registry.entries[three] == nil, "a refused claim holds nothing")

// A failed launch releases its claim and leaves the others.
let failed = UUID()
try expect(registry.claim(failed, key: "missing", name: "Broken") == .previewOnly(owner: "Club night"), "claim before launch")
registry.release(failed)
try expect(registry.entries.count == 2 && registry.deviceOwner == "Club night", "failed launch released")

// Closing the owner frees the devices for the next show started, not the running preview-only one.
registry.release(one)
try expect(registry.deviceOwner == nil, "no owner after the owner closes")
try expect(registry.entries[two]?.devices == false, "a running preview-only show stays preview-only")
try expect(registry.claim(three, folder: showA) == .devices, "the next show started owns devices")
try expect(registry.claim(one, folder: showA) == .duplicate(window: three), "reopened folder is still one window")

// Closing windows in any order leaves nothing behind.
registry.release(two)
registry.release(three)
try expect(registry.entries.isEmpty && registry.deviceOwner == nil, "registry empty after every window closes")
print("show windows: ok")
