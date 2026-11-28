// Show windows on one Mac (#669): one window per folder, one device owner,
// and the tabs and panel layouts each folder's page hands the app.
//   xcrun --sdk macosx swiftc native/macOS/ShowRegistry.swift native/macOS/FolderStore.swift native/Tests/ShowWindows/main.swift -o /tmp/show-windows && /tmp/show-windows
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
// Per-folder page state: tabs and panel layouts are kept apart, per folder,
// and an oversized value leaves the stored copy alone.
let suite = "show-windows-\(UUID().uuidString)"
let defaults = UserDefaults(suiteName: suite)!
defer { defaults.removePersistentDomain(forName: suite) }
let tabs = FolderStore(defaults: defaults, key: "tabs", limit: FolderStore.tabs.limit)
let layouts = FolderStore(defaults: defaults, key: "layouts", limit: FolderStore.layouts.limit)
try expect(tabs.value(for: showA.path) == nil, "nothing stored yet")
try expect(tabs.save("{\"version\":1}", for: showA.path), "tabs saved")
let bigLayouts = String(repeating: "x", count: 40_000)
try expect(layouts.save(bigLayouts, for: showA.path), "layouts larger than the tabs limit are kept")
try expect(layouts.save("{}", for: showB.path), "another folder's layouts")
try expect(tabs.value(for: showA.path) == "{\"version\":1}" && layouts.value(for: showA.path) == bigLayouts, "kinds kept apart")
try expect(layouts.value(for: showB.path) == "{}", "folders kept apart")
try expect(!tabs.save(String(repeating: "x", count: 4097), for: showA.path), "oversized tabs refused")
try expect(!layouts.save(String(repeating: "x", count: 65537), for: showA.path), "oversized layouts refused")
try expect(tabs.value(for: showA.path) == "{\"version\":1}" && layouts.value(for: showA.path) == bigLayouts, "refusal keeps the stored copy")
// Room view: one value for the machine, whichever show folder saved it.
let roomView = MachineStore(defaults: defaults, key: "room-view", limit: MachineStore.roomView.limit)
try expect(roomView.value == nil, "no Room view yet")
try expect(roomView.save("{\"quality\":\"desktop\",\"light-budget\":\"12\"}"), "Room view saved")
try expect(!roomView.save(String(repeating: "x", count: 1025)), "oversized Room view refused")
try expect(roomView.value == "{\"quality\":\"desktop\",\"light-budget\":\"12\"}", "refusal keeps the Room view")
try expect(tabs.value(for: showA.path) == "{\"version\":1}", "the Room view leaves folder tabs alone")
print("show windows: ok")
