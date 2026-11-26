import Foundation

/// Which window runs which show folder, and which show owns the devices.
///
/// Each Mac window owns one show process (see `LocalShow`). A Clojure runtime
/// cannot hold two shows: user namespaces such as `show` are global, and the
/// compiled host's isolate owns one show. So a second show is a second process
/// on its own loopback port, and this registry keeps the processes apart:
///
/// - A folder runs in at most one window. Opening it again selects that
///   window, because two processes would autosave and restore one show.
/// - The first running show owns devices and physical output. A show opened
///   while another owns them starts preview-only (`TOLGLOW_DESKTOP_DEVICES=none`),
///   so it cannot open the first show's interfaces or arm output. Devices go
///   to the next show started after the owner stops.
///
/// Used on the main thread only. Foundation-only so `native/Tests/ShowWindows`
/// can exercise it without AppKit.
final class ShowRegistry {
    enum Claim: Equatable {
        /// Start with this folder's devices and output.
        case devices
        /// Start preview-only; `owner` names the show holding the devices.
        case previewOnly(owner: String)
        /// The folder already runs in window `window`.
        case duplicate(window: UUID)
    }

    struct Entry: Equatable {
        let key: String
        let name: String
        let devices: Bool
    }

    static let shared = ShowRegistry()

    private(set) var entries: [UUID: Entry] = [:]
    /// Claim order, so the device owner and its name are deterministic.
    private var order: [UUID] = []

    /// One key per folder on disk: device and inode, so a symlink, a different
    /// letter case or a trailing slash still names the same folder.
    static func key(for folder: URL) -> String {
        var info = stat()
        let path = folder.standardizedFileURL.path
        if stat(path, &info) == 0 { return "\(info.st_dev):\(info.st_ino)" }
        return folder.resolvingSymlinksInPath().standardizedFileURL.path
    }

    /// Reserve `folder` for `window` before its process starts. A window that
    /// already holds a claim must release it first.
    func claim(_ window: UUID, folder: URL) -> Claim {
        claim(window, key: Self.key(for: folder), name: folder.lastPathComponent)
    }

    func claim(_ window: UUID, key: String, name: String) -> Claim {
        if let existing = order.first(where: { entries[$0]?.key == key }) {
            return existing == window ? (entries[window]!.devices ? .devices : .previewOnly(owner: deviceOwner ?? name))
                                      : .duplicate(window: existing)
        }
        let owner = deviceOwner
        entries[window] = Entry(key: key, name: name, devices: owner == nil)
        order.append(window)
        return owner.map { .previewOnly(owner: $0) } ?? .devices
    }

    /// The window's show stopped, failed to start or its window closed.
    func release(_ window: UUID) {
        entries[window] = nil
        order.removeAll { $0 == window }
    }

    /// The name of the show that owns the devices, if any.
    var deviceOwner: String? {
        order.lazy.compactMap { self.entries[$0] }.first(where: \.devices)?.name
    }
}
