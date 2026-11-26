import Foundation

/// Text a show window's page hands the app to keep per show folder: its view
/// tabs (#669) and its Perform panel layouts (#668). The loopback port, and with
/// it the page's own storage, changes on every launch, so the app keeps these.
///
/// One UserDefaults dictionary per kind, keyed by folder path. A value over
/// `limit` UTF-8 bytes is refused and the stored copy stays. Foundation-only so
/// `native/Tests/ShowWindows` can exercise it without AppKit.
struct FolderStore {
    let defaults: UserDefaults
    let key: String
    let limit: Int

    static let tabs = FolderStore(defaults: .standard, key: "studio-view-tabs", limit: 4096)
    /// Several named layouts per window class; generous, still well under what
    /// UserDefaults handles comfortably.
    static let layouts = FolderStore(defaults: .standard, key: "studio-panel-layouts", limit: 65536)

    func value(for folder: String) -> String? {
        (defaults.dictionary(forKey: key) as? [String: String])?[folder]
    }

    /// Store `value` for `folder`; false when it is over the limit.
    @discardableResult
    func save(_ value: String, for folder: String) -> Bool {
        guard value.utf8.count <= limit else { return false }
        var saved = defaults.dictionary(forKey: key) as? [String: String] ?? [:]
        guard saved[folder] != value else { return true }
        saved[folder] = value
        defaults.set(saved, forKey: key)
        return true
    }
}
