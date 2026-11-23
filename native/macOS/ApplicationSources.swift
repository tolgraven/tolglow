import Foundation

/// Install each bundled revision pair once, retaining the user's branches,
/// existing versions, and show code in their separately chosen folder.
struct ApplicationSources {
    let directory: URL
    let archive: URL
    var installed: Bool {
        ["tolglow", "afterglow"].allSatisfy { repo in
            FileManager.default.fileExists(atPath: directory.appendingPathComponent("\(repo)/.git/HEAD").path)
        }
    }

    init?(bundle: Bundle = .main, files: FileManager = .default) {
        guard let resources = bundle.resourceURL,
              let version = try? String(contentsOf: resources.appendingPathComponent("source-version.txt"),
                                        encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines),
              version.range(of: #"^[a-f0-9]{40}-[a-f0-9]{40}$"#, options: .regularExpression) != nil,
              let support = files.urls(for: .applicationSupportDirectory, in: .userDomainMask).first else { return nil }
        directory = support.appendingPathComponent("Tolglow/Sources").appendingPathComponent(version)
        archive = resources.appendingPathComponent("application-sources.zip")
    }

    func install() throws {
        let files = FileManager.default
        if installed { return }
        let parent = directory.deletingLastPathComponent()
        try files.createDirectory(at: parent, withIntermediateDirectories: true)
        let staging = parent.appendingPathComponent(UUID().uuidString)
        try files.createDirectory(at: staging, withIntermediateDirectories: false)
        defer { try? files.removeItem(at: staging) }
        let ditto = Process()
        ditto.executableURL = URL(fileURLWithPath: "/usr/bin/ditto")
        ditto.arguments = ["-x", "-k", archive.path, staging.path]
        try ditto.run()
        ditto.waitUntilExit()
        guard ditto.terminationStatus == 0 else { throw CocoaError(.fileReadCorruptFile) }
        do { try files.moveItem(at: staging, to: directory) }
        catch { if !installed { throw error } }
    }
}
