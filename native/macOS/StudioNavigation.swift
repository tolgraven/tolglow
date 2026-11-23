import AppKit
import WebKit

@MainActor
final class StudioNavigation: NSObject, WKNavigationDelegate, WKDownloadDelegate {
    typealias ChooseDestination = (String, NSWindow?, @escaping (URL?) -> Void) -> Void
    private struct Save {
        let directory: URL
        let temporary: URL
        let target: URL
        let replacing: Bool
    }
    private let origin: URL
    private let choose: ChooseDestination
    private var saves: [ObjectIdentifier: Save] = [:]

    init(origin: URL, choose: ChooseDestination? = nil) {
        self.origin = origin
        self.choose = choose ?? { filename, window, done in
            let panel = NSSavePanel()
            panel.nameFieldStringValue = filename
            if let window {
                panel.beginSheetModal(for: window) { result in done(result == .OK ? panel.url : nil) }
            } else { done(panel.runModal() == .OK ? panel.url : nil) }
        }
    }

    private func permits(_ url: URL?) -> Bool {
        url?.scheme == origin.scheme && url?.host == origin.host && url?.port == origin.port
    }

    func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        guard let url = action.request.url else { decisionHandler(.cancel); return }
        if url.scheme == "blob" {
            let creator = URL(string: String(url.absoluteString.dropFirst("blob:".count)))
            decisionHandler(permits(creator) && permits(action.sourceFrame.request.url) ? .download : .cancel)
        } else if permits(url) {
            decisionHandler(action.shouldPerformDownload ? .download : .allow)
        } else { decisionHandler(.cancel) }
    }

    func webView(_ webView: WKWebView, navigationAction: WKNavigationAction, didBecome download: WKDownload) {
        download.delegate = self
    }

    func download(_ download: WKDownload, decideDestinationUsing response: URLResponse, suggestedFilename: String,
                  completionHandler: @escaping (URL?) -> Void) {
        let name = URL(fileURLWithPath: suggestedFilename).lastPathComponent
        choose(name, download.webView?.window) { target in
            guard let target else { completionHandler(nil); return }
            let files = FileManager.default
            do {
                let directory = try files.url(for: .itemReplacementDirectory, in: .userDomainMask,
                                              appropriateFor: target, create: true)
                let temporary = directory.appendingPathComponent("download")
                self.saves[ObjectIdentifier(download)] = Save(directory: directory, temporary: temporary,
                    target: target, replacing: files.fileExists(atPath: target.path))
                completionHandler(temporary)
            } catch {
                NSApp.presentError(error)
                completionHandler(nil)
            }
        }
    }

    func downloadDidFinish(_ download: WKDownload) {
        guard let save = saves.removeValue(forKey: ObjectIdentifier(download)) else { return }
        let files = FileManager.default
        defer { try? files.removeItem(at: save.directory) }
        do {
            if save.replacing { _ = try files.replaceItemAt(save.target, withItemAt: save.temporary) }
            else { try files.moveItem(at: save.temporary, to: save.target) }
        } catch { NSApp.presentError(error) }
    }

    func download(_ download: WKDownload, didFailWithError error: Error, resumeData: Data?) {
        if let save = saves.removeValue(forKey: ObjectIdentifier(download)) {
            try? FileManager.default.removeItem(at: save.directory)
        }
        if (error as NSError).code != NSURLErrorCancelled { NSApp.presentError(error) }
    }
}
