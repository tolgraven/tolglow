// Inspect the visible CG window list of the specific launched app.
import AppKit
import CoreGraphics
import Foundation

let pid = Int32(CommandLine.arguments[1])!
NSRunningApplication(processIdentifier: pid)?.activate(options: [.activateAllWindows, .activateIgnoringOtherApps])
let windows = CGWindowListCopyWindowInfo([.optionOnScreenOnly, .excludeDesktopElements], kCGNullWindowID) as? [[String: Any]] ?? []
if let window = windows.first(where: { ($0[kCGWindowOwnerPID as String] as? Int) == Int(pid) && ($0[kCGWindowLayer as String] as? Int) == 0 }),
   let id = window[kCGWindowNumber as String] { print(id) }
