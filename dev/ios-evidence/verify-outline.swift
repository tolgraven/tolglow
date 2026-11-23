import AppKit
import Foundation
func yellowPixels(in file: String) throws -> Int {
    let png = try Data(contentsOf: URL(fileURLWithPath: "native-media/" + file))
    guard let image = NSBitmapImageRep(data: png) else { throw CocoaError(.fileReadCorruptFile) }
    var count = 0
    for y in stride(from: 0, to: image.pixelsHigh, by: 3) {
        for x in stride(from: 0, to: image.pixelsWide, by: 3) {
            if let c = image.colorAt(x: x, y: y)?.usingColorSpace(.sRGB),
               c.redComponent > 0.55, c.greenComponent > 0.5, c.blueComponent < 0.3 {
                count += 1
            }
        }
    }
    return count
}
let unselected = try yellowPixels(in: "plane-before.png")
let selected = try yellowPixels(in: "plane-review.png")
print("Yellow selection samples before and after:", unselected, selected)
guard selected - unselected > 100 else { exit(1) }
