// TolglowVideoBridge: receive macOS Syphon video for the JVM show (#672).
//
// The JVM owns this helper like the audience bridge: it starts one process per
// selected source and closes stdin to stop it. The helper never sends lighting
// output. Frames cross into the JVM through a memory-mapped "TGVF" file whose
// layout `tolglow.video.source/read-feed` reads:
//
//   0   "TGVF"          4 bytes
//   4   version         u32 (1)
//   8   sequence        u64, even when stable, odd while a frame is written
//   16  width           u32
//   20  height          u32
//   24  stride          u32 bytes per row
//   28  format          u32 (1 = BGRA8, sRGB-encoded as Syphon publishes)
//   32  frames          u64 source frames received
//   40  written-at      u64 Unix microseconds of the last completed frame
//   48  flags           u32 bit 0 = connected to the source
//   52  capacity        u32 pixel bytes available after the header
//   64  label           256 bytes UTF-8, NUL padded ("App · Server")
//   320 pixels          capacity bytes
//
// Commands
//   list [--watch]                     JSON lines describing Syphon servers
//   feed --app A --name N --out PATH   copy one source into PATH until stdin closes
//        [--max-width W --max-height H]
//   serve [--name N --width W --height H --fps F]
//                                      publish the labelled grid until stdin closes
//   self-test --out-dir DIR            publish two in-process servers, receive
//                                      them through the same feed path and verify
//                                      colours, resize, isolation and retirement

import AppKit
import Foundation
import Metal
import MetalPerformanceShaders
import Syphon

let headerBytes = 320
let labelOffset = 64
let labelBytes = 256

func emit(_ object: [String: Any]) {
    guard let data = try? JSONSerialization.data(withJSONObject: object, options: [.sortedKeys]),
          let line = String(data: data, encoding: .utf8) else { return }
    FileHandle.standardOutput.write(Data((line + "\n").utf8))
}

func fail(_ message: String) -> Never {
    emit(["event": "error", "message": message])
    exit(1)
}

func describe(_ server: [String: Any]) -> [String: Any] {
    [
        "uuid": server[SyphonServerDescriptionUUIDKey] as? String ?? "",
        "name": server[SyphonServerDescriptionNameKey] as? String ?? "",
        "app": server[SyphonServerDescriptionAppNameKey] as? String ?? "",
    ]
}

func servers() -> [[String: Any]] {
    SyphonServerDirectory.shared().servers.map { $0 as [String: Any] }
}

/// The writer side of one TGVF file. Readers retry until they see the same even
/// sequence before and after copying the pixels.
final class FeedFile {
    let capacity: Int
    private let base: UnsafeMutableRawPointer
    private let size: Int
    private var sequence: UInt64 = 0
    private var frames: UInt64 = 0
    private let lock = NSLock()

    init(path: String, capacity: Int, label: String) throws {
        self.capacity = capacity
        size = headerBytes + capacity
        // Never truncate: a reader may still have the file mapped, and reading
        // past a shrunken end faults. The file only grows.
        let fd = open(path, O_RDWR | O_CREAT, 0o600)
        guard fd >= 0 else { throw NSError(domain: "feed", code: Int(errno)) }
        defer { close(fd) }
        var info = stat()
        guard fstat(fd, &info) == 0 else { throw NSError(domain: "feed", code: Int(errno)) }
        if Int(info.st_size) < size, ftruncate(fd, off_t(size)) != 0 { throw NSError(domain: "feed", code: Int(errno)) }
        guard let mapped = mmap(nil, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0),
              mapped != MAP_FAILED else { throw NSError(domain: "feed", code: Int(errno)) }
        base = mapped
        // Mark the header unstable while it is rewritten.
        let previous = base.load(fromByteOffset: 8, as: UInt64.self)
        sequence = previous | 1
        put(sequence, at: 8)
        OSMemoryBarrier()
        memset(base + 16, 0, headerBytes - 16)
        "TGVF".utf8.enumerated().forEach { base.storeBytes(of: $1, toByteOffset: $0, as: UInt8.self) }
        put(UInt32(1), at: 4)
        put(UInt32(1), at: 28)
        put(UInt32(capacity), at: 52)
        let text = Array(label.utf8.prefix(labelBytes - 1))
        text.enumerated().forEach { base.storeBytes(of: $1, toByteOffset: labelOffset + $0, as: UInt8.self) }
        OSMemoryBarrier()
        sequence += 1
        put(sequence, at: 8)
        OSMemoryBarrier()
    }

    deinit { munmap(base, size) }

    private func put<T>(_ value: T, at offset: Int) {
        base.storeBytes(of: value, toByteOffset: offset, as: T.self)
    }

    var pixels: UnsafeMutableRawPointer { base + headerBytes }

    /// Write one frame. `fill` copies BGRA rows into the pixel area.
    func write(width: Int, height: Int, fill: (UnsafeMutableRawPointer, Int) -> Void) {
        let stride = width * 4
        guard stride * height <= capacity else { return }
        lock.lock(); defer { lock.unlock() }
        sequence += 1 // odd: a reader that sees this discards its copy
        put(sequence, at: 8)
        OSMemoryBarrier()
        fill(pixels, stride)
        frames += 1
        put(UInt32(width), at: 16)
        put(UInt32(height), at: 20)
        put(UInt32(stride), at: 24)
        put(frames, at: 32)
        put(UInt64(Date().timeIntervalSince1970 * 1_000_000), at: 40)
        put(UInt32(1), at: 48)
        OSMemoryBarrier()
        sequence += 1
        put(sequence, at: 8)
        OSMemoryBarrier()
    }

    func setConnected(_ connected: Bool) {
        lock.lock(); defer { lock.unlock() }
        sequence += 1
        put(sequence, at: 8)
        OSMemoryBarrier()
        put(UInt32(connected ? 1 : 0), at: 48)
        OSMemoryBarrier()
        sequence += 1
        put(sequence, at: 8)
        OSMemoryBarrier()
    }
}

/// Scale a received texture into a shared texture no larger than the limits and
/// copy it into the feed. Aspect ratio is preserved.
final class Downloader {
    let device: MTLDevice
    private let queue: MTLCommandQueue
    private var target: MTLTexture?
    /// One scaler for every frame; MPS kernels are reusable across encodes.
    private lazy var scaler = MPSImageBilinearScale(device: device)

    init(device: MTLDevice) {
        self.device = device
        queue = device.makeCommandQueue()!
    }

    func fit(_ width: Int, _ height: Int, maxWidth: Int, maxHeight: Int) -> (Int, Int) {
        let scale = min(1.0, Double(maxWidth) / Double(width), Double(maxHeight) / Double(height))
        return (max(1, Int((Double(width) * scale).rounded())), max(1, Int((Double(height) * scale).rounded())))
    }

    func copy(_ texture: MTLTexture, into feed: FeedFile, maxWidth: Int, maxHeight: Int) -> (Int, Int) {
        let (w, h) = fit(texture.width, texture.height, maxWidth: maxWidth, maxHeight: maxHeight)
        if target?.width != w || target?.height != h {
            let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm, width: w, height: h, mipmapped: false)
            descriptor.usage = [.shaderWrite, .shaderRead]
            descriptor.storageMode = .shared
            target = device.makeTexture(descriptor: descriptor)
        }
        guard let target, let buffer = queue.makeCommandBuffer() else { return (0, 0) }
        if w == texture.width && h == texture.height, let blit = buffer.makeBlitCommandEncoder() {
            blit.copy(from: texture, sourceSlice: 0, sourceLevel: 0, sourceOrigin: MTLOrigin(x: 0, y: 0, z: 0),
                      sourceSize: MTLSize(width: w, height: h, depth: 1), to: target, destinationSlice: 0,
                      destinationLevel: 0, destinationOrigin: MTLOrigin(x: 0, y: 0, z: 0))
            blit.endEncoding()
        } else {
            scaler.encode(commandBuffer: buffer, sourceTexture: texture, destinationTexture: target)
        }
        buffer.commit()
        buffer.waitUntilCompleted()
        feed.write(width: w, height: h) { pixels, stride in
            target.getBytes(pixels, bytesPerRow: stride, from: MTLRegionMake2D(0, 0, w, h), mipmapLevel: 0)
        }
        return (w, h)
    }
}

/// Follow one source by application and server name, so a sender that quits and
/// returns (with a new UUID) is picked up again without reselecting it.
final class Feed {
    let app: String
    let name: String
    let file: FeedFile
    let downloader: Downloader
    let maxWidth: Int
    let maxHeight: Int
    private var client: SyphonMetalClient?
    private var uuid: String?
    private let lock = NSLock()
    /// One writer at a time for the feed file and its download texture.
    private let writeLock = NSLock()
    private(set) var received = 0
    private(set) var lastSize = (0, 0)
    var onFrame: ((Int, Int, Int) -> Void)?

    init(app: String, name: String, out: String, device: MTLDevice, maxWidth: Int, maxHeight: Int) throws {
        self.app = app; self.name = name
        self.maxWidth = maxWidth; self.maxHeight = maxHeight
        downloader = Downloader(device: device)
        file = try FeedFile(path: out, capacity: maxWidth * maxHeight * 4, label: "\(app) · \(name)")
    }

    func matches(_ server: [String: Any]) -> Bool {
        let d = describe(server)
        return d["app"] as? String == app && d["name"] as? String == name
    }

    /// Reconcile with the directory. Returns true while connected. Clients
    /// are stopped outside the lock: `stop` waits for an in-flight frame
    /// handler, which itself takes the lock to count the frame.
    @discardableResult
    func refresh() -> Bool {
        let candidate = servers().first(where: matches)
        let candidateID = candidate.map { describe($0)["uuid"] as? String ?? "" }
        lock.lock()
        if let client, client.isValid, candidateID == uuid { lock.unlock(); return true }
        let old = client
        client = nil
        uuid = nil
        lock.unlock()
        old?.stop()
        guard let candidate else {
            file.setConnected(false)
            return false
        }
        let next = SyphonMetalClient(serverDescription: candidate, device: downloader.device, options: nil) { [weak self] client in
            self?.receive(client)
        }
        // A server that has stopped can stay listed until its retirement
        // notification arrives; its client never becomes valid.
        guard next.isValid else {
            next.stop()
            file.setConnected(false)
            return false
        }
        lock.lock()
        client = next
        uuid = candidateID
        lock.unlock()
        file.setConnected(true)
        emit(["event": "connected", "source": describe(candidate)])
        return true
    }

    private func receive(_ client: SyphonMetalClient) {
        guard let texture = client.newFrameImage() else { return }
        writeLock.lock()
        let size = downloader.copy(texture, into: file, maxWidth: maxWidth, maxHeight: maxHeight)
        writeLock.unlock()
        lock.lock()
        received += 1
        lastSize = size
        let count = received
        lock.unlock()
        onFrame?(count, size.0, size.1)
    }

    func stop() {
        lock.lock()
        let old = client
        client = nil
        uuid = nil
        lock.unlock()
        old?.stop()
        file.setConnected(false)
    }
}

func argument(_ name: String, _ args: [String]) -> String? {
    guard let i = args.firstIndex(of: name), i + 1 < args.count else { return nil }
    return args[i + 1]
}

func watchDirectory(_ changed: @escaping () -> Void) -> [NSObjectProtocol] {
    [NSNotification.Name.SyphonServerAnnounce, .SyphonServerUpdate, .SyphonServerRetire].map {
        NotificationCenter.default.addObserver(forName: $0, object: nil, queue: .main) { _ in changed() }
    }
}

/// Stop when the owner closes our stdin, as the show does on shutdown.
func exitWhenStdinCloses(_ cleanup: @escaping () -> Void) {
    let handle = FileHandle.standardInput
    handle.readabilityHandler = { h in
        if h.availableData.isEmpty {
            h.readabilityHandler = nil
            DispatchQueue.main.async { cleanup(); exit(0) }
        }
    }
}

func runList(_ args: [String]) {
    let report = { emit(["event": "servers", "servers": servers().map(describe)]) }
    _ = SyphonServerDirectory.shared()
    // The directory fills asynchronously from distributed notifications.
    RunLoop.main.run(until: Date().addingTimeInterval(0.5))
    report()
    guard args.contains("--watch") else { return }
    let observers = watchDirectory(report)
    exitWhenStdinCloses { observers.forEach(NotificationCenter.default.removeObserver) }
    RunLoop.main.run()
}

func runFeed(_ args: [String]) {
    guard let app = argument("--app", args), let name = argument("--name", args), let out = argument("--out", args) else {
        fail("feed needs --app, --name and --out")
    }
    guard let device = MTLCreateSystemDefaultDevice() else { fail("No Metal device") }
    let maxWidth = Int(argument("--max-width", args) ?? "") ?? 1024
    let maxHeight = Int(argument("--max-height", args) ?? "") ?? 1024
    let feed: Feed
    do { feed = try Feed(app: app, name: name, out: out, device: device, maxWidth: maxWidth, maxHeight: maxHeight) }
    catch { fail("Cannot open feed file: \(error)") }
    var lastReport = Date.distantPast
    feed.onFrame = { count, w, h in
        let now = Date()
        if now.timeIntervalSince(lastReport) >= 1 {
            lastReport = now
            emit(["event": "frames", "received": count, "width": w, "height": h])
        }
    }
    _ = SyphonServerDirectory.shared()
    RunLoop.main.run(until: Date().addingTimeInterval(0.3))
    if !feed.refresh() { emit(["event": "waiting", "app": app, "name": name]) }
    let observers = watchDirectory {
        let was = feed.received
        if !feed.refresh() { emit(["event": "retired", "app": app, "name": name, "received": was]) }
    }
    exitWhenStdinCloses {
        observers.forEach(NotificationCenter.default.removeObserver)
        feed.stop()
    }
    RunLoop.main.run()
}

// MARK: self-test

/// The labelled grid shared with `tolglow.video.source/grid-color`: 8 columns
/// and 4 rows of distinct sRGB colours.
func gridColor(column: Int, row: Int) -> (UInt8, UInt8, UInt8) {
    let r = UInt8((255.0 * Double(column) / 7.0).rounded())
    let g = UInt8((255.0 * Double(row) / 3.0).rounded())
    return (r, g, 255 - r)
}

func gridTexture(device: MTLDevice, width: Int, height: Int, solid: (UInt8, UInt8, UInt8)? = nil) -> MTLTexture {
    let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm, width: width, height: height, mipmapped: false)
    descriptor.usage = [.shaderRead]
    let texture = device.makeTexture(descriptor: descriptor)!
    var bytes = [UInt8](repeating: 255, count: width * height * 4)
    for y in 0..<height {
        for x in 0..<width {
            let (r, g, b) = solid ?? gridColor(column: x * 8 / width, row: y * 4 / height)
            let i = (y * width + x) * 4
            bytes[i] = b; bytes[i + 1] = g; bytes[i + 2] = r
        }
    }
    texture.replace(region: MTLRegionMake2D(0, 0, width, height), mipmapLevel: 0, withBytes: bytes, bytesPerRow: width * 4)
    return texture
}

func publish(_ server: SyphonMetalServer, _ texture: MTLTexture, _ queue: MTLCommandQueue) {
    guard let buffer = queue.makeCommandBuffer() else { return }
    server.publishFrameTexture(texture, on: buffer, imageRegion: NSRect(x: 0, y: 0, width: texture.width, height: texture.height), flipped: false)
    buffer.commit()
    buffer.waitUntilCompleted()
}

func spin(until deadline: TimeInterval, _ done: () -> Bool) -> Bool {
    let end = Date().addingTimeInterval(deadline)
    while Date() < end {
        if done() { return true }
        RunLoop.main.run(until: Date().addingTimeInterval(0.02))
    }
    return done()
}

func readPixel(_ path: String, x: Int, y: Int) -> (UInt8, UInt8, UInt8, Int, Int)? {
    guard let data = FileManager.default.contents(atPath: path), data.count > headerBytes else { return nil }
    return data.withUnsafeBytes { raw in
        let w = Int(raw.load(fromByteOffset: 16, as: UInt32.self))
        let h = Int(raw.load(fromByteOffset: 20, as: UInt32.self))
        let stride = Int(raw.load(fromByteOffset: 24, as: UInt32.self))
        guard w > 0, x < w, y < h else { return nil }
        let i = headerBytes + y * stride + x * 4
        return (raw[i + 2], raw[i + 1], raw[i], w, h)
    }
}

func runSelfTest(_ args: [String]) {
    guard let dir = argument("--out-dir", args) else { fail("self-test needs --out-dir") }
    guard let device = MTLCreateSystemDefaultDevice(), let queue = device.makeCommandQueue() else { fail("No Metal device") }
    let started = Date()
    let app = ProcessInfo.processInfo.processName
    let gridServer = SyphonMetalServer(name: "Tolglow Test Grid", device: device, options: nil)
    let solidServer = SyphonMetalServer(name: "Tolglow Test Solid", device: device, options: nil)
    var checks: [[String: Any]] = []
    func check(_ name: String, _ ok: Bool, _ detail: [String: Any] = [:]) {
        checks.append(detail.merging(["check": name, "passed": ok]) { a, _ in a })
    }
    let grid = gridTexture(device: device, width: 640, height: 360)
    let solid = gridTexture(device: device, width: 320, height: 180, solid: (255, 0, 255))
    let found = spin(until: 5) {
        let names = Set(servers().filter { describe($0)["app"] as? String == app }.compactMap { describe($0)["name"] as? String })
        return names.isSuperset(of: ["Tolglow Test Grid", "Tolglow Test Solid"])
    }
    check("discovers two simultaneous sources", found, ["servers": servers().map(describe)])
    let gridPath = (dir as NSString).appendingPathComponent("grid.tgvf")
    let solidPath = (dir as NSString).appendingPathComponent("solid.tgvf")
    let gridFeed: Feed, solidFeed: Feed
    do {
        gridFeed = try Feed(app: app, name: "Tolglow Test Grid", out: gridPath, device: device, maxWidth: 1024, maxHeight: 1024)
        solidFeed = try Feed(app: app, name: "Tolglow Test Solid", out: solidPath, device: device, maxWidth: 1024, maxHeight: 1024)
    } catch { fail("Cannot open feed files: \(error)") }
    check("grid client connects", gridFeed.refresh())
    check("solid client connects", solidFeed.refresh())
    var frameTimes: [Double] = []
    for _ in 0..<30 {
        let t0 = Date()
        let before = gridFeed.received
        publish(gridServer, grid, queue)
        publish(solidServer, solid, queue)
        _ = spin(until: 1) { gridFeed.received > before }
        frameTimes.append(Date().timeIntervalSince(t0) * 1000)
    }
    check("grid frames arrive", gridFeed.received >= 25, ["received": gridFeed.received])
    check("solid frames arrive", solidFeed.received >= 25, ["received": solidFeed.received])
    var colours: [[String: Any]] = []
    var gridOK = true
    for row in 0..<4 {
        for column in 0..<8 {
            let x = column * 80 + 40, y = row * 90 + 45
            let expected = gridColor(column: column, row: row)
            if let (r, g, b, _, _) = readPixel(gridPath, x: x, y: y) {
                let ok = abs(Int(r) - Int(expected.0)) <= 1 && abs(Int(g) - Int(expected.1)) <= 1 && abs(Int(b) - Int(expected.2)) <= 1
                gridOK = gridOK && ok
                colours.append(["column": column, "row": row, "rgb": [r, g, b]])
            } else { gridOK = false }
        }
    }
    check("grid colours survive Syphon and the feed file", gridOK, ["cells": colours.count])
    if let (r, g, b, w, h) = readPixel(solidPath, x: 10, y: 10) {
        check("second source stays isolated", r == 255 && g == 0 && b == 255 && w == 320 && h == 180, ["rgb": [r, g, b], "size": [w, h]])
    } else { check("second source stays isolated", false) }
    // Resize: the sender publishes a larger frame; the feed follows.
    let big = gridTexture(device: device, width: 1280, height: 720)
    let beforeResize = gridFeed.received
    publish(gridServer, big, queue)
    _ = spin(until: 2) { gridFeed.received > beforeResize }
    if let (_, _, _, w, h) = readPixel(gridPath, x: 0, y: 0) {
        check("sender resize reaches the feed", w == 1024 && h == 576, ["size": [w, h]])
    } else { check("sender resize reaches the feed", false) }
    // Retirement: stopping the grid server clears the connected flag only on its feed.
    gridServer.stop()
    let retired = spin(until: 5) { !gridFeed.refresh() }
    let flags: (UInt32, UInt32) = {
        let a = FileManager.default.contents(atPath: gridPath)!.withUnsafeBytes { $0.load(fromByteOffset: 48, as: UInt32.self) }
        let b = FileManager.default.contents(atPath: solidPath)!.withUnsafeBytes { $0.load(fromByteOffset: 48, as: UInt32.self) }
        return (a, b)
    }()
    check("a stopped sender reads as disconnected", retired && flags.0 == 0 && flags.1 == 1, ["flags": [flags.0, flags.1]])
    // Return: a new server with the same app and name is a new UUID; the same
    // feed selection follows it.
    let returned = SyphonMetalServer(name: "Tolglow Test Grid", device: device, options: nil)
    let reconnected = spin(until: 5) { gridFeed.refresh() }
    let beforeReturn = gridFeed.received
    for _ in 0..<5 { publish(returned, grid, queue); _ = spin(until: 0.5) { gridFeed.received > beforeReturn } }
    if let (_, _, _, w, h) = readPixel(gridPath, x: 0, y: 0) {
        check("a returning sender is followed by the same selection",
              reconnected && gridFeed.received > beforeReturn && w == 640 && h == 360, ["size": [w, h]])
    } else { check("a returning sender is followed by the same selection", false) }
    gridFeed.stop()
    returned.stop()
    solidFeed.stop()
    solidServer.stop()
    let sorted = frameTimes.sorted()
    emit([
        "event": "self-test",
        "passed": checks.allSatisfy { $0["passed"] as? Bool == true },
        "checks": checks,
        "grid": gridPath, "solid": solidPath,
        "publish-to-feed-ms": ["median": sorted[sorted.count / 2], "max": sorted.last ?? 0],
        "seconds": Date().timeIntervalSince(started),
    ])
    exit(checks.allSatisfy { $0["passed"] as? Bool == true } ? 0 : 1)
}

/// Publish the labelled grid as a Syphon server until stdin closes, for QA of
/// the whole chain from another process (the JVM hub runs `feed`).
func runServe(_ args: [String]) {
    guard let device = MTLCreateSystemDefaultDevice(), let queue = device.makeCommandQueue() else { fail("No Metal device") }
    let name = argument("--name", args) ?? "Tolglow Test Grid"
    let width = Int(argument("--width", args) ?? "") ?? 640
    let height = Int(argument("--height", args) ?? "") ?? 360
    let fps = Double(argument("--fps", args) ?? "") ?? 30
    let server = SyphonMetalServer(name: name, device: device, options: nil)
    let texture = gridTexture(device: device, width: width, height: height)
    var published = 0
    let timer = Timer.scheduledTimer(withTimeInterval: 1 / fps, repeats: true) { _ in
        publish(server, texture, queue)
        published += 1
    }
    emit(["event": "serving", "app": ProcessInfo.processInfo.processName, "name": name, "width": width, "height": height])
    exitWhenStdinCloses {
        timer.invalidate()
        server.stop()
        emit(["event": "stopped", "published": published])
    }
    RunLoop.main.run()
}

let args = Array(CommandLine.arguments.dropFirst())
switch args.first {
case "list": runList(args)
case "feed": runFeed(args)
case "serve": runServe(args)
case "self-test": runSelfTest(args)
default:
    FileHandle.standardError.write(Data("usage: TolglowVideoBridge list [--watch] | feed --app A --name N --out PATH | serve [--name N] | self-test --out-dir DIR\n".utf8))
    exit(2)
}
