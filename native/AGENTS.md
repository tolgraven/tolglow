# Native Apple clients

Read the repository AGENTS and docs/product/sensors-and-native.md if present.
This directory contains native SwiftUI clients; the browser remains re-frame.
Keep shared sensor transforms/OSC data in Core and IO in owned platform adapters.
Ordinary constructors do not start motion, networking, or a show.

- `Core` is a Foundation-only Swift package. Run `cd Core && swift test` where a
  Swift toolchain is installed. Generated Xcode projects are not source truth;
  edit project.yml and use XcodeGen. Do not claim Apple targets build on Linux.
- Route operator controls and paired audience samples through WatchConnectivity. Solo audience enrollment uses the shared Core Bluetooth central. Apple TN3135 restricts ordinary low-level UDP/TCP on Watch.
- Only SwiftUI's supported primary-action gesture shortcut is used for Double
  Tap. Do not infer raw finger trajectories or promise every Watch supports it.
- Motion values have explicit SI units and CoreMotion reference/calibration
  metadata. Do not conflate browser alpha/beta/gamma with CoreMotion roll/pitch/yaw.
- Host enrollment and explicit arming are required. Pairing tokens live only in
  iPhone Keychain and transient outgoing packets, never logs/UserDefaults/show files.
- Bound every queue. Coalesce motion, preserve gesture identities/order, fail
  visibly on gesture overflow. Do not retry ambiguous gestures or cue commands.
- Backgrounding disarms operator controls and stops phone capture. The audience
  relay may forward an explicitly opted-in Watch session while the iPhone locks.
  Use HealthKit workout processing on Watch and Bluetooth central delivery on
  iPhone. The App Clip supports foreground inputs.
- Physical iPhone/Watch motion, WCSession relay, local-network permission, Double
  Tap, signing, battery/latency and host acceptance all need device evidence.
