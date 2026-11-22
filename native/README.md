# Native iPhone and Apple Watch foundation

This is an iOS 18 / watchOS 11 SwiftUI client and a shared pure Swift package.
[macOS CI](https://github.com/tolgraven/tolglow/actions/runs/44891609591/job/136808451227)
verified **seven Swift tests and unsigned iOS/watchOS simulator builds** with
Xcode 16.4 / Swift 6.1.2. Simulator app launch and physical-device behavior remain
unverified. This is an initial native implementation; full parity with the
re-frame studio remains ongoing work.

The **Scan rig** tab captures tap-assisted ARKit/LiDAR observations, identifies
settled calibration patterns through actual output receipts and submits reviewed
fixture-placement proposals. See [rig calibration](../docs/rig-calibration.md)
for alignment, explicit output arming, numerical constraints and hardware gates.

The iPhone has a cue grid using the real Ring `/api/state` and `/api/command`
contracts, persistent cue launch/release by exact instance ID, master and blackout.
It refreshes explicitly and after accepted commands. An optional **Studio** tab
embeds the actual re-frame application in an owned WKWebView at the explicitly
configured origin, reusing its existing functionality. Navigation stays on that
origin; there is no privileged native JavaScript bridge. Closing the tab view
stops its web load. Native WebKit support for the studio's WASM threads, WebGL,
Neovim, downloads/uploads and background lifecycle requires physical-device QA;
no full feature parity is inferred from embedding the URL.

The native **Perform** controls have no second cue engine,
held-cue lease, authoring UI, live parameter editor or background polling yet.
Commands carry runtime/show/epoch identity and a unique command ID, and are not
retried after an ambiguous transport failure.

The conducting path is:

```
Watch CoreMotion + supported primary-action Button
  → bounded WatchConnectivity sendMessageData + application acknowledgement
  → explicitly armed paired iPhone relay + Keychain token
  → one OSC/UDP message → host enrollment + sensor middleware → common commands
```

The iPhone can also produce its own CoreMotion observations. A normal watchOS app
cannot open low-level UDP/TCP sockets. Apple's TN3135 explicitly documents this
restriction and that the simulator can misleadingly allow it. The implementation
uses WatchConnectivity; it does not create fake workout/audio sessions to bypass
OS policy. Both apps must stay foreground in this first version. Backgrounding
the phone disconnects its native network resources and disarms; the Watch disarms when
its scene leaves active state. Sustained background conducting is future work
requiring a legitimate Apple-supported lifecycle and battery/device measurements.

## Generate and build on a Mac

Install Xcode 16 or newer with iOS 18 and watchOS 11 SDKs and XcodeGen. From this
folder:

```sh
(cd Core && swift test)
xcodegen generate
open Tolglow.xcodeproj
```

For a repeatable unsigned simulator build gate, run `Tests/check_macos.sh`.
That script runs the package tests, generates the project, and builds both
simulator schemes; it does not install on hardware or configure signing.

Choose your development team for both targets, change **both** bundle IDs if
needed, and keep `WKCompanionAppBundleIdentifier` and the Watch bundle-ID prefix
consistent. Build `TolglowPhone` for the paired physical iPhone and Watch. The
`TolglowWatch` scheme also builds the Watch target. Generated projects, generated
Info.plists, DerivedData and Swift build caches are ignored. The `project.yml`
contains the motion/local-network/camera permission strings; it does not include signing
credentials, a fabricated binary, or background privileges.

The iOS studio URL defaults to `http://tolglow.local:8500`; this is configurable.
`NSAllowsLocalNetworking` permits local development origins, not a general ATS
bypass. Use a reachable `.local` hostname or HTTPS if a chosen deployment is
blocked by ATS. HTTP studio access needs the same trusted-network deployment as
the existing web studio; the native app does not add a remote authentication layer.

## Pair, calibrate, arm

1. In iPhone **Connect**, set the real host and the UDP port of its explicitly
   opened sensor listener. Connecting UDP does not prove host acceptance.
2. Activate WatchConnectivity on both apps. On Watch choose **Pair / announce
   ID**. The iPhone can select that exact ID; the announcement is discovery only.
3. Enroll the iPhone and/or selected Watch ID at the host and obtain separate
   host-issued pairing tokens. Save each token in the corresponding iPhone
   pairing field. Tokens live in `kSecAttrAccessibleWhenUnlockedThisDeviceOnly`
   Keychain entries, never UserDefaults or a show export. The Watch never receives
   the token; the iPhone attaches it to that device's outgoing packet.
4. Arm the selected Watch relay on iPhone. On Watch preview motion, hold the
   desired neutral pose, select **Neutral**, then **Arm**. For phone-only motion,
   preview/calibrate/arm in **Conduct**. Each arm creates a fresh session UUID.
   Enroll/arm the corresponding session in the host according to its own policy.
5. Configure the host's sensor mapping with explicit axis bounds, targets and
   gesture actions. Native observations do not silently choose effects. Use the
   host's admitted-sample/gesture status to verify actual acceptance.
6. The Watch's **Primary action** button has SwiftUI's
   `.handGestureShortcut(.primaryAction, isEnabled: motion.armed)`. On supported
   hardware and settings, Double Tap triggers that semantic button while its scene
   is frontmost. This is not raw finger detection or a stream of arbitrary gestures.

The app requests motion/local-network access only when the respective explicit
operation starts. Disarming clears pending motion; a datagram already handed to
the OS cannot be recalled. Disarm the host mapping when immediate output gating
is required. A network error, unreachable counterpart, gesture queue overflow or
send/acknowledgement deadline disarms the affected producer/relay. There are no
automatic mutation or gesture retries. WC acknowledgement means the iPhone
accepted the sample for UDP, not that the host applied it. UDP has no delivery
acknowledgement, encryption or ordering guarantee; use a trusted lighting LAN or
an authenticated network tunnel. A pairing token is bearer authorization, not
transport confidentiality.

## Sensor protocol and units

One OSC 1.0 datagram at `/tolglow/sensor/v1`, type tag `,s`, contains one UTF-8 JSON
string. The shared OSC encoder includes zero terminators and 4-byte alignment,
rejects malformed/trailing data when decoding, and bounds each datagram to 8192
bytes. The packet is atomic; axes are not split across separate OSC addresses.

```json
{"version":1,"device-id":"watch-example","session-id":"arm-session-uuid",
 "sample-id":12,"time-ms":123450.0,
 "values":{"roll":0.1,"pitch":0.0,"yaw":-0.2,"accel-x":0.0,"accel-y":0.0,
           "accel-z":0.0,"rotation-x":0.0,"rotation-y":0.0,"rotation-z":0.0},
 "gestures":[],"reference-frame":"x-arbitrary-z-vertical",
 "calibration-id":"neutral-pose-uuid","pairing-token":"host-issued-secret"}
```

`sample-id` increases within a session. `time-ms` is CoreMotion's monotonic device
timestamp; it is not wall time or the show's transport time. The host replaces the
control clock with its authoritative time/beat/meter and retains device time as
metadata. `roll/pitch/yaw` are radians from the relative quaternion
`conjugate(neutral) * current`; they are not obtained by subtracting Euler angles.
Euler coordinates have singularities and wrapping, so mappings must select useful
ranges. `accel-*` is gravity-removed `userAcceleration` multiplied by 9.80665,
in m/s²; `rotation-*` is CoreMotion rotation rate in rad/s. The frame is explicitly
`.xArbitraryZVertical`; there is no claim of geographic heading or coordinate
parity with browser `orientation-alpha/beta/gamma` values.

A semantic button gesture is `{"id":"unique-uuid","type":"primary-action",
"phase":"pulse"}`. The host must deduplicate/reject stale sessions and sample
IDs, authenticate tokens, and impose its own mapping/arming/cooldown policy.
Transport packets and logs must not retain pairing tokens after admission.

The sample rate is requested at 20 Hz, not guaranteed. Each transport has one
in-flight send, one latest continuous sample, and at most eight queued gesture
samples. A new gesture supersedes an older coalesced motion sample so IDs never
run backwards due to the local queue. Gesture overflow fails visibly; it does not
silently discard a cue action. Watch acknowledgement and UDP-send deadlines are
2 seconds; initial UDP readiness is bounded to 5 seconds. No send can block the
lighting frame owner.

## Verification and remaining gates

- Foundation XCTest source exercises JSON/OSC round trips, malformed/nonfinite
  samples, token attachment, relative quaternion calibration, bounded mappings,
  and coalescing/gesture ordering. **Seven tests passed on macOS CI.**
- The checked-in protocol fixture and source/build-manifest checks can be run with
  `python3 Tests/check_contract.py`. This validates data and static contract
  invariants; it does not execute Swift or replace an Apple build.
- The real JVM sensor decoder plus an owned loopback UDP socket accepted the
  shared fixture: **4 tests / 10 assertions passed** with
  `clojure -M native/Tests/check_host.clj` from the repository root. These packets
  were generated as protocol fixtures, not emitted by compiled Swift.
- Static Python checks: **6 tests passed**.
- Swift source syntax was additionally parsed with tree-sitter for both platform
  branches. This is not Swift type checking, SDK availability or actor validation.
- `swift test`, XcodeGen and both unsigned Xcode simulator builds passed on
  Xcode 16.4. Remaining gates include simulator launch, real device permission
  prompts and pairing, foreground motion latency/battery,
  primary-action Double Tap, disconnection/scene cleanup, host admission and
  exact cue/parameter behavior. The host sensor service and rich native mapping
  UI are independently tracked work; no source artifact proves their deployment.

Official references:

- [Apple TN3135: low-level networking on watchOS](https://developer.apple.com/documentation/technotes/tn3135-low-level-networking-on-watchos)
- [WCSession sendMessageData](https://developer.apple.com/documentation/watchconnectivity/wcsession/sendmessagedata(_:replyhandler:errorhandler:))
- [SwiftUI handGestureShortcut](https://developer.apple.com/documentation/swiftui/view/handgestureshortcut(_:isenabled:))
- [Primary action shortcut](https://developer.apple.com/documentation/swiftui/handgestureshortcut/primaryaction)
- [CMMotionManager device motion](https://developer.apple.com/documentation/coremotion/cmmotionmanager)
- [CMDeviceMotion](https://developer.apple.com/documentation/coremotion/cmdevicemotion)
- [OSC 1.0 specification](https://opensoundcontrol.stanford.edu/spec-1_0.html)

## Relative fixture brightness

On iPhone choose **Scan rig → Measure fixture brightness**, connect to the Studio origin and select one representative of each fixture type. Select the reference first. Cover the rear wide camera with the same neutral diffuser throughout the run. Position its face one metre from each fixture’s lens on the centerline of its beam. Keep the diffuser facing the source and other lighting steady. This measures relative on-axis illumination in that geometry. Beam width affects it. Absolute lux, total lumens and electrical watts are different quantities.

Choose a shutter speed before arming. The iPhone uses the rear physical wide camera, minimum ISO, fixed shutter, locked focus and white balance, unity zoom and SDR color. A capture waits for the host to confirm physical presentation, allows settling, then samples 15 frames for each of dark and full white. The center grid uses linearized Rec. 709 luminance. Clipped, unstable and low-contrast readings can be retaken. If the shutter or diffuser needs changing, stop and start the whole run again. At a mains or PWM frequency that produces unstable readings, try a longer shutter and a denser neutral diffuser.

Once every type has a complete dark/white pair, Save sends the measurements in reference order to the show. The host computes ratios using each pair’s white-minus-dark signal. Navigating away, backgrounding, a camera interruption or a lost connection stops the session. The host lease also releases output when an iPhone cannot send cancellation. iPhone ISP tone processing and spectral sensitivity can vary, especially for narrowband LEDs. Camera repeatability and fixture-to-fixture agreement require physical measurements on the intended hardware.
