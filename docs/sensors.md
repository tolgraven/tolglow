# Phone motion and native Watch conducting

Phone/browser and native iPhone/Apple Watch inputs share a pure conditioning and
control layer. The implemented core owns data transformations, calibration,
hysteresis, musical cooldowns and exact held-cue ownership. It does not own a
sensor, browser permission, socket, wall clock or show command queue.

The host service and OSC codec are implemented separately, with explicit pairing,
bounded admission, optional owned UDP input and frame-boundary evaluation. The
browser and native UI are separate lanes; source and synthetic/loopback tests do
not establish physical conducting latency, gesture quality or Apple build success.

## Browser library decision

Use **`@capacitor/motion` 8.0.1**, with a compatible Capacitor core 8.x, as the
maintained adapter to browser motion events. The inspected current official
repository revision is `87c0bb8045db2b4560d3db4b7d8e565c23ec1736` (2026-10-02).
The package is MIT licensed and version 8.0.1 was published on 2026-07-15. Its
implementation is small: `Motion.addListener("accel", callback)` maps to
`devicemotion`, and `Motion.addListener("orientation", callback)` maps to
`deviceorientation`. Each returned listener handle supports owned removal.

| Candidate | Verified source state | Decision |
| --- | --- | --- |
| Capacitor Motion 8.0.1 | Official maintained MIT package; WebPlugin event wrapper with owned handles | Use for acquisition, with our explicit unit/capability normalization |
| GyroNorm 2.0.6 | Package published 2017-02-28; repository last inspected commit `39d5fda182cd8655e32ae0d1e5f58eade3c8c1d2`, 2019-03-05 | Do not make this older fusion wrapper the new acquisition dependency |
| Full-Tilt | Last inspected repository commit `22a1007af229153e5b897afd1fc903d7d91fe481`, 2015-10-20 | Historical orientation reference; not selected as a maintained foundation |
| Direct browser APIs | Actual source beneath Capacitor; capabilities differ by browser/device | Keep a small capability-aware fallback, not a second competing sensor engine |

An executed Node 24.22.3 smoke test loaded the real Motion/core 8.0.1 packages and
passed three assertions for multiple listeners, individual handle cleanup and
orientation delivery using an injected EventTarget. The [retained report](benchmarks/2026-11-21-sensor-library-smoke.json)
records versions and package integrity. This establishes the package integration,
not browser permission behavior or physical sensor availability.

Browser acquisition requires a secure context on supported browsers. On iOS,
request `DeviceMotionEvent.requestPermission()` and, when available,
`DeviceOrientationEvent.requestPermission()` from an explicit user action. A
missing API, denial, absent sensor and a sensor that has not emitted are different
states. Do not replace missing/null measurements with zero. Remove only this
session's listener handles; a global remove-all operation can stop another owner.

The plugin's current TypeScript documentation reuses `RotationRate` for
orientation and labels acceleration loosely. Follow the actual Web API units:
acceleration is **m/s²**, rotation rates are **degrees/s**, and orientation alpha,
beta and gamma are **angles in degrees**. Convert rates/angles to radians at the
adapter boundary. The plugin provides neither universal sensor fusion nor a
uniform reference frame across platforms.

## Native Apple capabilities

`CMMotionManager` supports explicit acquisition intervals, hardware availability
checks and stop methods. Processed device motion includes user acceleration,
attitude, rotation rate and gravity. Core Motion acceleration uses g units; convert
at the boundary and record the chosen reference frame. Native roll/pitch/yaw must
not be mislabeled as browser alpha/beta/gamma: their Euler conventions differ.
Prefer a calibrated quaternion within native code before projecting named axes.

Apple's documented `.handGestureShortcut(.primaryAction)` attaches a semantic
shortcut to a Button or Toggle in the frontmost scene. The SwiftUI API is available
from watchOS 11; its documentation describes thumb/index double tap on Watch. It
is **not** an API for arbitrary finger positions, raw double-tap sensor samples or
an unrestricted background gesture stream. Use the same button action for touch
and the supported OS shortcut. Current Apple support lists Series 9 or later,
Ultra 2 or later, and SE 3; capability and real-device tests remain necessary.

Apple TN3135 explicitly restricts low-level networking on watchOS. Normal apps
cannot open direct UDP/TCP/Network-framework sessions; exceptions concern active
audio streaming, CallKit VoIP and a specific tvOS application-service case. The
simulator can falsely allow networking that a physical Watch rejects. Therefore
the native design uses **WatchConnectivity to the paired iPhone, then iPhone UDP**.
Do not manufacture an audio/workout mode merely to bypass an unrelated restriction.

`WCSession.sendMessage` requires an active session and a reachable counterpart;
messages are asynchronous and serially queued. An active Watch app can wake the
paired iOS app, but the reverse is not guaranteed. Treat reachability/dropout as
real lifecycle state. Native source in `native/` is foreground-oriented and
explicitly disarms on backgrounding. Swift/Xcode were unavailable in the Linux
environment; its Swift tests/build and physical latency are unverified, even though
its checked-in OSC fixture passed the real JVM decoder/owned loopback gate.

## Normalized observation contract

The fixed OSC address is `/tolglow/sensor/v1`, with **exactly one OSC string**
(type tags `,s`) containing UTF-8 JSON. HTTP ingestion uses the same object.
`tolglow.studio.sensor-osc` owns strict zero-padding/UTF-8/type validation, a
60,000-byte packet bound, JSON nesting bounds and optional explicit UDP ownership.
No OSC bundle or numeric argument is silently reinterpreted as this format.

```clojure
{:version 1
 :device-id "phone-or-watch-id"
 :session-id "fresh-session-per-source-arm"
 :sample-id 42
 :time-ms 12345.6                    ; monotonic source acquisition clock
 :pairing-token "enrollment-token" ; transport only, removed before model state
 :reference-frame "device"
 :calibration-id "optional-calibration-id"
 :values {:accel-x 0.3 :accel-y 0.0 :accel-z -0.1
          :rotation-x 0.2 :rotation-y 0.0 :rotation-z 0.0
          :orientation-beta 0.4}   ; browser intrinsic angle; not native pitch
 :gestures [{:id "stable-event-id" :type :primary-action :phase :pulse}]}
```

Values are finite scalar SI measurements: acceleration in m/s², rotation rates
in rad/s, and named angles in radians. Native `:roll`, `:pitch`, `:yaw` remain
distinct from browser `:orientation-alpha`, `:orientation-beta`,
`:orientation-gamma`. Samples need nonnegative safe-integer sequence IDs, bounded
source/session IDs and no null pseudo-measurements. Optional gesture confidence
is a bounded value; it is not automatically a calibrated probability.

The host authenticates a per-device pairing token stored only by credential
reference in show files. The iPhone relay attaches the token for the explicitly
selected Watch device. Pair responses expose the token once; public status,
model state and exported show data never contain it. A token authorizes admission
for its paired source, not arbitrary control operations embedded in a packet.
OSC UDP has no delivery guarantee or encryption of its own; the currently owned
receiver defaults to loopback and starts only when explicitly requested.

The host replaces acquisition `:time-ms` with authoritative show time and supplies
`:beat`, `:meter` and transport fields. Original acquisition time becomes
`:device-time-ms` for filtering. Source timestamps cannot change the show clock.
Receipt age is measured independently so queued old data cannot become fresh just
because it was admitted during a later frame.

## Composable pure context middleware

`afterglow.control.sensors` exposes `new-state`, `validate-state`, `calibrate`,
`set-armed`, `step`, `expire`, `acknowledge`, `rebase-clock`, and `middleware`.
State is immutable and portable. Inputs include clocks, calibration, all filter
progress and held ownership; no global atom or RNG is used.

```clojure
(def mapping
  {:version 1 :timeout-ms 500
   :channels [{:id :tilt :input :orientation-beta :calibrated? true
               :smoothing-ms 40
               :transforms [{:op :wrap :period (* 2 Math/PI)}
                            {:op :deadband :width 0.04}
                            {:op :map-range :in [-0.7 0.7] :out [0 1]}
                            {:op :curve :kind :smoothstep}]}]
   :bindings [{:id :level :channel :tilt
               :schema {:type :number :default 0 :bounds [0 1]}
               :command {:op :param/set :instance-id "0:1" :param :level}
               :epsilon 0.001}]
   :gestures [{:id :accent :type :primary-action :cooldown {:beats 1/4}
               :command {:op :cue/launch :cue-id :accent
                         :launch {:mode :one-shot :duration {:beats 1/8}}}}]})

((core/middleware mapping admitted-sample) context)
```

The middleware consumes and returns the **whole context**. It preserves unrelated
keys, stores per-source progress under `:sensor-states` keyed by
`[device-id session-id]`, writes conditioned projections under `:sensor-values`,
and appends validated `:events` and semantic `:commands` vectors. Missing source
states start disarmed. Multiple middleware functions compose normally.

Channel inputs are a scalar key or a magnitude of explicit scalar keys.
Transform pipelines support offset, scale, absolute/negated value, clamp, range
mapping, continuous deadband, wrap and linear/smoothstep/ease-in/ease-out curves.
Explicit exponential smoothing uses physical acquisition delta. Missing values
stay missing; unavailable source channels do not fabricate an orientation.

Bindings support typed scalar or multi-channel parameter values with captured
schemas, plus master/instance opacity. Integer, boolean and enum conversion is
explicit; vectors/colors/ranges must validate. Full parameter/registry admission
still belongs to the shared reducer, including effects that have hot-reloaded to
incompatible schemas. Use clamped transforms where a bounded control is desired.

Thresholds use distinct on/off levels for genuine hysteresis, with optional
`:direction :below`. Their cooldown is a beat/bar/phrase or explicit ms duration
resolved against captured meter. Musical cooldown does not advance merely because
wall time passes while transport is paused. Gestures are semantic pulses with
bounded duplicate receipts, not inferred raw finger APIs.

Held threshold cues use the existing pure begin/end/acknowledgement helpers.
Releases address the exact launched instance, including release-before-ack,
disarm, session replacement and dropout. No lookup by cue ID can terminate a
human's unrelated instance. The host stamps command identity/epoch/time; the
service supplies only its owned sensor source identity. Pure core intents cannot
carry a fabricated authority envelope.

## Host ownership and persistence

`tolglow.studio.sensors` owns only credentials, observed sessions, bounded mailbox
and optional socket. Mappings, calibration and progress live in:

```clojure
{:sensors {:version 1
           :sources {device-id {:config mapping
                                :pairing {:credential-ref "credential-UUID" :label "Phone"}
                                :armed-intent? true
                                :state portable-core-state}}}}
```

`pair!` produces a credential reference and one-time token. Pure `configure`
admits a mapping and returns any release intents. `ingest!` authenticates and
queues observations. They may arrive disarmed for calibration/visibility; `arm!`
requires an explicitly selected **fresh observed session** and discards queued
pre-arm observations. Physical sampling and permission do not implicitly arm
lighting output. `calibrate` requires disarmed output.

`advance! [manager model clock]` drains a bounded batch, applies authoritative
clocks and returns `{:model :commands :events}`. The host commits this with its
normal command stream and folds actual held launch receipts through
`acknowledge`. New samples are processed before dropout expiry, so a fresh packet
is not discarded merely because a frame interval was long. Invalid conditioned
parameter output is isolated to its source with a visible diagnostic and disarm;
it does not abort the entire lighting frame. Queue overflow explicitly disarms
input ownership rather than keeping an apparently healthy backlog.

Source identity, increasing sequence, physical timestamp and session ownership
checks reject stale/foreign packets. The host must serialize model operations;
this service does not create a competing model owner. Socket startup/stop and
credential IO remain outside the rendering transaction. Listener endpoint
preferences belong in ordinary persisted host settings, not a serialized socket.

Connections constructs/prepares/commits/retires the sensor manager with the other
owned resources and passes the same lazy credential vault. Restore opens no UDP
receiver and does not re-arm output. Saved armed **intent** remains visible.
After canonical instance-ID remapping, `prepare-restored-model` drops only old
unacknowledged callbacks; known held IDs remain for exact releases on the first
post-load advance. Clearing them earlier would orphan saved held effects.
Calibration/filter state remains data; no secret, live handle or source callback
is restored. `rebase-clock` shifts only absolute beat cooldown anchors.

## Verification and remaining gates

The pure core passed **11 tests / 71 assertions** covering transformations,
calibration, physical-time smoothing, all relevant schema conversions, real shared
command admission, hysteresis/captured 7/8-meter cooldown, paused transport, held
ownership, duplicate gestures, stale samples, missing data, dropout and composed
context preservation. The executed library smoke passed three assertions.

Host/OSC/connection tests passed **31 tests / 239 assertions**, including the
credential-close race regression. They exercise real wire encoding/decoding,
owned loopback UDP, pairing secrets/reference export, explicit fresh-session arm,
pre-arm queue discard, authoritative time, restored disarm, late held receipts,
bounded overflow, input-error isolation and staged resource cleanup. Final exact
counts are reported in the central log; no sensor hardware was activated.

Remaining acceptance includes actual iPhone/iPad Safari permissions and axis
calibration; sustained foreground/background/reconnect behavior; successful Apple
build/signing and physical Watch↔phone relay; gesture latency/jitter/dropout under
performance load; readable calibration/arming UI; and host Ring/tick/persistence
integration. Native source parse checks, a browser viewport and loopback UDP do
not substitute for these gates.

## Sources inspected on 2026-11-21

- [Official Capacitor Motion source and README](https://github.com/ionic-team/capacitor-plugins/tree/87c0bb8045db2b4560d3db4b7d8e565c23ec1736/motion)
- [Motion npm metadata](https://registry.npmjs.org/@capacitor/motion), [GyroNorm metadata](https://registry.npmjs.org/gyronorm)
- [GyroNorm inspected commit](https://github.com/dorukeker/gyronorm.js/commit/39d5fda182cd8655e32ae0d1e5f58eade3c8c1d2), [Full-Tilt inspected commit](https://github.com/adtile/Full-Tilt/commit/22a1007af229153e5b897afd1fc903d7d91fe481)
- [MDN DeviceMotionEvent](https://developer.mozilla.org/en-US/docs/Web/API/DeviceMotionEvent)
- [Core Motion manager](https://developer.apple.com/documentation/coremotion/cmmotionmanager), [acceleration units](https://developer.apple.com/documentation/coremotion/cmacceleration)
- [SwiftUI hand gesture shortcut](https://developer.apple.com/documentation/swiftui/view/handgestureshortcut(_:isenabled:)), [primary action](https://developer.apple.com/documentation/swiftui/handgestureshortcut/primaryaction)
- [Apple current supported-device troubleshooting](https://support.apple.com/en-us/108413)
- [TN3135 low-level networking on watchOS](https://developer.apple.com/documentation/technotes/tn3135-low-level-networking-on-watchos)
- [WatchConnectivity sendMessage](https://developer.apple.com/documentation/watchconnectivity/wcsession/sendmessage(_:replyhandler:errorhandler:))
