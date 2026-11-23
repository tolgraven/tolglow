# Host a dance floor on Linux

A Raspberry Pi or Linux show host can advertise the same Bluetooth LE stage as the Mac bridge. Guests join from the Dance floor app, a compatible App Clip or the corresponding Watch client. The bridge combines continuous sensor readings five times per second. Recognized step onsets flush on a separate 25 ms gateway tick and carry the last published levels. A Tolglow show receives aggregate channels and individual dance steps through its existing paired OSC sensor input.

## Start a stage

Use Linux with BlueZ, a running system D-Bus, Java 17 or later, Leiningen, and a powered Bluetooth controller that supports peripheral-role GATT and LE advertising. Run under a local account allowed to register a GATT application and advertisement with BlueZ. A Debian or Raspberry Pi OS installation can provide the required system components through `bluez`, `dbus`, `default-jre-headless` and `leiningen`. The service uses the existing adapter power and discoverability configuration. Select a different adapter with the final argument.

Run the gateway from this directory.

```sh
lein run --advertise "East stage" https://tolgraven.se east-stage.png hci0
```

The CLI prints a stage invitation and writes its scannable QR to the selected path. Use the HTTPS origin configured for your signed app or Clip. The 128-bit service UUID and local name are advertised over BLE. Scanning the QR pins the SHA-256 fingerprint of the ephemeral stage’s public key. Restarting the gateway creates a new key and invitation. Stop the process to withdraw the stage. Without OSC settings the console reports only the current participant count.

To connect to a show, enroll an OSC sensor named `audience` in Tolglow, set the environment variables below, and start the stage. Arm the new session after its first observation. The regular Tolglow configuration maps its aggregate channels to parameters and its `dance-step` gestures to cues. [Sensor setup and arming](../../docs/sensors.md)

| Environment setting | Value |
| --- | --- |
| `TOLGLOW_AUDIENCE_OSC_TOKEN` | The enrolled source's credential, held by the gateway and show. |
| `TOLGLOW_AUDIENCE_OSC_PORT` | Port of the paired Tolglow OSC listener. Set the port and token together. |
| `TOLGLOW_AUDIENCE_OSC_ID` | Enrolled source ID. Defaults to `audience`. Independent floor zones use distinct IDs and credentials. |
| `TOLGLOW_AUDIENCE_OSC_HOST` | Defaults to `127.0.0.1`. For a separate lighting computer, use a trusted private link or tunnel carrying OSC. Its UDP payload contains the pairing token. |

## What reaches the show

| Channel | Meaning |
| --- | --- |
| `audience-count`, `dance-effort` | Count of active guests and their equally weighted mean effort (0–1). |
| `dance-steps` | Newly delivered steps in this observation. Each has a unique `dance-step` pulse gesture. |
| `pulse-count`, `pulse-present`, `pulse-bpm` | Number sharing pulse, availability and mean BPM where present. |
| `motion-count`, `accel-x/y/z`, `rotation-x/y/z` | Number sharing six-axis measurements and their mean acceleration (m/s²) and rotation (rad/s). Only guests supplying axes contribute to that mean. |

The public identity characteristic returns 65 SEC1 P-256 key bytes followed by a fresh 16-byte challenge. A participant writes its 65-byte ephemeral public key. The two endpoints derive a per-guest AES-256-GCM key using ECDH and HKDF-SHA256. Valid sensor writes are 39 bytes (step, effort and optional pulse) or 63 bytes (also six float32 axes). The stage rejects stale serial numbers even after a guest has dropped out of the recent aggregate. It uses host receipt time for freshness and a bounded step-credit calculation to handle cumulative counters and wraparound. One guest's previous steps never fire again when they rejoin.

Fresh samples contribute for 2.5 seconds. Challenges and inactive keys expire after eight seconds. A BlueZ device disconnect removes its pending challenge, key and measurements immediately. The gateway emits a final zero-count observation when the floor empties. Step events are buffered up to 2,048 and sent in batches of at most eight gestures. Queue overflow and failed sends stop the stage visibly. Up to 128 peer records and 128 incomplete challenges can be held in memory. This implementation contains only the sensor service.

## ATT MTU and long writes

BlueZ handles ATT fragmentation below the exported D-Bus application. In BlueZ 5.83, the GATT server appends contiguous Prepare Write fragments for the same attribute handle. On Execute Write it passes the accumulated bytes and their starting offset to the attribute. Cancelling the transaction discards the prepared bytes. The external characteristic adapter reserves per-fragment authorization callbacks for characteristics explicitly marked `authorize`. The gateway exposes ordinary acknowledged `write` characteristics. Its D-Bus `WriteValue` consumes one complete hello or sealed sample at offset zero. A standalone partial envelope or a write beginning at a nonzero offset is rejected before a measurement can be accepted. [BlueZ ATT prepare/execute implementation](https://github.com/bluez/bluez/blob/d97d1951b447f8575e972458d3007afcfed6cf28/src/shared/gatt-server.c#L1243-L1262) · [Execution and cancellation](https://github.com/bluez/bluez/blob/d97d1951b447f8575e972458d3007afcfed6cf28/src/shared/gatt-server.c#L1399-L1498) · [External characteristic handling](https://github.com/bluez/bluez/blob/d97d1951b447f8575e972458d3007afcfed6cf28/src/gatt-database.c#L3329-L3361)

At an ATT MTU of 23 bytes, a Prepare Write carries up to 18 payload bytes. The 65-byte hello and 63-byte motion sample each need four contiguous fragments when each fragment uses the available payload. The 39-byte sample needs three. Reading the 81-byte identity can require Read Blob requests. Each continuation returns bytes from its requested offset and keeps the original challenge. The first read at offset zero creates the challenge.

For the venue acceptance test, record the Pi/controller and BlueZ versions and the negotiated MTU from `btmon`. Exercise a link at MTU 23 and the usual larger MTU. Verify the full identity, hello, one basic packet and one six-axis packet from the same Apple participant. For long writes, correlate the ATT prepare/execute transaction with one stage observation and one expected counting increment. Exercise a cancelled long write and a disconnect before commit, checking that neither emits a partial observation. Record whether CoreBluetooth negotiated the requested small MTU. If it selected a larger value, use a configurable BLE central to exercise the smaller one and report the Apple-device run separately.

## Verify the gateway

```sh
lein test
AFTERGLOW_ROOT=/path/to/afterglow lein with-profile +paired test
```

The tests start a temporary `dbus-daemon`, export the management interfaces on a mock `org.bluez` process connection, and use a separate bus connection for the real gateway. The manager enumerates the advertised properties and GATT hierarchy, reads the identity in offsets, writes an encrypted hello/sample exchange and unregisters. A different D-Bus sender cannot supply another central’s path. Independent Python cryptography vectors cover both encrypted sample versions. The paired lane uses real UDP, the normal Tolglow sensor manager and an armed gesture mapping.

A physical acceptance run needs a BlueZ LE peripheral adapter and an Apple client. Scan the generated invitation, join, send taps and optional pulse/motion, disconnect, and verify the final zero count on the show. Compare subsequent re-enrollment, radio dropout, ATT write limits and sustained client count on each intended controller and Pi model. The private-bus tests do not transmit on a Bluetooth radio.
