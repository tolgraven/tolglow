# Sharing rendering work from the dance floor

The stage can invite a foreground iPhone or iPad to evaluate lighting expressions. Scan the stage's QR with the **full app**, join its Bluetooth service, and choose **Share GPU with this stage**. Movement and GPU sharing have separate controls. The sensor-only App Clip and Watch never receive a compute grant.

## Stage configuration

On the Tolglow host select the distributed compute backend. Configure `TOLGLOW_AUDIENCE_COMPUTE_TOKEN` as a new random credential of at least 24 characters on the host and the Mac audience bridge. It must differ from `TOLGLOW_COMPUTE_TOKEN`, which enrolls operator workers. The bridge reaches the fixed loopback compute route on `TOLGLOW_AUDIENCE_COMPUTE_PORT` (default 8500). If Studio requires an operator login, the bridge can use its own host-held `TOLGLOW_STUDIO_TOKEN` for the HTTP upgrade. No login cookie, operator password or show command socket reaches the participant.

With that credential present, the bridge advertises its TCP Bonjour service on local interfaces with Network.framework peer-to-peer discovery enabled. Apple may choose a shared Wi-Fi network or a direct peer interface. Set the Mac's local network permissions for the bridge and the full app's permission on the phone. BLE carries the 5 Hz dance summaries and a participant-specific encrypted grant. Shader sources, fixture buffers and GPU results travel on the separate peer link.

The public QR fingerprints the stage key. The separate grant is AES-GCM sealed under the existing per-participant BLE session key, restricted to one stage and redeemable once for 20 seconds. It creates independent, ordered encrypted records in each direction. The stage admits four guest devices at a time, limits unauthenticated TCP connections to eight, and expires a session after 15 minutes, 45 seconds without work, or loss of the originating BLE session. The phone stops in the background, on a serious thermal state, in Low Power Mode, or on disconnect. Resuming requires another explicit enrollment.

Before connecting to the host, the guest must compile a shader and get the correct two numerical readbacks. The stage also times three fresh 16 KiB round trips. A median above 30 ms fails admission. The reported capacity is the smaller of 8,192 floats and six milliseconds of measured return bandwidth, with a minimum of 256 floats. These are eligibility limits. Physical Wi-Fi, peer-to-peer throughput, battery use and sustained thermal behavior need measurements on a phone and Mac.

## Verification before physical output

The guest bridge uses a distinct credential. The host applies the verification policy attached to that credential and echoes it in the welcome. The bridge rejects hosts that omit guest verification. The default full policy independently recomputes **every** returned fixture component on the host and publishes its own double-precision fields. Any mismatch above tolerance drops the entire batch and quarantines the socket. A guest within tolerance still cannot change a physical bit through a rounding boundary.

`TOLGLOW_AUDIENCE_VERIFICATION=redundant` permits pairs of independently admitted participants to evaluate the same slice. The host checks every corresponding result between the two and independently verifies up to four randomly selected rows in full after both replies arrive. A lone participant receives full local verification. Disagreement, a lost worker, or a missed deadline falls back to local rendering. Two colluding devices can agree on incorrect unsampled rows. Choose full verification when physical output must have an independent reference for every channel. The full policy can measure real guest throughput but adds host work.

## Build and reproduce

The iOS app bundles the WebGL2 dispatcher and Transit codec. It runs the local, immutable code in an ephemeral WKWebView. The show supplies GLSL field programs and numeric job inputs. The app does not load JavaScript or a Studio admin UI from a participant or stage.

From the paired Tolglow checkout containing the distributed renderer, rebuild with:

```sh
npm ci
npm ci --prefix native/AudienceCompute/GPU --ignore-scripts
node native/AudienceCompute/GPU/build.mjs
node native/AudienceCompute/GPU/proof.cjs /path/to/chromium
```

For split development checkouts, set `TOLGLOW_COMPUTE_SOURCE` to the checkout with the production dispatcher and its generated fixture. The bundled guest compiled the real paired shader and matched 24 CPU fixture values during the Linux Chromium/SwiftShader test. Native Xcode target compilation and a physical wireless trial are separate checks. WKWebView context creation can fail when the platform refuses GPU work. The app then declines enrollment.

For physical qualification, launch `TolglowAudienceBridge` with the stage name, HTTPS invitation origin and QR image path. Enroll one full-app device using the QR. Record the bridge's three-sample throughput receipt, foreground/thermal state and host pool completions under a 40 Hz show. Add another independently paired phone to exercise redundancy. Background one phone while commands continue and compare the emitted universes with a CPU copy of the show. The separate operator WKWebView QA mode in the distributed benchmark uses a Studio login and covers a different enrollment path.
