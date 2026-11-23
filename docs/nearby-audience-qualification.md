# Measuring audience participation at a venue

The [operating guide](nearby-audience.md) covers enrollment and mapping. This run measures what changes between a single dancer and a crowded room. Record the app revision, gateway model and OS, participating device models, zone layout and whether each run uses the Clip, full phone app or Watch. Record aggregate counts and timing. A camera recording or an individual pulse history is unnecessary for these measurements.

## From a QR scan to a lighting cue

Use a signed stage domain with its Apple App Site Association document and a published App Clip experience. Start with a phone that has never installed the parent app. Leave Wi-Fi unjoined and provide cellular internet for the first download. Scan the generated venue QR, admit Bluetooth, tap a step and observe an accent at the show. After installation, repeat on an isolated stage with Wi-Fi and cellular data disconnected and Bluetooth enabled. Repeat with the parent installed so the same invitation opens Dance floor there.

Place two stages with distinct keys within range. Try both stages from the same QR. Only the invited key should be accepted. Scan a second invitation while joined, then scan it twice. Record which stage actually receives the next tap. Stop sharing and verify the count returns to zero. With a running stage session, measure time from the last accepted packet until the host drops that contribution. The gateway participant expiry and the show’s independent gateway timeout cover different failures.

## Measure motion and step timing

Start with one guest and disable musical cooldown on a rehearsal accent cue. Record the expected and received counts for a known sequence of taps. Repeat for in-place dancing, a pause, people crossing the camera view, phone motion, paired Watch and a Watch-only stage connection when that build is available. Compare camera- and wrist-detected onsets against the same independent count. Report missed and extra steps separately. Restore the intended musical cooldown for performance.

For end-to-end latency, film a visible tap or step and the responding fixture in the same high-speed capture. Convert the frame difference to milliseconds using the **actual capture frame rate**. Record sample count, median, 95th percentile, maximum and camera frame duration. This avoids subtracting clocks on different devices. Test both the fresh cue onset and a low-amplitude sustained effort change. A level sent five times a second and a step event can have different latency.

## Grow and shrink a crowd

Start with one guest, grow to the available crowd and return to one. Log the actual number of participants at each plateau. Vary the distance, number of people between guest and gateway, and number of gateways individually. Drive a known cue from one zone and another cue from the second. Arm, exclude and unplug each zone in turn. When the only pulse-sharing guest withdraws, check that the pulse reading disappears while effort from the others continues.

Keep one person active while the others rest. With sixteen participants and one dancer reporting full effort, the expected global mean is approximately 1/16, regardless of which zone the dancer occupies. Record the queue high-water mark, rejected observations, missing cues and time to recover after a gateway is restarted. Distinguish detected sensor steps from successfully delivered cue commands. A counted tap sequence helps separate detector and transport errors.

Use another pass with guest computation explicitly enabled on one full-app device, then several. Record remote jobs accepted before the deadline, local fallback counts, incomplete frames, physical-output differences from the reference show, battery change and thermal state over the same duration. Turn a worker off and verify the participant can continue sending motion. Keep the Clip in sensor-only mode throughout.

## Reference captures

| Path exercised | Observation |
| --- | --- |
| Signed-free iPhone simulator → audience Clip | The reviewed build opened the Bluetooth stage discovery UI. [Recording](https://raw.githubusercontent.com/tolgraven/tolglow/972f74c53a0fcaef0339b00815a581dabd5e706c/audience/apple/discovery-6fffec7.mp4) |
| Enrolled OSC → show → DMX | Two zones launched separate one-shot cues. DMX returned to the starting values after their releases. [Show and universe trace](https://github.com/tolgraven/tolglow/blob/20ebb301f1200e23b60b05b3c349be1dbb9d3ade/audience-zones/show-cues.json) |
| Four-zone receiver over scripted show time | 96 simulated participants generated 34,560 mapped cue commands over three minutes with zero admission rejections. A separate wall-clock UDP run delivered 4,996 expected commands. [Source and results](https://github.com/tolgraven/tolglow/tree/5d479608182a4cf0f2436920b435a5c30e7fa7b5/audience-zones) |
| Guest camera landmarks → step detector | One kick produced one step through image translation, reordered subjects and an ambiguous crossing in the synthetic landmark test. [Animated trace](https://raw.githubusercontent.com/tolgraven/tolglow/77994cde68c71c303776a4ea94151ffe2ea1c148/audience/camera/tracking.gif) |

Public App Clip installation, venue Bluetooth capacity, watch workout sampling, physical cue latency and Apple GPU capacity per audience member still need device measurements.
