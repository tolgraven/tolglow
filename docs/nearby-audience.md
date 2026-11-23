# Dance floor participation

The packaged Mac app lets an operator start a stage, save its QR image, and close the invitation under **Connections → Stage invitation**. The QR is specific to that running stage. [Operator setup and Apple deployment](apple-audience-deployment.md)

The full iPhone app and the lightweight Dance floor App Clip have a common **Find nearby stages** action. The Mac bridge advertises a Bluetooth LE service, so the venue and phone can have different Wi-Fi connections or no Wi-Fi association. Installing the app or fetching a Clip for the first time still requires access to Apple’s distribution service. A guest chooses the stage and input. Leaving or losing the stage ends capture. Backgrounding the phone stops its camera and motion capture. In the full app a guest may keep an already joined Watch sharing while the phone locks. Incoming Watch samples drive BLE writes in that mode. The Clip uses foreground participation.

| Input | Available in | What the stage receives |
| --- | --- | --- |
| Tap | App Clip and iPhone | Tap onset and a decaying intensity |
| Camera | App Clip and iPhone | Effort and estimated steps from visible leg movement. Prop up the phone facing the dancer. Vision measures local knee and ankle movement relative to the hips. |
| Phone motion | Full iPhone app | Acceleration peaks and smoothed intensity |
| Wrist motion | Watch relayed by a joined iPhone or enrolled directly at a nearby stage | Wrist effort and steps, counted once for the person |
| Exercise pulse | Optional Watch dance workout | A fresh heart rate when the wearer expressly shares it. The stage exposes the mean of available readings and a presence flag. |

Core Motion and HealthKit are unavailable to App Clips at runtime. The Clip uses foreground Bluetooth, Vision and AVFoundation. Camera mode follows the person initially closest to the center. Both hips and at least two lower-body joints must be visible. When two bodies cross too closely or joints disappear, step detection waits for a fresh baseline. The meter measures leg movement relative to the hips so sliding the whole image sideways does not produce a step. Phone and Watch accelerometers detect movement peaks. Distinguishing individual styles of dance needs recorded validation with people. [Apple’s App Clip capabilities](https://developer.apple.com/documentation/appclip/choosing-the-right-functionality-for-your-app-clip)

## Host a stage

1. Generate the Xcode project in `native` with `xcodegen generate`. Build the **TolglowAudienceBridge** scheme for macOS. The bridge runs on a Bluetooth-equipped Mac. Its local OSC output can also reach a show through an enrolled local receiver.
2. Run the built tool with a stage name, the public HTTPS origin for your signed Clip, and an output filename. For example, `TolglowAudienceBridge "East stage" https://tolgraven.se east-stage.png`. Allow Bluetooth access. Without OSC settings, the bridge reports participant counts locally.
3. For show control, enroll a single sensor source named `audience` (or set `TOLGLOW_AUDIENCE_OSC_ID`). Set `TOLGLOW_AUDIENCE_OSC_PORT` to the local receive port and `TOLGLOW_AUDIENCE_OSC_TOKEN` to that source’s private credential before starting the bridge. Apply the normal sensor configuration and arm the fresh source session. [Sensor enrollment and mappings](sensors.md)
4. Display the generated QR. Full-app visitors can also browse advertised names manually. Scanning pins a particular stage’s public key. Restarting the bridge creates a fresh stage identity and QR.
5. Guests open **Dance floor**, select the stage, then choose their input. For paired Watch participation, join the floor on iPhone and use **Paired iPhone** on Watch. To participate with a Watch alone, leave the floor on your phone, choose **Nearby stage**, tap **Find stages**, and select the advertised stage. Selecting a stage stops browsing. The Watch opens the live session when you choose **Join with motion**, or after you enable **Share heart rate** and grant workout access. You can take your time with either prompt. A direct Watch appearance is a manual nearby enrollment. The signed QR identity check remains available on iPhone.

The registered observation has numeric channels `audience-count`, `dance-effort` (0–1), `dance-steps` (newly delivered), `pulse-present` and, while available, `pulse-bpm`. Map the `dance-step` gesture to a cue for a separate onset per estimated step. The bridge batches up to eight gestures into one OSC observation. Levels update five times a second. Coalesced phone writes carry a cumulative step counter. Host receipt times determine expiry and event timing. The bridge keeps current readings in memory. A saved show can contain the most recent configured aggregate channel, including its BPM when one wearer is participating.

In paired mode phone and Watch share a participant identity. Losing the phone connection ends wrist sharing until the guest explicitly rejoins. A direct Watch sends to the stage even when its paired phone is absent. If the paired phone reports it has joined, the Watch leaves its direct session and offers the paired route. Closing a solo session stops sampling, disconnects Bluetooth and discards the pending reading. Watch steps take precedence while available. A soloist has full weight. Each new guest contributes one equally weighted effort reading. Guests time out after 2.5 seconds, and the bridge sends a final count of zero. If the OSC delivery queue fills or sending fails, the bridge reports the error and stops the stage. The in-memory bound of 128 participants is a resource limit. Physical radio capacity must be measured separately. A nearby Mac can act as a gateway for a Linux lighting host. The bridge runs on macOS.

To use each admitted step to launch a registered accent cue, add the following entry to the audience sensor’s mapping. The musical cooldown lets a large crowd play one accent per sixteenth note in 4/4. Remove the cooldown to process every detected guest onset.

```clojure
{:version 1
 :channels [{:id :floor :input :dance-effort :smoothing-ms 100}]
 :gestures [{:id :floor-accent :type :dance-step :cooldown {:beats 1/4}
             :command {:op :cue/launch :cue-id :accent
                       :launch {:mode :one-shot :duration {:beats 1/8}}}}]}
```

The solo Watch uses watchOS Core Bluetooth as a central and supports running independently of the companion app. [Apple Core Bluetooth](https://developer.apple.com/documentation/corebluetooth/cbcentralmanager) · [Independent Watch app setting](https://developer.apple.com/documentation/bundleresources/information-property-list/wkrunsindependentlyofcompanionapp)

### Motion channels

In the App Clip, **Use iPhone or Watch motion in Tolglow** opens the system offer for its parent app. After installation, scan the same stage QR to continue.

Choose **Phone motion → Share phone acceleration and rotation** on iPhone, or **Share wrist axes** in the Watch dance view. The guest can turn sharing off during a song. Every 200 ms the bridge sends the mean signed, gravity-removed acceleration in m/s² (`accel-x`, `accel-y`, `accel-z`) and angular velocity in rad/s (`rotation-x`, `rotation-y`, `rotation-z`) from the guests currently supplying all six channels. The watch takes precedence when both devices send fresh axes. `motion-count` gives the number of contributing dancers across the selected zones. With one participant the means equal that device’s readings. With differently oriented devices, signed values can cancel. `dance-effort` gives an orientation-independent intensity for the crowd.

Map the resulting channels to effect parameters through normal sensor mappings. Each source uses device-local axes. The mixer removes the six channels when the final contributor withdraws or expires. Tap and camera participation still send the estimated effort and steps.

## Cover several areas of a venue

Give each Bluetooth gateway its own enrolled sensor ID, credential and zone QR. Set `TOLGLOW_AUDIENCE_OSC_HOST` on a remote gateway to the lighting host on your encrypted venue network or VPN. The default is loopback. Guests only need Bluetooth. The gateway-to-show OSC link carries its source credential, so keep it on the private link.

Configure each source with `tolglow.sensors.audience/gateway-mapping` and arm its observed session. In the initial show model, select the zones with `(assoc-in model [:variables :audience :gateways] ["front" "balcony"])`. The renderer and Studio subscriptions can read `[:variables :audience :levels]`. The dance-effort mean weights each gateway by its participating dancers. The pulse mean weights only the wearers with fresh, shared readings. Leaving or disarming a zone removes its contribution. Configure step gestures on the individual gateway sensor mappings to retain distinct events. Capacity and handoff across physical radios need a venue test.

A software rehearsal sends the same dance step from 96 people on each beat at 120 BPM. Four zones group steps into packets of at most eight gestures. Over three minutes of show time the host decoded 6,484 OSC observations and emitted 34,560 mapped cue launches. Its queue finished empty, with zero admission rejections or overflows. This measures the host using encoded packets and a deterministic clock. Radio capacity, UDP delivery and physical timing require separate measurements.

Run the rehearsal with the paired Afterglow checkout selected as for the test suite.

```bash
lein with-profile +test run -m clojure.main -e \
  "(require '[tolglow.sensors.audience-test :as a]) (prn (a/crowd-rehearsal 900))"
```

The solo Watch uses watchOS Core Bluetooth as a central and supports running independently of the companion app. [Apple Core Bluetooth](https://developer.apple.com/documentation/corebluetooth/cbcentralmanager) · [Independent Watch app setting](https://developer.apple.com/documentation/bundleresources/information-property-list/wkrunsindependentlyofcompanionapp)

## Dance-step timing

Tap, camera, phone and wrist motion send their cumulative step count as soon as the input detects an onset. Periodic readings recover a missed counter change. The paired iPhone uses the wrist counter while it is fresh and carries one vote for the person. The Watch relay keeps one acknowledged message in flight and one replaceable sample with its original measurement time.

The bridge takes a fresh effort and pulse snapshot five times per second. It submits onsets on the next 40 Hz show frame. Each OSC observation carries up to eight gestures and one frame admits up to four observations, allowing additional simultaneous steps to spill into the following frames. The bounded backlog reports overflow to the operator. A heartbeat can carry a due onset, and subsequent heartbeats carry zero until another step arrives.

Run `swift test --package-path native/Core --filter AudienceLatencyTests` to exercise detection, bounded host aggregation and the OSC encoder using a deterministic clock. Set `TOLGLOW_AUDIENCE_TIMING_DIR` to an existing directory to save the observed packet times. CI publishes them as `audience-onset-timing`. These measurements end at the encoded datagram. Qualifying physical latency also requires timing the guest’s sensor, WatchConnectivity, BLE, OSC delivery and the receiving show.

## Publish a scannable Clip

For pre-alpha testing, run the Clip in the iPhone simulator. On a development-signed iPhone, a registered Local Experience can open the **stage QR** directly from an HTTPS URL on a test host. Publishing a custom domain and an App Store experience belongs to the later public distribution test. [Simulator, local iPhone and public invitation instructions](nearby-audience-qualification.md#test-the-clip-before-distribution)

The QR holds a public fingerprint. A per-connection challenge and ephemeral phone ECDH key derive an AES-GCM sample key. The bridge forwards aggregate values and gesture identities to the enrolled lighting input. It exposes no operator commands or rendering credentials to guests. Nearby render work has a separate full-app consent control and uses a measured peer connection for its GPU buffers. The Clip only participates in sensing. [Configure and qualify guest GPUs](../native/AudienceCompute/README.md)

## Qualification

Build and exercise both iOS targets, Watch and bridge with `native/Tests/check_macos.sh`. Swift tests cover individual readings, multiple guests, expiration, duplicate/out-of-order packets, wraparound, pulse removal and step counting. A deterministic model test exercises 96 guests at five updates per second for three minutes with missed readings, duplicates, a guest rejoining, pulse opt-out and final expiry. It checks every tick’s individual step delta, participant count and mean effort. The Watch UI test exercises route selection on the actual simulator and captures the interaction. The physical acceptance run needs an iPhone, paired Watch and a Bluetooth Mac on separate networks. Include a solo Watch with its phone switched off, phone return, stage disconnection and the Bluetooth permission prompt. Test a signed QR launch, exercise and permission denial, stage restart, actual radio capacity at increasing crowd sizes and thermal behavior. Simulator build or replay results do not establish these properties.

## Operate audience zones in Studio

Open **Connections → Audience**. The list contains enrolled gateway sources whose mappings supply both `audience-count` and `dance-effort`. A source starts at **Waiting for gateway**. When its first observation reaches the show the status changes to **Joined**. **Arm** admits new observations from that gateway session. Use its checkbox to include the zone in the show’s audience mix. A gateway may continue collecting readings while excluded from this mix.

**Dancers** counts the participants in the selected, armed zones. **Dance effort** weights each zone by its dancers. **Pulse sharing** shows how many of those dancers share a fresh pulse measurement. The BPM signal stays in the show model for mappings. On signal expiry the zone drops out of the readings and requires a fresh observation and explicit arming. The operator’s selection survives show save/load. Studio receives this view through the Connections Transit stream. It contains the logical zones and aggregate measurements.

A REPL controller can use the same incremental command as the operator. Submit `:sensor-audience-zone` with `{:device-id "balcony" :included? true}` to the show. Other zones keep their selection. To reproduce the operator flow locally, build the Studio release then run `lein run -m tolglow.qa.audience-zones target/audience-zones`.
