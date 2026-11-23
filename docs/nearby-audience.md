# Dance floor participation

The full iPhone app and the lightweight Dance floor App Clip have a common **Find nearby stages** action. The Mac bridge advertises a Bluetooth LE service, so the venue and phone can have different Wi-Fi connections or no Wi-Fi association. Installing the app or fetching a Clip for the first time still requires access to Apple’s distribution service. A guest chooses the stage and input. Leaving or losing the stage ends capture. Backgrounding the phone stops its camera and motion capture. In the full app a guest may keep an already joined Watch sharing while the phone locks. Incoming Watch samples drive BLE writes in that mode. The Clip uses foreground participation.

| Input | Available in | What the stage receives |
| --- | --- | --- |
| Tap | App Clip and iPhone | Tap onset and a decaying intensity |
| Camera | App Clip and iPhone | Effort and estimated steps from visible leg movement. Prop up the phone facing one dancer. Vision processes joint positions locally. |
| Phone motion | Full iPhone app | Acceleration peaks and smoothed intensity |
| Wrist motion | Watch with its paired iPhone joined | Wrist effort and steps, counted once for the person |
| Exercise pulse | Optional Watch dance workout | A fresh heart rate when the wearer expressly shares it. The stage exposes the mean of available readings and a presence flag. |

Core Motion and HealthKit are unavailable to App Clips at runtime. The Clip uses foreground Bluetooth, Vision and AVFoundation. Camera movement is an estimate and requires visible joints. Phone and Watch accelerometers detect movement peaks. Distinguishing individual styles of dance needs recorded validation with people. [Apple’s App Clip capabilities](https://developer.apple.com/documentation/appclip/choosing-the-right-functionality-for-your-app-clip)

## Host a stage

1. Generate the Xcode project in `native` with `xcodegen generate`. Build the **TolglowAudienceBridge** scheme for macOS. The bridge runs on a Bluetooth-equipped Mac. Its local OSC output can also reach a show through an enrolled local receiver.
2. Run the built tool with a stage name, the public HTTPS origin for your signed Clip, and an output filename. For example, `TolglowAudienceBridge "East stage" https://tolgraven.se east-stage.png`. Allow Bluetooth access. Without OSC settings, the bridge reports participant counts locally.
3. For show control, enroll a single sensor source named `audience` (or set `TOLGLOW_AUDIENCE_OSC_ID`). Set `TOLGLOW_AUDIENCE_OSC_PORT` to the local receive port and `TOLGLOW_AUDIENCE_OSC_TOKEN` to that source’s private credential before starting the bridge. Apply the normal sensor configuration and arm the fresh source session. [Sensor enrollment and mappings](sensors.md)
4. Display the generated QR. Full-app visitors can also browse advertised names manually. Scanning pins a particular stage’s public key. Restarting the bridge creates a fresh stage identity and QR.
5. Guests open **Dance floor**, select the stage, then choose their input. For Watch participation, open the paired iPhone’s Dance floor and join first. On Watch select Dance floor and choose whether to share workout pulse.

The registered observation has numeric channels `audience-count`, `dance-effort` (0–1), `dance-steps` (newly delivered), `pulse-present` and, while available, `pulse-bpm`. Map the `dance-step` gesture to a cue for a separate onset per estimated step. The bridge batches up to eight gestures into one OSC observation. Levels update five times a second. Coalesced phone writes carry a cumulative step counter. Host receipt times determine expiry and event timing. The bridge keeps current readings in memory. A saved show can contain the most recent configured aggregate channel, including its BPM when one wearer is participating.

Phone and Watch share a participant identity for the joined session. Watch steps take precedence while available. A soloist has full weight. Each new guest contributes one equally weighted effort reading. Guests time out after 2.5 seconds, and the bridge sends a final count of zero. If the OSC delivery queue fills or sending fails, the bridge reports the error and stops the stage. The in-memory bound of 128 participants is a resource limit. Physical radio capacity must be measured separately. A nearby Mac can act as a gateway for a Linux lighting host. The bridge runs on macOS.

To use each admitted step to launch a registered accent cue, add the following entry to the audience sensor’s mapping. The musical cooldown lets a large crowd play one accent per sixteenth note in 4/4. Remove the cooldown to process every detected guest onset.

```clojure
{:version 1
 :channels [{:id :floor :input :dance-effort :smoothing-ms 100}]
 :gestures [{:id :floor-accent :type :dance-step :cooldown {:beats 1/4}
             :command {:op :cue/launch :cue-id :accent
                       :launch {:mode :one-shot :duration {:beats 1/8}}}}]}
```

## Cover several areas of a venue

Give each Bluetooth gateway its own enrolled sensor ID, credential and zone QR. Set `TOLGLOW_AUDIENCE_OSC_HOST` on a remote gateway to the lighting host on your encrypted venue network or VPN. The default is loopback. Guests only need Bluetooth. The gateway-to-show OSC link carries its source credential, so keep it on the private link.

Configure each source with `tolglow.sensors.audience/gateway-mapping` and arm its observed session. In the initial show model, select the zones with `(assoc-in model [:variables :audience :gateways] ["front" "balcony"])`. The renderer and Studio subscriptions can read `[:variables :audience :levels]`. The dance-effort mean weights each gateway by its participating dancers. The pulse mean weights only the wearers with fresh, shared readings. Leaving or disarming a zone removes its contribution. Configure step gestures on the individual gateway sensor mappings to retain distinct events. Capacity and handoff across physical radios need a venue test.

## Publish a scannable Clip

Set `AUDIENCE_DOMAIN` for the signed build. Add the domain to your developer team, enable associated domains for Clip and parent, and serve an Apple App Site Association document via HTTPS at `/.well-known/apple-app-site-association`. Include the signed Clip in `appclips.apps` and the signed parent in `applinks.details` for `/audience/*`. Configure the corresponding App Clip experience and invocation URL prefix in App Store Connect before distributing the printed QR. Verify the size of the archived Clip against Apple’s physical invocation limit and test a fresh scan on a device with no installed parent. The default domain in project settings is a build configuration, not a published experience. [Apple’s invocation guide](https://developer.apple.com/documentation/appclip/configuring-the-launch-experience-of-your-app-clip)

The QR holds a public fingerprint. A per-connection challenge and ephemeral phone ECDH key derive an AES-GCM sample key. The bridge forwards aggregate values and gesture identities to the enrolled lighting input. It exposes no operator commands or rendering credentials to guests. Nearby render work is a separate full-app opt-in with a higher-bandwidth transport. The Clip only participates in sensing.

## Qualification

Build and exercise both iOS targets, Watch and bridge with `native/Tests/check_macos.sh`. Swift tests cover individual readings, multiple guests, expiration, duplicate/out-of-order packets, wraparound, pulse removal and step counting. The physical acceptance run needs an iPhone, paired Watch and a Bluetooth Mac on separate networks. Test a signed QR launch, exercise and permission denial, stage restart, actual radio capacity at increasing crowd sizes and thermal behavior. Simulator build or replay results do not establish these properties.
