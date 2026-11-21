# Push preset and global control feedback

The passive Reagent/re-frame Push display consumes the shared Afterglow device
projection. It now shows cue preset state and a compact global control panel
while the master or tempo encoder is touched. These are presentation components;
the shared device command handlers remain the authority for editing values and
saving or clearing presets.

## Projection contract

`normalize-view` retains the following bounded metadata:

```clojure
{:instance-id "running-effect-id"
 :preset {:saved? true :changed? false :record? true :action :clear}
 :touched-control {:target :tempo :editable? false}
 :master 0.73
 :bpm 128.5}
```

The preset indicator is **Edited** when `:changed?` is true, otherwise **Saved**
when `:saved?` is true, otherwise **Default**. While Record is held, an active cue
instance also shows the action supplied by the shared projection: **Save preset ·
top button** or **Clear preset · top button**. This is an available action hint,
not an optimistic assertion that a save or clear has completed. Subsequent
snapshots provide the resulting state. A cue without a running instance cannot
show an action that the backend would reject.

Touching a global encoder displays its actual value, a shared rotary, range
endpoints and a level track. Master is normalized `0..1` in the protocol and shown
as `0..100 %`. Tempo uses the device's `20..200 BPM` editing range. External sync
with `:editable? false` shows **External sync · locked**. A synchronized tempo
outside that editing range still shows its true BPM; only the range marker is
bounded. Releasing the touch restores the ordinary header. Replacement snapshots
clear obsolete preset and touch feedback.

The control panel occupies the header and temporarily replaces its category,
envelope and ordinary master/tempo summaries. It preserves all eight encoder
columns, selected effect, status, supplied waveform samples and the audio display.
The screen remains exactly `960×160`. The passive route issues no commands, starts
no timers or API polling, and opens no audio, graphics, editor or device resources.

## Verification, 2026-11-21

- Frontend suite: **139 tests / 738 assertions**, zero failures or errors. The
  generated Node test runner was also executed directly and exited successfully.
- Optimized Studio release: **147 files**, zero warnings.
- Browser gate: **13 checks** passed using eleven projections produced by the
  actual `afterglow.control.model/apply-command` and
  `afterglow.control.devices.push-2/display-view` functions. They cover authored,
  edited and saved presets; Record action hints; master/tempo touch and release;
  external sync locking; an actual `240 BPM` reading; and an empty show.
- The optimized Chromium display passed exact geometry, header bounds, eight
  parameter slots, supplied waveform preservation and snapshot replacement checks.
  Audio layout used an explicit 128-bin presentation fixture. Requests were
  limited to the passive page and static assets; no API, graphics or editor
  resources were requested.

Session evidence is under `/tmp/studio-qa/`: `push-global-results.json`,
`push-global-projections.clj`, the browser harness and build/test logs. Fresh
screenshots are `push-global-master.png`, `push-global-master-clear.png`,
`push-global-tempo-locked.png` and `push-global-audio.png`. These temporary browser
artifacts accompany the checked-in normalization and state-transition tests.
This gate verifies browser rendering and shared protocol projections; it does
not claim physical Push USB, external clock hardware or audio-device validation.
