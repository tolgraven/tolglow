# Instrument stems

At a concert the engineer can send kick, snare, bass, guitar and vocals on
separate channels. Tolglow names those channels as **stems**, follows each
one's level, detects its onsets, and routes them to cues through the routing
it already has: onsets launch cues through
[event subscribers](scheduler.md), and levels drive cue parameters through the
[signal patch](routing-host.md). There is no separate stem routing store.
Issue [#675](https://github.com/tolgraven/tolglow/issues/675).

## Stems in the capture configuration

A stem is a named, one-based channel of the ordinary multichannel capture
([#500](https://github.com/tolgraven/tolglow/issues/500)), with an instrument
preset and optional detector settings:

```clojure
{:default? true :sample-rate 48000.0 :channels 8 :input-channel 1
 :stems [{:id :kick :channel 1 :preset :kick}
         {:id :snare :channel 2 :preset :snare}
         {:id :bass :channel 3 :preset :bass}
         {:id :guitar :channel 4 :preset :guitar :rise-db 4.0}]}
```

`:input-channel` still selects the channel for the ordinary analyzer (kick,
energy, bands and sections). Up to 16 stems; two stems may read the same
channel. The configuration is saved with the show, like the rest of the audio
connection, and capture still starts only when asked.

A `:file` input replays a multichannel 16-bit WAV through the same worker,
paced in real time, and opens no device. `capture/file-config` reads the
file's rate and channel count:

```clojure
(tolglow.audio.capture/file-config "set-stems.wav" {:stems [...]})
```

`:file-paced? false` reads as fast as possible and places the recording in
the past, for offline analysis.

The file must be an existing regular `.wav` or `.aiff` file; anything else
(a FIFO, a device, another format) is refused before it is opened. Only the
REPL starts a file replay: Studio's `/api/audio/start` refuses `:file`, since
Studio has no recording picker and the path would name any file on the host.

## What each stem measures

`tolglow.audio.stems` is pure: state and blocks in, features and events out.

| Step | What it does |
| --- | --- |
| Band | The stem's band through the shared Linkwitz–Riley filters of `tolglow.audio.shape` ([#676](https://github.com/tolgraven/tolglow/issues/676)). A named band (`:bass`, `:high`, …) or `[low high]` Hz. |
| Envelope | Peak follower with the stem's attack and release. `:level` is the envelope over the floor-to-full-scale decibel range, 0–1. |
| Onset | In 4 ms hops: the envelope rose `:rise-db` above the quietest of the last 20 ms, sits above `:floor-db` and within `:range-db` of the stem's slowly decaying peak, the detector has re-armed, and the gate has passed. |
| Gate | `:min-interval` in beats, converted at the captured tempo clamped to **70–180 BPM** (120 without a clock). The default sixth of a beat is 143 ms at 70 BPM and 56 ms at 180. A gate shorter than 20 ms at 180 BPM (`{:ms 19}`, `{:beats 1/32}`) is refused, so no stem emits more than 50 onsets a second. |
| Bleed | All stems share hops. A candidate is held 8 ms, then rejected as bleed if another stem had an onset within 12 ms whose peak was `:isolation-db` (18 dB) louder. Rejections are counted per stem. |

Presets set band, envelope and gate per instrument (`:kick :snare :hats :bass
:guitar :vocals :keys :full`); any value can be overridden per stem.

Every `:audio/onset` event carries its stem and channel, the hop start time,
flux, gate, BPM, the transport beat when a clock is present, and
`:transport-playing?`. Its emitter is `:stem/<id>`. Features carry, per stem,
`:level`, `:envelope`, `:level-db`, `:peak`, `:onset-count`, `:bleed-count`,
`:gate-ms`, `:bpm`, `:last-onset-ms` and `:last-onset-beat`.

## Routing

| From | Through | To |
| --- | --- | --- |
| `:audio/onset` with emitter `:stem/kick` | event subscriber `{:match {:type :audio/onset :emitter-id :stem/kick}}` | cue launch, effect launch, or any subscriber action, optionally on a `:grid` |
| port `stem/<id>/level` (0–1, open above the floor) | signal patch route | numeric cue parameter |
| port `stem/<id>/onset` (open for the block whose onset count advanced) | signal patch route, typically with an ADSR envelope | numeric cue parameter as a one-shot |

The ports appear in the patch bay's source list as soon as stems are captured.
`tolglow.audio.stem-routes` builds these subscribers and routes and reads them
back per stem, for the REPL and Studio alike.

## Transport stop

Stems drive show progress, so they pause with the transport
([clock domains](clock-domains.md)). While stopped, stem onsets launch nothing
(they are dropped and counted in `[:variables :audio :suppressed-onsets]`),
level routes are not fed and hold their value, and the onset port takes the new
count with its gate closed. Play resumes from fresh audio: no stopped onset is
replayed and no backlog launches. Capture and Studio's meters keep running.

## From the REPL

```clojure
(require '[tolglow.stems :as stems])
(stems/start! runtime {:default? true :channels 8 :input-channel 1}
              [{:id :kick :channel 1 :preset :kick} {:id :guitar :channel 4 :preset :guitar}])
;; or (stems/replay-file! runtime "set-stems.wav" [...])
@(:receipt (stems/route-onset! runtime :kick :flash))
@(:enabled (stems/route-level! runtime :guitar :breathe :level))
(stems/view runtime)   ; each stem with level, counts, gate and routes
(stems/status runtime) ; raw per-stem features
```

## In Studio

**Connections → Audio input → Instrument stems** names stems per channel with
an instrument preset; they are sent with **Start capture**. While capture runs
each stem has a card with its level meter, an onset lamp, onset and
bleed-rejected counts, its gate at the current tempo, its last onset beat, and
what it drives. **Launch on onset** and **Drive with level** save the same
subscriber and patch route as the REPL calls above; the card lists the new
route once the show has it, and × removes an onset route.

## Verification

- `tolglow.audio.stems-test`: synthesized kick, snare, bass and the #676
  guitar phrase with −24 dB neighbour bleed at 70, 120 and 180 BPM. Kick,
  snare and bass: every onset found, no false positives, timing within 8 ms.
  Guitar plucks over a ringing string: all found at 70 and 120 BPM, seven of
  eight at 180, none doubled, within 15 ms. A dropout removes that stem's
  onsets without false positives; trailing silence releases every level to
  zero. The tempo gate drops a retrigger 100 ms after a hit at 70 BPM and
  keeps it at 180. A WAV replays through capture's `:file` input.
- `tolglow.studio.stem-routing-host-test`: the four stems through the real
  show owner. Kick onsets launch a cue within an eighth of a beat after each
  kick; the guitar level drives a cue parameter and releases in the silent
  bar; the saved show keeps the stems and routes. A two-second stop drops the
  ten onsets inside it, holds the routed level exactly, and play continues on
  the next beats without catch-up.
- `dev/studio/stems-browser-qa.mjs` with `tolglow.qa.stems-studio`: the
  production Studio build in headless Chrome against a paced WAV replay.

Not verified: a physical multichannel interface, input-to-command latency on
real hardware, real concert stems, and Push or other controllers alongside.
