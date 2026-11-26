# Showcase show

The showcase show is a complete venue in one namespace. It has a rig, a page of
20 cues, two macros and a sequence restated from the operator's saved compound
cues, and a 32-bar timeline at 128 BPM that plays on the
[beat-clock scheduler](scheduler.md). The timeline replays the same way every
time from a seed. It was built for
[issue #656](https://github.com/tolgraven/tolglow/issues/656).

| Source | Contents |
| --- | --- |
| `src/tolglow/showcase/rig.clj` | Room, fixture types, patch, pixel layouts and ID groups |
| `src/tolglow/showcase/looks.clj` | The effects, the `:heartbeat` waveform, attached sources per cue and the grid catalogue |
| `src/tolglow/showcase/timeline.clj` | Pure timeline functions: validate, route, post and advance |
| `src/tolglow/showcase.clj` | `model`, macros, subscriber, `timeline`, `play!`, `status`, `stop!`, `replay` and `config` |
| `dev/tolglow/qa/showcase_capture.clj` | Writes the frames the Room preview receives during a replay |
| `dev/preview/showcase-capture.mjs` | Renders those frames with the released Room bundle in headless Chrome |

## Run it

```
lein run --config tolglow.showcase/config
lein run --config tolglow.showcase/config --port 4201
```

The config is the shipped `resources/tolglow.edn` with the showcase model and a
`:showcase-timeline` post module. It starts a 40 Hz preview show and serves
Studio on 127.0.0.1 (port 8500 unless `--port` is given). No physical output is
selected. The timeline starts on the next bar line and loops. Open Studio, choose
**Perform** and the **Showcase** page. The cue panel's **Scheduled** list shows
the timeline's upcoming events and running ramps.

Start options are the ordinary ones in [Starting a show](application-startup.md).
To load the show without autoplay, set `[:showcase-timeline :autoplay]` to
`false` in a copy of the config.

## Play it from the REPL

```clojure
(require '[tolglow.showcase :as showcase] '[tolglow.show :as show])

(def runtime (show/start! {:model (showcase/model)}))
(def player (showcase/play! runtime {:loop? true}))
(showcase/status player)
;; => {:timeline :first-light :pass 0 :bar … :rejected [] :missed [] :outcomes #{} …}
(showcase/stop! runtime player)
(show/stop! runtime)
```

`play!` posts the timeline's scheduler events at once and attaches a frame
observer for the rest. It starts on the next bar line at least two beats ahead.
`:origin` sets an explicit transport beat for bar 0. `status` reports rejected
operations, binds that never found their cue in `:missed`, and in `:outcomes`
any scheduled event that was rejected, missed or cancelled. `stop!` withdraws
what the timeline still has scheduled, stops its macros and ends the instances
it launched.

With the application running, the runtime is the `:show` module and the
player is under `:player` in the `:showcase-timeline` module value.

## Replay from a seed

```clojure
(def run (showcase/replay {:seed 656}))
(count (:dmx run))   ;; => 2402 frames of three 512-channel universes
(:status run)

(= (:dmx (showcase/replay {:seed 7})) (:dmx (showcase/replay {:seed 7})))
;; => true
```

`replay` builds a private show with a manual clock and a capture sink, plays
the timeline from transport beat 0 and ticks it every 25 ms. Nothing leaves the
process. Options are `:seed` (0 to 9999, default 656), `:from-bar`, `:to-bar`,
`:frame-ms`, `:timeline` and `:snapshot!`, a function called with each kept
frame's snapshot.

The seed feeds every seeded draw: the sparkle and confetti patterns, the
colour drift, the strobe chase order and the subscriber's random picks.
`tolglow.showcase-test` replays all 2402 frames twice from one seed and
compares the captured DMX byte for byte. It also checks that another seed
changes the output and that every scheduled event fired or completed.

## The rig

An 11 x 5 x 8 m room. Y is up and +Z faces the audience.

| IDs | Fixture | Placement |
| --- | --- | --- |
| `:beam-1` to `:beam-6` | 60w Beam MOVING HEAD LIGHT | Back truss, hung |
| `:mover-1` to `:mover-4` | LED MOVING HEAD 7x12W | Floor |
| `:wash-1` to `:wash-4` | 18x12 RGBW, 200+ W par | Two side booms, two back-wall uplights |
| `:blinder-1`, `:blinder-2` | 100Watt W/A LED COB PAR | Back truss, facing the audience |
| `:tube-1-head-1` to `:tube-8-head-10` | 10-pixel RGBW tube | Eight verticals across the back, universe 2 |
| `:ribbon-head-1` to `:ribbon-head-32` | RGBW pixel ribbon | An arch over the stage, universe 3 |
| `:hazer-1` | Fogger | Floor at the back left corner, blowing across the stage |

The fixture types restate the operator's own definitions as portable type maps.
Each carries a `:source` naming the original.
`tolglow.showcase-parity-test` checks that their footprints, calibration and
channel maps equal what `tolglow.config/fixture-data` and
`tolglow.fixtures/rgbw-60-moving-beam` expand to.

Fixture behavior that shaped the show:

- The beam heads hang with `:rotation-deg [0 0 180]`. Their tilt calibration
  covers 0 to 180 degrees, so an unrotated hung head stops at the horizon.
- The beam heads' shutter rests closed at DMX 0. Every effect that lights them
  writes `:shutter`.
- A par has no pan or tilt. It is aimed with `:rotation-deg`.
- The blinders are white and amber only, so their effects write `:white` and
  `:amber`.
- The model starts with 62 m3 of mixed haze saved at `venue-fog/saved-path`,
  so preview playback looks the same from the first frame.

The rig is sized to the Room preview: it draws at most 16 spotlights
(`MAX_LIGHTS` in `js/preview/scene-renderer.js`), and Studio sends it at most
128 emitting targets (`active-limit` in `src/tolglow/show/preview.clj`). The
rig has 16 conventional lights and 112 pixels.

## The cues

All 20 cues are on page `:showcase`. Every effect is stateless numeric IR with
explicit `:targets`. A legacy random draw became a hash of the head position,
the musical step and a `:seed` parameter.

### Ported from the legacy work

| Cue | Category | Legacy source |
| --- | --- | --- |
| `:pinstripes` | colour | `tolglow.fx/pinstripes`, `tolglow.cue/pinstripe` |
| `:bloom-halo` | colour | `tolglow.fx/bloom` with `:halo`, `tolglow.cue/bloom` |
| `:bloom-keyhole` | intensity | `tolglow.fx/bloom` with `:keyhole? true`, the `:bloom-keyhole` cue in `tolglow.page/effects`. A layer modifier: colour below times the incoming mask |
| `:dimmer-sweep` | intensity | `tolglow.fx/sweep` and `dimmer-sweep`, `tolglow.cue/sweep-column` |
| `:strobe` | intensity | `tolglow.fx/color-strobe`, `tolglow.cue/strobe`. Gates emission on the beat clock; the Room does not draw channel strobes |
| `:aim-lfo` | movement | `tolglow.cue/pointing-lfo`, the "all aim lfo" macro in `macros.clj` |
| `:can-can` | movement | `tolglow.fx/can-can`, the `:move-all` cues in `tolglow.page/movement`. Defaults from the "mixed movement ex" macro |
| `:tube-sparkle` | pixel | `tolglow.fx/sparkle`, `tolglow.cue/sparkle` |
| `:confetti` | pixel | `tolglow.fx/confetti`, `tolglow.cue/confetti` |

The legacy sources are in the operator's 2019–2024 checkout and
`macros.clj`, which are reference material and are not on this branch.

### New

| Cue | Category | Feature |
| --- | --- | --- |
| `:haze` | atmosphere | Hazer output; pulse source on `:level` |
| `:wash-tide` | colour | Three-stop gradient; sine on `:level`, triangle on `:shift` |
| `:wash-ember` | colour | Gradient; pulse on `:layer/priority` weaves it above and below `:wash-tide` each bar |
| `:beam-shafts` | colour | Registered `:heartbeat` waveform on `:level` |
| `:blinder-hit` | intensity | HDR `:drive` gain with spill |
| `:mover-orbit` | movement | Triangle on `:radius`, sine on `:height` |
| `:riser-chase` | pixel | Gradient and HDR `:drive`; triangle on `:width` |
| `:aurora` | pixel | Four-stop gradient; saw on `:scroll`, sine on `:level` |
| `:gather-path` | movement | Aim path from `tolglow.aim-paths/definition`: all six beams follow one travelling point |
| `:rolling-volume` | pixel | Spatial ball volume that rolls along the tube wall |
| `:colour-drift` | modulation | Seeded modulator that glides `:beam-shafts :hue` each bar |

The attached sources are in `looks/sources`. A launch that carries `:sources`
binds them to the new instance with ordinary `:modulation-bind` commands, so
Studio shows and edits them like any other attached waveform.

### Compound cues

| ID | Kind | Legacy source |
| --- | --- | --- |
| `:mixed-movement` | macro: pinstripes and can-can | `macros.clj` "mixed movement ex" |
| `:heavy-colour` | macro: sparkle, halo bloom and confetti | `macros.clj` "heavy color load" |
| `:strobe-chase` | ping-pong sequence over beams, movers, washes and tubes | `tolglow.page` "Chase Test 2" |
| `:phrase-accents` | event subscriber on `:clock/phrase` | New: a blinder hit, then three bars later a seeded pick of sweep or bloom |

HDR settings are in `showcase/hdr-settings`: `:cue-gains` read the `:drive`
parameter of `:blinder-hit` and `:riser-chase`, and spill reaches heads within
0.4 m.

## The timeline

`showcase/timeline` is data: `{:id :first-light :bpm 128 :meter … :length {:bar 32} :events […]}`.
Bars are zero-based transport positions, and 32 bars last 60 s.

| Bars | Section |
| --- | --- |
| 0–3 | Haze, ribbon aurora and a low wash |
| 4–7 | Beams on a heartbeat, aim LFO, halo bloom; sparkle ramps up |
| 8–11 | Floor heads orbit, risers climb, the washes weave, beam hue drifts, keyhole |
| 12–15 | The "mixed movement" macro takes the beams; dimmer sweep narrows |
| 16–19 | Breakdown: the gather path, the rolling volume, a strobe that speeds up into half a bar of blackout |
| 20–23 | Drop: blinders at drive 3, the "heavy colour" macro, fast can-can, overdriven risers |
| 24–27 | Drop B: aim LFO, the ember weave rebinds, then the strobe chase |
| 28–31 | Outro down to ribbon and haze |

Each event is `{:at {:bar n :beat b} :op …}` in the scheduler's vocabulary:
`:cue/launch`, `:instance/end`, `:param/set`, `:param/ramp`, `:master/set`,
`:macro/run` and `:macro/stop`. Ramps give `:to`, `:over` and `:curve`.

`timeline/prepare` routes the events:

- **Scheduler.** 65 events become `:schedule/post` batches at absolute beats
  with event IDs `first-light/<pass>/<index>`. The show owner fires them and
  plays the ramps.
- **Direct.** 19 events are submitted from the frame observer one frame
  before their beat: waveform binds, subscriber save and remove, and the launch
  and end of `:rolling-volume`. The scheduler has no action for the first two.
  It refuses to let a non-human source launch a show-control cue such as a
  spatial volume, and its `:launch` does not accept `:targets`. `route` in
  `timeline.clj` makes this choice. When the scheduler gains these actions,
  change `route` and the event conversion in `scheduled-event`.

A bind waits up to 16 frames for an instance of its cue that started at or after
its launch beat. Every ramp ends before its cue does: a ramp still running when
its instance ends is recorded as cancelled.

### Change it

- **A parameter value.** Edit the event's `:params`, or the default in the
  effect in `looks.clj`.
- **An event.** Add or edit an entry in `showcase/timeline`. The helpers
  `launch`, `end` and `ramp` build the common ones. `timeline/validate` rejects
  unknown actions, cues, macros and launch parameters, values outside a
  parameter's schema and ramps without a musical duration or known curve.
- **A cue.** Add an entry to `looks/catalogue` with `:effect`, `:category`,
  `:origin` and, for a port, `:legacy`. Give the effect explicit `:targets`.
  Attach sources in `looks/sources`.
- **A different timeline.** Pass `:timeline` to `play!` or `replay`.

`tolglow.showcase-test` pins counts such as the 65 scheduled events and the
cues running at given bars. Update them with intentional changes.

## Capture the Room

```
lein run -m tolglow.qa.showcase-capture target/showcase-clip 16 26
CHROMIUM_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
  node dev/preview/showcase-capture.mjs target/showcase-clip \
  --video room --stills 17.5,20.05 --size 1920x1080
```

The first command replays the timeline with the capture sink and writes
`rig.json`, `frames.ndjson` and `meta.json`. Each frame holds the bounded
`[:output :values]` rows and atmosphere that Studio sends the Room. The second
loads the released `resources/studio/preview.js` in headless Chrome with the
machine's GPU (`--use-angle=metal` on a Mac). It replays every frame in order
with the show's 25 ms frame period as the fog and film time step and saves
stills and an H.264 video through ffmpeg. `render.json` records the browser and
GPU. Other options: `--drift`, `--work-light` (default 0.04), `--exposure` and
`--software`.

A headless render is not a foreground display capture.

## Findings

Observed while building the show at `5cf910f6` with Afterglow `d4e6f4c2`.

1. **An effect without explicit `:targets` faults on a rig with a head that
   lacks the written attribute.** The compiler assigns every fixture and the
   renderer rejects the whole frame, so a stock colour cue lights nothing once a
   hazer or a white-only par is patched. A `:spatial-cue :spawn` that paints
   `:color` without `:targets` fails the same way.

   ```clojure
   (require '[afterglow.control.model :as control-model] '[tolglow.show :as show])
   (def rig {:room {:width 8 :height 4 :depth 8}
             :fixtures [{:id :rgb :position [0 1 0] :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3}}
                        {:id :hazer :position [3 0.3 -3] :universe 1 :address 10 :channels {:fog 1}}]})
   (let [m (-> (control-model/new-show {:show-id :repro :rig rig})
               (assoc-in [:registry :effects :wash] {:id :wash :version 1 :params {} :outputs {:color [1.0 0.0 0.0]}})
               (assoc-in [:registry :cues :wash] {:id :wash :version 1 :effect-id :wash}))
         rt (show/create-runtime {:model m :workspace-options nil})]
     (try (show/submit! rt {:op :cue/launch :cue-id :wash}) (show/tick! rt 0) (show/tick! rt 25)
          (let [s (show/snapshot rt)]
            [(some (comp :message :fault) (:instances s)) (get-in s [:output :values :rgb :emitted-color])])
          (finally (show/stop! rt))))
   ;; => ["Fixture does not implement batch attribute" [0.0 0.0 0.0]]
   ```

2. **A typed `:gradient` parameter cannot drive an output.** `{:op :param :key k}`
   on a `:gradient` schema fails to compile: "This parameter type is not
   supported by the numeric expression IR". `looks/gradient` lowers the stops
   into the expression when the effect is authored.
3. **A cue cannot declare the sources to attach on launch.** Bindings belong to
   running instances, so a grid launch from Studio gets none, and the
   scheduler has no bind action.
4. **The scheduler's `:launch` takes only `:mode`, `:duration` and `:params`,**
   and non-human sources cannot launch show-control cues. This is by design,
   and it is why the timeline has a direct path.
