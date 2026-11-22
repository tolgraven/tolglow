# Lighting capacity and music energy

In Studio, open **Connections → Lighting capacity** and enable **Meter the rig**.
This shows the fraction of the patched rig’s nominal full-white output used by
the latest committed lighting frame. Metering, audio limiting, and automatic
fog requests start disabled. The meter operates headlessly with the same
settings and show snapshot as Studio.

The meter reads the actual coarse/fine DMX bytes across every patched universe.
It decodes channel inversion and applies the physical dimmer once. It includes
RGB, white (including warm and cold emitters), amber, UV and dimmer-only lamps. Fog channels and other utility
channels do not add any light. A head with a pure UV emitter contributes to
rated emitter drive and has zero modeled visible output. Heads with a measured
relative full-white brightness use the calibrated value. Other rated fixtures
use Afterglow’s equal-power emitter and wavelength model relative to a 100 W
RGB fixture. Each unrated, uncalibrated fixture contributes one nominal unit,
shared across its heads when the patch specifies an emission share.

- **Visible output** weights the drive of each visible emitter by its nominal
  photopic response. A saturated red look therefore uses less of the rig’s
  white capacity than the same fixtures at full white. The tint shows additive
  emitter color. Ambient light, beam size, fog scattering and surface response
  affect a viewer’s perception in the venue.
- **Estimated emitter drive** scales known fixture ratings by the average
  emitter drive and physical dimmer. It excludes unrated fixtures, motors,
  electronics and fogger heaters. Use measured electrical data to size circuits.
- **Writing light** lists effects whose positive-opacity emission writes are
  still represented after later opaque replacements of the same field.
  Arithmetic native-layer modifiers appear alongside the earlier light they
  modify. A position-only modifier or a muted native member contributes no
  emission write. Overlapping additive, max and partial-opacity writers can
  share a channel.
  The meter reads their final combined output once.

## Audio limiting

Enable **Limit the look to audio energy** and select the quiet and loud targets.
Captured PCM goes through the same smoothed RMS envelope as audio routing. A
fresh block with zero RMS selects the quiet target. When capture stops providing
fresh blocks, lighting goes back to the operator master. Pause looks, blackout
and calibration take priority. A target above the authored look leaves that look
at its authored level. A dark cue stays dark. The feedback uses the previous
committed output, so an abrupt scene change can take another frame to settle.
This meter reports the renderer commit. A late-frame output policy or hardware
latency can present a different frame at the same instant.

Configure the show through its normal settings command. The settings persist
in the show backup. Each show epoch needs fresh audio before following resumes.

```clojure
(show/submit! runtime :settings
  {:scope :lighting-capacity
   :values {:enabled? true :follow-energy? true
            :minimum 0.1 :maximum 0.9
            :reference-rms 0.35 :signal-timeout-ms 750}})

(get-in (show/snapshot runtime) [:output :lighting-capacity])
```

## Fog requests

To use audio for fog, independently arm **Pulse selected foggers on high energy**
and select patched fixtures with a named `:fog` channel. Headless options are
`:fog? true` and `:fog-fixture-ids [:stage-fog]`. A crossing above 80% of
reference RMS requests half drive for 0.4 seconds. The envelope must fall below
55% and a six-second rest must elapse before another pulse. Losing fresh audio
or disarming cancels this source’s demand on the next frame. An active stationary
fog calibration scan also suppresses automatic fog requests. Manual fog cues
combine by maximum. A pulse shown here is a requested show output. The atmosphere simulator
uses fog DMX delivered by the output publisher at its delivery timestamps. The
fogger’s own DMX curve, warm-up behavior and physical delivery determine what
happens at the nozzle. Pausing the render process or
losing the output link can leave hardware holding its last channel value.
