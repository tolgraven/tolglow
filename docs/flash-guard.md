# Flash guard

The flash guard limits fast, large-area flashing in the light a show delivers.
By default it covers light from agent-launched cues only. Human-launched strobe
passes unchanged unless the operator turns the guard on for the whole show.
It is a photosensitivity mitigation, not a certification that a show is safe.
Fog, mirrors, unsynchronised fixtures and the venue's own warnings remain the
operator's responsibility. [Issue #836](https://github.com/tolgraven/tolglow/issues/836) ·
[Design: orchestration research, Safety](research/native-agent/orchestration.md) ·
[Source](../src/tolglow/flash_guard.clj)

This first slice measures delivered output. Predicting a proposal's flash rate
at admission, refusing it with `:flash-guard` and re-checking pending events
after a tempo change are still open on #836.

## Defaults

The operator has not confirmed these yet. They follow the research proposal and
are set in one place, `tolglow.flash-guard/defaults` and `presets`.

| Setting | Default | Meaning |
| --- | --- | --- |
| `:enabled?` | `true` | `false` turns the guard off entirely |
| `:scope` | `:agent` | `:agent` guards light from instances whose launch source adapter is in `:sources`; `:show` guards every head, human launches included |
| `:sources` | `[:agent]` | Source adapters the agent scope treats as agent-attributed. Add `:autopilot` or `:auto` to cover those launches too |
| `:preset` | `:club` | `:club` is 4 Hz (the UK HSE figure for strobes at public performances and clubs, as cited by the Epilepsy Society). `:broadcast` is 3 Hz (ITU-R BT.1702 and WCAG 2.3.1). Both presets limit saturated red to 3 Hz |
| `:limit-hz`, `:red-limit-hz` | from the preset | An explicit value overrides the preset, 0 to 60 Hz |
| `:area` | `0.25` | The smallest area-weighted luminance rise that counts as a flash, as a share of the whole rig's visible output |
| `:grant` | `nil` | `{:limit-hz 12 :burst-beats 8}` lets agent light flash faster, up to the granted rate, for bursts of at most that many beats |

## What counts as a flash

The guard reads each delivered frame after output response and the lighting
budget, before activity scoring. For every guarded head it reads the visible
brightness from the committed DMX bytes with the calibrated capacity meter, so
dimmer, colour response, inversion, fine channels and closed shutters are
included.

- **Area weighting.** Each head's brightness change is weighted by its share of
  the rig's full visible output and summed over the guarded heads. A full-rig
  white flash counts in full; eight pixels of a tube count for their small
  share. Fixtures flashing out of step add up, because rises are summed before
  the threshold is applied.
- **A flash** is a rise of that weighted sum of at least `:area` within about
  half a period at the limit (the run leaks with a time constant of
  `1 / (2 * limit-hz)` seconds and restarts after a net fall). Slow fades never
  count.
- **Saturated red** is light whose red is at least 0.8 of red, green and blue
  together (the WCAG definition). Red emitters are dim, so red is measured as
  the drive of the head's brightest red emitter times its dimmer, weighted by
  the same area shares. Its rises are counted separately against
  `:red-limit-hz`, so a white strobe at 3.75 Hz passes while the same strobe in
  saturated red is held to 3 Hz.
- **Brightness is luminous.** A head's brightness is its share of its own
  full-white output. A fixture whose RGBW policy turns white into the white
  emitter alone reads about a quarter of its full output in white, so its
  flashes weigh less than a full-white flash of all its emitters.
- **Strobe channels.** A fixture's own strobe free-runs without any change in the
  DMX stream. When a guarded head's strobe or shutter channel selects a strobe
  function faster than the limit, and those heads together exceed `:area`, their
  emission is removed while it lasts. A strobe range without a declared `:hz`
  span counts as faster than any limit. Free-running strobes at or under the
  limit are not added together, because their phases are unknown; that is a
  known gap.

## What the guard does

The guard counts flashes over the last second of show time. While that window
already holds the permitted number, it holds the next rise of each guarded head
at the head's previous delivered level until the window has room again. The
result is the same strobe at the limit rate.

- It changes emission only, through Afterglow's emission scaling: a head's
  physical dimmer, or else each emitter, rounded toward lower drive. Pan, tilt,
  motor speed, shutter and strobe channels, gobos, fog and every other channel
  pass unchanged. A move of any speed reaches the output unclamped.
- It reads committed bytes and never evaluates or advances an effect, so preview
  does not run a stateful signal twice. The same show and clock produce the same
  output byte for byte.
- It runs on show time and beats. A stopped transport stands it still with
  everything else, and resuming does not open a fresh window.
- Blackout, master at zero and calibration bypass it and carry its window over.
- With the default scope, a head is guarded while an agent instance contributes
  emission to it after layering. If an agent and a human both light a head, a
  held rise holds both on that head.

## Grants

A grant raises the limit for agent light, never for the saturated-red limit.
When the measured rate first exceeds the base limit a burst starts. It may last
`:burst-beats` beats; after that the base limit applies for as many beats again
before another burst can start. A grant is stamped with the show epoch when it
is given and lapses when the show restarts.

## Where it is enforced

The show owner enforces the guard in its frame path, on the instance's recorded
launch source. It does not depend on an orchestrator or on one tool: a
`launch_cue` through `show/dispatch-tool!` (Studio chat, rehearsal scripts,
agent runs), a cue launch the agent posted with `schedule_events`, and any
future agent route are all limited, because each records `{:adapter :agent}` on
the instance it launches. An orchestrator may check a proposal before it posts
it, but the owner is the enforcement point. Agent attribution comes from the
envelope `dispatch-tool!` builds; HTTP `/api/command` records `:web` (#895).

## Who may change it

Only an operator surface (Studio, web, Push, X-Touch, MIDI, OSC, sensor or REPL
sources) may change the `:flash-guard` settings. A settings command from an
agent or any other non-operator source is refused, so no agent, profile or
backend can lift the guard or grant itself a faster rate.

## From the REPL and Studio

Studio and the REPL use the same settings command and read the same projection.

```clojure
(require '[tolglow.show :as show])

;; Broadcast limits, for a streamed show.
@(show/submit! runtime :settings {:scope :flash-guard :values {:preset :broadcast}})

;; Guard human strobe as well.
@(show/submit! runtime :settings {:scope :flash-guard :values {:scope :show}})

;; Let the agent strobe at up to 12 Hz for eight beats at a time, this session.
@(show/submit! runtime :settings {:scope :flash-guard
                                  :values {:grant {:limit-hz 12.0 :burst-beats 8}}})

;; Remove the grant.
@(show/submit! runtime :settings {:scope :flash-guard :values {:grant nil}})

;; What it is doing now.
(get-in (show/snapshot runtime) [:output :flash-guard])
(get (show/variables runtime) :lighting/flash-guard)
```

The report carries `:scope`, the effective `:limit-hz`, `:base-limit-hz`,
`:red-limit-hz`, `:rate-hz` and `:red-rate-hz` (flashes in the last second),
`:guarded-heads` and `:limiting?`. While limiting it adds `:held-heads`,
`:reasons` (`:luminance`, `:saturated-red`, `:strobe-channel`) and, in the agent
scope, the `:instance-ids` whose heads were held. A grant adds `:grant` and the
`:burst` state.

The show emits a `:lighting/flash-guard` event in the `:lighting` category when
limiting starts (`:status :limiting`, `:reason :flash-guard`), when what it
holds changes, and when it stops (`:status :clear`). Event subscribers and the
REPL's event views receive it like any other lighting event.

Studio has no flash guard panel yet. The snapshot carries the report under
`[:output :flash-guard]`, beside lighting activity and the budget.

## Frame cost

See the pull request for the measured cost on the mixed lighting-controls rig.
