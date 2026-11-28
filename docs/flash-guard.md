# Flash guard

The flash guard limits fast, large-area flashing in the light a show delivers.
By default it covers light an agent influenced. Human-launched strobe passes
unchanged unless the operator turns the guard on for the whole show. It is a
photosensitivity mitigation, not a certification that a show is safe. Fog,
mirrors, unsynchronised fixtures and the venue's own warnings remain the
operator's responsibility. [Issue #836](https://github.com/tolgraven/tolglow/issues/836) ·
[Design: orchestration research, Safety](research/native-agent/orchestration.md) ·
[Source](../src/tolglow/flash_guard.clj) · [Influence](../src/tolglow/flash_guard/influence.clj)

This first slice measures delivered output. Predicting a proposal's flash rate
at admission, refusing it with `:flash-guard` and re-checking pending events
after a tempo change are still open on #836.

## Defaults

These are provisional, conservative values. The operator may loosen them in a
session; they are kept in one place, `tolglow.flash-guard/defaults`, `presets`
and `grant-caps`.

| Setting | Default | Meaning |
| --- | --- | --- |
| `:enabled?` | `true` | `false` turns the guard off for this session |
| `:scope` | `:agent` | `:agent` guards light an agent influenced (below); `:show` guards every head, human launches included |
| `:sources` | `[:agent]` | Source adapters that count as agents |
| `:preset` | `:broadcast` | `:broadcast` is 3 Hz, the line ITU-R BT.1702 and WCAG 2.3.1 draw. `:club` (opt-in) is 4 Hz, the UK HSE figure for venue strobes run by a person who can see the room, as cited by the Epilepsy Society. Both presets limit saturated red to 3 Hz |
| `:limit-hz`, `:red-limit-hz` | from the preset | An explicit value overrides the preset, 0 to 60 Hz |
| `:area` | `0.25` | The share of the rig that must flash together for a flash to count |
| `:min-change` | `0.1` | The rise above a head's last trough that makes a transition, as a share of that head's maximum |
| `:grant` | `nil` | `{:limit-hz 12 :burst-beats 8}` lets agent light flash faster for bounded bursts. At most 12 Hz and 8 beats |

## What counts as agent light

The show owner records agent influence from each committed command's source,
wherever the command came from: a tool call through `show/dispatch-tool!`
(Studio chat, rehearsal scripts, agent runs), an event the agent scheduled,
a ramp it started, or any future route. An instance's light is agent light
when an agent:

- launched it, directly or with a scheduled `cue/launch`;
- last wrote one of its parameters or its opacity: `set_parameter`, a
  scheduled `param/set` or `param/ramp` (including one addressed by cue ID), or
  a scheduled `instance/opacity`;
- published the effect revision it runs (`publish_effect`), or last saved the
  cue it was launched from (`put_cue`);
- configured the policy that launched it: auto (`configure_auto`, including a
  scheduled `auto/configure`), the energy autopilot (`configure_energy_autopilot`)
  or lighting scenes (`set_lighting_scene`, scheduled `lighting/scene`).

An operator's later write to the same parameter, cue or policy ends the agent's
claim on it. An agent write to a show variable cannot yet be traced to the cues
that read it, so while one is live the guard covers the whole show.

Agent attribution comes from the envelope `dispatch-tool!` builds. HTTP
`/api/command` and `/api/settings` will record `:web` once #895 lands; until
then an HTTP client can claim any source, so every HTTP client counts as an
operator and an external agent must not drive the show over HTTP until it has
its own transport identity.

## What counts as a flash

The guard reads each delivered frame after output response and the lighting
budget, before activity scoring, from the committed DMX bytes.

- **Intensity.** Each head's level is measured against its own maximum at the
  colour it shows: dimmer times its most driven visible emitter, zero behind a
  closed shutter. White on the white emitter alone reads 1.0, as a full red does.
- **Transitions, Harding style.** Each head tracks its own trough and peak. A
  head makes a transition when it rises at least `:min-change` above its last
  trough; it re-arms after falling at least as much from its peak. There is no
  leak, so a ramp that snaps off counts like a square strobe, and a slow fade
  counts once.
- **Area.** A head's share of the rig is the larger of its luminous share and
  its visual share. Visual share is one unit per fixture, split across a strip's
  pixels, at least two units for beams (15° or narrower), blinders and fixtures
  with a strobe function, or the fixture's (or fixture type's) declared
  `:flash-area`. Heads whose transitions fall within half a period at the limit
  add up, and a flash is counted when together they reach `:area`. A chase that
  moves a small band at a time does not.
- **Saturated red** is light whose red is at least 0.8 of red, green and blue
  together (the WCAG definition). It is counted separately against
  `:red-limit-hz`, which no grant raises.
- **Fixture strobes.** A strobe function free-running in a fixture shows no
  change in the DMX stream, and its phase is unknown. The guard looks for strobe
  ranges in every channel, not only `:strobe` and `:shutter`, and budgets them
  with worst-case phase: the sum of share times rate over lit, strobing heads
  stays at or under `:area` times the limit. It removes the emission of the
  largest contributors first until that holds. A strobe range without a declared
  `:hz` span counts as faster than any limit.

## What the guard does

The guard counts flashes over the last second of wall-clock time. While that
window already holds the permitted number, it holds each guarded head that
would make the next large-area transition just under its trough plus
`:min-change`, until the window has room again. The result is the same strobe
at the limit rate.

- It changes emission only, through Afterglow's emission scaling: a head's
  physical dimmer, or else each emitter, rounded toward lower drive, with
  coarse and fine bytes written as one value. Pan, tilt, motor speed, shutter
  and strobe channels, gobos, fog and every other channel pass unchanged. A move
  of any speed reaches the output unclamped.
- It reads committed bytes and never evaluates or advances an effect. The same
  show and clock produce the same output byte for byte.
- **Wall time.** Photosensitivity follows the clock on the wall, and the guard
  holds no effect state, so its window drains on the host clock even while the
  transport is stopped. Effects still freeze on stop; only the guard's count
  of recent flashes keeps draining. A master raise on a stopped show is not held
  once a second has passed.
- A rig edit keeps the window and each unchanged head's history.
- Blackout, master at zero and calibration bypass it and carry its window.
- **Mixed heads.** If an agent and a human both write a head, safety wins: a
  held rise holds both. The report names the human instances affected as well
  as the agent's.

## Grants

A grant raises the limit for agent light, never the saturated-red limit. When
the measured rate first exceeds the base limit a burst starts. It lasts
`:burst-beats` forward beats; a backward jump of the beat ends it early. The
cooldown that follows is as long as the burst cap, drains on forward beats
only and survives relaunches. A grant is stamped with the show epoch when it is
given and lapses when the show restarts or another show is loaded.

## Who may change it

Only an operator surface may change the `:flash-guard` settings: Studio, web,
Push, X-Touch, MIDI, OSC or the REPL. A command must name its source. A settings
command from an agent, a sensor, any other non-operator source, or with no
source at all is refused, so no agent, profile or backend can lift the guard
or grant itself a faster rate.

Settings loaded with a saved show, profile or checkpoint may only tighten the
defaults: the guard stays on, limits and areas take the stricter value, the
agent sources keep the defaults, any grant is dropped and agent influence from
another session is forgotten. An operator can loosen them again in the session.

## From the REPL and Studio

Studio and the REPL use the same settings command and read the same projection.
At the REPL, `show/configure-flash-guard!` sends it with the REPL as source.

```clojure
(require '[tolglow.show :as show])

;; The 4 Hz club figure, for a venue strobe run by someone watching the room.
@(show/configure-flash-guard! runtime {:preset :club})

;; Guard human strobe as well.
@(show/configure-flash-guard! runtime {:scope :show})

;; Let the agent strobe at up to 12 Hz for eight beats at a time, this session.
@(show/configure-flash-guard! runtime {:grant {:limit-hz 12.0 :burst-beats 8}})

;; Remove the grant.
@(show/configure-flash-guard! runtime {:grant nil})

;; What it is doing now.
(get-in (show/snapshot runtime) [:output :flash-guard])
(get (show/variables runtime) :lighting/flash-guard)
```

The report carries `:scope` (`:show` while it covers everything, including
while an agent variable write is live), the effective `:limit-hz`,
`:base-limit-hz`, `:red-limit-hz`, `:rate-hz` and `:red-rate-hz` (flashes in the
last second), `:guarded-heads` and `:limiting?`. While limiting it adds
`:held-heads`, `:reasons` (`:luminance`, `:saturated-red`, `:strobe-channel`),
the agent `:instance-ids` and the `:human-instance-ids` whose heads were held.
A grant adds `:grant` and the `:burst` state.

The show emits a `:lighting/flash-guard` event in the `:lighting` category when
limiting starts (`:status :limiting`, `:reason :flash-guard`), when what it
holds changes, and when it stops (`:status :clear`).

Studio has no flash guard panel yet. The snapshot carries the report under
`[:output :flash-guard]`, beside lighting activity and the budget.

## Frame cost

Measured on the 2,048-pixel, 64-head mixed rig (2,112 meter heads), whole-show
scope, JDK 25: a frame the guard passes costs about 0.1 ms and 27 KB; a frame
that holds every head about 2 ms, most of it Afterglow's emission scaling of a
full frame copy. See the pull request for the measured runs.
