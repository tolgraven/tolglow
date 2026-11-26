# Rehearsal with the agent

A rehearsal plays a recorded passage through the real show with the same
agent in two modes and writes a review you can judge from. In the **club**
mode nobody plays: the agent answers each section line of the music on its
own. In the **concert** mode the same agent posts the song's section anchors
and plays fills, while recorded stems and Push gestures vary between two
takes. The review measures what happened against the beat grid. Whether it
felt musical is your call; the report has a column for your notes.
Issue [#670](https://github.com/tolgraven/tolglow/issues/670), roadmap step 3
of [#682](https://github.com/tolgraven/tolglow/issues/682).

[Harness](../src/tolglow/rehearsal.clj) ·
[Passages](../src/tolglow/rehearsal/passage.clj) ·
[Recorded agent](../src/tolglow/rehearsal/agent_script.clj) ·
[Review](../src/tolglow/rehearsal/review.clj) ·
[Auto show](auto-show.md) · [Stems](stems.md) · [Scheduler](scheduler.md)

## What a passage holds

A passage is data. It has a tempo, a music section map (what Live locators or
AbleSet song sections would give) and the inputs a performance produced, each
on a transport beat:

| Input | How it reaches the show |
| --- | --- |
| Section lines | The recorded agent is asked on the first frame at or past each line. Its tool calls stream through `tolglow.integrations.agent/run!` and are dispatched by `show/dispatch-tool!`, the path a live provider uses, from a recorded transport that never opens a connection. |
| Stem onsets (kick, snare) | `:audio/onset` events with emitter `:stem/<id>` through `show/audio-callbacks`, the capture boundary. Routed to cues with `tolglow.stems/route-onset!`. |
| Stem levels (bass) | Stem features through the same callbacks, routed to a cue parameter with `tolglow.stems/route-level!`. |
| Push gestures | Shared commands with the source a Push 3 session gives (`{:adapter :push-3 …}`). A gesture names the running instance its pad or encoder was bound to (`:select`), resolved on replay. |
| Transport stops | `:transport/set` from the REPL replaying the recording, with the recorded hold. |

Inputs between two frames reach the show on the next frame, as a capture
block does. Bars and beats count from zero, as the scheduler does.

The show is the 2023 gig rig with the R1o red-show cues
([auto show](auto-show.md#section-plans)) plus two section anchors
(`:verse-wash`, `:chorus-wash`) and two hits (`:kick-hit`, `:snare-hit`).
Anchors and hits are marked `:auto? false`, so auto mode never picks them.

## One agent, one taste

`agent-script/moods` maps a music section kind to auto settings: groove and
return play beat one-shots in a section plan (the return can cut to a strobe
bar), the breakdown slows to bar-long purple looks under a lit limit, the quiet
passage moves heads in the dark with at most one light, a verse holds two-bar
sweeps with their glow, a chorus plays two-beat accents. At each section line
the agent sends `configure_auto` with that mood and a seed derived from the
passage, the take and the section. In a concert its first round also sends
`schedule_events`: each section's anchor on its first bar for the section's
length, and in a chorus a four-bar `ease-in` build of the anchor's level.

## From the REPL

```clojure
(require '[tolglow.rehearsal :as rehearsal])

(def club (rehearsal/club 670))        ; run and review the club passage
(println (:report club))
(:flags club)

(def concert (rehearsal/concert 670))  ; takes :a and :b, compared
(println (:report concert))

(rehearsal/write! "target/rehearsal" club concert)
;; => ["target/rehearsal/club.edn" "target/rehearsal/club.md" ...]

;; A late agent, answering each section line two beats late:
(:flags (rehearsal/club 670 {:passage {:agent-lag-beats 2}}))

;; The take itself, for your own reading:
(def take (rehearsal/run-take (tolglow.rehearsal.passage/concert 670 :b)))
(count (:frames take))
```

`bin/repl run tolglow.qa.rehearsal review target/rehearsal 670` writes both
reviews. The EDN artefact keeps everything except per-frame records, which it
summarizes as a per-beat timeline (music and auto section, lit heads,
brightness, strobe, running cues) and a launch list; `:agent-wall-ms` holds
the host's agent turnaround, kept apart from frame timing.

## What the review measures

**Club**

- *Agent responses.* For each section line: the beat the change was accepted,
  how late that was, *carry-over launches* (looks the previous mood launched
  between the line and the change) and whether the suggestion landed in a
  later section than it was for (stale).
- *Phrasing.* Each auto section the planner entered, on or off the bar and
  phrase grid. The first section after an agent change begins with that change
  and is reported at it. Auto launches: how many started exactly on a beat and
  how far after their beat the latest one started; a decision fires on the
  first frame at or after its beat, so up to one frame is expected.
- *Phrase by phrase.* Launches, cues, new cues and *novelty*: the Jaccard
  distance between a phrase's set of launched `[cue colour]` pairs and the
  previous phrase's. Below 0.2 counts as a repeat; the longest run of repeats
  is reported, since it may be a held motif or a stall.
- *Contrast.* Per music section: mean lit heads and brightness from the same
  calibrated meter as the budget, the share of dark frames (at most one head
  lit) and of frames with a strobe cue running.
- *Stops.* Whether the beat held, decisions and launches while stopped, and
  decisions in the first beat after play resumed.

**Concert**, per take and across the two:

- *Anchors.* Each section's anchor started on its bar, late or missed, and
  whether both takes started it at the same beat.
- *Variation.* Per section: auto fills and stem hits in each take, how many
  `[cue start-beat]` pairs they share, the Jaccard distance between the takes,
  shared fills whose seeded parameters differ, and the Push gestures of each.
- *Gestures.* Each Push gesture's target, receipt and the beat it was
  applied. *Overwritten* gestures: a performer's parameter value that another
  writer replaced while the performer held that instance. Agent ramps with
  their outcome; a performer grabbing a ramped parameter cancels it with
  `:human-override`.

Flags list measured problems only: off-grid plan boundaries, late or stale
agent changes, launches more than a frame after their beat, three or more
repeated phrases, strobe in a quiet or breakdown section, a quiet section not
darker than the groove, a stop that did not hold, missed or late anchors,
anchors that differ between takes, overwritten or refused gestures.

## Room-only rehearsal

`rehearsal/play!` plays a passage in real time on a running show, whose own
scheduler commits the frames Studio's Room renders. It arms no output. The
runtime must hold `(rehearsal/room-model passage)`, which starts with the
transport stopped at beat 0: the agent's first round (a concert's anchors) is
taken before play starts, because in real time the transport would already
have passed bar 0 and the scheduler refuses a position behind it.

```
lein with-profile +dev run -m tolglow.qa.rehearsal room 18871 club 670
```

starts that show with Studio on `127.0.0.1:18871`, prints the agent and
gesture receipts when the passage ends, and samples `/api/state` while it
plays. Open Studio's Room to watch. `concert-a` and `concert-b` play the two
takes.

## Seeded replay

The same passage and seed reproduce every accepted action, committed instance,
receipt and output hash per frame (`rehearsal/replay-signature`). Another seed,
or another take, is another performance: the take seed changes the drummer's
fills, the bass rests, the auto choices and the performer's gestures. A
real-time `play!` is not frame-for-frame repeatable; use `run-take` for that.

## Verification

- `tolglow.rehearsal-test` (real show owner, captured output): the club passage
  replays exactly and another seed differs; four agent changes accepted within
  one frame through the tool path; plan boundaries on the bar grid; launches
  within one frame of their beat; quiet darker than the breakdown, darker than
  the groove, neither strobing; the stop holds and resumes with at most one
  decision. A two-beat-late agent shows carry-over launches; a 40-beat-late one
  is named stale. Two concert takes keep all four anchors on their bars, differ
  in chorus fills, fill parameters and stem hits, accept every Push 3 gesture,
  and take B's grab cancels the build ramp with `:human-override` and holds;
  a taken sweep stays with the performer until they release it while auto goes
  on launching its own. A concert take replays exactly. The artefact round-trips
  as EDN. A Room rehearsal of four bars at 180 BPM on a started show posts the
  anchors before play and launches them, the hits and the gesture.
- `tolglow.rehearsal.review-test`: the overwritten-gesture, anchor, repeat,
  stop and contrast detectors fire on hand-built frames.
- `tolglow.automation.policy-test/beat-decisions-stay-on-the-grid-when-frames-miss-the-beat`:
  the first club rehearsal found beat one-shots sliding off the beat at
  124 BPM (a 125 ms frame is 0.258 beats and each interval started from the
  late frame). Musical intervals now start where the previous one fell due.

## Not yet

- The passages are synthetic recordings made from seeds, not captures of a
  real set. A real multichannel recording can enter through stem capture's
  `:file` input ([stems](stems.md)); a recorded Push session as data needs a
  capture format from the surface layer.
- Push gestures are shared commands with a Push 3 source, not device
  captures, and include no encoder `:gesture/begin` stream or group control.
  No CV input is modelled.
- A stale agent suggestion is measured and named, not refused: the show has no
  notion of the music's sections, so a late `configure_auto` still applies.
  Scheduled anchors are refused once their bar has passed.
- A disconnected agent is not simulated beyond lateness.
- Studio cannot start a rehearsal or read its review. During a Room rehearsal
  it shows the show as usual (Room, **Let it wander**, the schedule), since the
  rehearsal uses the same commands.
- No physical output, Live set or provider was used. Whether a passage feels
  musical needs you in the Room, then on fixtures.
