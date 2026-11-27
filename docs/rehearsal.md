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
| Section lines | The recorded agent hears of each line a bar ahead, as a Live locator or AbleSet song section gives it the song's map (`:agent-lead-beats`, default 4). Its tool calls stream through `tolglow.integrations.agent/run!` and are dispatched by `show/dispatch-tool!`, the path a live provider uses, from a recorded transport that never opens a connection. |
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
sweeps with their glow, a chorus plays two-beat accents. For each section line
the agent sends `configure_auto` with that mood, a seed derived from the
passage, the take and the section, and `:at` the section's first bar
([auto show](auto-show.md#on-a-musical-line)), so the change applies on the
downbeat. A line it hears only after it has passed (`:agent-lag-beats`) is
answered at once; `:agent-schedules? false` makes it answer every line at once,
as before #767. `:agent-latency-beats` (default 0) delays each reply after the
agent heard of the line, in transport beats. The first round is the agent's
plan before the downbeat and takes no latency. A reply that arrives after its
line applies at once if it is less than a bar late, marked late
([auto show](auto-show.md#on-a-musical-line)). Every round of a take belongs to
one agent conversation, so the agent owns what it posted in earlier rounds. In a concert its first round also sends
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
;; Replies that take five beats: a bar ahead is not enough, changes land late:
(:responses (rehearsal/club 670 {:passage {:agent-latency-beats 5}}))
;; The agent before #767, applying each change when it hears the line:
(:flags (rehearsal/club 670 {:passage {:agent-lag-beats 0 :agent-schedules? false}}))

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

- *Agent responses.* For each section line: the beat the change was
  accepted, whether it waited on the line (*On the line*), the beat it applied
  and how late that was, *carry-over launches*, *early launches* and whether
  the change landed in a later section than it was for (stale). Each launch is
  attributed to the auto session that made it (its source session, recorded
  per frame), not to when it was seen. Carry-over launches are the previous
  session's launches from the line on; early launches are the new session's
  launches before the line. A planner that decided before the change on the
  line's frame would therefore show carry-over even though the change applied
  on time.
- *Phrasing.* Each auto section the planner entered: on the bar grid, on a
  multiple of its own length, on the phrase grid. A section whose timer starts
  after the frame that entered it is reported at that frame. Auto launches are
  measured against their own interval: whether each decision fell due on its
  interval's grid (a bar line for a bar interval, and so on; an interval that is
  not a whole number of steps, such as `{:beats 3/2}`, need not) and how long
  after that point it fired; up to one frame is expected. Decisions taken the
  moment a configuration applies are immediate by design and counted apart.
  The first section of a configuration begins with the agent's change and is
  reported where the change applied, never as a plan boundary.
- *Phrase by phrase.* Launches, cues, new cues and *novelty*: the Jaccard
  distance between a phrase's set of launched `[cue colour]` pairs and the
  previous phrase's. Below 0.2 counts as a repeat; the longest run of repeats
  is reported, since it may be a held motif or a stall. Two phrases with no
  launches are not compared.
- *Contrast.* Per music section: mean lit heads and brightness from the same
  calibrated meter as the budget, the share of dark frames (at most one head
  lit) and of frames with a strobe cue running.
- *Stops.* Whether the beat held, decisions and launches while stopped, and
  decisions in the first beat after play resumed.

**Concert**, per take and across the two:

- *Anchors.* Each section's anchor started on its bar, late or missed, and
  whether both takes started it at the same beat. Both takes post the same
  anchor events, so this checks the scheduler, not the agent's choices.
- *Agent.* Per take, the same lateness, carry-over and early launches as in
  the club.
- *Variation.* Per section: auto fills and stem hits in each take, how many
  `[cue start-beat]` pairs they share, the Jaccard distance between the takes,
  shared fills whose seeded parameters differ, and the Push gestures of each.
- *Gestures.* Each Push gesture's target, receipt and the beat it was
  applied. *Overwritten* gestures: a performer's parameter value that another
  writer replaced while the performer held that instance. Agent ramps with
  their outcome; a performer grabbing a ramped parameter cancels it with
  `:human-override`.

Flags list measured problems only: off-grid plan boundaries, late or stale
agent changes, carry-over and early launches, decisions due off their
interval's grid or firing more than a frame late, three or more
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
fills, the bass rests and the auto choices, and each take has its own fixed
table of Push gestures. A
real-time `play!` is not frame-for-frame repeatable; use `run-take` for that.

## Verification

- `tolglow.rehearsal-test` (real show owner, captured output): the club passage
  replays exactly and another seed differs; four agent changes heard a bar
  ahead and applied on the first frame at or past their line through the tool
  path, with zero carry-over and zero early launches, the new mood deciding on
  the line's own frame; the groove's and return's first sections reported with
  the change, not as plan boundaries; plan boundaries on the bar grid; every
  decision due on its own interval's grid and fired within one frame of it;
  quiet darker than the breakdown, darker than the groove; no strobe frame in
  the breakdown or the quiet, and none after the quiet change applied; the stop
  holds and resumes with at most one decision. A two-beat-late agent shows
  carry-over launches; a 40-beat-late one is named stale; one answering on
  arrival 0.3 beats early launches the new mood before each line, and one
  answering on arrival at the line leaves the previous mood one decision on
  it. Replies taking a beat still land on every line; replies taking five beats
  land 1–1.5 beats late, applied on arrival, marked late, with carry-over
  measured and flagged. Two concert takes keep all four anchors on their bars (a scheduler
  check, given identical anchor events), raise no flags (zero carry-over and
  zero early launches on every line), differ
  in chorus fills, fill parameters and stem hits, accept every Push 3 gesture,
  and take B's grab cancels the build ramp with `:human-override` and holds;
  a taken sweep stays with the performer until they release it while auto goes
  on launching its own. A concert take replays exactly. The artefact round-trips
  as EDN. A Room rehearsal of four bars at 180 BPM on a started show posts the
  anchors before play and launches them, the hits and the gesture.
- `tolglow.rehearsal.review-test`: the overwritten-gesture, anchor, repeat,
  stop and contrast detectors fire on hand-built frames, and two silent
  phrases are not a repeat. A launch the previous session made on the frame a
  change applied counts as carry-over. A `{:beats 3/2}` interval is on its grid while a
  whole-beat interval off its grid is flagged; a change on its line is measured
  where it applied, with its first section not counted as a plan boundary, and
  the same frame counts as carry-over for a change applied on arrival.
- `tolglow.automation.policy-test`: the first club rehearsal found beat
  one-shots sliding off the beat at 124 BPM (a 125 ms frame is 0.258 beats and
  each interval started from the late frame). Regressions cover that, auto
  enabled just before a bar or phrase line, a frame gap landing just before a
  line, an interval changed mid-run, a section plan entered just before a line
  (each decides once, then on the next line at least half an interval on), and
  a clock rebase that shifted the interval off the grid.

## What the first rehearsal found

- Beat one-shots slid off the beat at 124 BPM: each interval started from the
  frame that took the decision. Fixed in the planner with one grid rule for
  intervals and section plans (see [auto show](auto-show.md)).
- `configure_auto` applied when it arrived. An agent answering a section line
  on the first frame past it landed up to a frame late, and on that frame the
  previous mood still decided once: on seed 670 the groove launched 3 looks on
  the breakdown's first frame, and the return entered its strobe bar on the
  quiet's first frame, before the agent's change released them. In the concert
  the verse looks flashed on each chorus downbeat the same way (1–2 launches
  per line). Answering 0.3 beats early removed the carry-over but launched the
  new mood 1–2 times in the previous section. Since #767 the agent puts its
  change `:at` the line. On seed 670, with launches attributed to the session
  that made them:
  - Replies with no latency, or a beat of latency: the club and both concert
    takes show zero carry-over and zero early launches on every line, with no
    strobe frame in the quiet, and the concert raises no flags.
  - Replies five beats after the agent heard of the line (one bar ahead):
    changes arrive 1.2–1.4 beats late in the club and one beat late in the
    concert. They apply at once, marked late. The previous mood launches 1–5
    looks into the new section (club) and 1–2 (concert). The quiet shows 0.04
    strobe frames, and the review flags every late line.
- A Push-sourced write cancels an agent ramp with `:human-override` only when
  its source adapter is the device profile (`:push-3`), as real Push sessions
  send. Tests that use `{:adapter :push}` (for example the R1o scenario) are
  read as non-human and record `:superseded`.

## Not yet

- The passages are synthetic recordings made from seeds, not captures of a
  real set. A real multichannel recording can enter through stem capture's
  `:file` input ([stems](stems.md)); a recorded Push session as data needs a
  capture format from the surface layer.
- Push gestures are shared commands with a Push 3 source, not device
  captures, and include no encoder `:gesture/begin` stream or group control.
  No CV input is modelled.
- A stale agent suggestion is measured and named, not refused: the show has no
  notion of the music's sections, so a late `configure_auto` without `:at`
  still applies (one with `:at` a passed line is refused).
- The agent hears of a line a bar ahead from the passage's section map. A club
  agent that only listens has no such map; detecting a section change from the
  audio ahead of its downbeat is not modelled.
  Scheduled anchors are refused once their bar has passed.
- A disconnected agent is not simulated beyond lateness.
- Studio cannot start a rehearsal or read its review. During a Room rehearsal
  it shows the show as usual (Room, **Let it wander**, the schedule), since the
  rehearsal uses the same commands.
- No physical output, Live set or provider was used. Whether a passage feels
  musical needs you in the Room, then on fixtures.
