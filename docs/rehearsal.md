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

## Stale replies and a disconnected agent

Every `configure_auto` the recorded agent sends names what it was based on
([auto show](auto-show.md#stale-agent-suggestions-670)): the auto session it
read when it heard of the line, and a window from where it heard of the line
(or the line, when it heard late) to the end of the section. The
show refuses a reply that arrives after its section has ended, or after
someone else changed auto meanwhile. The review lists it as *refused-stale*,
and the previous mood plays on. Before #670 such a reply applied in the
following section.

`:agent-outage` simulates a disconnected agent from one section on:
`{:from-section i :mode :refused}` refuses every connection at once, and
`{:from-section i :mode :silent}` never answers. The host gives up after
`:agent-timeout-ms` of host time (default 10 000 ms, the Room path's wait).
A silent run hangs on its own thread while the show plays on; it never blocks
frames, stems, gestures or stops, and it is closed when the passage ends. The
review lists these as *agent-failed* or *timed-out* and flags them. Its
*Continuity* section says whether auto kept deciding on its grid: a gap
longer than two of its own intervals plus a frame, in transport beats (so a
stop is not a gap), is a stall. It also counts auto changes still waiting and
agent runs the host had to close when the passage ended.

```clojure
(:continuity (rehearsal/club 670 {:passage {:agent-outage {:from-section 1 :mode :silent}}}))
;; => {:decisions 192 … :stalls [] :pending-auto-changes 0 :agent-runs-open-at-end 0 …}
```

## From Studio

**Perform → Tools → Rehearse with the agent** arms, starts and stops a Room
rehearsal on the running show. You choose the passage (club, or concert take
A or B), the seed, the agent's reply time in beats and an optional outage.
**Arm** loads the rehearsal show in place of the open one, so the first arm
needs **Replace the open show with the rehearsal show** ticked. Arming runs in
the background (**Arming**, then **Armed**). It saves the open show as a named
checkpoint, waits until the rehearsal show's restore has committed, then
takes the agent's first round with the transport stopped at bar 0. That round
is bounded by the agent timeout: a silent agent is cancelled and logged as
timed out. **Stop** while arming cancels the arm.

The saved show goes beside that show's recovery files (or into a
`tolglow-before-rehearsal` folder in the system temporary directory when the
show has no recovery folder). It gets a unique name,
`before-rehearsal-<show>-<time>-<id>.transit`, and the newest five per show
are kept. It never becomes the show's "Latest named" checkpoint, which stays
your own save. **Restore … from before the rehearsal** (`session/restore!`)
brings it back. That works only while the open show is still the rehearsal
show, and it uses the checkpoint up. Edits you made to the rehearsal show
since arming need **Discard the edits made to the rehearsal show**
(`:discard-edits? true`), both to restore and to re-arm. An edit is any
accepted change that is not the rehearsal's own: not auto mode's, not an
agent's, not the rehearsal's transport or recorded gestures, and not a
command that only plays the show (a cue launch, a parameter or knob move,
transport). So an auto plan edit, a knob or knob look, a macro or a setting
all count, as will whatever the show saves next. Checkpoints are matched by
their exact show name and a hash of the show ID before the oldest are pruned,
and a checkpoint a session still offers is never pruned. If you arm a
rehearsal over another show, the earlier checkpoint is no longer offered by
Restore, but it stays on disk beside that show's recovery files. A show stop cancels
every agent run the session started and starts no review. The panel then
lists the mood the agent will set at each section line. **Start** plays the
passage in real time; watch it in the Room. **Stop**, or the passage ending,
stops the transport, which pauses everything. The panel keeps the run's agent
replies (when each was heard and accepted, its receipts and the host's
turnaround in ms) and gesture receipts.

The review is the headless seeded replay of the same passage, seed and agent
options, computed in the background after arming or on **Review the replay**.
It shows the measured flags, what the agent changed and how each change landed
(outcome, applied beat, lateness, carry-over and early launches), phrasing,
phrase-by-phrase repetition with novelty, continuity, and the full Markdown
report. A concert review compares both takes. The REPL has the same
operations in `tolglow.rehearsal.session` (`arm!`, `start!`, `stop!`,
`review!`, `restore!`, `status`). A failed arm reads `:status :failed` with
the reason under `:failure`, which the panel shows under the buttons. Nothing is sent to fixtures.

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
  attributed to the auto session that made it (its source session), read
  from the show's committed launch receipts after every run of ticks rather
  than from frames, since a look launched and ended within one tick appears
  in no frame. Carry-over launches are the previous session's launches from
  the line on; early launches are the new session's launches before the line.
  Checked by mutation: with the old planner made to decide before the change
  on its frame, the club reports 3, 1 and 1 carry-over launches on its later
  lines instead of 0.
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
  change applied counts as carry-over, including one that only the launch
  receipts show. A `{:beats 3/2}` interval is on its grid while a
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

- `tolglow.automation.suggestion-test` (real owner, injected clock, agent tool
  path and a recorded provider round): stale session, passed window, a window
  held across a ten-second stop, an `at` outside its window, a change refused
  when due after an operator restarted auto, a basis that survives auto's own
  decisions and chains through receipts, an event outside its window refusing
  a `schedule_events` batch, an in-window event refused when due after an
  operator restart, a backward seek before the window's start, and a malformed suggestion refused by the
  tool schema.
- `tolglow.rehearsal-test` adds a 40-beat-late agent whose breakdown reply is
  refused as stale, with no breakdown section ever played. A refused and a
  silent agent from the breakdown on leave the groove's session playing to the
  end: no stall, nothing pending or open, and the stop still holds and resumes
  without catching up. In a silent-agent concert, both takes keep all four
  anchors and accept every gesture.
- `tolglow.rehearsal.session-test` (a started show):
  - arming needs confirmation and the page's show identity, saves the replaced
    show, and re-arming the session's own show needs no confirmation;
  - a concert take starts, stops with the transport and keeps its receipts;
  - the review compares both takes, and a review of another choice runs
    without playing;
  - restore brings the red show back;
  - a silent agent at arm returns the request at once, is logged as timed out
    and leaves no thread, and a still-hanging round ends when the show stops;
  - a stop after the passage ended returns its status;
  - a new review supersedes the one still running.
- `tolglow.qa.rehearsal-browser` (headless Chrome 155, Metal, against the
  production build): the arm refusal and confirmation, moods, the seeded
  review (outcomes, twelve phrases), start, stop and the run's replies from
  Studio, read back at the REPL, and a REPL arm reaching the panel. This is not
  a foreground display or iPad Safari check.

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
