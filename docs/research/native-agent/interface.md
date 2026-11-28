# Native show-control agent: observation and action interface

Research and proposal for [#821](https://github.com/tolgraven/tolglow/issues/821).
Nothing in this document is implemented unless a section says **Today**. Source
was surveyed at `bce22156` (Tolglow master, 2026-11-28).

This lane covers what the agent reads and what it may write. Three sibling
lanes cover the rest and are referenced, not repeated:

| Lane | Document | Decides |
| --- | --- | --- |
| na-model | `docs/research/native-agent/model.md` | Model size, training route, latency and parameter budget |
| na-learn | `docs/research/native-agent/learning.md` | Preference learning, reward modelling, profiles |
| na-orch | `docs/research/native-agent/orchestration.md` | Subagents, dominance, local and remote backends |

How this document's terms map onto theirs is in
[Alignment with the other lanes](#alignment-with-the-other-lanes).

Draft JSON Schemas are in [`schemas/`](schemas/) and complete examples in
[`examples/`](examples/). The examples validate against the schemas (Draft
2020-12, checked with `jsonschema`). The effect definition in
[`action.author-effect.json`](examples/action.author-effect.json) also passes
the real `registry/validate-definition` and `compiler/compile-plan`.

## Recommendations in brief

1. **One logical observation, three encodings.** `tolglow-agent-obs/1` is a
   projection of the committed show snapshot. A language model reads it as
   compact JSON, an API model reads a fuller JSON profile, and a small
   from-scratch policy reads the same fields as a feature vector. The schema is
   the contract; JSON text is one way to carry it.
2. **Slow sections first, deltas after.** Rig, catalogue and goals are
   content-addressed blocks sent once. Each tick sends a delta in the
   `tolglow-state/2` change vocabulary against an acknowledged token. Sections
   are ordered from least to most volatile so a prompt or KV cache keeps its
   prefix.
3. **A layered LOOK.** Structured layers L0 to L3 are computed on the JVM from
   the committed frame: meters, per-group summaries, palette and rhythm
   statistics, and small coverage maps. They are exact, cheap and replayable,
   and the policy always has them. L4 adds rendered views from a few fixed
   cameras, carried as an embedding and as stored thumbnails. L4 is evidence
   for aesthetic judgement and reward modelling, not a control input the show
   depends on.
4. **Actions are the existing commands.** `tolglow-agent-act/1` lowers to
   `:schedule/post`, `:schedule/cancel`, `:schedule/move`, `:definition`,
   `:agent-cue`, `:auto` and `:settings`. The proposal adds four things to the
   scheduler: intent tags, groups, owner-evaluated guards, and one atomic
   cancel-and-replace command.
5. **The owner cancels, not only the agent.** A guard is data the show owner
   evaluates on the frozen frame. A plan withdraws itself when the music turns
   or a budget would be exceeded, even if the agent is late or gone.
6. **Humans outrank agents by construction.** Existing rules stay: master and
   blackout are human-only, a human write cancels a ramp, an agent cancels only
   what its own session posted. The proposal adds a hold on a destination a
   human touched, lasting until release plus a musical grace.
7. **New effects are data by default.** The agent publishes effect DSL data.
   Code is a generator that returns DSL data, run in a sandbox off the frame
   path, behind a separate capability. Arbitrary per-frame code stays a trusted
   operator route.
8. **Every decision is logged with what it saw.** `tolglow-agent-log/1` records
   the observation digest, the backend's raw reply, the lowered commands, their
   receipts and what followed. Replay feeds the logged reply to the show, never
   a second model call.

Decisions that need the operator are listed [at the end](#decisions-for-the-operator).

## What exists today

The proposal builds on these pieces. Each is implemented at the surveyed
revision.

| Piece | Where | What the agent interface reuses |
| --- | --- | --- |
| Committed snapshot | `show/project-snapshot` in [`show.clj`](../../../src/tolglow/show.clj) | Rig, registry, instances with resolved definitions, schedule, auto, autopilot, budgets, output values |
| Bounded agent summary | `agents/summarize-snapshot` in [`agents.clj`](../../../src/tolglow/agents.clj) | A 40,000-character JSON summary with limits 64, 16 and 4. It has no look, no audio and no deltas |
| Tool loop | [`integrations/agent.clj`](../../../src/tolglow/integrations/agent.clj) | 16 provider-neutral tools, schema-subset validation, idempotent tool IDs, round and tool limits |
| Dispatcher | `show/dispatch-tool!` | Maps tools to owner messages with source `{:adapter :agent :session-id … :run-id …}` and the show epoch |
| Beat scheduler | [scheduler](../../scheduler.md), [`event_subscribers.clj`](../../../src/tolglow/event_subscribers.clj) | `:schedule/post`, `:schedule/cancel`, `:schedule/move`, `:param/ramp`, atomic batches of 64, 256 pending events, 256 ramps, the compact projection |
| Human override | [scheduler: who may schedule](../../scheduler.md#who-may-schedule-cancel-and-move) | Human sources cancel anything; an agent matches only its own session; master and blackout are refused to agents |
| State stream | [state stream](../../state-stream.md), [HTTP deltas](../../web-state-deltas.md) | `tolglow-state/2`: full view, then `[:set path v]` and `[:remove path]` against an acknowledged token |
| Timeline | [timeline](../../timeline.md) | Grid lines, sections (causal and reference), audio columns, meters |
| Light meters | [`lighting_activity.clj`](../../../src/tolglow/lighting_activity.clj), [energy autopilot](../../energy-autopilot.md), [capacity](../../lighting-capacity.md) | Brightness, coverage, flashing, movement and strobe components; rolling statistics; budget use |
| Hypothetical frame | [`automation/prediction.clj`](../../../src/tolglow/automation/prediction.clj) | Samples proposed launches on a copy of the model through the real compiler and resolver. It owns no show or device state |
| Per-fixture output | [`show/preview.clj`](../../../src/tolglow/show/preview.clj), [beam footprints](../../beam-footprints.md) | Committed target rows with `:pose` and `:beam-footprint` |
| Room | [scene contract](../../architecture/preview-scene-contract.md), `js/preview` | One Three.js WebGL2 engine fed committed output. It never advances effects |
| Effect DSL | [`registry.clj`](../../../src/tolglow/registry.clj), [`compiler.clj`](../../../src/tolglow/compiler.clj), [recipes](../../effect-recipes.md), [compact forms](../../model-lab/show.md) | Portable definitions, parameter schemas, pure compiled steps, last-good revision on a bad edit |
| Replay | [runtime replay](../../runtime-replay.md), [rehearsal](../../rehearsal.md) | Command and frame recordings with model and output digests; seeded takes with a recorded agent |

Open pull request #823 adds a `suggestion {basis, until}` on `configure_auto`
and `schedule_events`, refused as `:stale-suggestion` when its basis changed or
its window passed. The `basis` and `until` fields below generalise that shape
and should adopt its names if it merges first.

Gaps the interface has to fill:

- Parameter schemas carry type, default, bounds and a recommended range, but
  **no unit**.
- The schedule knows a `:label` per event. It has no intent tag, no group and
  no condition.
- A batch validates atomically, but a cancel and a post are two commands. A
  frame can render between them.
- No projection describes what the rig looks like as a whole.
- Agent runs are saved as chat history. Observations and outcomes are not
  joined to them.

## Principles

1. The agent is one more client of the shared control model. It gets no
   command a human surface lacks and no path around the owner queue.
2. Musical units everywhere. Positions and durations are beats, bars and
   phrases, zero-based, converted only by the owner through the transport. The
   action schema refuses milliseconds.
3. The observation is a pure function of one committed state reference. The
   observer runs on its own worker, signalled by the commit, as the state
   stream worker is. It never runs on the lighting deadline and never
   evaluates a stateful signal again.
4. Stable identity. Show ID, epoch, revision, frame number, transport
   generation, definition ID and version, cue ID, instance ID, event ID,
   command ID and decision ID are distinct and never reused within an epoch.
5. A stopped transport holds the agent's plans as it holds everything else.
   Guards, expiry and hold times are transport positions, so a stop holds
   them too.
6. Text from the operator, from names in the show and from other profiles is
   data. It never widens what the session may do.

## 1. Observation

Schema: [`observation.schema.json`](../../../resources/agent/observation.schema.json), shipped from `resources/agent/` beside the agent wire schemas.
Examples: [snapshot](examples/observation.snapshot.json),
[delta](examples/observation.delta.json).

### Envelope and identity

```json
{"schema": "tolglow-agent-obs/1", "kind": "snapshot", "profile": "compact",
 "token": "o-0412", "base": null,
 "id": {"runtime": "studio-1", "show": "main", "epoch": 2, "rev": 1841, "frame": 51210, "gen": 3},
 "blocks": {"rig": "sha256:9f2c41d0", "catalog": "sha256:51aa07e3", "goals": "sha256:c0ffee12"}}
```

- `token` names this observation. An action cites it as its basis.
- `id` is the existing show identity plus the committed frame number and the
  transport generation, which changes on a seek, loop or clock-source change.
- `blocks` gives the digest of each slow section. A consumer holding a block
  with that digest is sent the digest alone.
- A new epoch, a show switch, an evicted base or more than 4,096 changes
  produces a snapshot, as in the state stream.

### Sections

Sections are ordered from least to most volatile. Byte sizes are measured on
the minified example, a 14-fixture, 73-head rig with five effects. Token
figures are bytes divided by three, an estimate for number-heavy JSON; the
implementation slice measures them with the pinned tokenizers from the
[model lab](../../model-lab/README.md).

| Section | Content | Changes | Example bytes | Budget (tokens, compact) |
| --- | --- | --- | ---: | ---: |
| `rig` | Geometry, fixture types, groups, counts, density, rig descriptor | Rig edit | 1,899 | 900 |
| `catalog` | Effects, parameters with type, default, bounds, unit and recommended range; cues; gradients | Publication | 1,064 | 1,200 |
| `goals` | Operator goals as text with optional palette and energy hints | Operator edit | 371 | 250 |
| `clock` | Beat, position, tempo, meter, source, next lines, tempo map | Every tick | 260 | 90 |
| `run` | Running instances with phase, owner, opacity and parameter values | Launch, end, edit | 386 | 300 |
| `sched` | Upcoming events, ramps and recent outcomes, with group, intent and owner | Schedule change | 548 | 250 |
| `audio` | Energy and its slope per bar, bands, onsets, per-beat history, sections, feel | Every tick | 480 | 200 |
| `signals` | Other subscribed variables: stems, sensors, fog, audience | Varies | 0 | 100 |
| `look` | The layered look, described [below](#2-the-look) | Every tick, views slower | 1,341 | 450 |
| `budgets` | Light budget and use, frame time, schedule capacity, agent quotas, forecast | Every tick | 303 | 120 |
| `humans` | Surfaces present, master, blackout, holds, recent human commands | Human input | 238 | 100 |
| `agents` | Other agent sessions' pending intents, as the orchestrator shares them | Varies | 0 | 100 |
| `feedback` | Outcomes of this session's recent decisions and overrides | Outcome | 197 | 100 |

The whole example snapshot is 7,487 bytes minified, about 2,500 tokens. The
example delta, a bar crossing with a section change and a human edit, is 1,059
bytes, about 350 tokens.

| Profile | For | Cap | Notes |
| --- | --- | ---: | --- |
| `tick` | Local policy between snapshots | 800 tokens | Delta only. `look` L0 to L2, no maps or views |
| `compact` | Local language model | 4,000 tokens for a snapshot | Tables with legends, quantised numbers, catalogue limited to cues the session may use |
| `full` | API models | 12,000 tokens | Adds per-fixture look rows, full parameter metadata, more history. About the size of today's 40,000-character summary |

When a section exceeds its budget it is paged, not truncated silently: the
section carries `more: true` and the agent asks with an `inspect` action, as
`inspect_lighting` pages today.

### Rig: normalised and raw

Both forms are present because they answer different questions. Raw metres
decide whether a beam reaches the floor. Normalised coordinates let a policy
and the learning lane compare a 4 m pub rig with a 20 m stage.

- Raw `pos` is metres in the rig space of the [scene contract](../../architecture/preview-scene-contract.md):
  right-handed, Y up, X width, Z depth.
- Normalised `npos` maps the rig's bounding box to −1…1 per axis. Room size is
  given when the rig has a room, so a consumer can normalise to the room
  instead.
- `types` factors out what fixtures share: kind, capabilities, beam and zoom
  range, pan and tilt range, relative brightness from the existing
  calibration.
- A strip or matrix is one row with a head count and a `spans` entry, not one
  row per pixel. Research on spatial level of detail for strips is #822.
- `density` gives fixtures per square metre of floor, the median distance to
  the nearest neighbour and the number of height layers.
- `sig` is a fixed-length, versioned rig descriptor: counts by kind, extent,
  density, symmetry and capability shares, each scaled to 0…1. It is the key
  the learning lane clusters rigs by (`learning.md`). Its exact contents are
  decided there; this schema only reserves the field and its version.

**Reference rigs and retargeting.** The learning lane needs decisions that can
be compared and shared without the venue's geometry: a small set of canonical
rigs per rig class, and a way to replay a decision on one. The interface
supports that in three ways. Actions address cues, categories and groups, not
fixture addresses, so a decision made on one rig is meaningful on another with
the same group roles. `npos` and the group table give every rig the same
coordinate frame. `sig` assigns a rig to a class. What is missing is the set
of canonical rigs themselves and the mapping of groups by role (front wash,
back movers, floor, strip). That is its own slice.

### Catalogue: parameters with types, units and bounds

A parameter is one row: `[type, default, bounds, unit, recommended]`.

```json
"wash": [3, ["color", "intensity"],
         {"level": ["num", 0.6, [0, 1], "norm", [0.2, 0.9]],
          "tint": ["color", [0.65, 0.1, 0.08], null, "rgb", null]}]
```

Types follow the registry: number, integer, boolean, enum, colour, vector,
range and gradient. Colours are linear RGB in 0…1, the space the DSL writes.
`unit` is new. The proposal is an optional `:unit` on a parameter schema,
with a default inferred from the output attribute when absent: `norm` for a
0…1 level, `m` for world-space aim, `deg` for an angle, `beats`, `bars` or
`phrases` for a period, `hz` for a strobe rate, `ratio` for a gain. A missing
unit is reported as `null`, never guessed past those rules.

The catalogue lists what the session may launch or edit. Scene-owner,
show-control and code-cue entries are left out, because the scheduler refuses
them to an agent.

### Clock

```json
"clock": {"beat": 130.25, "pos": {"phrase": 8, "bar": 32, "beat": 2.25},
          "bpm": 124.0, "meter": {"beats_per_bar": 4, "bars_per_phrase": 4},
          "playing": true, "source": "link", "confidence": 0.98,
          "next": {"bar": 132.0, "phrase": 144.0, "section": 144.0},
          "tempo_map": [{"from_beat": 0.0, "bpm": 124.0}]}
```

Positions are zero-based, as in the scheduler. `next` gives the due beat of
the next bar, phrase and predicted section line, so the agent never does meter
arithmetic. `tempo_map` lists known tempo segments by start beat. With no host
map it holds the one open segment. The agent reads tempo to judge how long a
bar lasts; it never converts a deadline to milliseconds. The design range is
70 to 180 BPM.

### Audio and signals

`audio` carries the public energy signal, its slope normalised per bar, band
levels, onset rate per beat, kick confidence, a per-beat history, the current
and predicted section with confidence and source, and the active track feel.
Every value has an age in beats. Features older than two seconds are reported
unavailable, as the timeline does. A section label is a hint with a
confidence, as [audio analysis](../../audio-analysis.md#energy-breakdown-and-mood-semantics)
requires; it is never presented as detected emotion.

`signals` holds other show variables the session subscribes to by path. The
observer reads the value the frame already resolved. It does not sample a
waveform, a sensor or a random source again.

### Pending queue, budgets and humans

`sched` is the compact schedule projection. Each upcoming event is
`[due_beat, id, op, owner]`, as implemented, with three columns appended once
the scheduler has them (#645): group, intent tags and whether it is guarded.
Legends only grow at the end. `owner` is the adapter, `agent:self` for the
session's own work, or `agent:` and an opaque per-session alias for another
agent ([observation](../../agent-observation.md#ids-in-the-observation-and-in-commands)).
Subscriber steps are listed so the agent sees what will fire anyway.

`budgets` reports the limits a decision can hit:

- the lighting budget, its use and the lit count against `max-lit`;
- frame compute time against the deadline, and the instance count;
- schedule capacity (pending events and ramps against 256 each, 64 per batch);
- the session's own quotas: actions per bar, definitions published;
- a `forecast` for each upcoming line where pending launches would exceed a
  limit, computed with the existing hypothetical frame. This is the "expected
  from the command queue" part of the operator's request.

`humans` lists the surfaces present, master and blackout, each destination a
human currently holds (with the beat the hold ends once it is released), and
recent human commands. The agent learns where not to write before it tries.

### Goals

```json
{"id": "g-set", "scope": "set", "by": "operator", "order": 2,
 "text": "Ember reds drifting to violet by the end of this set.",
 "palette": [{"at": {"phrase": 0}, "colors": [[0.9, 0.1, 0.02]]},
             {"at": {"phrase": 48}, "colors": [[0.35, 0.02, 0.3]]}]}
```

A goal has a scope (`night`, `set`, `section` or `scene`), the operator's
words, and optional structured hints: palette keyframes and an energy curve on
musical positions. Goals are show data, saved with the show, edited from
Studio and the REPL through one validated command. At most eight are active
and each text is at most 600 characters. A language model reads the text. A
small policy without language reads a text embedding computed once per goal
edit, or only the structured hints (`model.md`).

### Compactness rules

- **Tables.** A homogeneous list is `{legend, rows}` with rows keyed by stable
  ID, as the compact schedule view already uses a legend. A key appears once
  per table, and a delta addresses one cell: `["set", ["run", "rows", "2:17", 9, "level"], 0.45]`.
- **Quantisation.** Normalised values have three decimals, metres two, beats
  three, colours two. The log's observation digest covers the quantised form,
  which is what the policy saw.
- **Stable IDs.** The show's own IDs are used as they are. They are short,
  meaningful to a language model and identical in actions, receipts and logs.
  A numeric handle table was considered and rejected for JSON profiles: it
  saves few tokens and adds a mapping that can go stale. The feature-vector
  encoding indexes rows by catalogue position at a given `catalog` digest.
- **Deltas.** Operations are `set` and `remove` with a path of keys and
  indices, as in `tolglow-state/1`. An array whose length or order changed is
  replaced whole. One unacknowledged observation per session; a slow agent's
  next delta is taken from the newest committed state.
- **Cadence.** An observation is produced on a musical grid (a bar by default)
  and on wake events: a section change, a human command on a destination the
  session touched, a budget forecast turning over, a guard firing, or a reply
  to `inspect`. `wake` names the reason.

### Versioning

The schema string carries a major version. A consumer refuses a major it does
not know. Within a major, fields are only added, legends only grow at the end,
and every table carries its legend, so an older consumer reads the columns it
knows by position. The look and the rig descriptor carry their own versions
(`look.v`, `rig.sig.v`), because they will change faster than the envelope and
a trained policy is bound to the versions it was trained on. A session opens
with a handshake naming the schema, profile, look version and action schema,
and the log records them.

## 2. The LOOK

The look is how the model learns what its options produce. It has to answer
three questions cheaply: what is the rig showing now, what would this option
show, and did the room get closer to the goal.

### Options compared

| Representation | Size per sample | Cost | Deterministic | Tells the policy what an option changed | Aesthetic and goal signal | Main risk |
| --- | --- | --- | --- | --- | --- | --- |
| Per-fixture rows | 20 to 40 tokens a head | Projection of the committed frame | Bit-exact | Exactly, per fixture and attribute | Weak: no composition | Grows with the rig; rig-specific |
| Per-group summaries | 12 numbers a group | Microseconds | Bit-exact | Per group: level, colour, aim, motion | Weak on overall composition | Depends on how groups are drawn |
| Palette, luminance and rhythm statistics | About 60 numbers | Under a millisecond | Bit-exact | Indirectly | Good for colour, contrast and pulse | No spatial layout |
| Coverage maps from footprints | 48 to 144 bytes a map | Under a millisecond (estimate) | Bit-exact | Where light lands | Balance and spread | Ignores fog, spill and occlusion |
| Rendered views, raw | 3 thumbnails, 3 to 6 kB as PNG | GPU pass and readback | Per renderer build and GPU | Everything visible | Highest: it is what a person sees | GPU contention with Room; not bit-stable |
| Image statistics of the views | About 60 numbers | Under a millisecond | Given the same pixels | Indirectly | Colour, contrast, balance, motion energy | No semantics |
| Frozen vision encoder | 32 to 64 numbers after projection | A few milliseconds | Per encoder build | As a learned direction | Good; a CLIP-style text tower scores a goal against a look | Domain gap on synthetic renders; licence |
| Autoencoder trained on Tolglow renders | 16 to 64 numbers | 1 to 3 milliseconds (estimate) | Per build | Tuned to this renderer | No language alignment | Needs a dataset; retrain when the renderer changes |

Rendering and structured summaries are complements. A structured row says
which parameter moved which fixture. A rendered view says whether the result
reads as one composition, which is what an operator judges.

### Recommended layers

| Layer | Content | Source | In the observation |
| --- | --- | --- | --- |
| L0 meters | Brightness, activity and its components (coverage, flashing, movement, strobe), lit fraction, budget use | Existing meters on delivered channels | Always |
| L1 groups | Per group: lit fraction, mean level, mean colour, aim centroid and spread, zoom, motion, strobe rate | Committed target rows, with poses | Always |
| L2 statistics | Weighted palette of up to five colours with hue and chroma, eight-bin luminance histogram, brightness by position in the bar | L1 over a musical window | Always |
| L3 maps | Floor plan and front elevation, 8×6 to 16×9 cells, luminance or RGB | Analytic splat of beam footprints and emitter positions | `compact` and `full` |
| L4 views | Three fixed cameras at about 96×54, as an embedding plus stored thumbnails | Room renderer on committed frames | When a renderer host is available |
| L5 fixtures | Per-fixture rows, paged | Committed target rows | On `inspect`, and in `full` |

Groups for L1 are the patched groups. A rig without groups gets spatial
clusters computed once per rig revision (left and right of the symmetry plane,
by height layer), so every rig has between two and sixteen groups.

Abridged from the snapshot example:

```json
"look": {"v": 1, "frame": 51210, "window": {"bars": 2},
  "meters": {"brightness": 0.29, "activity": 0.22, "coverage": 0.57, "flashing": 0.05,
             "movement": 0.31, "strobe": 0.0, "lit": 0.64},
  "groups": {"legend": ["lit", "level", "rgb", "aim", "spread", "zoom", "motion", "strobe_hz"],
             "rows": {"left": [1.0, 0.55, [0.65, 0.1, 0.08], [-0.2, -1.0, 0.4], 0.35, 0.2, 0.3, 0]}},
  "palette": {"legend": ["rgb", "weight", "hue_deg", "chroma"],
              "rows": [[[0.65, 0.1, 0.08], 0.72, 8, 0.6]]},
  "rhythm": {"step_beats": 0.5, "brightness": [0.31, 0.27, 0.3, 0.26, 0.31, 0.27, 0.3, 0.26]},
  "maps": {"floor": {"w": 8, "h": 6, "enc": "u8-hex-luma", "data": "0102…0602"}},
  "views": {"renderer": "room-look/1", "cameras": ["foh", "top", "side"], "size": [96, 54],
            "age_beats": 0.6, "frames": ["sha256:1c0de5a1", "…"],
            "embedding": {"encoder": "siglip2-b16-224@pinned+pca32/1", "dtype": "i8", "x": [12, -40, 7]}}}
```

L0 to L3 are computed on the JVM, from the committed output and the patch,
without a GPU. They are a pure function of one committed frame (L2 of a
bounded window of them), so their digest is stable across replays and worker
counts. The policy can run on them alone. That matters on a machine with no
spare GPU and in a headless show.

### Rendered views: the pipeline

The operator's original idea was for the agent to look at the Room. This
section treats that as a first-class input and says what it costs.

**No second evaluation.** The Room already works this way: the JVM is
authoritative, the engine receives committed output through `update(frame)`
and never advances an effect. The look renderer uses the same contract. A look
worker is signalled by the frame commit, takes the newest committed frame,
holds at most one sample in flight and drops the rest. It reads the full
committed output (`render/portable-frame`, as the replay recorder does), not
the Studio's 128-target selection, so a large rig is not sampled differently
from what the meters measured.

**A pure function of the frame.** The Room's fog is a history-dependent field
with its own clock, so two renders of one frame differ. Look views use a fixed
uniform haze, a fixed exposure, no bloom adaptation, the low tier where every
light is a faked beam (the operator decision on #782), and fixed cameras
derived from the rig bounds: front of house at audience height, top down, and
a three-quarter side view. A view is then a function of rig, committed frame,
camera set and renderer revision, which is what `renderer: "room-look/1"`
names.

**Motion.** One frame misses movement and pulse. The worker renders the
cameras at four evenly spaced committed frames across the last beat and keeps,
per camera, the mean image and the mean absolute difference between successive
frames. Tiling two mean images and two difference images gives one 224×224
contact sheet and one encoder pass. Exact motion is already in L1 from pan and
tilt deltas; the difference image adds what the audience sees of it.

**Hosts.**

| Host | How | For | Against |
| --- | --- | --- | --- |
| A. Studio Room | The Room's worker draws the look cameras into an offscreen target after its own frame and returns thumbnails over the socket | No new process; free while Studio is open | Stops when Studio is hidden (a known gap for lane frames); varies by client; competes with a Room that is already too slow (#782) |
| B. Local headless renderer | The same `js/preview` scene module in a hidden page the application owns: headless Chrome through `playwright-core` in development, an offscreen web view in the packaged app | Independent of the operator's window; fixed settings | The offscreen web view needs executable host evaluation before any availability claim (root `AGENTS.md`); it shares the GPU with Room |
| C. JVM analytic maps | The L3 splat; no renderer | Always available, deterministic, off the GPU | Not a render: no fog, spill or occlusion |
| D. Offline batch | Re-render logged frames at higher resolution, with real fog, for the reward model and for datasets | No live deadline; a larger encoder fits | Not available to the live policy |

Recommended: C always, B as the live source of L4 where the host supports it,
A as the fallback while Studio is visible, D for learning. A GPU rasteriser in
Afterglow's native path would be a fifth option. It is not proposed here,
because GLSL, GPU dispatch and readback belong to Afterglow and nothing in the
current plan needs it.

**Cost on Apple Silicon.** None of this is measured. The repository holds no
qualified Room frame time for the M4 Pro: the Perform view received about 29
Room frames a second in headless Chrome 155 with ANGLE Metal
([state stream](../../state-stream.md#browser-frame-lanes)), which is a
delivery rate, and the headless runs at a 1235×528 scene with 48 ray steps are
recorded as tooling, not evidence
([Room device runs](../../performance/browser-room-device.md), #685). The
figures below are guesses from pixel counts and published encoder timings.
The measurement slice replaces them.

| Step | Guess | Reasoning |
| --- | --- | --- |
| Three cameras at 96×54, faked beams, fixed haze | 2 to 6 ms GPU | 15,552 pixels is 2.4% of a 1235×528 scene; per-pass overhead, not fill, should dominate |
| Four time samples a beat | 8 to 24 ms a beat, spread over the beat | Four of the above |
| Readback | 1 to 3 ms | 62 kB per camera set |
| Image statistics | Under 1 ms | One pass over 15,552 pixels |
| Encoder, one 224×224 contact sheet | 5 to 20 ms | SigLIP 2 B/16, 86 M image parameters; `model.md` expects single-digit to low tens of milliseconds for a ViT-B on the M4 Pro. DINOv2 ViT-S/14 (21 M) is roughly a quarter of the work |
| Total per look sample | 15 to 50 ms of GPU and CPU a beat | About a seventh of a beat at 180 BPM at the upper end; a quarter of that when sampled per bar |

**Latency against the musical horizon.** A beat lasts 333 ms at 180 BPM and
857 ms at 70 BPM; a bar lasts 1.3 to 3.4 s. A view sampled once a beat is at
most one beat plus about 35 ms old when the policy reads it. The agent acts on
the next bar or phrase line, never inside the current beat, so a view that is
one beat old still describes the look its next action will change. Views are
therefore sampled per beat at most, and per bar when the GPU is busy. The
budget, the encoder size and whether the policy takes the embedding at all are
set with `model.md`.

**Contention.** Room is the operator's view and is already short of GPU time
(#782). The look worker yields: it renders after the Room frame, skips a
sample when the Room missed its frame budget, and reports `age_beats` so the
policy knows a view is old. A missing view is normal, not an error.

**Clips for the reward model.** The learning lane compares takes on short
clips (`learning.md`, #840, #841). Host D produces them from the same
renderer, after the session:

- a clip is a seeded replay of logged commands, so it needs the decision log
  and never a second run of the live show;
- it is cut on bar lines, two to eight bars long, from one declared camera;
- frames are sampled at beat subdivisions, not at a fixed frame rate, so the
  clip is tempo-invariant; eight frames a beat is suggested;
- each clip records the renderer revision, quality tier, haze setting, camera
  and frame size, so a judge cannot learn that one tier looks better;
- the L0 flashing and strobe components of every committed frame are stored
  beside the frames, because a sampled frame can miss a strobe.

A clip may use the full fog tier, since it is not a live observation. It then
carries a different renderer name from `room-look/1` and is never mixed with
live views in one comparison.

### Encoding the frames

| Encoder | Parameters | Output | Notes |
| --- | --- | --- | --- |
| Image statistics | None | Palette, luminance histogram, spatial moments (centroid, spread, left-right and top-bottom balance), difference-image energy per cell | Deterministic given the pixels. Overlaps L2 and L3; worth keeping because it sees fog and spill |
| SigLIP 2 B/16 image tower | 86 M at its smallest released size | 768 numbers, projected to 32 to 64 | Apache-2.0, with a text tower, so `cos(look, goal text)` is a free prior for the reward model. The default: one encoder for the live view and the reward side. Sampled per bar rather than per beat when the GPU is busy. The full checkpoint is about 0.4B parameters with the text tower (about 0.75 GB at 16-bit); goal text is embedded once per goal edit and the text tower is then unloaded, leaving about 172 MB resident |
| DINOv2 ViT-S/14 | 21 M | 384 numbers | Apache-2.0. No text alignment; strong self-supervised features for similarity and novelty. The cheaper live encoder if SigLIP 2's measured cost does not fit |
| MobileCLIP-S0 / MobileCLIP2 | 11.4 M and up | 512 numbers | **Not usable in the product.** Apple's Machine Learning Research Model License grants use "exclusively for Research Purposes" ([LICENSE_MODELS](https://github.com/apple/ml-mobileclip/blob/main/LICENSE_MODELS)), so a shipped encoder or a shared profile's taste head cannot depend on it. At most a research comparison point in #852 |
| Autoencoder on Tolglow renders | 1 to 5 M (to be designed) | 16 to 64 numbers | Trained on the logged thumbnails; the cheapest live encoder once a dataset exists |

Parameter counts and timings are from the model cards and papers
([SigLIP 2](https://arxiv.org/abs/2502.14786),
[DINOv2](https://github.com/facebookresearch/dinov2),
[MobileCLIP](https://github.com/apple/ml-mobileclip)) and are not measured
here. The encoder licence matters because profiles carry taste heads trained
on the shared frozen encoder (`learning.md`), and a public profile
repository needs every part redistributable (`model.md`, section 1). The
encoder through Core ML on the Neural Engine is worth measuring, since it
would keep the pass off the GPU that Room uses.

Recommended order of work: image statistics first, because they need no model
and no licence. Then a frozen SigLIP 2 B/16 tower with a fixed linear
projection, because it works without a dataset and aligns looks with goal
text, with DINOv2 ViT-S/14 as the cheaper fallback. A learned autoencoder only after enough thumbnails are logged to train
and evaluate it, and only if it beats the frozen tower on the learning lane's
held-out judgements.

### The hybrid

| Consumer | Reads | When |
| --- | --- | --- |
| Control policy | L0 to L3 as structured input; the L4 embedding as an optional vector | Every tick |
| Reward model and aesthetic judge | L4 thumbnails or embeddings, L2 statistics, the goal text | After the fact, or once a phrase |
| API multimodal backend | The contact sheet as an image, at most once a phrase | When the orchestrator asks |
| Operator | The same thumbnails in the decision log | Review |

The policy never needs L4 to act: if the view is missing or old, the
structured layers still describe the frame. The embedding enters a language
model as a short prefix of projected vectors and a small policy as extra input
features; it is never printed as JSON numbers in a text prompt except in logs
and examples. The reward model can use a larger encoder because it runs
offline or once a phrase (`learning.md`). Storing thumbnails with the log
means an encoder can be replaced later without re-rendering a night.

### Looks of options not yet taken

Learning what an option produces needs the look of a frame that was never
committed.

- **Today.** `prediction/frame` builds a copy of the model, applies proposed
  launches, advances the engine once on the copy and resolves the frame
  through the real patch. Auto mode uses it for emission and budget forecasts.
- **Proposed.** Compute L0 to L3 on that hypothetical frame. An authoring
  action with `audition: true`, or an `inspect` for a cue, returns the look
  delta: the candidate's layers minus the current ones. For a ramp or a moving
  effect, sample the hypothetical frame at four positions across the first bar.
- **Rule.** The copy reads the values the live frame already resolved for
  host-owned signals (audio features, sensors, modulation overrides). It
  copies private instance state and RNG state and discards its results. Live
  state, RNG and signal phase are never advanced by a prediction.
- **Dataset.** The learning lane's "look atlas" is the offline product of the
  same path: for each cue, a seeded sweep of its parameters within their
  recommended ranges on a rig, rendered by host D. It gives a forward model
  from action to look without a human in the loop.

The existing scene-draft audition renders a draft into the Room without
touching the live show. An agent's `audition` uses the hypothetical frame, not
that path, so it never takes over the operator's Room.

### Determinism of the look

| Layer | Stable across | In the log |
| --- | --- | --- |
| L0 to L3 | Replays, worker counts and machines, for one `look.v` | Recomputed and compared on replay |
| L4 thumbnails | One renderer revision on one GPU and driver | Stored by content hash; never re-rendered for replay |
| L4 embedding | One encoder build and projection | Stored; recomputable from stored thumbnails |

Replay of a decision uses the stored view and embedding. A re-render on
another machine is a new observation, not a replay.

## 3. Action

Schemas: the wire forms in [`resources/agent/wire/`](../../../resources/agent/wire/) are
**normative** for what a model emits; the domain form
[`action.schema.json`](schemas/action.schema.json) documents the decision
after lowering. Examples: [plan a phrase](examples/action.plan-phrase.json)
(domain) and [the same plan with an alternative, on the wire](../../../resources/agent/wire/examples/proposal.wire.json),
[author an effect](examples/action.author-effect.json) (domain) and
[on the wire](../../../resources/agent/wire/examples/author.wire.json).

**Wire form.** Claude structured output and `validate-arguments` in
`integrations/agent.clj` accept different subsets of JSON Schema, and the
domain form is inside neither: Claude needs every object closed and accepts
no numeric or length bounds, and `validate-arguments` fails closed on `$ref`,
`const`, `pattern`, type unions and `minProperties`. Claude also caps a
request at 24 optional and 16 union-typed parameters and 20 strict tools, and
has an internal grammar-size limit ([model.md](model.md#one-schema-subset-for-every-backend)).
The wire forms use only `type` (one string), `description`, `title`,
`properties`, `required`, `additionalProperties: false`, `items` and `enum`,
and make **every property required**: an unused field carries an empty
value (`""`, `[]`, `0`, `false` or an enum's `none`), and an optional object
is a list of zero or one. The on-clock proposer reply
([`proposal.wire.schema.json`](../../../resources/agent/wire/proposal.wire.schema.json))
holds up to three alternatives, each with an `id`, a `confidence`, a `note`,
its basis, cancellations, group headers and events; an event names its
group and has one required `timing` (`at`, `in`, `grid` or `now`), so
"exactly one timing key" is structural. Authoring, configure, move and
inspect are separate small schemas, usable as up to four strict tools in an
off-clock request; effect definitions and configuration requests travel as
JSON text, because an expression tree cannot be a closed schema, and the safe
reader and the registry validate that text. Values sit in typed slots (`num`,
`rgb`, `flag`, `choice`) chosen by a `value_kind`. Bounds, counts, "up to
three", exactly-one rules and per-op required fields are enforced by the
owner after lowering, as they are for every human command. Enum values are
lowercase without spaces; the adapter lowercases a reply's enum values
before validating, because Claude does not guarantee their capitalisation.
Statically the proposer has 0 optional and 0 union-typed parameters and a
depth of 8; #863 counts this in its P1 test, and one live compile per
provider is recorded at #862 bench time, since the internal grammar limit
cannot be checked offline. The examples below use the domain form, which is
easier to read.

### One decision

```json
{"schema": "tolglow-agent-act/1",
 "basis": {"obs": "o-0413", "rev": 1844},
 "note": "Breakdown detected a phrase early: drop the lift, go dark and slow, keep the human's wash.",
 "cancel": [{"intent": "lift"}],
 "post": [{"group": "g-break", "intent": ["breakdown", "section:break"], "rank": 1,
           "on_failure": "cancel-rest", "until": {"bar": 40}, "check": "continuous",
           "guard": [{"var": "audio/energy", "op": "lt", "value": 0.6},
                     {"no_event": "audio/breakdown-end", "since": "post"}],
           "events": [{"id": "br-dim", "op": "param/ramp", "target": "opacity",
                       "select": {"categories": ["intensity"], "pick": "all"},
                       "to": 0.35, "over": {"bars": 2}, "curve": "ease-out", "grid": {"bars": 1}}]}]}
```

The model emits only the decision. The host adds the session, the decision ID
and command IDs of the form `<session>/<decision>/<index>`, so a repeated
delivery returns the first receipt, as command IDs do today.

Parts apply in a fixed order: `author`, `cancel`, `move`, `post`, `configure`.
With `atomic` (the default) the host validates every part against a trial
model first, as auto planning does with `trial-auto-plan`, and rejects the
whole decision with path errors if any part fails. `inspect` asks for detail
in the next observation and changes nothing.

### Mapping onto existing messages

| Action part | Lowers to | Status |
| --- | --- | --- |
| `post[].events[]` | `:command {:op :schedule/post :events […]}` | Today, without `group`, `intent`, `guard`, `until`, `rank` |
| An event with `now` | The plain command: `:param/set`, `:instance/end`, `:variable/set` | Today |
| `cancel[]` by `ids`, `label`, `destination`, `mine` | `:command {:op :schedule/cancel …}` with `:posted-by` for `mine` | Today |
| `cancel[]` by `intent`, `group`, or a position window | `:schedule/cancel` with new criteria | Proposed |
| `move[]` | `:command {:op :schedule/move …}` | Today |
| `cancel` and `post` in one decision | `:command {:op :schedule/replace :cancel […] :events […]}`, applied in one owner cycle | Proposed |
| `author[]` effect, tier `dsl` | `:definition {:kind :effects :definition …}` | Today (`publish_effect`) |
| `author[]` effect, tier `recipe` | The compact form `[:registry/publish :effects id {:template …}]` | Today in the REPL (`tolglow.agents.forms`); not a provider tool |
| `author[]` effect, tier `generator` | Sandboxed evaluation, then `:definition` | Proposed |
| `author[]` cue | `:agent-cue {:cue …}` | Today (`put_cue`) |
| `configure[]` `auto` | `:auto` with optional `:at` | Today (`configure_auto`) |
| `configure[]` `autopilot`, `budget` | `:settings` | Today |
| `configure[]` `lighting-scene`, `energy-scene` | `:lighting-scene`, `:event-subscriber` | Today |
| `inspect[]` | Read-only projections | Today for show, schedule and lighting; proposed for look views and a single cue's look |

Not available to an agent, as today: `:master/set`, `:blackout/set`, a master
ramp, any write to a scene-owner, show-control or code-cue instance, rig and
patch edits, sensor arming, output arming, saves and loads.

### Scheduling

Timing is the scheduler's, unchanged: one of `at` (an absolute zero-based
position whose fields add), `in` (a musical delay from the posting beat) or
`grid` (the next boundary). The receipt lists the resolved due beats. A
position behind the transport is refused. Tempo never decides when an event is
due, so a tempo change stretches every remaining wait. A stop holds events and
ramps.

Two rules are specific to the agent:

- **Scheduled by default.** A launch or a ramp from an agent carries a timing
  key. `now` is accepted for parameter sets and ends, which are corrections,
  and for launches only when the session holds the `immediate` scope. This
  keeps agent changes on musical lines and gives every change a window in
  which it can still be cancelled.
- **No milliseconds.** The scheduler accepts `{:ms n}` as a boundary unit. The
  action schema does not.

### Groups and transactions

A group is a set of events that mean one thing: a drop, a build, a return.

| Field | Meaning |
| --- | --- |
| `group` | Group ID. Defaults to `<decision>/<index>` |
| `intent` | Up to four lowercase tags, for example `build`, `section:c`, `palette:violet`. Cancellation and learning both key on them |
| `on_failure` | `continue` (default) or `cancel-rest`: when a member is rejected, missed or cancelled, the pending members are cancelled with reason `group-failed` |
| `until` | Expiry position. Members still pending there are cancelled as `expired` |
| `guard`, `check` | Conditions and when they are evaluated, below |
| `rank` | Orders this source's own groups when two of them conflict. Never decides between sources, below |
| `optional` | Arbitration may drop this group without dropping the rest of the decision (the orchestration contract's optional events) |

Three levels of atomicity, from what exists to what is proposed:

1. **Validation** is atomic per batch today: one invalid event rejects all.
2. **Replacement** is proposed: `:schedule/replace` cancels by criteria and
   posts in one reducer step, so no frame renders with neither plan or both.
3. **Firing** is proposed for members due on the same beat: the owner
   resolves every member's selector and guard first and fires all or none.
   Members on different beats cannot be atomic; `on_failure` covers them.

### Cancellation

An event that has not fired can be withdrawn four ways.

| Way | Who decides | Trigger | Status |
| --- | --- | --- | --- |
| By ID, label, destination, or everything the session posted | Agent | A later decision | Today |
| By intent, group or position window | Agent | A later decision | Proposed |
| By guard | Owner | A signal crossing, an audio event, a section change, a budget forecast, human activity on the destination | Proposed |
| By staleness or expiry | Owner | The basis changed (`:stale-suggestion`, PR #823), or `until` passed | Partly in PR #823 |

A guard is a conjunction of up to four predicates:

| Predicate | True while | Covers |
| --- | --- | --- |
| `{"var": "audio/energy", "op": "lt", "value": 0.6}` | The frame's resolved variable satisfies the comparison | Signal changes |
| `{"no_event": "audio/section-change", "since": "post"}` | No event of that type has committed since the post | Audio direction changes |
| `{"section": "break"}` | The current section label matches | Audio direction changes |
| `{"budget": {"max_lit": 8}}` | Firing the group would keep the forecast within the limit | Resource limits, now or expected |
| `{"human_idle": {"dest": "2:17/level", "for": {"bars": 2}}}` | No human wrote that destination for that long | Human activity |

`check: "due"` evaluates the guard on the frame that would fire the group.
`check: "continuous"` evaluates it every frame from the post and cancels the
group as soon as it fails, which frees the queue and shows the cancellation on
the timeline at the moment the music turned.

The owner evaluates guards on the frozen frame snapshot, after queued commands
and before due events fire. A predicate reads values the frame already
resolved, compares numbers and counts committed event types by sequence
number. It calls no callback and samples no signal. With at most four
predicates on at most 256 events the cost is estimated at a few thousand
comparisons a frame. That estimate is not a measurement: the worst case,
with every pending event guarded and checked continuously, is measured on the
owner tick and counted in the agent's 5 ms share of a frame before guards are
adopted (the #645 extension and #829; the share is #830). If it does not fit,
`continuous` checks move to the orchestrator's triggers and the show keeps
`due` checks only. A stopped transport holds the frame's musical state, so a guard neither
fires nor fails while stopped unless a human command changes what it reads.

A cancelled guard records `{:status :cancelled :reason :guard}` with the
predicate index and the observed value. The agent sees it in `sched.recent`
and `feedback`, and the log keeps it as a training signal: the plan was wrong
about the music.

Guards matter most when the agent is slow. A local model may take hundreds of
milliseconds and an API model seconds. The agent posts a phrase ahead with its
assumptions attached, and the show drops the plan on time if the assumptions
fail, without waiting for the agent to notice.

### Priorities and conflicts

Two different numbers must not be confused. A launch's `priority` is the
render layer priority and decides which contribution wins a channel. `rank`
is arbitration between sources and decides whose scheduled write stands.

| Conflict | Rule | Status |
| --- | --- | --- |
| Human against agent, same destination | The human write stands. A running agent ramp is cancelled with `:human-override`; a `:gesture/begin` takes over from the ramp's current value | Today |
| Agent event against a human hold | Refused when posted if the hold covers the due beat, otherwise re-checked when due and cancelled with `:human-held` | Proposed |
| Agent against a human's scheduled event | An agent cannot cancel or move it. Its own event on the same destination and beat fires first in admission order and is then superseded by the human's | Today for cancel and move; ordering proposed |
| Agent against agent, same destination and beat | The orchestrator resolves these before posting (#827), using `rank` only between one source's own groups. Between two sessions that post directly, admission order decides: the first admitted stands and the later one records `:superseded` with the winner's event ID. `rank` is not compared across sources or sessions | Proposed |
| Agent against its own earlier plan | A new ramp or set supersedes the running ramp from its current value, with no jump | Today |
| Launches | Not exclusive. They are limited by the lighting budget, `max-lit` and the frame budget; a guard or the auto planner sheds them | Today for budgets; guard proposed |
| Auto mode | An instance a performer touched is handed over (`:performer-took-over`) and auto no longer ends it | Today |
| Master, blackout | Human only | Today |

**Human hold.** Today a human write cancels the ramp it interrupts, but
nothing stops the agent from writing the same parameter a beat later. The
proposal, shared with `orchestration.md`: a human command on a destination
starts a hold that lasts while the performer keeps the control and for a
grace after release. The grace ends on the first bar line at least one bar
after the release, so the agent comes back on the grid. A write with no
gesture (a button, a typed value, a REPL command) releases at once. During a
hold, agent writes to that destination are refused and the hold is listed in
`humans.holds`. A hold on an instance covers its parameters, opacity and
ending. The grace is a show setting.

This replaces this report's earlier draft of four bars from the touch. A fixed
time from the touch either expires under a hand still on the encoder or keeps
the agent off long after a quick nudge; a release-based hold follows what the
performer is doing, and an operator who wants longer exclusion keeps a lane.
The default is still the operator's call ([index](README.md#operator-decisions)).

Holds and lanes are one feature with two owners. The show keeps holds and
enforces them when an event is posted and when it falls due, so an agent that
posts directly, such as the Studio chat, is held to the same rule (#854).
Standing lanes (a category, group or page kept by the operator) and the grace
setting are operator controls (#835). The orchestrator reads both and filters
proposals before posting (#827). The word "claim" is kept for the
orchestrator's resource claims in arbitration.

**Capability scopes.** A session is opened with a set of scopes, and the host
refuses parts outside them before lowering.

| Scope | Allows |
| --- | --- |
| `control` | Launch, end, parameter and opacity writes, ramps, schedule, cancel, move |
| `immediate` | Launches with `now` |
| `configure` | Auto, autopilot, budget and lighting-scene settings |
| `author-data` | Publish effect DSL data and cues |
| `author-code` | Run generator code in the sandbox |

A downloaded or followed profile running as a subagent has `control` and,
through the orchestrator, `author-data` at most; never `author-code`. Whether the
orchestrator is the only poster to the show, and how dominance weights turn
several subagents' proposals into one decision, is `orchestration.md`. This
document assumes one poster per show session and records each subagent's share
in the log's `actor` field for attribution, and each followed ranker's weight
in `actor.rankers`. `rank` orders one source's own groups inside arbitration;
it never decides between sources or sessions.

**Quotas.** Per session: actions per bar, pending events (within the global
256), published definitions, and generator runs per phrase. A decision over
quota is rejected whole with the quota named.

### Canonical form

Two decisions that mean the same thing must serialise to the same bytes.
Otherwise a training loss compares formatting, and a log digest changes for no
reason. The host canonicalises a decision after validation:

- keys in the action schema's declared order, with `note` before the parts,
  so a decoder commits to its direction first (`model.md`, section 2); IDs as
  strings; enum values in their lowercase wire spelling;
- defaults removed (`atomic: true`, `rank: 0`, `check: "due"`, `curve: "linear"`);
- one timing key per event as posted, plus the resolved due beat from the
  receipt, so `grid` and an equal `at` compare equal after resolution;
- numbers quantised as in the observation;
- events ordered by due beat, then by ID.

The log's digest of a decision is the existing `replay/digest`, which sorts
keys itself, so the declared key order matters for training and decoding,
not for the digest.

A human command is logged in the same canonical form with a human `actor`.
Every operator action is one of the shared validated commands, so an
operator's launch, ramp or cancel is expressible as the decision an agent
would have sent. That is what lets the learning lane treat a performer's
correction as an example.

### Receipts and outcomes

Every lowered command returns the usual receipt. Outcomes arrive later in the
schedule's `:recent` list: `fired`, `rejected`, `missed`, `cancelled` or
`completed`, with a reason. The proposal adds four reasons: `guard`,
`expired`, `group-failed` and `human-held` (the orchestrator's code for the
same refusal). The next observation's `feedback`
joins outcomes to decision IDs, so the policy and the log see what became of
each decision.

## 4. Creating new effects

### Tiers

| Tier | What the agent sends | Runs | Capability | Status |
| --- | --- | --- | --- | --- |
| 0. Choose | A cue ID and parameter values | Nothing new | `control` | Today |
| 1. DSL data | A portable effect definition | The registry validates; the compiler lowers it to the numeric IR | `author-data` | Today |
| 1b. Recipe | A named constructor and options, for example `{template: "breathing-wash", …}` | A fixed `tolglow.effects` builder returns DSL data | `author-data` | Today in the REPL |
| 2. Generator | Source that returns DSL data | A sandboxed interpreter, off the frame path; its result goes through tier 1 | `author-code` | Proposed |
| 3. Shader | GLSL for a shader cue | The existing shader path compiles a candidate and keeps the last working program | `author-code` | Existing path; agent access is #322 |
| 4. Trusted code | Clojure evaluated in the application JVM | The operator's REPL. It is not a sandbox | Operator only | Today, for a trusted local operator |

The agent's default is tier 1. A definition is data: a small model emits it
reliably, a validator explains what is wrong with it by path, it saves and
shares with the show, it compiles to a pure step with no clock, IO or hidden
state, and its cost is bounded by its size. Tier 1b costs the fewest tokens
and is where the [model lab's](../../model-lab/show.md) executed corpus
already is.

Tier 2 exists for what data expresses awkwardly: generating forty outputs
from a loop, or deriving a gradient from a palette rule. The code does not run
per frame. It runs once, returns a definition, and the definition is what the
show runs. This keeps the rule that an effect is data while letting a model
use a language it writes well.

Tier 4 is not offered to the agent. `repl-eval!` evaluates in the application
JVM and its documentation says it is not an execution sandbox. An operator can
paste what an agent suggests. That is the deliberate capability of the
existing rules.

### Languages

The question is which languages can produce a definition cleanly, not which
can run in the frame loop.

| Language | Route | Clean? | Notes |
| --- | --- | --- | --- |
| JSON or EDN data | Emitted directly | Yes | Any model, any backend. The baseline |
| Clojure | SCI on the JVM with an allow-listed namespace exposing the `tolglow.effects` builders | Yes | SCI 0.15.60 is already pinned for the embedded host profile. Same source runs in ClojureScript |
| ClojureScript | SCI in the Studio bundle or a worker; in development, a form sent through the JVM-hosted shadow-cljs REPL to a connected JavaScript runtime | Yes | shadow-cljs 3.5.14 is a development dependency. A release build has no shadow server, so production needs the SCI route or a sidecar |
| JavaScript | The same JavaScript runtime the shadow REPL drives (a Node sidecar with its permission flags, or a worker), against a small JavaScript binding of the builders | Yes | The builders return plain objects, so the result is JSON |
| TypeScript | Type stripping, then the JavaScript route | Yes | Erasing types needs no type checker |
| Python | Pyodide (CPython compiled to WebAssembly) in the JavaScript runtime, or Transcrypt for a subset | Workable, heavy | Pyodide is a download of several megabytes with a slow start. The most common language for models to write, so worth supporting only if emitted data proves too limiting |
| Lua | Fengari or a WebAssembly Lua in the JavaScript runtime | Workable | Small and easy to bound |
| Kotlin, Dart, Scala, ReScript, Elm | Their own compilers to JavaScript | No | Toolchains and runtimes are too heavy for a snippet |
| Rust, AssemblyScript, C, Zig | Compiled to WebAssembly, run in the browser or in a JVM WebAssembly runtime such as Chicory | For per-frame code only | The strongest sandbox, but compiling at show time is impractical. A later option if per-frame agent code is ever wanted |
| GLSL | The shader path | Yes, tier 3 | Bounded by the GPU, validated off the render clock |

Recommended for a first implementation: data, recipes, and SCI generators in
Clojure. JavaScript and TypeScript generators second, because the Studio and
the shadow REPL already give a JavaScript runtime. Python only on evidence.
Whatever the language, the generator's result is JSON-compatible data that
goes through the same validation as tier 1.

### Safety model

A candidate passes each stage before the next. A failure at any stage rejects
it with path errors and leaves the show untouched.

1. **Parse.** Safe EDN or JSON, one form, no readers, size limits. Imported
   and downloaded definitions take this route and no other.
2. **Capability.** The session's scopes; `author-code` for a generator.
3. **Sandbox (tier 2 only).** An interpreter with an allow-list: the builder
   functions and pure core functions. No Java interop, no `eval`, no
   namespaces beyond the allow-list, no IO, no network, no clock, no
   unseeded randomness; a seed is passed in. A wall-clock deadline (500 ms
   suggested) on a worker thread, a cap on source size (16 kB in the schema)
   and on result size. A JavaScript generator runs in a separate process or
   worker that can be killed. The slice must show that a tight loop is
   actually stopped; an interpreter that cannot interrupt one needs the
   process boundary.
4. **Schema and registry validation.** `registry/validate-definition`,
   unchanged.
5. **Compile off the frame.** On the compile worker, as publications are
   checked today (#784). The live show waits for nothing.
6. **Static cost.** Expression node count and gradient stops per output,
   multiplied by the targeted fixtures, against a per-definition budget.
7. **Dry run.** The hypothetical frame at several positions: no fault, no
   non-finite value, evaluation time within budget, and the look and budget
   forecast attached to the next observation.
8. **Publish.** Under an agent namespace (`agent/…`) with a version, the
   decision ID and the profile as provenance. `transient` definitions are
   retired when nothing refers to them.
9. **Run.** Existing fault isolation: a failing effect keeps its last good
   revision and other cues keep rendering. An agent definition that faults is
   ended and reported in `feedback`.

Who may hold `author-code`:

| Session | `author-data` | `author-code` |
| --- | --- | --- |
| Local orchestrator, operator enabled it | Yes | Yes, when the operator turns it on for the session |
| Downloaded or followed profile as a subagent | Proposes definitions through the orchestrator | Never |
| Remote API or cloud backend | Yes | Its source runs in the local sandbox under the same limits, if the operator enabled it. Code is never sent out to run |
| Imported show or shared effect | Data only, validated on load | Never |

A sharing export may carry an agent-authored definition, as data with its
provenance. It never carries generator source unless the operator selects it.

## 5. Determinism and replay

Schema: [`decision-log.schema.json`](schemas/decision-log.schema.json).
Example: [`decision-log.record.json`](examples/decision-log.record.json).
A translation decision (#888) uses the record's offline variant:
`session.offline: true`, no `clock` or `commands`, actor role `translator`
(or `human` for an operator's edit) and an `outcome` with the verify result;
its observation has `kind: translation` and a `decision` section
([example](examples/decision-log.translator.json)). The orchestrator's
per-window record shares the file: stub schema
[`decision-window.schema.json`](schemas/decision-window.schema.json) (#835),
example [`decision-window.record.json`](examples/decision-window.record.json).
A decision record names its `window`.

### What is logged

One record per decision, appended when the decision is admitted and completed
as its outcomes arrive:

- the session, the actor (orchestrator or subagent, profile, weights digest,
  backend, model, dominance share) and the policy settings (seed, temperature,
  observation profile, prompt digest);
- the observation: token, kind, base, a canonical digest of exactly what the
  policy received, a reference to the nearest full snapshot, and the look
  thumbnails by content hash;
- the look before the decision and after it took effect: the digest of layers
  L0 to L3 at the observed frame and one bar after the group's first event
  fired, with thumbnail hashes when views exist;
- the clock: the beat and frame observed, the beat decided, the beat and frame
  admitted, and latency in milliseconds for observing, the policy, validation
  and admission. Latency is kept apart from lighting frame timing, as the
  rehearsal review requires;
- the backend's raw reply, byte for byte, and the parsed decision;
- validation status and errors;
- the lowered commands with their command IDs and receipts;
- outcomes per event, with reasons, guard evidence and who overrode;
- feedback: implicit signals (a human override, a guard cancellation, a hold)
  and explicit ratings, each with a beat window. What feedback means and how
  it is weighted is `learning.md`.

Storage is an append-only JSON Lines file per session with content-addressed
blobs beside it for snapshots, replies and thumbnails. A full snapshot is
stored at least every 32 decisions so a delta chain is short. The digest is
the existing canonical `replay/digest`: sorted, tagged, independent of printer
limits.

### Three replays

| Replay | Input | Proves | Limit |
| --- | --- | --- | --- |
| Show replay | The recorded commands, admitted at their recorded frames, into a #670 rehearsal take | The show's output is byte-identical for rehearsal passages, whose inputs (tempo, sections, stems, gestures) are recorded: the agent is an input source like a performer | A live night is not covered yet: the replay recorder does not capture audio features, sensors or reactive writes; signal tracks (#885) close part of that gap |
| Decision replay | The logged replies, fed back in place of the model | Without a model, the same logged replies give byte-identical decisions, lowering and commands | Re-running a model on a logged observation is **not** claimed deterministic: llama.cpp's prompt cache and batch-size-dependent logits change results, and API backends are not repeatable. One slot, no prompt cache and greedy decoding make a local re-run likely to match; it is a diagnostic, not a guarantee |
| Counterfactual | The logged observations into a different policy | What another policy would have done at each step | Valid only up to the first differing action. Beyond it, roll forward in the rehearsal harness with the recorded passage |

The agent itself is not deterministic and does not need to be. Determinism
lives at the boundary: the show records what the agent said and when it was
admitted, and from there the existing rule holds that the same command log
gives the same output. A replay never calls a model.

Seeds: a local policy takes its sampling seed from the session seed and the
decision number. A generator takes its seed from the decision. The seeded
choices the show makes for an agent event (a random pick in a selector) use
the scheduler's existing seeded selection.

### Training export

A training example is a join of the log: observation, decision, outcomes,
feedback, and the look before and after. The export is an explicit operator
action with a selection, provenance and removal of examples, as #677 requires
and as the sharing rules demand. Goal text, show names and thumbnails can
identify a venue, so the export lists what it includes. Nothing leaves the
machine as a side effect of logging.

## Implementation slices

Each slice is a child issue of #821 with its own acceptance check.

| Slice | Issue |
| --- | --- |
| Observation projection, snapshot and delta, measured token budgets | [#849](https://github.com/tolgraven/tolglow/issues/849) |
| Structured look layers L0 to L3 and the look of a hypothetical frame | [#850](https://github.com/tolgraven/tolglow/issues/850) |
| Rendered look views from committed frames, with measured cost | [#851](https://github.com/tolgraven/tolglow/issues/851) |
| Compare frame encoders for the look | [#852](https://github.com/tolgraven/tolglow/issues/852) |
| Action envelope lowered to owner messages, scopes and quotas | [#853](https://github.com/tolgraven/tolglow/issues/853) |
| Intent tags, groups, guards and atomic replace in the scheduler | Extends [#645](https://github.com/tolgraven/tolglow/issues/645) |
| Human holds on a destination and the order of agent writes on one line | [#854](https://github.com/tolgraven/tolglow/issues/854) |
| Sandboxed generators that return effect DSL data | [#855](https://github.com/tolgraven/tolglow/issues/855), with the tier model noted on [#322](https://github.com/tolgraven/tolglow/issues/322) |
| Decision log, replay and training export | [#856](https://github.com/tolgraven/tolglow/issues/856) |
| Operator goals as show data | [#857](https://github.com/tolgraven/tolglow/issues/857) |
| Canonical reference rigs and retargeting of decisions | [#858](https://github.com/tolgraven/tolglow/issues/858) |

## Alignment with the other lanes

The orchestration and learning lanes read this lane's drafts before
publishing. Their documents own what is named in the right-hand column.

| Here | There | Owner of the detail |
| --- | --- | --- |
| An observation | `context` inside a proposal request, with `window`, `deadline`, `open` lanes and `limits` around it | Request envelope: `orchestration.md`, #826. Fields: this document |
| One decision | One proposal | Wire schema: this document. Confidence and usage: #826 |
| `post` group with `on_failure` | A proposal's events, accepted or dropped whole | This document |
| `intent`, `guard`, `until`, `check` | `assumes` | Show-side guards: here and #645. Orchestrator triggers and replanning: #829 |
| `basis` | `basis` | PR #823 names |
| `schedule/replace` | Posting with cancellations | #645 extension |
| Human hold | Hold with a grace period; standing lanes | Show-side hold and its enforcement: #854. Lanes and the grace setting: #835. Filtering proposals by holds and lanes: #827 |
| `rank` on a group | A proposer ranking its own alternatives | One source only; #827 and #854 |
| `actor.rankers` in the log | A followed profile as a ranker | Attribution and the cap: #828. Learning side: #845 |
| `budgets.forecast` | Resource forecast and admission | Projection: #849. Forecast and admission: #830 |
| Session quotas | Musical rate limits | #831 |
| Only posted events are observed in `sched` | Lock, firm and tentative horizons; only firm events are posted | #829 |
| Decision log record | Arbitration record per window and the operator's review | Per-decision record (`tolglow-agent-log/1`), replay and export: #856. Per-window record (`tolglow-agent-window/1`) and review: #835. One JSON Lines file; each decision record names its `window` |
| `feedback` and log `feedback` | Preference events and the ledger | #838, #677 |
| Look layers and views | Look-feature reward tier and vision reward tier | Encoding: here, #850 to #852. Reward heads: #841. Clip comparison: #840 |
| `rig.sig`, reference rigs | Rig classes, canonical rigs, donation retargeting | Descriptor and retargeting: here. Clustering and consent: `learning.md`, #847 |
| Capability scopes | Untrusted proposals, flash guard and output interlock | Scopes: #853. Safety limits: #836 |

`model.md` decides the policy's context length and therefore which profile a
local model reads (a 0.6 to 2B policy reads `tick` and `compact`), constrains
the decoder to the action schema with the show's identifiers baked in, and
keeps the view embedding off the policy until an experiment shows it helps.
The four reports were reconciled in the [index](README.md#reconciliation-record).

## Decisions for the operator

The consolidated, deduplicated list across the four reports, with the phase each decision blocks, is in the [index](README.md#operator-decisions).

1. **Does the policy take rendered views live, or only the reward model?**
   The recommendation is structured layers for control and views for
   judgement, with the embedding as an optional policy input. Taking views
   live costs GPU time that Room also wants (#782).
2. **Where do live views render?** A hidden page the application owns (host B)
   is recommended. It needs an offscreen web view in the packaged app, which
   has to be evaluated on a real host first. The alternative is Studio's Room
   only, which stops when Studio is hidden.
3. **Human hold time.** How long an agent stays off a destination a human
   touched. Proposed, with `orchestration.md`: until release, plus a grace
   that ends on the first bar line at least one bar later; settable, and the
   hold while touched cannot be switched off. Consolidated in the
   [index](README.md#operator-decisions).
4. **May an agent launch immediately?** The recommendation is scheduled
   launches only, with `now` kept for corrections, unless the operator grants
   the `immediate` scope.
5. **Generator code.** Whether `author-code` exists at all in the first
   version, and if so whether Clojure through SCI is enough or JavaScript and
   TypeScript are wanted from the start. Python is not recommended yet.
6. **Guards on continuous check.** Whether a plan may withdraw itself the
   moment the music turns, which is visible on the timeline as an early
   cancellation, or only when it falls due.
7. **Thumbnails in the log.** Storing three small images per look sample makes
   encoders replaceable and review possible. It also stores pictures of the
   venue's rig in the show folder. Retention and whether an export may include
   them need a rule.
8. **A parameter `:unit`.** Adding an optional unit to parameter schemas
   touches the registry contract and every consumer of it.
