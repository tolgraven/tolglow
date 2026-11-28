# Agent observation

A show-control agent reads the show as `tolglow-agent-obs/1`: one JSON
document projected from the latest committed show state, first as a snapshot
and then as deltas against the token the agent last applied. The design and
its reasoning are in the [interface research](research/native-agent/interface.md)
(section 1) for [#821](https://github.com/tolgraven/tolglow/issues/821). This
page describes what [#849](https://github.com/tolgraven/tolglow/issues/849)
implements.

- Code: [`tolglow.agent-observation`](../src/tolglow/agent_observation.clj)
- Schema: [`observation.schema.json`](research/native-agent/schemas/observation.schema.json),
  shipped byte for byte as `resources/schemas/agent-observation-1.schema.json`
  (a test keeps the two equal and validates the research examples against it)
- Tests: [`agent_observation_test.clj`](../test/tolglow/agent_observation_test.clj)
- Measurement: [`tolglow.perf.agent-observation`](../dev/tolglow/perf/agent_observation.clj)

## Reading it

From the REPL, on a running show:

```clojure
(require '[tolglow.show :as show])

(def snap (show/agent-observation runtime {:profile :compact :session "planner"}))
(get snap "kind")                      ; => "snapshot"

;; After applying snap, ask for what changed since:
(def delta (show/agent-observation runtime {:profile :compact :session "planner"
                                            :base (get snap "token")}))
(get delta "changes")                  ; => [["set" ["clock" "beat"] 20] ...]

;; A table paged to its budget carries "more" and "total"; read the rest:
(show/agent-observation-page runtime ["rig" "fixtures"] {:offset 25 :limit 25})
(show/agent-observation-page runtime ["sched" "upcoming"] {:offset 64})
```

Over HTTP, from the Studio server of the same show (read-only, `GET`):

```
/api/agent/observation?session=planner&profile=compact
/api/agent/observation?session=planner&profile=compact&base=o-8fcd98e1c53c3ae5
/api/agent/observation?session=planner&profile=compact&have=sha256:4e4857325ebe6a67,sha256:f85bd19d032930ee
/api/agent/observation/page?path=rig.fixtures&offset=25&limit=25
```

Both return the same projection of the same committed state, through the same
token store: the HTTP body is the canonical wire JSON of what
`show/agent-observation` returns, and a session started at the REPL continues
over HTTP.

## Sessions and bases

Each consumer names a `session` (a short identifier; `default` when absent).
The runtime keeps each session's last four observations per profile, and a
base the consumer cites is refreshed, so it stays held however often other
sessions read. At most 64 sessions are kept, least recently used first out.
There is no consumer queue: a delta always describes the newest committed
state, however long the consumer took.

A base unknown to that session and profile (never seen or evicted), another
show or epoch, a delta over the profile's token cap or more than 4,096 changes
yields a snapshot.

## Envelope

```json
{"schema": "tolglow-agent-obs/1", "kind": "delta", "profile": "compact",
 "token": "o-3bbf304bb68eaac8", "base": "o-8fcd98e1c53c3ae5",
 "digest": "sha256:1ea2eef328aa6d69",
 "id": {"runtime": "…", "show": "gig-2023", "epoch": 0, "rev": 28, "frame": 81, "gen": 0},
 "blocks": {"rig": "sha256:4e48…", "catalog": "sha256:f85b…", "goals": "sha256:4f53…"},
 "wake": ["grid:bar", "run"],
 "changes": [["set", ["clock", "beat"], 20],
             ["set", ["run", "rows", "0:22", 9, "level"], 0.45],
             ["remove", ["run", "rows", "0:19"]]]}
```

- `token` names this observation; an action cites it as its basis. `base` is
  the token the delta applies to.
- `digest` covers the observed state (block digests and the dynamic
  sections), not the envelope. The same take replayed, or run with one or two
  effect workers, gives the same digest at every frame.
- `id` is the show identity, the committed frame number and the transport
  generation.
- `blocks` gives the digest of each slow section. With `have`, a snapshot
  sends a held block as its digest alone.
- `wake` says why this observation differs from its base: `grid:bar`,
  `grid:phrase`, `block:rig`, `block:catalog`, `block:goals`, `run` (a launch
  or an end), `sched` or `human`. A snapshot names `start`, `resync`,
  `epoch` or `overflow`. The schema also reserves `event:<type>`, `budget` and
  `reply` for push cadence, which is not implemented.

Deltas use `set` and `remove` with a path of keys and indices, as the
[state stream](state-stream.md) does. Map entries and table cells are
addressed one by one. A numeric vector (a colour, a position) and a row or
array whose length changed are replaced whole, so a consumed schedule event
costs one operation. A changed slow block is replaced whole, except in `tick`.

### Canonical JSON and digests

A digest is `sha256:` and the first 16 hex digits of the SHA-256 of the
value's canonical JSON:

- object keys sorted by UTF-16 code units at every level; two keys with the
  same text (a keyword and a string) are refused, never merged;
- numbers rounded to at most six decimals and written without exponent or
  trailing zeros, as ECMAScript's `Number#toString` writes them (and so
  RFC 8785): `2.0` is `2`, `0.0001` is `0.0001`, `-0.0` is `0`; non-finite
  values and magnitudes of 1e21 or more are `null`;
- no whitespace, strings escaped as JSON requires.

A consumer in another language reproduces it with `JSON.stringify` of a
key-sorted value (or any RFC 8785 implementation), since every number the
projection writes is already in that form. `content-digest` does the same on
the JVM.

## Sections

Sections run from least to most volatile, so a prompt or KV cache keeps its
prefix. Tables are `{legend, rows}` with rows keyed by the show's own IDs.

| Section | Content |
| --- | --- |
| `rig` | Raw metres (`pos`, scene contract axes) and normalised `npos` (-1…1 over the rig's bounding box), fixture types with kind, capabilities, beam angle and watts, groups with normalised centroid and extent, counts by kind, density, room size and the version 1 rig descriptor `sig` |
| `catalog` | Effects as `[ver, cats, params]` with each parameter `[type, default, bounds, unit, recommended]`, and cues as `[effect, prio, params]`. Scene-owner, show-control and code-cue entries are left out |
| `goals` | Always `[]` until goals become show data. Goal items in the schema use `order` (1 first) and `by`, and are closed objects |
| `clock` | Beat, zero-based position, bpm, meter, playing, source, the next bar and phrase lines, the tempo map (one open segment) |
| `run` | Running instances: cue, effect, version, phase, priority, opacity, start and end beat, owner, parameter values, and `operator` (true for a scene-owner, show-control or code-cue instance, which stays with the operator) |
| `sched` | Upcoming events `[due_beat, id, op, owner]` in due order, ramps `[id, dest, progress, value, to, end_beat, owner]`, the last eight outcomes `[id, status, reason]` |
| `audio` | `ok: false` without fresh analysis (older than two seconds); otherwise energy, RMS, relative energy, bands, breakdown and class with their age in beats |
| `look` | L0 meters and L1 group rows, below |
| `budgets` | Frame deadline, instance count, schedule capacity, and the lighting budget when one is set |
| `humans` | Master (0…1) and blackout |
| `signals`, `agents`, `feedback` | Present and empty |

`owner` is the adapter that launched or posted something (`push`, `studio`,
`agent`, `auto`, …). A source whose session is the consumer's own `session`
reads `agent:self`; no session or conversation ID is projected.

Units follow research decision D7 (an engineering default): a parameter's
`unit` is its declared `:unit` when the schema has one, `rgb` for colours and
gradients, `norm` for a number that drives a 0…1 output attribute directly,
`m` for a vector that drives aim, otherwise `null`. Nothing here writes or
validates `:unit`.

Quantisation: normalised values and beats to three decimals, metres and
colours to two.

### Look

The look reads the delivered output frame the owner committed:

- **L0 meters**: `lit` (share of heads emitting more than 1/255) and `level`
  (mean of each head's brightest emitted component), plus `brightness`,
  `coverage`, `flashing`, `movement`, `strobe` and `activity` when the
  lighting activity meter runs (`:settings :lighting-activity :enabled?`).
- **L1 groups**: per patched group (or per fixture kind on a rig without
  groups) `[n, lit, level, rgb, aim, spread, zoom, strobe]`. `aim` is the mean
  committed beam direction of lit movers and `spread` is one minus its length.
- **L5 fixtures** (`full` only): `[level, rgb, strobe]` per lit head, paged.

Palette, luminance and rhythm statistics (L2), coverage maps (L3) and
rendered views (L4, [#851](https://github.com/tolgraven/tolglow/issues/851))
are not implemented.

## Profiles and caps

| Profile | Contents | Cap (estimated tokens) |
| --- | --- | ---: |
| `tick` | Dynamic sections only; slow blocks by digest. For a policy between snapshots | 800, every envelope |
| `compact` | Everything, sections paged to their budgets | 4,000, every envelope |
| `full` | Budgets tripled, L5 fixture rows | 12,000, every envelope |

Section budgets (compact tokens, 3,440 in all, leaving room for the envelope):
rig 800, catalog 1,000, goals 200, clock 90, run 300, sched 250, audio 120,
look 400, budgets 100, humans 60, signals, agents and feedback 40 each.
`clock`, `audio`, `budgets` and `humans` are fixed sets of fields. A section
over budget pages its largest table in ID order (the schedule's arrays in due
order) and marks it: `more: true` and `total` on a table, `more: true` and
`upcoming_total`/`ramps_total` on the schedule. Nothing is cut without that
mark; `inspect` reads the rest.

The cap is then checked on the encoded envelope. A delta over its cap is sent
as a snapshot (`wake: ["overflow"]`); a snapshot over its cap is paged again
with budgets scaled by 0.7, down to a fifth of the budget.

Tokens are estimated as UTF-8 bytes divided by three, the research estimate
for number-heavy JSON. The pinned model-lab tokenizers have not been run on
these documents: they need a checksum-locked download (`dev/model-lab`).

`inspect` pages carry the `token`, `digest` and `blocks` of the observation of
the same committed state, so a consumer can tell whether a block changed
since the snapshot it continues.

## Ownership, privacy and safety

- The projection is pure. It reads the committed model, the delivered output
  frame and the meters already published on it. It evaluates no effect,
  waveform, signal or random source again, and never writes show state. A
  test checks the state reference is identical after every profile.
- It runs on the caller's thread (the REPL, an HTTP request), never on the
  frame owner. A test blocks a projection mid-way while the owner commits 20
  frames; the observation then describes the one state it captured.
- Slow sections are built once per input identity, unpaged, and shared by
  every profile and `inspect`; paging is cached per profile. The rig's
  nearest-neighbour and mirror searches use a uniform grid hash.
- Only IDs, numbers and enumerations are projected. The interface research
  uses the show's own IDs as they are and treats text from the operator, from
  names in the show and from other profiles as data that never widens what a
  session may do. This implementation goes one step further for text that
  is not an ID: schedule labels, names, notes and descriptions are not read at
  all, and an ID that is not a short identifier (letters, digits and
  `_.:/+-`, at most 64 characters) is sent as `~` and twelve hex digits of its
  digest, stable across frames. So an instruction written into a label or an
  ID by one agent never reaches another as text. Operator goal text, when
  goals land, will be the one free-text field, marked as data in the schema.
- Provider configuration, credentials, integration settings and session IDs
  are never read. A value of an unknown type is dropped or refused, never
  written as its `toString`. A test plants a sentinel in settings, a fixture
  name, an effect name and description, the agent manager's credential, an
  agent's conversation ID on a launch and a schedule label, then sweeps every
  profile and four pages for it. A sentinel in the show ID is projected,
  as `id.show` only, because it is an ID.

## Measured cost and size

`$W/bin/repl run tolglow.perf.agent-observation 400` on the M4 Pro, JDK 25.0.2,
load average about 20 from other lanes, 400 frames after 200 warm-up frames.
Each call is one `show/agent-observation` on the reader thread after an
untimed owner tick, including the cap check on the encoded envelope. Studio's
`show/snapshot` is timed the same way.

Mixed rig (1,000 RGBW pixels, 48 dimmers, 48 pars, 16 moving heads, 26
instances):

| Read | p50 ms | p95 ms | Bytes p50 | Tokens p50 / max |
| --- | ---: | ---: | ---: | ---: |
| `tick` delta | 1.22 | 2.64 | 1,019 | 340 / 424 |
| `compact` delta | 1.03 | 1.73 | 1,022 | 341 / 425 |
| `compact` snapshot | 1.12 | 1.70 | 7,375 | 2,459 / 2,463 |
| `full` delta | 7.81 | 12.96 | 6,104 | 2,035 / 2,122 |
| `full` snapshot | 7.98 | 13.18 | 16,792 | 5,598 / 5,607 |
| Studio `show/snapshot` | 0.98 | 2.00 | | |

R1o red show on the 2023 gig rig under auto (16 fixtures):

| Read | p50 ms | p95 ms | Bytes p50 | Tokens p50 / max |
| --- | ---: | ---: | ---: | ---: |
| `tick` delta | 0.27 | 0.37 | 500 | 167 / 611 |
| `compact` delta | 0.17 | 0.21 | 503 | 168 / 612 |
| `compact` snapshot | 0.25 | 0.32 | 4,897 | 1,633 / 1,681 |
| `full` snapshot | 0.27 | 0.34 | 5,273 | 1,758 / 1,906 |

Cold rig block on a 2,048-fixture grid rig: `rig-section` 20–29 ms (five
runs); the first observation on a fresh store 38 ms (`compact`, which builds
the unpaged rig) and 15–16 ms for `full` and `tick`, which reuse it; an
`inspect` page 0.1 ms. Before the grid hash and the shared cache these were
about 2.2 s per profile and per page.

The research estimated a 7,462-byte (about 2,500-token) compact snapshot and a
1,059-byte (about 350-token) bar-crossing delta for a 14-fixture rig with all
look layers. Deltas here are 0.5 kB on the gig rig and 1.0 kB on the mixed
rig. The `full` cost is the L5 fixture table, its paging and the cap check.

Excluded: wire transfer, the consumer, and any tokenizer. Timings come from a
loaded machine and are not a frame-deadline measurement: the projection never
runs on the owner.

## Not implemented

Goals as show data, audio sections and feel, human holds and recent commands,
`budgets.forecast` ([#830](https://github.com/tolgraven/tolglow/issues/830)),
L2–L4 look layers, push cadence and the reserved wake reasons, the
`translation` kind ([#888](https://github.com/tolgraven/tolglow/issues/888)),
a catalogue limited per session to the cues that session may use, and a
Studio view of the observation.
