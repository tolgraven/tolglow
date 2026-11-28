# Agent observation

A show-control agent reads the show as `tolglow-agent-obs/1`: one JSON
document projected from the latest committed show state, first as a snapshot
and then as deltas against the token the agent last applied. The design and
its reasoning are in the interface research for
[#821](https://github.com/tolgraven/tolglow/issues/821) (section 1 of
`docs/research/native-agent/interface.md`, PR #860). This page describes what
[#849](https://github.com/tolgraven/tolglow/issues/849) implements.

- Code: [`tolglow.agent-observation`](../src/tolglow/agent_observation.clj)
- Schema: [`resources/schemas/agent-observation-1.schema.json`](../resources/schemas/agent-observation-1.schema.json)
- Tests: [`agent_observation_test.clj`](../test/tolglow/agent_observation_test.clj)
- Measurement: [`tolglow.perf.agent-observation`](../dev/tolglow/perf/agent_observation.clj)

## Reading it

From the REPL, on a running show:

```clojure
(require '[tolglow.show :as show])

(def snap (show/agent-observation runtime {:profile :compact}))
(get snap "kind")                      ; => "snapshot"

;; After applying snap, ask for what changed since:
(def delta (show/agent-observation runtime {:profile :compact :base (get snap "token")}))
(get delta "changes")                  ; => [["set" ["clock" "beat"] 20.0] ...]

;; A table paged to its budget carries "more" and "total"; read the rest:
(show/agent-observation-page runtime ["rig" "fixtures"] {:offset 25 :limit 25})
```

Over HTTP, from the Studio server of the same show (read-only, `GET`):

```
/api/agent/observation?profile=compact
/api/agent/observation?profile=compact&base=o-8fcd98e1c53c3ae5
/api/agent/observation?profile=compact&have=sha256:4e4857325ebe6a67,sha256:f85bd19d032930ee
/api/agent/observation/page?path=rig.fixtures&offset=25&limit=25
```

Both return the same projection of the same committed state: the HTTP body
is the canonical wire JSON of what `show/agent-observation` returns.

## Envelope

```json
{"schema": "tolglow-agent-obs/1", "kind": "delta", "profile": "compact",
 "token": "o-3bbf304bb68eaac8", "base": "o-8fcd98e1c53c3ae5",
 "digest": "sha256:1ea2eef328aa6d69",
 "id": {"runtime": "…", "show": "gig-2023", "epoch": 0, "rev": 28, "frame": 81, "gen": 0},
 "blocks": {"rig": "sha256:4e48…", "catalog": "sha256:f85b…", "goals": "sha256:4f53…"},
 "wake": ["grid:bar", "run"],
 "changes": [["set", ["clock", "beat"], 20.0],
             ["set", ["run", "rows", "0:22", 9, "level"], 0.45],
             ["remove", ["run", "rows", "0:19"]]]}
```

- `token` names this observation; an action cites it as its basis. `base` is
  the token the delta applies to.
- `digest` covers the observed state (block digests and the dynamic
  sections), not the envelope. The same take replayed, or run with one or two
  effect workers, gives the same digest at every frame. A consumer can check a
  reconstruction with `content-digest`.
- `id` is the show identity, the committed frame number and the transport
  generation.
- `blocks` gives the digest of each slow section. With `have`, a snapshot
  sends a held block as its digest alone.
- `wake` says why this observation differs from its base: `grid:bar`,
  `grid:phrase`, `block:rig`, `block:catalog`, `block:goals`, `run` (a launch
  or an end), `sched` or `human`. A snapshot names `start`, `resync` (unknown
  or evicted base), `profile`, `epoch` or `overflow`.

A missing or evicted base, another show or epoch, a profile change or more
than 4,096 changes yields a snapshot. The runtime keeps the last 32
observations; there is no consumer queue, so a slow consumer's next delta
comes from the newest committed state.

Deltas use `set` and `remove` with a path of keys and indices, as the
[state stream](state-stream.md) does. Map entries and table cells are
addressed one by one. A numeric vector (a colour, a position) and a row whose
length changed are replaced whole. A changed slow block is replaced whole,
except in `tick`.

## Sections

Sections run from least to most volatile, so a prompt or KV cache keeps its
prefix. Tables are `{legend, rows}` with rows keyed by the show's own IDs.

| Section | Content |
| --- | --- |
| `rig` | Raw metres (`pos`, scene contract axes) and normalised `npos` (-1…1 over the rig's bounding box), fixture types with kind, capabilities, beam angle and watts, groups with normalised centroid and extent, counts by kind, density, room size and the version 1 rig descriptor `sig` |
| `catalog` | Effects as `[ver, cats, params]` with each parameter `[type, default, bounds, unit, recommended]`, and cues as `[effect, prio, params]`. Scene-owner, show-control and code-cue entries are left out |
| `goals` | Always `[]` until goals become show data |
| `clock` | Beat, zero-based position, bpm, meter, playing, source, the next bar and phrase lines, the tempo map (one open segment) |
| `run` | Running instances: cue, effect, version, phase, priority, opacity, start and end beat, owner, parameter values |
| `sched` | The compact schedule projection: upcoming events, ramps with progress, recent outcomes |
| `audio` | `ok: false` without fresh analysis (older than two seconds); otherwise energy, RMS, relative energy, bands, breakdown and class with their age in beats |
| `look` | L0 meters and L1 group rows, below |
| `budgets` | Frame deadline, instance count, schedule capacity, and the lighting budget when one is set |
| `humans` | Master and blackout |
| `signals`, `agents`, `feedback` | Present and empty |

Units: a parameter's `unit` is its declared `:unit` when the schema has one,
`rgb` for colours and gradients, `norm` for a number that drives a 0…1 output
attribute directly, `m` for a vector that drives aim, otherwise `null`. Adding
`:unit` to the registry contract is an open operator decision; nothing here
writes or validates it.

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
| `tick` | Dynamic sections only; slow blocks by digest. For a policy between snapshots | 800 per delta |
| `compact` | Everything, sections paged to their budgets | 4,000 per snapshot |
| `full` | Budgets tripled, up to 256 upcoming events, L5 fixture rows | 12,000 per snapshot |

Section budgets (compact tokens): rig 900, catalog 1,200, goals 250, clock 90,
run 300, sched 250, audio 200, look 450, budgets 120, others 100. A section
over budget pages its largest table in ID order and marks it `more: true`
with `total`. Nothing is cut without that mark.

Tokens are estimated as UTF-8 bytes divided by three, the research estimate
for number-heavy JSON. The pinned model-lab tokenizers have not been run on
these documents: they need a checksum-locked download (`dev/model-lab`).

## Ownership and safety

- The projection is pure. It reads the committed model, the delivered output
  frame and the meters already published on it. It evaluates no effect,
  waveform, signal or random source again, and never writes show state.
  A test checks the state reference is identical after every profile.
- It runs on the caller's thread (the REPL, an HTTP request), never on the
  frame owner. A test blocks a projection mid-way while the owner commits 20
  frames; the observation then describes the one state it captured.
- Only IDs, numbers and enumerations are projected. Names, notes, provider
  configuration, credentials, integration settings and human surfaces'
  session IDs are never read; agent and auto sessions keep their session ID
  in `owner` so an agent recognises its own work. A test plants a sentinel
  secret in settings, a fixture, an effect and the agent manager's
  credential and sweeps every profile and page for it.

## Measured cost and size

`$W/bin/repl run tolglow.perf.agent-observation 400` on the M4 Pro, JDK 25.0.2,
load average about 22 from other lanes, 400 frames after 200 warm-up frames.
Each call is one `show/agent-observation` on the reader thread after an
untimed owner tick. Studio's `show/snapshot` is timed the same way.

Mixed rig (1,000 RGBW pixels, 48 dimmers, 48 pars, 16 moving heads, 26
instances):

| Read | p50 ms | p95 ms | Bytes p50 | Tokens p50 / max |
| --- | ---: | ---: | ---: | ---: |
| `tick` delta | 1.06 | 1.64 | 1,021 | 341 / 425 |
| `compact` delta | 0.89 | 1.41 | 1,024 | 342 / 426 |
| `compact` snapshot | 0.83 | 1.26 | 7,634 | 2,545 / 2,547 |
| `full` delta | 4.42 | 6.74 | 7,005 | 2,335 / 2,424 |
| `full` snapshot | 4.29 | 6.42 | 18,088 | 6,030 / 6,033 |
| Studio `show/snapshot` | 0.95 | 1.80 | | |

R1o red show on the 2023 gig rig under auto (16 fixtures):

| Read | p50 ms | p95 ms | Bytes p50 | Tokens p50 / max |
| --- | ---: | ---: | ---: | ---: |
| `tick` delta | 0.26 | 0.57 | 500 | 167 / 629 |
| `compact` delta | 0.14 | 0.35 | 503 | 168 / 630 |
| `compact` snapshot | 0.09 | 0.23 | 5,144 | 1,715 / 1,759 |
| `full` snapshot | 0.11 | 0.26 | 5,552 | 1,851 / 1,978 |

The research estimated a 7,462-byte (about 2,500-token) compact snapshot and a
1,059-byte (about 350-token) bar-crossing delta for a 14-fixture rig with all
look layers. The compact snapshot here is 5.1 kB on the gig rig and 7.6 kB on
the 1,112-fixture mixed rig, whose fixture table is paged. Deltas are 0.5 kB
on the gig rig and 1.0 kB on the mixed rig, inside the 800-token tick cap.
The `full` cost is the L5 fixture table and its paging.

Excluded: wire transfer, the consumer, and any tokenizer. Timings come from a
loaded machine and are not a frame-deadline measurement: the projection never
runs on the owner.
