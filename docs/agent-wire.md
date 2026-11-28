# Agent wire form

What a model may emit to the show (#863). The schemas in
[`resources/agent/wire/`](../resources/agent/wire/) are **normative**: code loads
them from there (`tolglow.integrations.agent-wire/schema`), tests check them and
API adapters send them. The domain action schema of the native-agent research
(#853) documents what the owner lowers a reply to; it is not a wire schema. The
[native-agent research set](research/native-agent/README.md) links to these files
rather than keeping a copy.

| Schema | Sent as | Use |
| --- | --- | --- |
| [`proposal`](../resources/agent/wire/proposal.wire.schema.json) | reply format | On-clock proposer reply: up to three alternatives, each with `id`, `confidence`, `note`, basis, cancellations, group headers and events with one required `timing` |
| [`author`](../resources/agent/wire/author.wire.schema.json), [`configure`](../resources/agent/wire/configure.wire.schema.json), [`move`](../resources/agent/wire/move.wire.schema.json), [`inspect`](../resources/agent/wire/inspect.wire.schema.json) | strict tools, `tool_choice` auto | Off-clock authoring, configuration, moving pending events, asking for detail |
| [`translation-choice`](../resources/agent/wire/translation-choice.wire.schema.json) | reply format | One cross-rig translation decision (#888) |

Each has an example under [`examples/`](../resources/agent/wire/examples/).

## Rules

- Only the keywords `type` (one string), `description`, `title`, `properties`,
  `required`, `additionalProperties: false`, `items` and `enum`. That is
  `validate-arguments`' own keyword set (`tolglow.integrations.agent/schema-keywords`)
  without its bounds, which Claude structured outputs reject.
- Every property is required and every object closed. An unused field carries
  an empty value (`""`, `[]`, `0`, `false` or an enum's `none`); an optional
  object is a list of zero or one.
- Enum values are lowercase without spaces. Providers do not guarantee enum
  casing, so a reply's enum values are lowercased before validation, which then
  compares exactly.
- API backends get the generic schema with identifiers as strings. The owner,
  not the schema, decides whether a cue, instance or macro exists.
- No sampling parameters and no forced tool choice. Several alternatives come in
  one reply. A reply request declares no tools, so its static count is the whole
  request, and needs an explicit `:max-tokens` of at least 3,400 for a proposer
  reply (three six-event alternatives; thinking counts toward it on current Claude
  models, so `proposer-max-tokens` is 8,192). `:effort` sits beside the format in
  `output_config`. `providers/effort-levels` records which levels each Claude
  model accepts
  ([effort](https://platform.claude.com/docs/en/build-with-claude/effort), read
  2026-11-28): `xhigh` is not on Opus 4.6, Sonnet 4.6 or Mythos Preview, Opus 4.5
  takes only `low` to `high`, and Haiku 4.5 takes none. A level the model lacks,
  or a model the table does not list, is refused before the request is built.
- OpenAI requests send `max_completion_tokens`, because OpenAI deprecated
  `max_tokens` and its reasoning models refuse it. Anthropic, OpenRouter and
  self-hosted OpenAI-compatible servers get `max_tokens`.
- A reply request is not streamed unless `:stream? true`. Either form passes the
  same stop-reason check.
- OpenRouter requests carry `provider.require_parameters`, which routes only to
  endpoints that accept the schema parameters. OpenRouter notes that enforcement
  varies by provider, so it is no guarantee the schema is honoured; the validation
  below is. OpenAI-compatible strict tools also send `parallel_tool_calls: false`,
  since strict schemas are not guaranteed for parallel calls there.

## Static limit check

`(tolglow.integrations.agent-wire/report)` counts each request shape against
Claude's explicit limits per request, across all strict schemas: 20 strict tools,
24 optional parameters and 16 union-typed parameters
([structured outputs](https://platform.claude.com/docs/en/build-with-claude/structured-outputs#schema-complexity-limits),
read 2026-11-28). It also reports objects, properties, enum values and container
depth as grammar-size proxies, and every keyword, type, closure or enum-casing
violation. `tolglow.integrations.agent-wire-test` fails the build if a shipped
schema breaks a limit or the keyword subset, and checks that an over-limit fixture
is refused.

| Shape | Strict tools | Optional | Unions | Objects | Properties | Enum values | Depth |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Proposer (`output_config.format`) | 0 | 0 | 0 | 12 | 64 | 72 | 8 |
| Off-clock strict tools | 4 | 0 | 0 | 10 | 29 | 29 | 4 |
| Translation choice | 0 | 0 | 0 | 2 | 8 | 0 | 3 |
| All six as strict tools | 6 | 0 | 0 | 24 | 101 | 101 | 8 |

## From reply to show

A reply passes, in order:

0. **Stop reason** (`read-reply`): a reply cut off at `max_tokens` (OpenAI
   `length`) is `:truncated`, a safety refusal (Anthropic `refusal`, OpenAI
   `content_filter` or a message refusal) is `:refused` and any other stop is
   `:unexpected-stop`, each before parsing. Thinking blocks are not reply text.
   A streamed reply is read the same way from its events: the last finish
   reason (Anthropic `message_delta` with its refusal category) and any OpenAI
   refusal deltas. A stream without its completion marker is a provider error.
1. **Enum lowercasing** (`lower-enums`) wherever the reply follows the schema.
2. **`validate-arguments`** against the wire schema: a missing field, an unknown
   key (for example `ms` in a timing, or a goal's `order` on a group header), a
   wrong type or an enum value outside the list is refused with its path.
3. **Owner bounds** (`owner-bounds`): counts and ranges the wire cannot carry,
   such as at most three proposals, `confidence` from 0 to 1, four intent tags,
   one `until` position and nonnegative positions. They use `validate-arguments`'
   bound keywords on the same paths.
4. **Basis echo**: the host keeps a record of each request it sent
   (`request-record`, keyed by request ID) with the basis of the frozen snapshot
   (`request-basis`: observation token, revision, show epoch, auto session, the
   source the request is for and that source's own pending plan) and an optional
   `:deadline-beat`. The bounded, expiring store of records (evicted on answer or
   deadline) belongs to the #826 orchestrator. Each proposal's `basis_obs` and
   `basis_rev` must echo that record, or it is refused as `:basis-mismatch` with
   its path. The basis used from here on is the host's, never the model's.
5. **Lowering** (`lower-proposal`) of each proposal's events to the
   `schedule_events` vocabulary: `at` sums its fields as a zero-based position,
   `in` and `grid` take one nonzero unit, `now` posts without timing, and only the
   slot `value_kind` names is read. A group header becomes the events' label.
   Guards, expiry and cancellation by intent are carried as data for the proposal
   contract (#826); only events reach the scheduler today.
   Proposal IDs are unique within a reply and at most 64 characters.
6. **Arbitration and the owner.** Against a fresh snapshot, `check-basis` drops a
   reply that is `:late` (the transport reached the record's deadline beat, which
   holds while the transport is stopped) or whose epoch, auto session or own plan
   changed (`:stale-basis`). The recorded plan is the source's pending events
   (ID, op, anchor, the posted action, and the due beat of absolute events only)
   and its running ramps. The plan simply proceeding is not a change: an event
   firing, a ramp starting or completing, and a clock rebase moving relative
   due beats (a Link nudge or a seek). A change is an own event removed without
   firing (cancelled, missed, rejected), a new own event, or an absolute event
   that moved; each is reported with its event ID and `:removed`, `:added` or
   `:changed`. Unrelated commands only bump the show revision and do not make it
   stale. Then
   `post-proposal!` posts one atomic batch through `show/dispatch-tool!` with the
   **arbitrated snapshot's revision** as the expected revision, so a command that
   commits between arbitration and posting is refused (`:stale-revision`). The
   owner also refuses an unknown cue (`Unknown cue ghost`) or a passed position;
   the refusal carries the wire path it names.

All alternatives in one reply share one basis. Exclusivity among them is the
arbiter's job (#826): it posts at most one alternative per source per window.
Until #826 settles the basis and the arbitration window, the host test's
arbitration step is provisional.

From the REPL, against the active runtime `rt`:

```clojure
(require '[tolglow.integrations.agent-wire :as wire] '[tolglow.show :as show])
(let [source {:adapter :agent :session-id "repl"}
      model (:model @(:state rt))
      record (wire/request-record "repl-1" (wire/request-basis model source "o-0413"))
      reply (assoc-in (wire/example :proposal) [:proposals 0 :basis_rev] (:revision model))
      {:keys [ok? proposals errors]} (wire/admit-proposals record (update reply :proposals #(vec (take 1 %))))]
  (cond
    (not ok?) errors
    (seq (wire/check-basis record (:model @(:state rt)))) :stale-basis
    :else (wire/post-proposal! (partial show/dispatch-tool! rt) 0 (first proposals)
                               (:revision (:model @(:state rt))) {:command-id "repl/wire-1" :run-id "repl"})))
```

The example names cues of its own recording, so a show without them refuses it
with `Unknown cue`; replace the IDs with your own cues to post it. The posted
events appear in `show/schedule`, the shared schedule projection Studio
lists, like any other agent post.

`tolglow.integrations.agent-wire-host-test` runs this from a recorded Claude reply
and a recorded OpenRouter reply through a capture at the HTTP boundary into a
headless show with a capture sink.

## Recorded live check (manual, not run)

The internal compiled-grammar size limit and the 180 s compile timeout cannot be
checked offline. At #862 bench time, with the operator's key, run **one live
compile per provider** (Claude and each OpenRouter route) of the final wire
schemas: one `reply-request` for `proposal` and `translation-choice` and one
`tools-request`. Record the model, the SHA-256 of each schema resource, whether
it compiled and the first-request time. Until that record exists, acceptance by
Claude is a static expectation, not an observed fact.
