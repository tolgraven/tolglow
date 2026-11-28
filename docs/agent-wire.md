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
  one reply. OpenRouter requests carry `provider.require_parameters` so only
  providers that honour the schema serve them.

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

1. **Enum lowercasing** (`lower-enums`) wherever the reply follows the schema.
2. **`validate-arguments`** against the wire schema: a missing field, an unknown
   key (for example `ms` in a timing, or a goal's `order` on a group header), a
   wrong type or an enum value outside the list is refused with its path.
3. **Owner bounds** (`owner-bounds`): counts and ranges the wire cannot carry,
   such as at most three proposals, `confidence` from 0 to 1, four intent tags,
   one `until` position and nonnegative positions. They use `validate-arguments`'
   bound keywords on the same paths.
4. **Lowering** (`lower-proposal`) of each proposal's events to the
   `schedule_events` vocabulary: `at` sums its fields as a zero-based position,
   `in` and `grid` take one nonzero unit, `now` posts without timing, and only the
   slot `value_kind` names is read. A group header becomes the events' label.
   Guards, expiry and cancellation by intent are carried as data for the proposal
   contract (#826); only events reach the scheduler today.
5. **The owner** (`post-proposal!` through `show/dispatch-tool!`): one atomic batch
   per proposal with its `basis_rev` as the command's expected show revision. The
   owner refuses an unknown cue (`Unknown cue ghost`), a passed position or a
   stale basis (`:stale-revision`); the refusal carries the wire path it names.

From the REPL, against the active runtime `rt`:

```clojure
(require '[tolglow.integrations.agent-wire :as wire] '[tolglow.show :as show])
(let [{:keys [ok? proposals errors]} (wire/admit-proposals (wire/example :proposal))]
  (if ok?
    (wire/post-proposal! (partial show/dispatch-tool! rt) 0 (first proposals)
                         {:command-id "repl/wire-1" :run-id "repl"})
    errors))
```

The example's basis revision (1844) belongs to its recording, so a live show
refuses it as stale; set `basis_rev` to the show's `:revision` to post it. The
posted events appear in `show/schedule`, the shared schedule projection Studio
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
