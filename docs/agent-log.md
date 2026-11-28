# Agent decision log

Status: implemented for the tool loop (#856, first slice). The Studio chat and
the proposer path do not write a log yet; see [Gaps](#gaps).

Every provider round of `tolglow.integrations.agent/run!` is one decision.
With the option `:on-decision`, `run!` hands the caller one portable map per
round after its tools ran. `tolglow.agent-log` turns that map into a bounded
record, and `tolglow.agent-log.writer` appends it to a JSON Lines file. Replay
feeds the logged reply back in place of the model.

The design is in the research report,
[interface.md, section 5](research/native-agent/interface.md#5-determinism-and-replay).
The implemented schema is
[`resources/schemas/agent-log-1.schema.json`](../resources/schemas/agent-log-1.schema.json).

## The record

`tolglow-agent-log/1` keeps the research schema's fields and adds `round`,
`session.run`, `session.conversation`, `request`, `omitted` and `redacted`.

| Field | Holds |
| --- | --- |
| `session` | Show, epoch and runtime from the host, plus the run and conversation. A command ID is `<run>/<tool call ID>` |
| `actor`, `policy` | The role and profile from the host. Backend and model come from the provider config |
| `observation` | The token, kind and digest of what the policy received. When no resolver supplies one (#923), the request messages count as the observation: their digest, with their blob as `snapshot_ref` |
| `request` | Digest of what was sent, the messages blob, and the digest of the declared tools. Replay must use the same tools |
| `output` | Blob of the raw streamed reply, byte for byte; blob of the parsed text and calls |
| `validation` | `accepted`, or `rejected` with per-call path errors (at most 16) |
| `commands` | Each call's command ID, tool, validity, argument digest and arguments. Arguments over 1 KiB go to a blob. Each dispatched call also has a bounded receipt (status, code, message, event IDs, due beats) with the digest of its full result |
| `outcomes` | Outcomes known at once: invalid calls and owner refusals. Later scheduled-event fates are not joined yet |
| `clock` | Beat, frame and revision observed, from the host. `latency_ms.policy` is the provider round; `latency_ms.admit` is validation and dispatch. Both are kept apart from frame timing |

Bounds: one JSON line is at most 64 KiB and one blob at most 4 MiB. A blob
over its bound is not written and is named in `omitted`. A record over its
bound is refused, never truncated: the hook counts it as `refused` on the
writer and the run goes on unchanged. `validate-record` checks the schema
(through `validate-arguments`), the live and offline rule, and the size.

Digests use `afterglow.runtime.replay/digest` over JSON data, so key order and
keyword or string keys do not change them. Blob addresses are the SHA-256 of
the exact UTF-8 bytes.

## Private data

- `run!` replaces the provider credential in every string before the hook
  sees the decision. Provider headers never reach the log.
- `scrub` replaces map entries with secret names (authorization, API keys,
  passwords, access tokens and similar) and secret-shaped text (provider key
  prefixes, bearer tokens), including a secret-named JSON field inside the
  raw reply. A field split across stream fragments is not recognised. A record
  with anything scrubbed has `redacted: true`. A scrubbed reply replays with
  the scrubbed value.
- The log stays local and keeps full fidelity apart from secrets: goal text,
  show names and the request messages are kept, as in a backup. Writing a log
  shares nothing. The training export (an explicit selection, with provenance
  and removal) is not built yet.

## The writer

`writer/open!` starts one daemon thread that drains a bounded queue (default
256 records). `writer/log!` uses `offer` and never blocks: when the queue is
full, or the writer is closed, the record is dropped and counted. Blobs are
written once to a temporary file, then moved atomically to
`blobs/sha256/<hex>` beside the log. Then the line is appended. Nothing runs
inside `swap!`. `stats` reports accepted, written, dropped, refused and
failed counts. `close!` writes what is queued and waits for it.

```clojure
(require '[tolglow.agent-log :as log] '[tolglow.agent-log.writer :as writer])
(def w (writer/open! "target/agent-log/night-1.jsonl"))
(def context (log/model-context (:model @(:state rt)) {:runtime "studio-1"
                                                      :actor {:role "orchestrator" :profile "default"}}))
(agent/run! config credential messages agent/default-tools dispatch! emit!
            {:on-decision (log/decision-hook w context)})
(writer/close! w)
```

The context is read when the policy observes. `model-context` takes show,
epoch, beat, frame and revision from a committed model.

## Decision replay

`replay-decision` never calls a model. It decodes the logged raw reply with
`agent/decode-reply`, the decoder a live round uses, then validates each call
with `agent/call-problems`. The result is compared with the logged commands:
command ID, tool, validity and argument digest. Replay refuses, in this order:

| Code | When |
| --- | --- |
| `:invalid-record` | The record fails `validate-record` |
| `:epoch-mismatch` | The show is in another epoch |
| `:basis-mismatch` | The supplied revision, observation digest or token differs from the record |
| `:basis-missing` | The basis gives none of them |
| `:tools-mismatch` | The tool declarations differ from the logged digest |
| `:missing-reply` | The reply was left out, is missing, no longer matches its address, or was cut at the capture bound |
| `:reply-unreadable` | The reply does not decode |
| `:commands-differ` | The reply no longer yields the logged commands |

`replay!` then sends each valid, first-time command through a dispatcher.
Each command keeps its logged command ID, run, conversation and
`:expected-epoch`, so the show owner refuses another epoch as well. It returns
the receipts and whether they match the logged ones.

```clojure
(log/replay! (first (writer/read-records "target/agent-log/night-1.jsonl"))
             {:blob (writer/blob-loader "target/agent-log/night-1.jsonl")
              :basis (log/model-basis (:model @(:state rt)))}
             #(show/dispatch-tool! rt %1 %2 %3))
```

## REPL and Studio

The REPL owns the whole path today: `decision-hook`, the writer,
`read-records` and `replay!`. Studio has no log view. Its chat
(`agents/start-chat!`) does not pass `:on-decision` yet: where a show keeps
its logs, and how long, are operator decisions (D6 covers thumbnail
retention).

## Gaps

- Studio chat, background tool jobs and the proposer path (#917, #924) are
  not wired to a writer.
- Show replay byte for byte against a #670 take's `replay-signature` is not
  claimed. The test replays one decision onto a fresh owner and compares the
  receipts and the scheduled events.
- The full snapshot at least every 32 decisions needs the observation resolver
  (#923).
- Later outcomes from the schedule's `:recent` list, feedback, the look before
  and after, `window` and rankers are not written. Neither is the training
  export.
- A round cancelled during its tool calls, or failed by its provider stream,
  logs no decision.
