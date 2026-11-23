# Reproduce the preview worker acknowledgement race

Apply this local diagnostic to a disposable checkout of
`355c97e3eef0a6cb542fdd604ad1789173aba867` (draft #489). Run the following commands from the repository root with Node 24
or later and the locked npm dependencies installed.

```sh
node dev/preview/replay-worker-visibility-ack.mjs > /tmp/preview-worker-ack.jsonl
node --test test/js/preview/worker-clock.test.mjs test/js/preview/preview-activity.test.mjs test/js/preview/render-quality.test.mjs
```

The first command exits nonzero if the production message flow, Auto policy,
or telemetry differs from the behavior shown here. It uses the CPU-only
boundary from the worker-clock tests with a synthetic clock, page visibility
events, and simulated GPU completion. It runs the actual proxy, state channel,
worker, display clock, and Auto policy. The timings are simulated milliseconds.

| Scenario | RAF to ACK | Reported `readyForFrameMs` | Reported `measuredFrameMs` | Result |
| --- | ---: | ---: | ---: | --- |
| Continuous, scale 1.0 | 7 | 7 | 7 | Desktop holds scale 1.0 |
| Hide, ACK while hidden, then show | 7 | 5087 | 16 | Desktop holds scale 1.0 |
| Hide, show, apply visible state, then ACK with three preceding over-budget samples | 7 | 5087 | 5087 | Desktop drops scale to 0.5 |
| Same order at scale 0.5 with seven preceding over-budget samples | 7 | 6747 | 6747 | Desktop drops to mobile at scale 0.65 |
| Continuous, scale 0.5 with seven preceding over-budget samples | 7 | 7 | 7 | Desktop holds scale 0.5 |

In the first failing run the generation 1 frame has a 5080 ms RAF timestamp
and is transferred at 5084 ms. Hide advances to generation 2. Show advances to
generation 3 and applies the updated state at 5085 ms. The proxy closes the old
bitmap and sends its ACK at 5087 ms. The worker releases the pending frame slot
and schedules generation 3. The first generation 3 frame starts with `dt=0` and
publishes the erroneous timing sample. Its own ACK measures 7 ms. The target
remains 60 Hz throughout.
