# Browser binding feasibility for Tolglow #540

The Full ring preserved the actual Studio chart paths and numeric captions in a test-only Chromium build. It reduced retained heap and backing storage by about 86% in a 128-history stress case. Both adapters had higher median p95 than the current Full records when rendering the same charts. Keep this binding test-only. [Tolglow #540](https://github.com/tolgraven/tolglow/issues/540)

## Ownership and binding

Studio's visible chart owns local observation state. A future ring could be allocated by that mounted chart and updated from its lifecycle callback. Store an immutable render result or invalidation value in reactive state, and perform the ring mutation outside `swap!`. Preserve source and schema transitions and release the ring with the chart. Push currently keeps its observations in the re-frame display state. This experiment leaves that path and Studio's live path untouched. [Studio local chart lifecycle](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/src/studio/tolglow/studio/parameter_visualization.cljs#L270-L294) · [Push display state](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/src/studio/tolglow/studio/push_display.cljs#L25-L39)

The benchmark page owns the binary64 ring. The snapshot adapter materializes persistent sample records before calling the renderer. The read-view adapter exposes the ring through a read-only ClojureScript sequence scoped to a single synchronous call. Both return realized path or markup strings. The benchmark does not publish the ring or its view through re-frame, and it opens no device or show transport. [Test-only bridge](history_binding_bench.cljs)

## Actual chart results

The standalone Shadow release imports the unchanged `append-sample`, `components`, `chart-paths` and `history-chart` from the pinned application source. Chromium passed **2,976 byte-identical path comparisons** across eight scalar traces and all six history windows. It passed **96 exact chart markup comparisons** including captions, duplicates, same-beat replacement, gaps, finite overrange values, a source switch and a rewind. [Product history renderer](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/src/studio/tolglow/studio/parameter_visualization.cljs#L154-L213)

| History binding | Admission + path median p95 | Median p95 at full history | Live heap + backing, 128 full histories |
| --- | ---: | ---: | ---: |
| Current Full records | 6.6 ms | 14.2 ms | 6,177,992 B |
| Full ring with immutable draw snapshot | 9.9 ms | 23.5 ms | 886,072 B |
| Full ring with scoped read view | 17.3 ms | 15.8 ms | 885,412 B |

The timing replay feeds eight traces at simulated 40 Hz and builds eight paths on each admitted state, alternating the app's 4- and 16-beat windows. It uses the median of three fresh-page p95 results after a warm-up. Saturated run-level p95 varied from 4.6 to 15.8 ms for current records and from 10.7 to 24.7 ms for the scoped read view. Browser timer resolution was 0.1 ms and cannot resolve the fastest admission-only samples. [Timing and heap receipt](binding-report.json)

The memory stress test retains 128 independent histories of 256 samples each. It records CDP live heap and backing storage deltas after garbage collection on a fresh page. The result measures the history store under this workload. It excludes whole-app heap, Reagent, layout, SVG rasterization, Push capture and display transport. The measured adapters still allocate per-sample maps and vectors while computing each path. That bridge work offsets the cheaper admission and lower retention cost in the timing replay.

## Reproduce the standalone build

Set `TOLGLOW_REPO` and `AFTERGLOW_REPO` to local checkouts with the commits below. Set `NPM_DIR` to a compatible existing `node_modules` containing Playwright Core and Studio dependencies. Java 21, Leiningen, cached Maven dependencies and Chromium were used. `LEIN_BIN` and `LEIN_HOME` can select an existing Leiningen install and offline cache profile.

```sh
artifact="$PWD"
scratch="$(mktemp -d)"
mkdir -p "$scratch/paired-afterglow"
git -C "$TOLGLOW_REPO" archive 56a785c527034a5914f0f1833c10741c2f4c8282 \
  project.clj dev/tolglow/build_manifest.clj src/studio src/shared | tar -x -C "$scratch"
git -C "$AFTERGLOW_REPO" archive 99f61c5bed2850daaeb87bc5b84ddf0d3c60bf64 \
  project.clj src | tar -x -C "$scratch/paired-afterglow"
cp "$artifact/shadow-cljs.edn" "$scratch/"
cp "$artifact/history_binding_bench.cljs" "$scratch/src/studio/"
ln -s "$NPM_DIR" "$scratch/node_modules"
(
  cd "$scratch"
  AFTERGLOW_ROOT="$scratch/paired-afterglow" "${LEIN_BIN:-lein}" -o with-profile +cljs \
    run -m shadow.cljs.devtools.cli --npm release history-binding-bench
)
PLAYWRIGHT_MODULE="$NPM_DIR/playwright-core" node benchmark.mjs "$scratch/build/main.js"
```

The offline release compiled 119 files with zero warnings. The tested bundle SHA-256 is `778e57d168fd8e1bc8e033b47b69b3a49e413d827c28ed42e3e5c20bb025b7c5`. The harness uses Chromium 156.0.8078.79 and the sibling incremental ring and precision fixture directories.
