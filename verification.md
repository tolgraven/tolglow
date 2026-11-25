# Studio Solo and Mute

The Studio patch is `55ebc053b41d1e5299a37f4453c626c33923b4fd`. The test tree combines shared selection in Tolglow `614f64c8` and Afterglow `2084af01` with Studio cue reorder `86e32071`. [Exact revisions](revisions.txt) · [Tolglow #532](https://github.com/tolgraven/tolglow/issues/532)

Studio reads the committed selection projection and sends `:effect-selection/toggle` and `:effect-selection/configure`. The card header has 44-pixel Solo and Mute targets, a separate suppression label, and the existing Detail or Reorder control. Shift-click modifies its own action. Add once arms the next selection action for touch or keyboard. Defaults offers independent Solo and Mute modes. The category and layer rows show mixed selection when only some target members are selected.

The disposable loopback show launched three effects at levels `0.72`, `0.49`, and `0.90`, with three, three, and two dedicated virtual heads. The nine recorded card states lit 8, 3, 6, 3, 3, 0, 3, 6, and 8 heads. The parameter values stayed fixed. The clip is a 10-second sequence of the committed Chromium screenshots. [Initial mix](frames/00-initial.png) · [All effects gated](frames/05-exclusive-solo-ice-zero-output.png) · [Configured defaults](studio-selection-defaults.png) · [Short clip](studio-solo-mute.mp4)

| Check | Result |
| --- | --- |
| `npm run studio:build` | Studio and compute worker release bundles built with zero warnings |
| `npm run studio:test` | 336 tests and 1,978 assertions passed |
| `lein -o test tolglow.effects.selection-test tolglow.studio-test` | 26 tests and 258 assertions passed |
| `lein -o check-definition-spacing` | 547 Clojure files checked |

[Production build](production-build.log) · [Studio suite](studio-tests.log) · [JVM focused suite](jvm-focused.log) · [Definition spacing](definition-spacing.log)

## Browser verification

Chromium 156 loaded the optimized bundle with a SHA-256 matching the generated file. The replay checked 31 projected states with captured output and stable levels. It covered the keyboard and Shift-click on desktop, and touch at 1,024, 390, and 320 CSS pixels. All touch widths retained a 44 × 44 pixel Solo and Mute target on the same row with zero horizontal overflow. The run recorded zero page errors. [Full report](browser-replay.json) · [Browser log](browser-run.log) · [Replay script](replay.mjs) · [Tablet](1024-touch-selection.png) · [Phone](390-touch-selection.png)

The open Studio reflected REPL changes. Changing Solo to additive left Mute exclusive. Ordinary actions returned to their configured modes after releasing Shift or consuming Add once. Category actions shared the same selection policy and showed mixed state when appropriate. Starting and cancelling a #545 reorder preserved the active selection. An in-flight action was rejected after a REPL removal of its exact target. Studio projected a restored layer under a new ID with both members still soloed and only one muted. [Mixed layer](studio-layer-mixed.png)

The browser run used a captured-output loopback show and Chromium touch emulation. [Host launcher](../tolglow-532-selection-owner/motion/studio-preview.clj) · [Shared JVM replay](../tolglow-532-selection-owner/current-replay.edn)
