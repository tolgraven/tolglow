# Initial scanner errors checked against Clojure source

Afterglow master 7da8eab and Tolglow master e87e43f. These are source-level dispositions for the clj-kondo outputs. Paths and line numbers below refer to the scanned revisions.

| Source | Finding and status |
|---|---|
| Afterglow effects/fun.clj:244 | **Confirmed.** dimmer-sparkle's partial dimmer loop passes head and assign function to build-head-function-assigner, whose signature is [function head assign-f]. A head using a multifunction channel needs :dimmer, head and the callback, the same order used in effects/channel.clj. The full dimmer-channel path is separate. |
| Afterglow effects/oscillators.clj:237,314 | **Confirmed, two paths.** During simplification of non-frame-dynamic width or direction, the source calls resolve-param with one vector containing all four intended arguments. Fixed-width, fixed-direction and still-frame-dynamic cases bypass this path. effects/oscillators_test.clj already covers literal endpoints, fixed shapes and validation but lacks this simplification case. |
| Afterglow show.clj:174,180,189 (two bindings),198,233 | Six Claypoole pdoseq bindings. Its pool argument precedes the binding vector. These names are lexical in the expanded macro. |
| Afterglow test/afterglow/reactive_test.clj:178,194 | Captured locals in reactive/registration macros, exercised by the reactive test suite. |
| Tolglow cue.clj:242 | **Confirmed.** multi-lfo passes the keyword :alpha as the final argument to apply. vars/auto already accepts each entry, vector or keyword as a regular argument and returns a vector. |
| Tolglow osc.clj:187,189,190,192,198,225 | **Confirmed, same API mismatch at six call sites.** demo and aim-setup send one map; to-vars! only accepts prefix and map, and even that overload returns a lazy map of unary set-variable! calls, but set-variable! needs key plus value. demo also lazily maps receiver registrations and registers /1/aim-a twice. Its sibling paths can remain unregistered. |
| Tolglow param.clj:677 | **Confirmed.** color's resolve-fn expects [show snapshot head], but the non-frame-dynamic branch provides only snapshot and nil. All three branches classify numeric components without including base c, despite c being permitted to be a dynamic color parameter; a changing base alone can be mistaken for a constant. |
| Tolglow fx.clj:63,90 | **Confirmed in effect-maker helpers.** The no-argument function literal around a map calls that map with no arguments. The generated function calls auto-resolve with no p. Calling map over both parameters should supply each p and the explicit show/snapshot/head. |
| Tolglow util.clj:304 | **Confirmed for provided options.** patch! merges an alternating keyword/coordinate vector as the map operand, so a further map/sequence becomes a nested vector element, not flat keyword arguments. patch-cfg! is a separate implementation. |
| Tolglow graphics/pixelflow.clj:20 | **Confirmed namespace form.** [Dw.filter.DwFilter] has a package but no class, and violates clojure.core.specs.alpha ::package-list. The same namespace already imports DwFilter from the real Java package on line 17. Current main Lein manifest also does not include the original optional PixelFlow/Quil libraries. |
| Tolglow graphics/pixelflow.clj:113 (three expressions) | Valid custom inline-2 infix macro; its parser transforms forms starting with numbers to prefix arithmetic recursively. |
| Tolglow graphics/particles.clj:201; graphics/pixelflow.clj:521; viz.cljc:427 | Quil defsketch binds the sketch names. |
| Tolglow src/studio/ namespace errors (8) | Reagent with-let bindings, including the discard binding. The optimized Studio compiler and re-frame tests cover these namespaces. |
| Tolglow config.clj:330–332 | get-getter expands directly to defn and introduces the three names. |
| Tolglow utils/iters.clj (18); test/tolglow/utils/iters_test.clj (6) | Custom iterator loop bindings, recurs and accumulator counts. tests cover 1–4+ arities, nil, empty and multiple accumulators through the normal functions. |
| Tolglow test/tolglow/audio/capture_test.clj:33 | Intentional invalid nil channel count in a thrown? test. |
| Tolglow test/tolglow/event_subscribers_test.clj:141, test/tolglow/studio/connections_host_test.clj:27,52,54 and test/tolglow/studio_test.clj:24 | Captures and with-runtime fixture macros introduce the local names. |

In total, 3 of 11 initial Afterglow error rows and 12 of 59 Tolglow rows correspond to real source defects. Related defects identified by reading the surrounding source are described in their respective rows. The 12 Tolglow rows represent six shared OSC call sites, two effect-maker paths and four independent rows (cue, param, util, PixelFlow import). “Macro binding” is a disposition of the reported lint row and does not establish feature parity for the containing namespace.


## Repairs and regression checks

Tolglow commit `51b486e` repairs the confirmed findings. Its six new tests exercise cue construction and launch, oscillator index bounds, bar-based random changes, sampled and frame-dynamic colors, per-head inputs, fixture patches and OSC handler registration and delivery. The parity suite passed 18 tests with 103 assertions. PixelFlow's redundant import was removed. Its optional Processing/Quil runtime was unavailable here.

Afterglow commit `4ccbdf5` repairs both waveform simplification paths and multifunction dimmer assignment. A related fix preserves the simplified oscillator when all its inputs become constant. The channel and oscillator suites passed nine tests with 33 assertions. [Afterglow changes](https://github.com/tolgraven/afterglow/commit/4ccbdf5)
