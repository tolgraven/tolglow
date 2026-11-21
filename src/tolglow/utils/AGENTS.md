# Collection and numeric helpers

Keep these helpers usable without show state or hardware dependencies. Match
core Clojure value semantics before pursuing speed; benchmark with realistic
head counts and record allocations as well as elapsed time.

`iters.clj` wraps mutable Java iterators. Iterators and transient collections
belong to a single invocation/worker and must not be shared across threads.
Realize `multi-iter-lazy` rows before advancing the outer iterator. Multi-input
mapping stops at the shortest input, including when another input is infinite.
Preserve eager vector output for `mapv-fast` and `mapcatv`.

Run `tolglow.utils.iters-test` after changes here. A function named `fast` does not
establish a speed advantage; compare with core `mapv` and transducer pipelines
before using it in the renderer.
