# Combined Room defaults

The `d5664f52` release from PR #489 starts the Apple M4 ANGLE Metal worker in Smooth mode with work light 0. It renders eight active heads, accepts Follow show frames, and then resumes interpolation. The committed head pan values advance while the controls change. The browser loaded the rebuilt Studio release and the same preview and worker bytes as the original physical packaged-app qualification.

Studio passed 311 tests and 1,720 assertions. Its two production bundles built cleanly. The paired Clojure run used Afterglow `7a54b5d5` and passed 962 tests and 8,515 assertions.

Five exact-revision graphics cases passed on Chrome ANGLE Metal on the same M4.
They covered additive beam crossing, shadow cache and moving dark casters,
fog orbit, opaque depth and attenuation, and transparent boundaries. These
checks read actual GPU output at controlled views. They add no foreground
timing claim.
