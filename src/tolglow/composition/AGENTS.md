# Pure composition

These namespaces receive explicit logical clocks and return data. Tweens
and envelopes must sample arbitrary times without relying on previous frames.
Test zero-duration envelope stages, release during attack, and terminal values.
Beat sequences receive explicit musical position and tempo: beat position drives
boundaries, while tempo only estimates milliseconds for the UI. A paused clock
does not advance just because elapsed milliseconds increase. Backward seeks need
a fresh execution; do not infer past tempo changes from the current BPM.

Sequence state belongs to one unique execution owner. Launch/release tokens are
correlated by the shell with captured runtime instance IDs; never stop by cue
key or slot position. Repeated ticks must not duplicate launches. Missed steps
currently skip to the final step with bounded traversal; do not silently change
that to a burst of hardware commands. Macro slots have no implicit clock.

Response middleware transforms an instance's per-frame parameter copy before
the compiled effect runs. Never write those modulated values back into the model
or they compound across frames. Compile response chains per response revision;
do not capture them permanently in an effect-only compiler cache. A completed
response retains its terminal level until explicitly removed, and does not end
the underlying effects. Selectors and multiple response order are explicit.

Use the actual production compiler/runtime for integration tests. New expression
operators belong in the shared Afterglow IR, not a second arithmetic interpreter.
