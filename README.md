# Effect provenance in Studio Debug

Captured on Tolglow `e256343f87e5029c3986538e6884ff3b72687f72` and Afterglow `e1eb9ff50985a9e3e304b54ec7a97db735956c5a`. The show used preview-only output. Chromium received the byte-matched optimized Studio bundle from the merged cue timing build.

Two effects emitted the same custom keyword and map in the same frame. Studio committed distinct Ember and Ice rows and showed the corresponding subscribers. The Ember effect completed in its emitting frame. Its subscriber-launched follow-up was visible as Unobserved. One Debug socket stayed open, and there were no browser exceptions. The focused JVM suite passed 62 tests and 607 assertions.
