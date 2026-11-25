# Studio cue timing walkthrough

Tolglow `f7710ef1051d136004e69f40ec543a626b3159ad`, Afterglow `a3530e450913792994d3a787b31b5f05a852041e`.

A disposable loopback Ring host and production Studio bundle were used with Chromium. The replay queues and cancels a one-shot, cancels a held pad on release while the transport remains paused, and fires a new cue after transport resumes. Its scheduled beat was 16 and its owner admission was beat 16.00905. The phase capture shows the live LFO editor saved with Show transport. The same queue also worked at a 390-pixel touch viewport with a 44-pixel control height.

The final focused Tolglow lane passed 54 tests and 529 assertions. The paired Afterglow pure suite passed 640 tests and 9,947 assertions. Studio passed 309 tests and 1,708 assertions. The optimized Studio and compute bundles built with no warnings.

The Push replay uses captured MIDI and in-memory DMX. Two notes pressed at 130 and 135 milliseconds fire in order at beat 1 after a tempo change from 120 to 60 BPM. The Live tests use an injected MIDI transport and clip host. The browser footage is interface verification.

## Missed launch notice

Tolglow `59a7a9cf17f8943ab00638a3aa9bf7707cfb42e3`, Afterglow `e1eb9ff50985a9e3e304b54ec7a97db735956c5a`, optimized Studio JS SHA-256 `807c5f65800654fbd8616738f802f4f5c711c8a0e0ebe131eada1856e1d54b55`.

Two browser sessions watched Ember queue. Changing the source made it miss. The initiating touch session received one “Ember missed its scheduled launch because the show clock changed” notification. The observing session received none. The notice cleared normally while the stream remained connected.

The updated focused lane passed 55 tests and 535 assertions. The updated Studio source suite passed 311 tests and 1,718 assertions. Both optimized bundles built with zero warnings.
