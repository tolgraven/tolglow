# Video bridge (Syphon)

`main.swift` is a macOS command-line helper the JVM show starts per Syphon source (#672). It receives frames and writes the TGVF feed that `tolglow.video.source` reads. It never sends lighting output and starts nothing on its own; the owner closes stdin to stop it.

- Keep the TGVF layout in step with `tolglow.video.source/read-feed` and `docs/video-strips.md`. Never truncate a feed file a reader may have mapped.
- Syphon.framework is not vendored and is never taken from another application's bundle. `build` fetches the official Syphon-Framework source at a pinned commit and builds it, or links against `SYPHON_FRAMEWORK_DIR` (an operator-provided build of the official framework). Change the pin deliberately.
- Run `TolglowVideoBridge self-test --out-dir <dir>` after changes; it checks discovery of two sources, colours, resize, isolation, retirement and return against in-process servers. `lein run -m tolglow.qa-video-syphon` checks the chain into captured show DMX.
- A real Resolume sender, the packaged app's helper and signing still need evidence on that setup.
