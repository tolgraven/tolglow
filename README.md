# Violet-stage Room reference

The browser capture uses Tolglow’s production SceneRenderer in Chromium WebGL2 with SwiftShader. It reads the display raster directly from the canvas. A small disconnected show drives the light output, fixture poses, and settled fog. The camera moves around one committed frame at 40 seconds.

Start with [the wide still](orbit/000.png) and [the foreground wing still](orbit/025.png). The [33 native PNGs](orbit) preserve the original 960 × 540 display raster. The [animated orbit](violet-orbit.gif) uses those same frames at 12 fps and encodes them with a 256-color palette.

- [Committed show input](show-frame.json)
- [Per-frame renderer settings](render.json)
- [Color comparison and warm aperture checks](comparison.json)
- [Capture code and reproduction commands](https://github.com/tolgraven/tolglow/pull/491)
- [Paired Afterglow source](https://github.com/tolgraven/afterglow/commit/482faf3040292d44af7c8eb2f875f204797646bd)
