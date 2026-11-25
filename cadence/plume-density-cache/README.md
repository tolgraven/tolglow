# M4 dense plume cache experiment

This scratch test uses the production Room renderer at `38d19ea2`, Chrome 158.0.8127.88, and ANGLE Metal on an Apple M4. It compares the existing GLSL with a candidate that evaluates plume density into a half-float texture once per draw. The ray march uses linearly filtered density from that texture inside the plume bounds. The experiment leaves light sampling, step budgets, and image resolution intact.

Full fog GPU timings bracket the density pass, volume pass and depth composite. The fixed fresh-plume test used an Auto fog raster of 301 × 135 and a Desktop fog raster of 480 × 270. Both scenes used eight live heads and the same camera, beams and density field for each matched comparison. Baselines repeated pixel-for-pixel. Blocks alternated baseline and candidate in both directions.

At 80³ samples, one comparison reduced median Auto GPU time from 3.027 ms to 2.346 ms. Desktop fell from 7.368 ms to 6.691 ms. The gain includes regenerating the texture on every draw. Root-mean-square error of the largest RGB8 difference per lit pixel was 0.50 for Auto and 0.41 for Desktop. The static Auto image had 582 pixels whose largest channel difference exceeded three values. The Desktop image had 35. Extent-proportional grids occupied fewer samples with similar timing and changed more pixels in the Auto view.

The motion run stepped the plume 12 times at 50 ms intervals, with a small camera orbit. It measured changes relative to the analytic reference at the same time and camera. The largest lit-pixel spatial RMS was 0.50 for Auto and 0.44 for Desktop. The largest frame-to-frame residual RMS per lit channel was 0.33 and 0.31. This is a short synthetic optics test. Full interactive cadence and longer multi-plume scenarios remain to be measured.

`gap-attribution.json` separately analyzes the earlier live pass comparison at `25714194`. Across two full-render controls, 270 of 364 presentation gaps at least 25 ms followed worker acknowledgements that arrived after a nominal refresh interval. That finding describes the combined GPU, transfer and acknowledgement path. It leaves some missed refreshes unexplained.

Run from a checkout of `38d19ea2` with its Node dependencies, on an M4 with the shared Chrome debugging session at 127.0.0.1:9223.

```sh
mkdir -p target/bloom-air
cp /path/to/plume-density-cache.mjs target/bloom-air/
node target/bloom-air/plume-density-cache.mjs ./plume-density-cache.json 32 48
node target/bloom-air/plume-density-cache.mjs ./plume-density-cache-fine.json 64 80
node target/bloom-air/plume-density-cache.mjs ./plume-density-cache-isotropic.json 80 iso80 iso96
node target/bloom-air/plume-density-cache.mjs ./plume-density-cache-motion.json 80 --motion
```

The test closes its one background tab. It uses generated fixture data directly in the renderer. No show or output devices connect.
