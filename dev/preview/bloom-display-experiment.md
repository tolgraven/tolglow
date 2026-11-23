# Room bloom and display experiment

This private experiment starts from Tolglow `f087e180f39c735c91b5307295ceff3b3e583160`. It reuses the production Room renderer and the normal frame worker. The application bundles and renderer stay untouched.

Three `0.187.0` runs its bright extraction, all five horizontal and vertical blur pairs, and the five-mip combination in both arms. The fused arm intercepts the pinned `UnrealBloomPass._fsQuad` only when it would draw the final `CopyShader` into the composer input. The copy uses unit opacity and premultiplied `ONE, ONE` additive blending. The next pass samples that same mip texture and adds all four channels at the same internal resolution before the existing display transform. The GLSL3 variant rounds the sum through `packHalf2x16` and `unpackHalf2x16` to model the original RGBA16F write. The adapter checks the Three revision, draw shape, target format, absence of masking and the composer pass order. Recheck these assumptions on a Three upgrade before enabling the probe.

Glass runs before bloom with its original immutable fogged source. Disabled bloom takes the original output shader. Exposure, grading, grain, FXAA, blur settings and presentation resampling follow the original passes. The experiment changes one draw and adds one texture fetch and half-float rounding to the display shader. A performance gain needs a device measurement.

## Pixel and draw gate

```sh
CHROMIUM_PATH=/path/to/chromium node --test test/js/preview/experiments/bloom-display-browser.test.mjs
```

The test reads the completed canvas after the actual WebGL Room pass chain and checks that the baseline repeats. With SwiftShader and the 193 × 112 scene rendered to a 257 × 149 canvas, the SDR haze was byte-identical and dropped from 145 to 144 draws. Disabling bloom kept both frames at 132 draws and byte-identical. With glass and the extended tone-map shader branch, draws fell from 146 to 145 and two of 153,172 RGBA8 values differed by one level each. The gate for that case is one level at most and no more than 0.1% changed channels. That case runs the extended transform on an SDR surface. It leaves extended HDR output on a physical display unverified.

## MacBook Air comparison

Use an unlocked, foreground Mac on AC power. Close other active rendering tests and keep the display mode, browser zoom and window fixed. This replays one captured show frame in a new headed Chromium window. It does not launch the show host or open output devices. Generate the input with `lein run -m tolglow.qa.violet-stage /tmp/bloom-air-capture` against the paired Afterglow release, or use the archived frame below. [Captured stage input](https://raw.githubusercontent.com/tolgraven/tolglow/b08610fedfe7c88e0c60523921eb0ce2df8f1643/show-frame.json)

Keep the Mac's screen-lock guard running across all four arms. Chromium focus and visibility are recorded separately because they can remain true while macOS locks. A capture with unknown lock state is diagnostic only. Run frequency and active-residency sampling in a separate comparison to avoid changing the main A/B load.

At DPR 2, a 1403 × 630 CSS canvas with fixed Mobile tier and scale 0.5 produces a 914 × 410 scene, 365 × 164 fog and 2806 × 1260 display. At DPR 1.8 use a 1559 × 700 CSS canvas to obtain the same targets. Resize or maximize the headed window before retrying if the full canvas cannot fit. The fog raster is Mobile on this master checkout and differs from the Auto fog raster in #489.

```sh
cat > /tmp/bloom-air-dimensions.json <<'EOF'
{"css":[1403,630],"scene":[914,410],"fog":[365,164],"display":[2806,1260],"quality":"mobile","scale":0.5}
EOF
CHROMIUM_PATH='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' \
  node dev/preview/measure-bloom-display.mjs \
  /tmp/bloom-air-capture/show-frame.json /tmp/bloom-air-dimensions.json /tmp/bloom-air-receipt.json
```

The runner uses baseline, fused, fused, baseline order with a 20-second warmup and a 20-second measurement per arm. It records only bitmaps accepted by the DOM presenter, counts window RAF separately, and rejects foreground loss, worker fallback, an unexpected raster or changing render settings. The receipt includes the snapshot, bundled worker, Room loader and both display shader source hashes. Compare per-arm accepted Hz and p50/p95/p99 intervals. CPU submission, GPU fence wall time, bitmap export and completed worker time have separate fields. The shader hashes cover source input and GLSL version, separate from the device’s translated shader binary. Leave the native probe and readback tools out of timed trials.

This procedure measures the optimizer alone on a fixed frame. If both repetitions agree, rerun the two arms on the same moving and repeated-plume workloads before drawing a broader cadence conclusion. Keep their exact camera, accepted show frames, quality and raster recorded. Use a separate correctness run for pixel readback and material inspection.
