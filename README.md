# Violet-stage Room preview on an Apple M4

The deterministic rehearsal uses the production Room renderer and a disconnected show runtime. It draws frame 161 at 40,000 ms. The rig has seven emitting heads, a modeled fog machine, a rear curtain, and a stage-wing occluder. Two violet and magenta beams cross through the room haze, three amber apertures light the lower stage, and the wing cuts off light and visibility as the camera passes behind it.

`violet-stage.png` is sample 11. `violet-stage-orbit.gif` is all 33 camera samples at 12 fps and lasts 2.75 seconds. Use `violet-stage-pngs.zip` to review the source color. The GIF’s mean absolute RGB8 channel error is 0.50 across all pixels and 1.11 where at least one original channel is 24 or higher. The 95th percentile of the lit per-pixel maximum channel error is 6. The worst isolated channel error is 150.

`trim-comparison.png` and `trim-comparison-detail.png` show the same camera and input before and after the stage-lip material change. The original polished trim is on the left. The painted stage finish is on the right. The installed material reduced the highest amber red channel in the measured stage-lip rectangle from 86.5 to 0.681 linear HDR units across all 33 samples. The original opaque samples match with fog enabled and at zero density. The diagnostic overrides the one trim mesh in memory to reproduce the prior material in the same bundle.

## Fixed settings

MacBook Air with Apple M4, Chrome 158.0.8127.37, ANGLE Metal, Three.js r187. The desktop scene and display targets are 960 × 540 with 4× MSAA and FXAA. The fog target is 480 × 270 with 48 steps and four shadow-casting lights. The work light is zero. The mixed fog density is 0.102231 and the reported fog level is 0.119961. Bloom is 0.28, exposure 0.85 and grain 0.0015. The display transform is SDR. This is a manually sampled OffscreenCanvas capture with forced GPU synchronization. Measuring preview FPS requires the live worker and bitmap presentation path.

## Repeat

Check out the application revision in `render.json` with the CI-pinned companion Afterglow at `bcf63ec2fe920543fd72e61e8a961cbf765be433`. Install the locked Node dependencies and build the preview bundles with `npm ci` and `npm run preview:build`. The regenerated show frame matched the capture input byte for byte, SHA-256 `2c30fe8d27fc14f54f67c8bfa5c7bc073f28742c619d8ae6bcbe4c0c11496d6c`.

```sh
proof=target/violet-stage
/Users/tol/.local/bin/tolgraven-cpu run --mode host -- lein run -m tolglow.qa.violet-stage "$proof"
cp /path/to/evidence/capture.mjs /path/to/evidence/probe-amber-highlight.mjs "$proof/"
node "$proof/capture.mjs" "$proof" 33
node "$proof/probe-amber-highlight.mjs" "$proof" 33
ffmpeg -v error -framerate 12 -i "$proof/orbit/%03d.png" \
  -vf 'split[a][b];[a]palettegen[p];[b][p]paletteuse=dither=sierra2_4a' \
  -loop 0 "$proof/violet-stage-orbit.gif"
```

The tracked `dev/tolglow/qa/violet_stage.clj` entry point constructs and ticks a local runtime with no Stage or device sink. The capture uses an in-memory replay of the emitted fixture values, optical poses and atmosphere in `show-frame.json`. Both scripts attach to the shared development Chrome CDP session at 127.0.0.1:9223 and create and close a single background tab. They require the M4 Metal GPU for this recorded reference. `dev/preview/violet-stage.mjs` in the application repository provides the software-rendering variant.
