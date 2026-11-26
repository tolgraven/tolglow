# Video on LED strips (2D mapping)

Run Resolume or another Syphon sender on the same Mac, pick its feed, and lay pixel strips over the picture. Each strip pixel takes the colour under its share of the strip. The result is an ordinary effect output, so a video cue sits in the cue grid next to every other cue. [Issue #672](https://github.com/tolgraven/tolglow/issues/672) · To project the same sources onto planes, wavy sheets or spheres in the room, see [video on 3D surfaces](video-surfaces.md).

## Sources

`[:settings :video :sources]` names the sources a show uses. They are saved with the show and reopened on load.

| Kind | Settings | Use |
| --- | --- | --- |
| `:test-pattern` | `:width`, `:height`, `:pattern` (`:grid` or `:gradient`) | The labelled 8 × 4 colour grid for checking endpoint order, crop and isolation. |
| `:image` | `:file` (inside the show directory) or `:resource` (bundled under `video/`) | A PNG or JPEG still, at most 8192 × 8192 pixels and 64 MiB. |
| `:feed` | `:file` (inside the show directory) | A TGVF file written by the native video bridge or a test. |
| `:syphon` | `:app`, `:name`, optional `:max-width`/`:max-height` (16–4096, default 1024) | A Syphon server, selected by application and server name. |

A Syphon selection follows the sender by name. When Resolume quits and returns, the strips pick it up again without reselecting. Status shows the source as **stale** after a second without a new frame or when the sender has gone. The strips keep the last frame meanwhile, so a sender restart does not black out the rig. A sender that changes resolution keeps its mapping, because strip coordinates are fractions of the frame.

The show opens sources on its own thread after a settings change commits, never inside the lighting frame.

**Limits.** A show uses at most 8 video sources, and at most 4 of them can be Syphon sources. Each Syphon source runs one helper process. Edits beyond these limits are refused, and so is loading a show that exceeds them. File sources resolve against the show directory: the working directory, which the Mac app sets to the show folder, or `:video-options {:show-directory …}`. A path with a `..` segment is refused with `:video/path-traversal`. A path outside that directory, including one reached through a symlink, is refused with `:video/path-outside-show`. Studio's edit is rejected with that code. A saved show carrying such a path lists the source as unavailable with the code and never reads the file. A still's byte size and header dimensions are checked before it is decoded (`:video/image-too-large`). The preview JPEG is never larger than 1920 pixels on either side. An encoding over 4 MiB is refused with HTTP 413.

## Mapping strips

A mapping places strips over one source. Coordinates run from 0 to 1: `u` left to right, `v` top to bottom.

```clojure
{:id :video/wall :source :syphon/arena
 :strips [{:id :tube-1 :layout :tube-1             ; a pixel layout's heads in wire order
           :points [[0.1 0.9] [0.1 0.1]]           ; first pixel samples near the first point
           :height 0.04}                           ; sampling extent across the strip, fraction of frame height
          {:id :truss :targets [:par-1 :par-2 :par-3]
           :points [[0.2 0.5] [0.5 0.6] [0.8 0.5]] ; a polyline
           :height 0.2 :filter :nearest}]}
```

Point order sets strip direction. `:reverse? true` makes the first head sample the far end without moving the geometry. The strip's pixel count comes from its pixel layout or target list.

**Filtering.** `:box` (the default) averages each pixel's footprint: its share of the strip length times the sampling height. Up to 4 × 4 bilinear reads are spread evenly over it. `:nearest` reads the single source pixel under the footprint centre. Colours are averaged in linear light (sRGB is decoded first), and the result is the linear RGB the effect stack uses. A footprint outside the frame reads black.

## A video cue

```clojure
{:id :video-wall :version 1
 :params {:drift {:type :number :default 0.0 :bounds [-1 1]}}
 :outputs {:color {:op :video :mapping :video/wall
                   :placement {:x {:op :param :key :drift}}}}}
```

`{:op :video}` feeds `:color`. With `:channel :luma` it feeds a scalar attribute such as `:dimmer` with Rec. 709 luminance. `:placement` moves the whole mapping: `:x` and `:y` in source units, `:scale` about the frame centre, `:rotation` in degrees. Each value is a numeric expression, so cue parameters, modulation and macros can move the sampled area. A video output writes only the heads its mapping resolves.

Because this is a normal effect:

- priority and blend compose it with other cues, and a higher cue can take over one strip while video keeps the rest;
- instance opacity and envelopes scale it;
- stopping the transport holds the frame the show stopped on. Play resumes on the newest frame without catching up. A paused instance keeps the frames it last rendered while other cues continue;
- forecasting never interpolates it, and it runs on the ordinary CPU path. The typed batch backend falls back with the reason `:video`.

The show reads every source once per lighting frame. All video cues in that frame see the same frames.

## Studio

**Rig & space → Video strips** lists the sources with their status (live, stale, frozen while the transport is stopped, or unavailable with the reason). **Add test grid** adds the calibration grid. **Find Syphon sources** asks the native helper which servers are publishing. **Use** selects one.

**Map strips on this source** creates a mapping. The editor shows the latched frame, refreshed up to four times a second. Over it, each strip is drawn as the footprints the sampler reads, filled with the colours it sampled. A white dot marks the first pixel. Click a strip to select it. Drag its line to move it, or drag an end handle to change direction and length. The strip editor rotates by 15°, shortens or lengthens about the centre, sets sampling height and filter, flips the first pixel, and removes the strip. **Add a strip** places an unmapped pixel layout across the frame.

## REPL

The same operations work from the REPL. Geometry helpers are in `tolglow.video.mapping`.

```clojure
(require '[tolglow.show :as show] '[tolglow.video.mapping :as vm])

@(show/video-action! runtime :edit {:op :put-source :id :test-grid
                                    :source {:kind :test-pattern}})
@(show/video-action! runtime :edit {:op :put-mapping
                                    :mapping {:id :video/test-grid :source :test-grid}})
@(show/video-action! runtime :edit
   {:op :put-strip :mapping-id :video/test-grid
    :strip {:id :tube-1 :layout :tube-1 :height 0.04
            :points (vm/rotate [[0.1 0.5] [0.9 0.5]] 15 16/9)}})

(show/video-action! runtime :status {})   ; sources and sampled strip colours
(show/video-action! runtime :syphon {})   ; Syphon servers through the helper
```

Studio sends the same `:video-mapping` message through `POST /api/video-mapping`. It reads `GET /api/video`, `GET /api/video/syphon` and `GET /api/video/frame?source=:id&width=640` (a JPEG of the latched frame). A REPL edit shows up in the open editor through the state stream.

## The native feed boundary

Syphon shares GPU textures between processes on one Mac. The JVM cannot read them directly. The helper `native/VideoBridge` (`TolglowVideoBridge`) is a Swift tool linked against Syphon.framework.

- `list` prints the Syphon servers as JSON lines.
- `feed --app A --name N --out FILE` receives one server through `SyphonMetalClient`. It scales each frame on the GPU to fit `--max-width`/`--max-height` and copies it into a memory-mapped **TGVF** file. It stops when its stdin closes.
- `serve` publishes the labelled grid, and `self-test` runs the whole receive path against two in-process servers.

A TGVF file is a 320-byte header followed by BGRA8 pixels. The header holds the magic, a seqlock sequence (odd while writing), width, height, stride, frame count, write time in Unix microseconds, a connected flag, capacity and a UTF-8 label. `tolglow.video.source/read-feed` documents the layout. The writer never truncates the file, so a reader that has it mapped never faults. The JVM copies a frame only when the sequence moves, and retries a torn copy.

The hub starts one helper per selected Syphon source. It finds the helper through `TOLGLOW_VIDEO_BRIDGE` or the `:video-options {:bridge path}` runtime option. Without a helper, a Syphon source stays listed as unavailable with the reason, and the rest of the show runs.

### What the operator runs

Syphon.framework is not vendored, and it must never be copied out of another application's bundle. `native/VideoBridge/build` fetches the official [Syphon-Framework](https://github.com/Syphon/Syphon-Framework) source (BSD 2-clause) at a pinned commit, refuses any other revision and builds the framework with `xcodebuild` under `target/syphon-framework/`. The only published binary release (SDK 5, 2019) predates the Metal client and server the helper uses, so the build starts from source. It needs Xcode and network access on the first run:

```sh
native/VideoBridge/build   # writes target/video-bridge/TolglowVideoBridge
TOLGLOW_VIDEO_BRIDGE=$PWD/target/video-bridge/TolglowVideoBridge lein run -m tolglow.qa-video-syphon
```

`SYPHON_FRAMEWORK_DIR=<directory containing Syphon.framework>` skips the fetch for an operator-provided build of the same official framework.

The packaged Studio app does not bundle the helper yet. Until it does, set `TOLGLOW_VIDEO_BRIDGE` before starting the show. The app's `LocalShow.swift` passes its environment to the show process.

**Browser clients.** Studio in any browser sees the latched frame as JPEG stills and the sampled colours. It never receives the Syphon texture. A full-rate preview in the Mac window needs the paired Afterglow fork's GPU transport. Shader cues publish their frames into the same latch under feed ids, so strips map onto them like any source; see [Shader cues](shader-cues.md).

## Cost

`lein run -m tolglow.bench-video-strips` maps 2,000 RGB pixels on 25 strips of 80, box filtered with a sampling height. These are medians on the M4 Pro (JDK 27, other agents loading the machine):

| Step | 1024 × 576 | 1920 × 1080 |
| --- | --- | --- |
| Sample along a cached plan | 0.22 ms | 0.22 ms |
| Compiled video output (sample, colour map, assignments) | 1.2 ms | 1.1 ms |
| Plan rebuild (placement moved every frame) | 3.0 ms | 2.2 ms |

A whole headless show frame with that cue takes a 3.0 ms median (p95 4.7 ms). The same rig with a solid-colour cue takes 0.72 ms. At 60 Hz the frame budget is 16.7 ms. The extra cost over a solid cue is the ordinary per-target path. A typed contribution block for video would remove most of it.
