# Preview camera navigation

The live preview offers **Orbit**, **Explore** and **Auto tour**. These controls
only move the preview camera; they never move fixtures or launch lighting effects.
The same camera controller runs in the DOM whether scene rendering uses the
background worker or the main-thread fallback.

## Using the camera

- **Orbit** lets you drag with a mouse or one finger to orbit. Pinch or use
  `+`/`-` on the focused canvas to zoom. Select an audience or overview
  preset, focus a fixture, or press `Home` to restore the audience view.
- **Explore** lets you drag with a mouse or one finger to look around. Use
  `WASD`, the arrow keys, or `H`/`J`/`K`/`L` (left, back, forward, right)
  to walk along the ground. The camera eases into motion, slows after release,
  and eases through changes of direction. Press `Q`/`E` to go down/up or hold
  Shift to double the selected speed. The six hold buttons offer the same
  directions on iPad and phone. The speed setting ranges from 0.25 to 8 metres
  per second.
- **Touch and trackpad** two-finger swipes move along the ground. Swipe up to
  move forward and right to strafe right. Pinching zooms. Keyboard movement or
  a two-finger swipe starts Explore from Orbit or Auto tour.
- **Auto tour:** moves closer to fixtures that are actually emitting light. It
  visits up to 16 of the brightest fixtures, changing subject every eight seconds
  and immediately retargeting when a subject goes dark. With no lit fixtures it
  moves toward a room overview. Dragging takes over in Explore. Selecting a
  preset also ends the tour. Start Auto tour again to resume automatic movement.

Keyboard controls only act on a focused preview canvas. REPL, editor and form
input keep their keys. Blur, hidden/offscreen state, editing suspension and
unmount clear held movement. The camera pauses when its preview is hidden or
offscreen. Auto tour respects the browser's reduced-motion preference; manual
camera controls remain available. Tour movement and manual travel are bounded
per animation frame, including when a new snapshot or fixture placement arrives.
Room walls and fixture bodies have a camera clearance margin. Existing outside
cutaway presets approach the room gradually instead of snapping to its bounds.

## Integration contract

`CameraNavigator` owns camera input and its animation lifecycle. `WorkerRoomPreview`
sends its resulting camera pose through the existing bounded worker-state channel;
the worker does not install input handlers or a second navigator. `SceneRenderer`
installs the same navigator only for the main-thread fallback. Worker startup
failure preserves the chosen mode, speed, editing suspension and camera pose.
Canvas bounds refresh projection aspect before sending worker state.

Both preview implementations expose:

| Method | Purpose |
| --- | --- |
| `setNavigationMode("orbit" / "walk" / "tour")` | Select a mode and clear held input |
| `setNavigationSpeed(metresPerSecond)` | Validate and set camera speed |
| `moveCamera(direction, pressed)` | Hold/release `forward`, `backward`, `left`, `right`, `up` or `down` |
| `setInteractionActive(boolean)` | Suspend during rig editing; preserve mode and clear held input |
| `getNavigationState()` | Return UI metadata, including mode and current tour subject |
| `onNavigationChange(callback)` | Observe metadata and return an unsubscribe function |

Rig tools must suspend navigation before drawing/dragging and release suspension
on completion, cancellation and unmount. This prevents camera and editing gestures
from competing. Re-frame owns the controls and observed metadata in
`:camera-navigation`; camera pose remains a rendering concern. Camera actions
do not emit Ring lighting commands or modify show state.

Tour selection reads authoritative `rig.fixtures` positions and
`output.values[id]["emitted-color"]`, ranked by linear luminance and fixture ID.
This follows the result of running effects without guessing which effects emit
light or inventing output for dark movement cues. Fixture scans occur on snapshot
changes, not every camera frame. Collision queries use nearby spatial cells;
there is no per-motion scan across every pixel in a large rig.

## Verification and current limits

Pure planner tests cover deterministic lit-fixture selection, bounds, camera
clearance and authoritative target coordinates. Real Chromium input tests cover
worker and forced fallback ground movement, acceleration and braking, tablet
swipes and pinches, trackpad translation and pinch, pointer look and orbit,
gesture cancellation, typing and blur guards, editing suspension, tour
retargeting, manual takeover, reduced motion, hidden state, projection aspect
and disposal. Re-frame tests verify validated local camera effects and
preservation of lighting show state.

Validation on 2026-11-21: 24 preview tests and 138 frontend tests (730 assertions)
passed. The optimized Studio build compiled 148 files with zero warnings. Compiled
UI checks at 1440×1000, 1024×768 and 390×844 verified real mouse/touch movement,
touch drag look, mode changes, reachable 44-pixel controls and no page errors.

This is camera navigation, not a visual-quality milestone. Chromium's software
renderer tests do not establish physical GPU performance or Safari/iPad device
performance. Tour visibility is based on emitted light, not occlusion or cinematic
shot scoring. Camera settings are session-local; show-file camera persistence is
not implemented by this change.
