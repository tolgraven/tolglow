# Measure room fog with iPhone

In the iPhone companion, open **Rig → Measure room fog**, connect to Studio and
choose a patched fog machine or **Unmetered · manual burst**. The latter works
when the show has no patched fogger. The phone measures the loss of contrast along one
sightline. A second sightline or a repeat run can reveal uneven mixing in the
room.

1. Set up a stand and a matte two-tone target in the room. Illuminate it with a
   fixed light. Keep the light, target, background and phone position unchanged
   for the full run. Measure the distance from rear camera to target in metres (supported range 0.1–1000 m).
2. Start with clear air. Enter the distance and choose **Lock camera and align**.
   The camera locks shutter, ISO, focus, white balance, zoom and SDR transfer
   for the run. Fill the left box with the white target and the right box with
   its adjacent black patch. The crop outlines follow the sampled pixels.
   If a patch clips, cancel and select a shorter shutter time.
3. Choose **Start clear-air baseline**. At least eight accepted samples establish
   the reference. While those arrive, hold the fog machine off. Choose
   **Mark fog active**, then operate a normal fog cue or the machine's controls.
   The phone records samples across the onset and mixing period. Its buttons
   only mark the capture.
4. Release the fog output and choose **Mark clearing**. Keep the app foreground
   and leave the camera and target untouched as contrast recovers. **Fit
   measurements** asks the host to estimate clearance. If it needs a longer
   trace, leave the camera observing and fit again.
5. Review the returned half-life, sample count and fit quality, and choose
   **Apply to show** to save. When Tolglow observed enough delivered fog DMX,
   the host can also fit a mixing time and an extinction coefficient. A
   manually operated external fog machine can provide a clearance-only result.

Both patches are measured in the same SDR frame. Each uses a 16 by 16 sample
grid, rejects clipping and checks that its four quadrants agree. The signal is
linear luminance of white minus black. Its ratio to the clear-air signal is the
measured transmission. Ratios are sensitive to changes in target illumination
as well as fog. The phone checks camera attitude and acceleration, stops on
foreground loss or camera interruption and sends readings at about one per
second. Camera frames stay on the phone. A fresh run needs fresh clear air.

## Host contract

The phone reads the runtime identity, epoch and available foggers from
`GET /api/fog-calibration`. All POST commands there carry `op`, `runtime-id`,
`expected-epoch` and a unique `session-id`.

| Operation | Additional data |
| --- | --- |
| `start` | Measured `path-m` and, for a metered fog cue, `fixture-id` |
| `samples` | `samples` array with `phase`, `elapsed-ms`, linear `bright` and `dark` |
| `fit` | Accepted reply contains the current fit, including `clearance-half-life-s` |
| `apply` | Save fitted values after explicit review |
| `cancel` | End the measurement |

Sample times use the phone's monotonic clock at camera delivery, relative to
the midpoint of the accepted session-start exchange. This avoids differences
in calendar clocks. The alignment uncertainty includes camera delivery latency
and network asymmetry. A start round trip over one second requires a new
attempt. The host owns the fit and the record of delivered output.

Run `swift test` from `native/Core` for target sampling, contrast quality and
pose checks. `native/Tests/check_macos.sh` also builds both Apple simulator
targets. Repeat the procedure on an iPhone with the room, target and machine
that will be used for the show to evaluate repeatability.
