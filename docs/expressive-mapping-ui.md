# Expressive mapping in Studio

Connections → **Make every touch count** edits the [shared expressive policies](expressive-mapping.md) for Push 3, Seaboard M and
explicit generic MPE policies. The panel is Reagent/re-frame; it does not discover
or connect MIDI devices. Use the same stable device ID as the controller connection.
Saving or restoring a profile never opens hardware.

Choose a cue, then edit either its overrides or defaults for every cue on the
selected device. A device expression switch disables expressive parameter writes
while preserving note launches and releases. Cue enablement inherits by default;
a cue cannot override a disabled device. Automatic mapping can be disabled while
retaining manual mappings. Per-parameter **Off** stores nil, **Manual** stores a
binding, and **Inherit / automatic** removes that scope's override. Reset cue removes
its whole override. Drafts remain separate per device while switching cues.

Manual bindings expose the documented input axis, response curve, inversion and
input/output range. Compound colors, vectors and ranges support independent
component bindings. The shared parameter widgets format values and display the
backend's sampled transfer shape. Range input preserves incomplete typing and
rejects non-finite numbers. Axis switches combine device and cue policy; a cue
cannot re-enable an input disabled at device scope.

Every preview calls `POST /api/expressive/preview` with the full draft device
policy. The response supplies actual mappings, diagnostics, schema, effect
version, supported axes/ranges and sample values. No semantic inference or mapping
math is copied into ClojureScript. **Parameter version** can select an actual
running instance's captured schema; the default previews the cue definition.
Refreshing explicitly re-reads versions after definition changes.

Glide means MPE pitch bend, slide means CC74, and pressure means the reported
pressure dimension. These are not calibrated native Push pad X/Y coordinates or
physical force. Device mode selects documented capabilities for preview; generic
MPE requires explicit axes. Controller kind and temporary preview selection are
local UI choices, not added to the portable settings schema.

**Save device mappings** posts validated `expressive` settings with the selected
device draft and the latest known other device policies. Settings are included in
show export/import. The backend remains authoritative. Preview and save replies
are scoped to runtime identity, epoch, request generation, device, cue and running
instance. A replacement show resets drafts. Inputs are disabled during saving so
an older acknowledgement cannot mark a newer edit saved. Delayed previews are
ignored after switching target or editing again.

Controls have at least 44 px touch targets. The layout uses four columns on wide
screens, two on iPad-sized widths, and a single column for phone selection controls;
parameter cards retain compact side-by-side range endpoints. Browser viewport
checks do not certify physical MIDI devices, touch sensors, or Safari.

Validated against an optimized Studio build integrated with master `9a70836` on 2026-11-21:

- Frontend: 159 tests / 861 assertions, zero failures; the generated Node runner
  was executed explicitly and returned exit 0.
- Optimized build: 150 files, zero warnings.
- Fourteen actual Chromium workflows against the Ring host passed: connected
  Push 3 capability preview, note/pressure parameter writes, manual slide with
  smoothstep/inversion/range, reload restoration, independent RGB components,
  device disable retaining note release/new launches, device/cue re-enable,
  parameter Off/reset, Seaboard ownership, generic MPE/namespaced profile IDs,
  desktop/iPad/phone geometry, and full show export/import. No browser errors.
- Viewports: 1280×1000, 834×1112 and 390×844; the panel had no horizontal overflow
  and all tested interactive targets were at least 44 px high.

MIDI transports were injected capture fixtures connected to real device sessions
and the actual Studio model. No physical device output, hardware calibration or
Safari certification is claimed. Source tests cover stale preview/save replies
across runtime, epoch, cue, device and instance changes, separate device drafts,
namespace-safe JSON, incomplete numeric edits, and component preservation.

Local evidence and the reproducible Ring/session/browser harness are preserved in
`/tmp/tolglow-expressive-qa/`. The roadmap validator also passed with 107 items.
Newer master changes are handled separately by the integration coordinator.
