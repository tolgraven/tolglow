# Local studio UI

The browser application lives in `resources/studio/` and is served by the local
`tolglow.studio` Ring application at `/`. It is a working first studio iteration, separate
from the historical Afterglow web UI. It uses ClojureScript, Reagent and re-frame,
with a local WebGL2 preview and an optional real Neovim WebAssembly
editor. It is not a claim of full product completion or AAA rendering quality.

## Performing

The Perform view presents one 8×8 page at a time, a transport/BPM control, master
intensity, blackout, active-instance inspector, and a live rig preview. Page cells
are coordinate records and refer to stable cue IDs. Active pads are projections
of actual instances; releasing instances remain visually distinct until the
engine retires them.

* **Toggle** launches a persistent instance or releases active instances for that
  cue. **One-shot** uses the cue's declared duration. **Hold** launches on pointer
  down and releases its exact instance on pointer up/cancel.
* Touch pointers have independent ownership. Cancellation, lost capture, window
  blur, hidden visibility and page exit release held instances. If release
  precedes acknowledgement, the arriving instance ID is immediately ended with
  the original epoch. The session heartbeat supports the shell's final lease
  timeout if a browser disappears completely.
* A 500 ms long press opens cue actions in toggle/one-shot mode; Edit grid also
  makes ordinary taps open actions. Users can copy a cue,
  paste a new cue into an empty cell, assign an existing library cue, clear a
  cell, inspect/launch it, edit its definition or append it to the composition.
  Clearing a cell preserves the definition. Copy/paste creates a new stable cue
  ID referencing the same effect, with independent cue parameter defaults.
* Shift+F10 or the context menu key opens cell actions. Arrow keys navigate the
  grid. Enter/Space activate a focused pad, including keyboard-held release.
  Outside editors, **B** toggles blackout, **Space** transport, and **1–7** switch
  views (Assistant, Connections and Develop are 5, 6 and 7). All inputs have labels; a skip link reaches the main controls.
* Inspector controls follow captured effect parameter schemas: bounded numbers
  use sliders, integers use integral steps, booleans checkboxes, enums selects,
  RGB(A) colors a color input that retains alpha and vectors component inputs.
  Parameter writes are coalesced at the I/O boundary. Optimistic values remain
  visible until the corresponding acknowledged revision arrives, and incomplete
  numeric edits (such as a leading minus sign) survive frame updates.

Auto mode is a disclosure beneath the inspector. It configures cue interval,
count, page/all scope, launch mode and duration through `/api/auto`; the server's
policy supplies cue selection and emission dependencies. Persistent mode leaves
instances active until explicitly released. This is a policy-driven local mode,
not an LLM that autonomously invents new definitions.

Response envelopes have a separate disclosure. An attack/release gate multiplies
named numeric parameters globally, across the selected effect type, or on the
selected instance. Both stages share a musical unit (or explicit milliseconds),
with linear, smooth, ease-in and ease-out curves. Starting/retriggering and releasing
invoke the response API. Release reaches zero and stays there until the response
is removed; removal restores the original, unmodified parameter values.

## Compositions and definitions

Compose reuses one ordered slot model for macros and sequences. Slots contain a
cue ID, parameter overrides and a duration. Duration controls default to musical
units: fractional beats, bars and phrases, with milliseconds as an advanced
compatibility option. New sequence slots default to one beat. The UI exposes cue
selection, duration, reorder and removal. Each slot expands into the same typed
parameter controls used by live effects. Overrides compose with cue/effect
defaults, survive save/reopen, and can reset to defaults without changing a
running effect. Macros launch together; sequences use once,
loop, ping-pong or random traversal. Save, save-and-play, stop and reopening saved
compositions all invoke the corresponding local API.

In **Definitions**, search the registry by effect or cue name or ID. The selected
entry shows its version. **New** opens an unpublished draft. The same toolbar
switches between EDN and the visual builder and validates and publishes the
current editor. Publication feedback appears beside the editor settings.
On a phone the registry scrolls horizontally and leaves the code below it
available for editing. The text editor and the Bruvbox Neovim editor share the
same publication action. EDN source is loaded from the server, validated,
compiled and published. The server prints a loaded definition as pretty-printed
EDN, and **Generate EDN** uses the same layout
([shared printer](repl-studio-parity.md#read-a-definition-as-studio-shows-it)).
The visual builder
creates RGB constant color effects with an adjustable intensity parameter,
duration and attack/release envelope, optionally with a matching cue. This is a
small working code-free subset; it is not yet a general visual expression graph.
Fixture types have their own editor in Rig & space. Empty grid cells can place
the resulting cue.
“Build registry page” reconciles an 8×8 page with registered effects, preserves
existing occupied cells, and reports new placements, conflicts and overflow.

In the visual builder, **Evaluate when inputs change** is the opt-in for a
reactive effect. Choose target fixtures, bind aliases to ratom IDs or
subscription queries, and pick the alias that drives the dimmer. Leave the beat
transition blank for an immediate update. For an existing reactive effect,
loading its definition opens the same inputs and targets in the visual builder.
Its output expressions stay in EDN. The owner validates the published
definition and rejects edits based on an older revision. Reload the definition
after a show replacement or after saving changes to its EDN before publishing
again from the visual builder.

Existing edits send `expected-version` for optimistic publication. The shell
owns actual version advancement. “Publish valid edits” debounces publication;
validation errors remain visible and do not replace the last valid definition.
This option is explicit and off when loading another definition. Arbitrary
Clojure source is not evaluated by the definition endpoint. Validation runs at
publish time; the textarea itself does not claim a live Clojure linter.

The definition status follows the text, not the edit history. Studio keeps the
published baseline: the source last loaded from or accepted by the running show.
Text equal to it shows that baseline's status (`ember · v2` or Published); any
other text shows Unpublished changes. A publication reply marks Published only
when the editor still holds the text that was sent, so an edit (or a late Neovim
delivery) that arrives while the request is in flight stays unpublished and, with
Publish valid edits on, is published next. A refused publication follows the same
rule: Publication failed describes the text Studio still holds, while newer text
stays Unpublished changes (the result names the refusal of the earlier text) and,
with Publish valid edits on, goes out after the usual 850 ms quiet time. Switching
from Neovim to the text editor and **Generate EDN** are edits too: their status
follows the baseline. Neovim echoes of the text Studio already holds change
nothing.

Every controlled checkbox in Studio uses one component
(`tolglow.studio.checkbox/input`). React restores a controlled checkbox to its
rendered state when the change event ends, before Reagent's next frame renders
the new state; the component renders pending updates first, so the box never
shows its old state for a frame. A checkbox whose handler only sends a request
shows the committed state until the reply arrives.

The default editor is a labeled accessible textarea with Tab indentation and
Ctrl/Cmd+Enter publication. Selecting Neovim loads the actual pinned WASM runtime
reused from Tolgraven. Its Worker owns the editable buffer; the UI consumes
`ext_linegrid` redraws, sends native input/paste/mouse RPC and reads real buffer
lines before publication or return to the textarea. Escape, Insert, Tab and Undo
buttons support touch use. Worker and ResizeObserver teardown are explicit.

Neovim requires a secure cross-origin-isolated context with SharedArrayBuffer
(localhost or HTTPS with COOP/COEP). The accessible editor remains available when
that is unavailable. Assets are local, approximately 30 MB; initial startup is
therefore heavier than the rest of the studio. The studio does not include
Tolgraven's plugins, host filesystem access, external-process language servers,
or a durable filesystem. `:w` writes the browser's temporary buffer file, not the
host checkout or lighting registry. See
[`vendor/neovim/README.md`](../resources/studio/vendor/neovim/README.md) for provenance
and bundled licenses.

A code-cue editor can construct the engine with `pinBuffer: true` (#311; no Studio
view uses it yet). It pins the buffer current at start and keeps reading, writing
and diagnosing that buffer when the user opens another file in the window. Each
`onChange` carries a draft `{sessionId, bufferId, changedtick, sourceSha256,
source}`. `setDraftDiagnostics(draft, findings)` applies findings (zero-based line,
UTF-8 byte column) only while session, buffer, changedtick and digest still match,
and resolves `false` for an earlier draft, a closed buffer, a stopped editor or a
Neovim that has exited (`:qa!`). Any edit clears the findings. They show their message at the end of the line, also in
insert mode. `:edit!` and reopening after `:bdelete` keep the observer and report
the buffer's new text as a change; while the buffer is unloaded the engine reports
one close through `onError`. The caller still fences replies against its own show
epoch, cue and definition version, and converts host positions to byte columns.
The host's definition API reports data paths, not source positions.

### Assigning and inspecting effect blocks

In **Library**, open an effect in the EDN editor and select **Assign blocks**.
Choose a parameter or output under **Block binding**, pick a material or waveform,
then select **Add stage**. The catalog includes the effect’s own prefab definitions.
Choose **Inputs** to change a constant, reference another parameter, or use the
beat, bar, phrase or time clock. **Coordinates** selects world XYZ or surface UV.
The arrow buttons change stage order. Each control addresses its stage by a
stable ID (metadata on the stage map, never written to the EDN), not by the
index it was rendered at, so edits that land before the next render, such as two
quick taps on Remove, reach the stage they were made for. **Wet amount** controls how much of that
stage reaches the next one. **Add private or variable binding** accepts a private
varying name or a space-separated show variable path such as `weather density`.

**Write stages to EDN** updates the block map in the current draft and preserves
surrounding source and comments. In a pretty-printed definition the block map
gets the lines a fresh print would give it, after `:blocks` or on the next line
as its size needs; a block map written on one line among other entries stays on
one line. **Validate & publish** sends that definition to
the show. With **Publish valid edits** enabled the normal live-edit policy applies.
In the visual RGB builder, use **Assign blocks** before **Generate EDN**.

In **Perform**, expand the running effect and open **Block trace**. Choose its
binding and fixture. The stage and component selectors compare the dry value,
signed offset, stage result and final value on one scale. The plot retains up to
96 observed show timestamps. Closing the inspector, changing its target or
scrolling it outside the viewport releases the old WebSocket query. Push uses
the same plot when its display mode is `:block-trace`, fed by the selected live
instance’s passive display snapshots.

## Rig and preview

Rig & space edits room dimensions and fixture names, XYZ positions in metres,
universes and addresses. The existing channel maps and fixture metadata survive
position edits. Quick add creates a 12-channel RGBW moving-head template; saving
submits the complete rig for validation. The runtime is responsible for rejecting
overlapping or invalid channel mappings. A top-down SVG plan edits fixture and
truss X/Z positions by dragging, or by keyboard arrows in 0.1 m steps (Shift for
1 m). Height is retained. Truss forms edit name, height, length and Y rotation;
these are saved in the same rig as fixture positions. The plan shows up to 128
fixtures. These are draft edits until Save rig, after which the performance
preview uses the saved geometry. This is a plan editor, not yet a 3D transform
gizmo or fixture attachment/structural engineering system.

The fixture library searches the saved catalog or explicitly searches Open
Fixture Library, downloads mode information, and imports a chosen mode for
review. Local uploads accept OFL JSON, QLC+ QXF and GDTF. Import does not publish
a definition or patch a fixture automatically. Provenance, license information
and diagnostics stay visible. Unsupported channel semantics require review
rather than hidden guesses.

A code-free type form edits labels, footprint, channel coarse/fine offsets and
inversion, with an advanced EDN definition for full metadata. It can start from a
custom RGBW PAR template. Publication validates the type; a separate patch step
sets the instance address, XYZ position, orientation, RGBW mixing and explicit
moving-axis calibration. Unmapped channels or invalid physical mappings block
patching. Existing unsaved rig edits must be saved before adding an instance.
The UI supports these three import families, not every fixture format.

The preview consumes `output.values[fixture-id].emitted-color`, positions and
pan/tilt values. It never invents light from an active cue indicator; blackout and
master therefore affect the preview through resolved output. A single WebGL2
fragment shader ray marches a finite room with floor/wall/ceiling surfaces,
fixture bodies, cone illumination, attenuation and volumetric scattering. Colors
are tone mapped for display, not photometrically calibrated fixture predictions.

A persistent CPU 32×16×32 density field starts with uneven low haze and accepts
shaped plumes. Bounded drift, diffusion and decay evolve it independently of
effect frames; a 3D texture supplies the ray marcher. Reducing atmosphere to zero
removes visible volume while surfaces still receive light. The “Atmosphere” and
“+ Plume” controls affect local preview fog only; they never trigger a physical fog
machine. This is a coarse visual approximation, not fluid simulation.

The first sixteen fixtures and eight trusses are rendered, with a visible notice
above those limits. Trusses are yaw-oriented metal box beams with analytic ray
intersections. They stop fog integration at the nearest beam and obscure fixture
bodies behind them; they are not detailed lattice meshes or a general shadow map.
There are 40 ray steps on phone widths and 64 otherwise, bounded device-pixel
ratios, one RAF loop capped at 30 fps, and a 10 Hz fog update. Hidden tabs/views pause graphics
work. Drag or arrow keys orbit, wheel/pinch/+/- zoom, and Home or reset restores
the camera. Graphics context loss has a visible recovery state. The preview
currently lacks comprehensive light/shadow occlusion between scene objects,
realistic gobos, fixture-specific optics, reflections, detailed truss meshes, architectural imports,
Gaussian splats and accurate fog fluid dynamics. Those remain renderer work,
not hidden simulated functionality.

### Room in its own window

**Pop out** moves the Perform Room into a separate window so the cue grid
keeps the main window; **Move to display** sends that window to another display
(and pops it out first when the Room is docked). **Redock**, or closing the
window, brings the Room back; reloading or navigating that window closes it
and redocks too, and reloading or closing Studio closes it. `room_window.cljs` owns the state
(`[:ui :room-window]`: docked, popping, popped, redocking) and the window
handle.

The window is an empty same-origin `window.open` page. Studio portals its own
Room panel into it, so the panel reads the same app-db, the same state socket
and the same commands; a cue changed in Studio reaches the Room on the next
render. The window loads its own copy of the preview module
(`engine-loader.js`), so the single preview engine, its worker, observers,
display clock and pixel ratio belong to the window that shows the canvas. The
docked engine is disposed in the React commit that mounts the popped-out one,
and the reverse on redock: there is one engine, one worker and one WebGL
context throughout, and the show itself is never touched. Each document makes
a fresh canvas; no canvas moves between documents. The engine restarts on each
move, so the Room shows "Preparing preview" for its first frame again.

While popped out the Room renders whenever its window is visible, although
Studio has keyboard focus (the canvas announces an external `room-window`
owner, as PiP and sharing do). If the Studio window itself is hidden, the Room
window flushes pending Reagent renders from its own animation frames.

Why not the alternatives: Document Picture-in-Picture is Chromium-only, stays
small and always on top, and cannot be placed on or fill another display. A
second Studio route subscribed as another viewer would open a second socket
and app instance and needs its own renderer lifecycle coordination; the portal
keeps one of each.

Move to display uses the Window Management API (`getScreenDetails`, with its
permission prompt) to pick a display other than Studio's, cycling through
three or more, and moves and resizes the window to that display's available
area. Press Fullscreen there to fill it. A single display (`screen.isExtended`
false) is reported without a prompt. A denied permission, a browser without
the API, or a browser that silently ignores the move each leave the window
usable with an explanation of manual placement. Removing the Room's display is
announced, and reconnecting one offers Move to display again. With display
access granted, the Room remembers the display it was placed on (label and
rectangle), so losing that display is announced even when two or more remain;
Move to display then uses another free one. In the packaged Mac app, moving a
fullscreen Room window first leaves fullscreen and places it once that
animation ends, and the Room window closes if Studio's web content process
ends. PiP and sharing
controls move with the Room; their capture belongs to the panel, so popping
out or redocking ends an active PiP or share instead of leaving an encoder on
a canvas that no longer exists. Start it again from the new location.

`dev/studio/room-pop-out-qa.mjs` exercises this in headless Chrome against a
production build with emulated displays (CDP `Emulation.addScreen`) and a
granted or denied permission. It also loses the Room's own display among three,
emulates a hidden Studio (document hidden, Studio animation frames held) and
checks that the Room keeps following the show, and shares video from the
popped-out Room with the real encoder while the script answers the receiver
endpoints: Stop sharing, moving the window while sharing (one session) and
redocking (session stopped, no further uploads). That is not evidence for
physical displays, hot-plugging, a really hidden or minimised window, Safari,
the packaged Mac app, a LAN receiver or real AirPlay receivers.

### Arranging the Perform panels

**Edit layout** in the Perform heading enters an explicit layout edit mode for
the Cues, Live, Room and Tools panels and any added panels (#668). Outside it
nothing changes: pads, sliders and touch gestures behave as always. In edit
mode every panel's content is inert and dimmed and a frame shows the grid.
Drag a panel's title onto another panel to move it there (onto a later panel
lands after it, onto an earlier one before it); the panel under the pointer is
marked as the drop target. Drag the corner to resize it in grid columns and
20 px rows, with the size shown while dragging. Holding a drag near the top or
bottom of the visible area scrolls the page, so a panel can be dropped onto
one that started off screen; it stops at the end of the page as it was when the
drag began. Every move and resize also works without
dragging: the title and corner are buttons that take arrow keys (Home and End
move to the ends; Shift steps the corner by four), and each frame has Earlier,
Later, Narrower, Wider, Shorter and Taller buttons whose accessible names start
with those words. A screen reader hears the new position and size from a live
region that stays mounted. Escape cancels a drag in progress, and otherwise
cancels editing. Panels flow in their list order without dense packing, so the
visual order is the keyboard and screen reader order; a gap can remain where a
wide panel does not fit beside the one before it. A save is confirmed only
after local storage accepted it; if the browser refuses (private mode, full
storage), Studio says so and the layout stays in use until reload.

**Named layouts.** Each screen size class keeps any number of named layouts
and remembers the active one; the menu beside Edit layout switches between
them and the built-in layout. While editing, **Name** renames the layout,
**Done** saves it (a new layout gets a free name such as "Layout 2"; a name
another layout already has gets a number), **Save as new** keeps the original
and saves the edits under a new name, **Delete layout** (confirmed in a second
step) removes it, **Cancel** discards the edits, and **Reset to default** switches
this screen to the built-in responsive layout without deleting saved layouts.

**Added panels.** **+ Parameter tile** adds a tile showing one cue parameter,
picked from a menu (running cues first) on the tile or in its edit frame.
**+ Visualization** adds one panel that plots the running effects' numeric,
vector and colour parameters. Added panels have **Remove** in their frame. A
tile binds to a cue ID and a parameter name, not to an instance, so it follows
the cue's latest launch, says when the cue is not running and survives
reloads. Tiles write through the same `:studio/param-set` command and shared
widgets as the Live cards.

**Content follows the tile size.** A tile's tier comes from its share of the
grid: *full* (the shared control and a history, colour or 3D visualization),
*compact* (a numeric readout and the compact control; the Live cards drop
their plots) and *single*, the 1×1 tile (one reading; the Room keeps only its
3D view). A tile is single at up to a sixth of the columns (two on desktop,
one on tablet or phone) or at most 8 rows, and compact at up to a third of the
columns or at most 14 rows. Growing a tile back restores its full controls and
keeps its chosen parameter. A tile's **Source** opens the source editor inside
the tile, with its own element IDs, not in the Live card (which may be in another
window).

**Any panel in its own window.** Every frame has **Detach**/**Redock**. The Room
uses its pop-out window above. Other panels use `tolglow.studio.panel-window`,
which generalises the same mechanism and reuses its window state machine: the
panel's own Reagent content is portalled into one same-origin `window.open`
window that receives Studio's stylesheets. It reads the same app-db and sends
the same events over Studio's single socket; nothing is duplicated and no
cue is relaunched. While a panel is detached its tile shows a placeholder with
Show window and Redock, and its content exists in that window only. Closing
the window, pressing Redock, closing Studio, or switching to a layout without
that panel (including the layout of another screen size class) brings it back.
Plots in a panel window follow that window's visibility, not Studio's, so they
keep drawing while Studio is hidden, and the cue pad previews in a Cues window
follow that window's focus. Content in a window takes its tier from the window,
not from the tile it left: the full width of the window's size class and its
height in rows (single up to 179 px, compact up to 299 px, else full), for the
Room as for other panels. Removing a detached tile in edit mode closes its
window at once; Cancel brings the tile back docked.

Layouts are kept per screen size class, matching the built-in breakpoints:
phone (up to 600 px, 2 columns), tablet (601–1199 px, 6 columns) and desktop
(1200 px and wider, 12 columns). They are device preferences, so they live in
this browser's local storage (`tolglow.studio.panel-layouts`, version 2; the
single-layout version 1 becomes "Layout 1"), not in the show document, a
backup or the portable settings API; another device or browser keeps its own.
In the packaged Mac app the loopback port, and with it the page's storage,
changes on every launch. When the app's window binding (#669,
`window.tolglowStudioWindow`) carries a `layouts` copy, Studio uses whichever
of that copy and local storage was saved last (each records `savedAt`), and
every save also posts `{type: "layouts", layouts, request}` to the app's
`tolglowStudioWindow` message handler so the app can keep it per show folder.
The app answers with a `tolglow-studio-window` event whose detail is
`{type: "layouts", request, kept}`; it refuses a copy over 64 KB. Inside the app
a save is confirmed only once the app says it kept its copy, because only that
copy survives a relaunch; a refusal, or no answer within 3 s, warns that the
layout will be gone after the app relaunches. In a browser local storage
decides, as above.
Editing a new layout starts from a measurement of the built-in layout on
screen. A saved layout is repaired on load: unknown panels are dropped, missing
Perform panels come back at a default size, invalid tile bindings are cleared
and sizes are clamped to the grid. If the window changes size class while
editing, editing ends without saving. On a phone a saved layout replaces the
Cues/Live/Room section tabs with the arranged stack. The Room's 3D view fills
its tile and the controls under it scroll when the tile is small.

Panels are keyed children of one CSS grid. Moving one moves its DOM node and
resizing changes its grid span, and switching between layouts keeps every
panel both layouts share, so no panel is remounted: a running cue's controls,
the Room's preview engine and worker, and the single state socket are the same
objects before and after. Detaching a panel does remount its content, in the
new window. Detached windows are not reopened on reload, because a browser
only opens windows from a user gesture.

`dev/studio/panel-layout-qa.mjs` drags, resizes, cancels, saves, reloads,
detaches, repairs and resets at 1440×900, 1024×768 and 390×844 in headless
Chrome, with CDP touch events at the two touch sizes. It also names, switches
and deletes layouts, adds a tile, picks its parameter, shrinks it to 1×1 and
grows it back, adds the visualization panel, and (desktop and tablet) detaches
the Live panel and a tile, acts in the Live window, then redocks by closing
the window and by Redock. It shrinks the Room and Live windows and checks
that their content turns compact, opens the tile's own source editor in the
tile window, removes a detached tile in edit mode, holds a second finger on
another handle during a tablet auto-scroll and lifts it, and (desktop) checks
that a Cues window samples its pads while Studio lacks focus (Studio's focus is
faked; headless Chrome reports focus for every page). Throughout it checks that
the running cue keeps its instance ID, that the card and Room canvas stay the
same DOM nodes where no panel was detached, and that there is one socket and
one preview worker. It simulates the Mac app's bindings with stand-in message
handlers: the layouts answer (kept, refused, none) and the window host's
identify and close messages. It is not evidence for iPad Safari, a physical
touch screen or the packaged Mac app.

In the packaged Mac app `native/macOS/RoomWindows.swift` hosts every blank
same-origin `window.open` from Studio's main frame in its own native window,
cascaded and titled from the page's `document.title`. Studio identifies each
window right after opening it (`{action: "identify", role: "room" | "panel",
id}` on the `tolglowRoomWindow` handler, posted from the new window's own
document when it has the handler, else from Studio's, which names the window
opened last). Only the Room's window is placed on another display. Closing a
window sends `{status: "closed", role, id}` on the `tolglow-room-window` event,
so the Room and each panel redock only for their own window.
`native/Tests/RoomWindows` checks the roles, close replies and display choice;
the window lifetime itself has not been exercised in the running app.

`dev/studio/definition-controls-qa.mjs` uses Playwright `check()` and `uncheck()`
on Publish valid edits, Create matching cue, Evaluate when inputs change and the
auto-launch visual activity box; holds a refused publication while a newer edit
is typed and checks the follow-up; checks the status after Generate EDN; and
taps one stage's Remove twice in one task. Headless Chrome only.

## API and lifecycle

The local Studio listens on loopback. Connected operators can use the show's
REPL and any enabled workspace, terminal or agent facilities. A LAN bind needs
either authenticated access or the explicit `:trusted-lan? true` setting.

### Connecting an iPad

Keep Studio on loopback and forward a dedicated HTTPS hostname to that listener
using a TLS reverse proxy. Preserve the public Host header. Set the following
module options in your local Tolglow configuration. Use the actual HTTPS origin
where the iPad will connect.

```clojure
:web-server {:enabled true :host "127.0.0.1" :port 8500
             :public-origin "https://lighting.example.net"
             :auth {:token-env "TOLGLOW_STUDIO_TOKEN"}}
```

Set the named environment variable to a random password of at least 24
characters before starting Tolglow. Open the public URL in Safari, sign in as
`studio` and enter that password. The browser receives an HTTP-only, same-site
session cookie for all Studio routes and WebSocket upgrades. Sessions expire
after twelve hours and restarting the listener invalidates them. For a native
client, send `Authorization: Bearer <operator token>` to the same HTTPS origin.
Keep the token out of show files and URLs.

A venue host can also terminate TLS in Jetty. Point `:tls` at a PKCS12 keystore
with a certificate the iPad trusts for the chosen hostname. Jetty uses the
configured port exclusively for HTTPS in this mode.

```clojure
:web-server {:enabled true :host "0.0.0.0" :port 8500
             :allowed-hosts ["lighting.local"]
             :auth {:token-env "TOLGLOW_STUDIO_TOKEN"}
             :tls {:keystore "/etc/tolglow/studio.p12"
                   :password-env "TOLGLOW_KEYSTORE_PASSWORD"}}
```

For an explicitly trusted, isolated network, `:trusted-lan? true` allows an
anonymous listener, granting every connected client full operator privileges.
Browser mutations and both WebSocket upgrades check scheme, hostname and port.

Studio opens one state WebSocket for each connected tab. The first frame is a
full Transit projection. The server coalesces subsequent changes to the
configured output cadence, with a 40 Hz ceiling, and sends typed path patches
against the last acknowledged state. A reconnect or missing base requests a
fresh snapshot. The HTTP state route also serves explicit inspection requests.

Commands carry unique command IDs, `{adapter:"web",session-id}` source, runtime
identity and the current show epoch. Delayed held releases retain their original
identity, so they cannot address a replacement server process. The commands use
the shared reducer operations, not
private widget mutation functions. JSON page cells use `{x,y,cue-id}` records
because vector map keys do not round trip through JSON objects.

Definitions, rig, compositions and import/export use their documented studio
endpoints. Error responses appear in a toast or the editor validation area.
Export produces EDN; import accepts show files up to 16 MiB and returns to the new epoch. No physical output is
enabled by opening the studio. Output mode is reported by the shell.

The selected view, cue page and launch mode are portable UI preferences saved
through `/api/settings` and restored on startup. The browser restores only this
explicit whitelist, never provider credentials or editor/chat drafts. These are
server-owned show settings rather than browser local-storage state.

Application state is the re-frame app-db. `events.cljs` owns explicit event
transitions and effect descriptions; `subs.cljs` defines projections;
`views.cljs` renders Reagent components; `chat.cljs` supplies provider/chat events
and views. `interop.cljs` owns lifecycle acquisition/cleanup for two specialized
JavaScript engines: WebGL rendering and Neovim's WASM/MessagePack canvas. These
engines neither mutate application controls nor own show commands. The HTML is
only a mount point. There is no parallel vanilla JavaScript application.

Controlled form updates dispatch synchronously from input handlers, keeping
React's value/selection restoration consistent. Network operations stay in
effects. Authoring views receive bounded projections so renderer clock updates
do not redraw unrelated editors. Standard command/gesture events remain queued.

Studio navigation takes its order, titles and named Lucide icons from the shared
Reitit page routes. The sidebar and phone navigation use the same links. The
number keys 1–8 follow their displayed order. Short labels fit the phone bar,
while each link keeps its full accessible name. Icons ship with the application
and work offline. On phones all eight cue columns fit across the page, and the
transport includes the file controls.

## Assistant and provider connection

On the first visit, **Connect a model** puts the provider, model ID and API key
together. Open **Custom endpoint** to override a hosted provider’s URL. Choosing
**Self-hosted / compatible** shows the server endpoint immediately and makes the
API key optional. You can draft a message before saving the connection.

Run `lein run -m tolglow.qa.studio-browser assistant-layout` after building the Studio bundle to exercise first use, connection editing, a loopback model response and restored history in an isolated preview show.

Once the provider is ready, the conversation is the primary workspace. Open
**Model connection** to edit settings. **Clear saved key** appears when the
selected provider has a key. Credentials are cleared from the form on submit.
The server uses its separately configured local credential store when available,
or session memory otherwise. Only opaque references enter portable show data;
the actual keys are excluded from public configuration and exports. Saving
configuration does not call a model.

Chat uses the actual `/api/chat` SSE response. Text deltas accumulate into the
assistant message; tool starts/results appear as expandable activities. The
server owns conversation history: `/api/chat/history` restores saved conversations,
the picker selects one through `/api/chat/select`, and continued requests supply
its conversation ID rather than a client copy of previous messages. Late stream
or history responses cannot replace a newer conversation. Streaming follows the
latest message until the user scrolls up. The server supplies rig/show
context and validates every semantic tool operation. Stop aborts the stream,
which cancels future generation; it does not undo already accepted show commands.
No synthetic assistant replies or simulated model sessions are used. Provider
availability and a configured model determine whether a real conversation can run.
Restoring a show/history does not resume a generation automatically. Interrupted
conversations show their status and can be continued by sending another message.

## Connections and musical modulation

The sixth view keeps sync pairing and audio capture explicit. Read-only opening
loads port/input lists and status. Saving a sync configuration never connects it;
Connect starts only enabled adapters, and audio capture has its own Start button.
Configured MIDI ports retain exact name guards, and missing or renamed ports need
explicit reselection. Live pairing includes bridge identity, track/clip/rack
indices, optional expected names, note-to-cue launches with musical durations,
and controller-to-parameter bounds. The selected clock reports waiting, stale,
paused or locked state without selecting a nearby player automatically. Audio
shows measured energy, timing and the host's consumed-event count. Under
**Audio input**, match **PCM channels** to the device's interleaved stream width.
Choose **Mix** to average a two-channel input, or select a number under
**Input channel**. Widths above two require a channel selection, up to 64 PCM
channels. The waveform identifies the selected channel. [Capture configuration and REPL](../src/tolglow/integrations/README.md#explicit-studio-audio-capture)

Connections subscribes to sync and capture status on the acknowledged show
socket while the page is visible. Leaving the page or hiding the document
releases that interest. Port and mixer inventories support explicit refresh.
Runtime and epoch changes invalidate callbacks and load the new configuration.
Draft pairing/mapping edits survive ordinary status refreshes. Browser checks
verified inert opening, clock source selection, actual saved pairing/mappings and
reload restoration, explicit audio input preparation and a 390 px layout. No
physical resource was started by these checks.

The live inspector provides per-parameter random motion. The global panel can
select instances, effect definitions, cues or categories, choose parameter/range
policies, and save the controller as a launchable cue. Intervals accept fractions
in beats, bars and phrases. Manual knob edits hold the parameter until resumed;
the overlay does not overwrite canonical values. Actual browser checks verified
bounded fractional-beat glides, pause/resume, manual holds, controller stop and
cue-owned controller cleanup against the real server. Active controllers and
instances were removed after the checks.

## Passive Push display route

`/push-display` mounts a separate Reagent/re-frame view at exactly 960×160. It
never initializes studio polling, keyboard/gesture listeners, WebGL or Neovim.
The display transport calls `globalThis.tolglowPushDisplay.update(view)` with a
complete snapshot; that function dispatches synchronously and flushes Reagent.
`ready` becomes true after mounting. The worker waits two animation frames before
capture. Snapshot replacement clears absent prior fields and bounds the visible
parameters, row labels and cells to eight.

The display shares pure typed presentation with the web inspector: numeric and
choice labels, booleans, enum values, colors, vectors/ranges, category tags and
musical envelope stage labels. Category colors are independent of cue swatches.
Sparkline rendering requires supplied finite samples and caps them at 256; no
fabricated animation substitutes for signal data. The display currently shows
the selected effect's eight parameters and the first cue row, not a full grid
overlay or a native Push 3 graphics implementation.

The optimized route was browser-tested at 960×160 with every supported parameter
type, real supplied samples and complete snapshot replacement. Its requests
contained only static assets: no API calls, graphics/editor imports or canvases.
The actual Node capture worker and JVM process bridge also produced PNG captures.
In this software-rendered Chromium environment, 60 capture frames measured a
median 66.8 ms and p95 73.8 ms (14.79 fps); this is local transport evidence, not
physical Push USB throughput or a performance guarantee.

## JVM console and editor lifecycle

The console button and Ctrl+backtick open a Quake-style drawer. Its Pop out action
opens `/repl?session-id=...`, sharing the named JVM session while keeping each
window's draft, stream token and editor instance separate. The standalone route
loads session snapshots without starting ordinary show polling or graphics.
Opening or restoring a session never evaluates source.

Evaluation uses the direct SSE endpoint and preserves session/runtime/epoch/run
identity on every callback. The transcript bounds history to 100 requests and
300 events per request; the JVM separately bounds source, printed values and
stdout. A data result (map, vector, set or sequence) and error data print over
several lines with the [shared printer](repl-studio-parity.md#read-a-definition-as-studio-shows-it)
inside those bounds; strings and other values print on one line as before. Persisted history restores source and final result/error. The current
window also retains streamed output when later history polls arrive. Closing the
drawer detaches its stream and polling; interruption is a separate explicit
cooperative request, not a hard-kill guarantee.

Accessible text and actual Neovim WASM share explicit buffer, selection and
current-form evaluation. Selection reads the live Neovim buffer and visual marks;
character and line selections account for UTF-8 byte columns. Block selections
are not treated as contiguous source. The current-form helper finds balanced
collections/strings around the caret while skipping line comments and character
literals; the JVM reader remains authoritative. Ctrl/Command+Enter evaluates a
selection or buffer, Shift adds current-form selection, and Ctrl/Command+Space
requests completion. Completion and documentation query already loaded JVM vars
without evaluating draft source. Named scratch buffers are saved explicitly.

Neovim workers and observers belong to the mounted console editor and stop on
unmount. Switching back to accessible text first reads its actual buffer. The
shared editor engine accepts a filename and optional cursor position; the
existing definition editor retains its original behavior. The generated WASM
artifacts are unchanged.

## Phone motion input

Connections includes an explicitly paired browser motion panel using the pinned
`@capacitor/motion` 8.0.1 adapter. Pairing creates a server source, then saves an
unarmed preview mapping. “Enable motion” requests browser motion/orientation
permission directly from the click; neither opening the view nor restoring a
show requests access. The stable browser device ID is local storage metadata.
The pairing credential lives only in an owned in-memory resource, never app-db,
portable preferences, snapshots or exported shows. A refreshed browser must pair
again before acquiring sensors.

Actual acceleration, rotation-rate and intrinsic orientation channels are
normalized to m/s², rad/s and radians. Null, nonfinite and unavailable axes remain
absent. Samples coalesce to at most 20 HTTP requests per second with only one in
flight; axes older than 250 ms are omitted. A silent desktop shows “no sensor
readings received”, with arming unavailable. This wrapper supplies neither
sensor fusion nor an absolute world pose.

Calibration records a disarmed resting position. The code-free mapping captures
a current numeric parameter's actual schema and maps ±0.7 radians of tilt into
its declared bounds. An optional accent cue uses a musical one-shot and cooldown.
Advanced EDN exposes the shared conditioning/gesture schema. Arming is a separate
operation tied to the observed acquisition session. Late arm acknowledgements
are disarmed if that session has stopped. Leaving Connections, hiding the page,
page exit and runtime replacement remove only this owner's listeners, abort its
request and request disarm; the server also expires dropped input ownership.
The explicit native OSC receiver controls do not start a socket automatically.
See [the sensor contract](sensors.md) for transport and native Watch limits.

The optimized bundle passed nine actual-browser workflows against the local
Ring/owner service: inert opening, unarmed pairing, silent-device handling,
typed mapping publication, normalized sample admission, calibrated control,
explicit stop, view-exit cleanup, and 820/390 px layouts with 44 px actions. The
motion-input workflows used explicitly labeled software `DeviceMotionEvent` and
`DeviceOrientationEvent` fixtures delivered through the real Capacitor adapter;
they do not establish physical phone availability, Safari permission behavior,
or conducting latency. No OSC receiver was opened. Console/page errors were
empty. The recorded production build compiled 145 files with zero warnings.


## Development workspace evidence

The Develop view opens paired Git worktrees and edits their files separately
from live effect definitions. Executed browser checks verified creation, exact
hash saves, actual Neovim editing, preservation of an externally changed file and
an unsaved draft, explicit conflict resolution, and paired diffs. The integrated
xterm controls an actual owned Docker PTY; opening, input and explicit shutdown
were exercised. The fixed-check fingerprint originally rejected the repository's
large documentation assets; the backend now streams hashes with a larger bounded
budget, and the integrated rerun remains a separate gate.

The development workspace manager catalog is currently **ephemeral** across
process restart. Existing worktree files remain on disk, but a fresh process does
not recover the catalog; saving/restoring a show does not currently repair this.
This is a known lifecycle gap, not a completed persistent development environment.

## Running cue controls

Each card in **Perform → In the mix** places the cue title above its state and category labels. **Detail** opens exact parameter editing, envelope controls and effect inspection. **Compact** returns to the instrument bank. Both modes edit the running cue instance.

Use **Source** beside a control to attach or edit a driver. A parameter that has a working driver can share the card’s result display. Select its name above the graph to switch between driven parameters. **Use base value** hands the parameter back to the manual control. **Resume LFO** restores the driver. The waveform picker has its own shape preview.

Compact banks fill the card width. Wider screens can place eight controls in a row. A narrower card wraps the same controls into rows. Pause, zero, release and opacity stay below the bank.

## Responsive Studio qualification

The `studio` CI job exercises live control geometry and route navigation after building the production application. Its **studio-layouts** artifact contains screenshots and CSS-pixel measurements for each tested viewport. Download the artifact from the architecture regression run to inspect a changed layout.

The page-by-page results below come from touch-enabled Chromium and desktop Chromium against a release build served by Ring. The layouts used 390 × 844, 1024 × 768 and 1440 × 900 CSS pixels.

| Workspace | Operator interaction exercised | Measured change or result |
| --- | --- | --- |
| Perform | Four simultaneous cues, horizontal compact parameters, monitor focus, schema layouts and fallbacks | The iPad focus graph grew from 155 × 68 to 213 × 83 px with all four cue cards visible. [#129](https://github.com/tolgraven/tolglow/pull/129) |
| Compose | Save and reopen sequence and macro slots with parameter overrides and musical timing | An iPad step row went from 171 to 52 px high. All four steps fit. [#140](https://github.com/tolgraven/tolglow/pull/140) |
| Rig & space | Explore, switch tools and resume path authoring with its draft intact | On the same eight-fixture show, the iPad Explore canvas grew from 486 to 818 px wide. [#140](https://github.com/tolgraven/tolglow/pull/140) |
| Connections | Observe live readings, change enrolled zones and explicitly arm the observed session | Three browser sizes received the same gateway updates, expired sessions and opt-in pulse coverage. [#142](https://github.com/tolgraven/tolglow/pull/142) |
| Assistant | Configure a provider, save and send a message through a loopback provider | The first connection’s Save action moved from y=1,147 to y=407 on phone. [#156](https://github.com/tolgraven/tolglow/pull/156) |
| Definitions | Find and load a definition, edit its WASM Neovim source and use validation and publishing | Phone Publish moved from y=1,040 to y=333. The editor grew from 332 × 350 to 348 × 456 px. [#161](https://github.com/tolgraven/tolglow/pull/161) |
| Develop | Edit a real paired workspace, inspect its Git diff and switch between tasks and source without losing a draft | The selected source first appears at y=480 on phone, down from y=2,584. [#168](https://github.com/tolgraven/tolglow/pull/168) |
| Debug | Filter and inspect events, return keyboard focus and inspect DMX output | Phone shows 10 complete event rows, up from four. The iPad shows nine, up from seven. [#163](https://github.com/tolgraven/tolglow/pull/163) |

Each linked PR includes release screenshots, an interaction recording and a reproducible browser check.

Perform also passed in Playwright’s Linux WebKit with iPhone, iPad and desktop profiles. Its iPad view displayed all four cues, a 213 × 83 px spatial monitor and eight controls on Orbital wash. The tests edited a live value, switched between its time graph and spatial view, paused and resumed the cue, opened the XYZ controls and pressed a cue long enough to open its radial menu. Touchscreen taps and keyboard input used Playwright’s input drivers. The hold used a captured mouse pointer. All three profiles had zero document overflow and zero page errors. Physical iPad Safari remains a separate check.

To check cue gestures in Chromium, run `lein run -m tolglow.qa.performance-layout target/cue-gestures gestures` after building Studio. The check holds a touch contact on a cue, opens its actions, then plays two other cues with separate contacts and cancels them both. It also holds back a real Ring launch reply while the same pointer releases and plays the pad again. The late reply ends the original instance and the next press remains live until release. A further check drops an accepted launch reply and the first release request. Studio recovers the instance by its gesture ID in the state socket and retries its exact release. The saved screenshots and instance IDs come from the preview-only show.

After `npm ci` and `npm run studio:build`, install WebKit with `npx --no-install playwright-core install --with-deps webkit`. Run `STUDIO_TEST_BROWSER=webkit lein run -m tolglow.qa.performance-layout target/webkit-perform` for the screenshots and measurements.

## Build, verification and remaining evidence

Install pinned npm dependencies with `npm ci`, then run `npm run studio:build`
for the production Shadow CLJS bundle or `npm run studio:dev` for live compilation.
The `:cljs` Clojure CLI alias supplies Reagent/re-frame/Shadow dependencies. The
compiled bundle is served at `/studio/js/main.js`; generated files are not source.

The recorded sensor integration checkpoint of `npm run studio:test` runs 120 tests and 644 assertions against the actual
registered re-frame handlers and interceptors, with effects left unapplied.
Coverage includes held acknowledgement races, independent pointers, page-exit
cleanup, optimistic parameter writes, musical durations, expected versions,
runtime identity, partial projections, incomplete numeric edits, saved settings,
stale chat/history responses, typed fixture mapping and obsolete fixture-service
responses after show replacement, composition overrides, and graphical rig
pointer ownership and save acknowledgements that arrive after newer rig edits.
Additional cases cover inert Connections lifecycle and port guards, typed mapping
payloads, audio selection, modulation ranges/tokens, passive display snapshots,
patch routing drafts, waveform sampling controls, and REPL history/session guards
and form selection. Nine optimized-bundle console workflows passed against the real
JVM service, including streamed output, shared popup/session history, cooperative
interruption and actual Neovim WASM editing. These tests do not establish native
iPad keyboard or Safari behavior.
The last test build had zero warnings.

Optimized-bundle browser checks observed real cue launches and parameter changes
(including an End→ArrowLeft slider change from 1.0 to 0.999), held releases,
definition publication v1→v2→v3 with actual Neovim WASM editing, composition
playback, rig save/reload, WebGL rendering, fog plume/reset controls, grid
arrow/context keyboard access, long-press actions and no horizontal overflow at
820 px iPad and 390 px phone viewport sizes. Musical one-shots remained paused
with the transport and completed after resume. Chromium CDP touch events verified
two independent simultaneous holds and touch cancellation. A focused numeric
edit test verified typing a negative fixture position. These are Chromium checks,
not physical Safari or iPad hardware evidence.

A subsequent production-bundle pass against the restarted server verified nine
session workflows: conditional snapshots retaining all 64 cells; preference
restore after reload; the real history endpoint; generated registry pages;
code-free fixture publication, patching and removal; an uploaded OFL file with
explicit mode selection; musical response release/removal; and the 390 px fixture
layout. Six additional authoring checks verified slot overrides in actual macro
playback, fixture drag/truss keyboard placement, saved truss geometry reaching
WebGL uniforms without GL errors, UI restoration of the original rig, and the
phone plan layout. Both passes captured no console or page errors. On the eight
fixture demo, one observed full response was 14,567 bytes and idle partial
responses were 1,511 bytes; these are payload observations, not a large-rig
throughput benchmark.

The JavaScript engines, transport and worker wrapper pass `node --check` under
Node 24.22.3. A live captured-output API smoke test launched and ended a real
held Ember instance and verified the returned instance definition/parameter
schema. Assistant provider loading, input and responsive layout were checked
against the real application API; no external provider conversation was run
without configured credentials. Physical iPad Safari, MIDI devices, multi-touch
hardware and production Art-Net have not been validated by these checks.

Manual regression coverage should include: toggle and one-shot behavior;
two-pointer holds with reversed acknowledgement order; pointer cancel and tab
hide; keyboard holds; master/blackout output; copy/paste and empty-cell placement;
sequence save/run/stop; invalid definitions and conflicting versions; rig address
rejection; import epoch changes; editor switch preserving the WASM buffer;
worker failure; resize/rotation; preview context loss and zero-density fog.

## Group lighting

In **Perform → Live → Group lighting**, select a patch group or **All lights**,
adjust **Light target**, and tap **Apply target**. For a subset, choose **Custom
fixture group**, name it, and select the fixtures by name or ID. The fixture
picker lists emitters from the live rig, and existing scene groups reappear
when a saved show is opened.

The output readout comes from the committed show frame. It displays the
requested level alongside the group's actual fraction of available light and
reports when another light source or the rig's output limits prevent the
requested level. **Something different** varies the scene at the chosen
target. If a show variable sets the level, its path appears beside the reading.
Varying the look preserves that binding. **Set manual target** switches to the
chosen percentage. **Release group** gives up the scene's fixtures. Active groups have
disjoint membership.

## Composing on a touch screen

In **Compose**, select a cue and use **Add cue** to append a slot. Each row keeps its move and remove buttons beside its cue name. Open **Parameters** to override inputs or **Layer transforms** to change how this slot affects earlier heads. A sequence exposes each step's duration and musical unit. A macro launches its slots together. Saved compositions appear in a horizontal list when the show has any.

In **Rig & space**, **Explore**, **Move** and **Rotate** use the full editor width for the room. **Draw path** opens duration, playback, curve, target and publication controls beside the canvas on wide screens or below it on phones. Changing tools keeps the path draft. **Aim at surface**, **Pixel layouts** and the volume tools open their own controls. On a narrow screen, swipe the tool row or focus its buttons with Tab to reach the later tools.

After an optimized build, run `lein run -m tolglow.qa.performance-layout target/authoring-layout authoring` to start a disposable, preview-only show with the QA cues. It saves and reopens a macro, switches between sequence and macro controls, changes the rig tool and records geometry at phone, iPad and desktop sizes.

### Live parameter sources

In **Perform → Live**, each running parameter has a **Source** action. Choose a registered waveform, enter its period in beats, bars or phrases (fractions such as `3/2` work), and set its phase and output range. Color, vector, integer and choice parameters keep their own value types. Grouped XYZ controls share one source editor with an axis selector. Changes apply to the selected running instance.

The knob still edits the base value. Editing the base or choosing **Use base value** temporarily gives it control, **Resume LFO** resumes the generator, and **Detach LFO** removes that binding. Source details also identify random controllers, live patches, Live automation, spatial inputs, response envelopes and fixture blocks when the server reports them.

Manual controls use the available strip width. A running source adds an observed result to the strip. When several parameters are driven, the monitor selector switches between them. A steady signal still drives its chart. Using the manual base or encountering a source error removes the trace when that was the only driver. Its source controls remain available. A fixture block shows the native result for the fixture named above its chart. Its source editor can select a different fixture. When the rig has no matching fixtures, the monitor reports that state. Waveform selectors always preview the registered shape. These shape previews describe the selected waveform; time histories show samples from the show.
