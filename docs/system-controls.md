# Instance and category performance controls

The shared reducer and engine implement pause/resume, forced removal, opacity and
independent attack/release controls. Web, Push, X-Touch, agents and REPL callers use
the same operations. The UI and hardware mappings are separate integration gates.

## Commands

Every operation uses the normal show/epoch/source/command-ID envelope. Supply the
current authoritative `:time-ms`; retries use the same complete command.

```clojure
{:op :instance/pause :instance-id "0:3"}
{:op :instance/resume :instance-id "0:3"}
{:op :instance/remove :instance-id "0:3"}
{:op :instance/end :instance-id "0:3"} ; graceful, resumes a paused instance first
{:op :instance/opacity-set :instance-id "0:3" :value 0.65}
{:op :instance/opacity-toggle :instance-id "0:3"}
{:op :instance/envelope-set :instance-id "0:3"
 :envelope {:attack {:beats 1/2} :release {:bars 1}}}
{:op :group/apply
 :selector {:categories [:movement :beam-shape] :match :any}
 :action {:op :instance/opacity-set :value 0.5}}
```

An envelope update may supply attack, release or both. Durations are nonnegative
single-unit maps: `:beats`, `:bars`, `:phrases`, or explicit `:ms`. Captured launch
meter resolves bars/phrases. Zero is immediate. No duration comes from wall time.

Definitions, cues and launch options may contain `:system {:opacity ... :attack
... :release ...}`, with launch values overriding cue values and cue values
overriding definition values. These options are independent of authored effect
parameters. Definition hot reload retains current system controls; restart reload
resets the attack gate and private input snapshot.

`:instance/remove` removes immediately. Existing `:instance/end :force? true` has
the same forced semantics. Graceful end starts release once and explicitly resumes
a paused instance so its release can complete. Pausing an already releasing
instance freezes its remaining release until resumed or ended again.

Opacity faders apply immediately. Toggling off remembers the last positive value;
toggling back restores it. Setting opacity to zero does not stop private state or
clock advancement; pause is a separate operation.

## Category selection and public views

`afterglow.control.categories/definition-categories` derives the union of output
attributes and explicit definition `:categories` tags. Color, intensity, movement,
beam shape, strobe, atmosphere and modulation are distinct categories. A cue's
emitted-color swatch is not a category tag or category display color.

Selectors accept `:categories` with `:match :any` (default) or `:all`, plus optional
`:instance-ids`, `:cue-ids` and `:effect-ids`. All supplied dimensions intersect.
Values are distinct ID vectors; category IDs are keywords. An empty selector `{}` deliberately selects all active instances. An explicitly empty vector selects
none. Unknown explicit instance IDs reject the entire operation; a valid category
with no active members produces an accepted empty result.

Selection uses each running instance's captured definition, not a newer unrelated
registry version. Stable priority, launch order and ID determine operation order.
A bulk command is one revision with one bounded retry receipt. It validates before
publication and returns `:instance-ids`; overlapping category membership never
applies an action twice. Supported actions are the individual system operations,
graceful end and forced removal. Bulk commands cannot smuggle another command
source, epoch, instance selection or arbitrary registry operation into their action.

`views/snapshot` adds `:groups [{:id category :color hex :instance-ids [...] :paused?
...}]`. Each instance exposes selected `:system` values (`:opacity`, `:paused?`,
optional `:attack`/`:release`) and categories. Private prior input state, gate timers
and clock offsets are not in this public projection.

## Pause freezes evolution

Pause freezes private milliseconds, beats, frame number and canonical effect state.
It regenerates contributions from the last accepted input state and clock, so the
engine never stores typed output arrays in a persistent show model. Re-evaluation
cannot repeat intents, complete the instance or adopt a hot reload while paused.

Manual parameter edits and other current frame inputs still affect output. A paused
stateful effect therefore holds its evolution rather than becoming an immutable
photograph of the entire room. UI labels/help should say **Pause evolution**.
Pure effect code remains required: the engine cannot undo side effects hidden in a
user step. Paused effects still consume evaluation time to regenerate their output.

Resume subtracts the paused interval from the private clock. Timed/one-shot expiry,
musical wave phase and release timers consequently do not jump forward. Private
frame number advances once on the next accepted evaluation. Global show frame
number remains the publication identity and continues during pause.

Portable instance `:system` bookkeeping contains `:clock-offset-ms`,
`:clock-offset-beats`, `:last-clock`, optional `:paused-clock`, `:last-input-state`,
`:last-delta-ms`, `:last-frame-number`, and optional `:gate {:kind :attack|:release
:timer ... :from ...}`. Clock-domain rebasing shifts only absolute beat anchors in
clocks and beat timers. It never edits opaque effect state or elapsed offsets.

## Independent AR and compositing

The system gate ramps contribution influence, not a user parameter named `level`.
An attack begins at launch. Editing attack starts a transition from the current gate
level toward one; editing a running system release restarts its remaining transition
from its current level toward zero. Both remain continuous at the edit instant.

With no explicit system release, existing authored release behavior is preserved.
An explicit system release owns the tail deadline and evaluates the underlying
step as running until that deadline. This allows a compiled effect without its own
envelope to fade out. An authored attack/envelope is otherwise preserved, so it can
combine with the independent system gate. Self-finishing effects may finish normally
before a graceful end begins; system release is not an automatic resurrection tail.

Effective opacity is the fader value multiplied by the system gate. Ordinary
assignments and typed batches carry the same opacity metadata. The renderer applies:

```text
result = previous + opacity × (blend(previous, proposed) − previous)
```

This works for replace, maximum and additive blends. Zero skips writes entirely,
revealing earlier contributions and preserving absent-attribute defaults. It does
not replace earlier effects with black. Geometry interpolates componentwise in world
space; this is not path planning or a shortest-angle rotation interpolator. Raw
emitter channel overrides remain distinct from the combined RGB color attribute.

## Verification

Focused Afterglow tests exercise reducer retries/rejection/selection, stateful pause,
musical and millisecond expiry, graceful end from pause, continuous AR edits, paused
hot reload, malformed step rejection, regenerated typed arrays and two-worker
completion-order equality. Renderer tests compare dense, rich, exact-ratio, typed,
ordinary and geometry paths with fixed output bytes. Cross-repo tests use the real
scalar and batch compilers through control, engine and DMX packing.

The final focused gate passed **81 tests / 612 assertions**, with reflection
checks enabled. Integrated counts are recorded in the central work log. The actual
compiler integration passed 2 tests / 33 assertions. These tests use no physical
output, and do not claim completed UI, fader mapping or hardware verification.
