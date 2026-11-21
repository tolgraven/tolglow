# Legacy application compatibility

This work follows the initial default-branch baseline. The integration checkout
now includes the existing Afterglow `tolmaster` changes, including `Param`,
thi.ng color handling, and the fork's buffer/rotation/scheduling work. The
[branch survey](branch-compatibility.md) records the historical evidence and
differences; it is not the current integration status.

## Reproducible loading

The required published coordinates were verified against Clojars:
`thi.ng/color` **1.5.1** and `thi.ng/math` **0.3.2**. Color declares math as a
transitive dependency. The root build manifests own these pins. The legacy
test lane also uses the documented stable Beat Link replacement for the
unavailable historical snapshot.

An actual `require` of `tolglow.core` first failed because
`taoensso.timbre.profiling` no longer exists in the selected dependency set.
`tolglow.param` and `tolglow.fx` now use `taoensso.tufte/p`, renamed locally to
`pspy`, preserving the profiling sites and their returned values. Tufte was
already an Afterglow dependency.

Core, setup, and parameter namespaces previously imported the experimental Quil
visualizer despite having no active calls to it. Those unused eager imports
were removed. Loading the lighting application now does not require Processing,
create a sketch, or depend on a desktop window. The existing explicit visualizer
entry point remains separate; its graphics experiments are not validated by
these headless tests.

`tolglow.live` previously installed its demo when loaded with a show bound.
It now provides **`start-demo!`**, which must be called explicitly in the show
context. Requiring or reloading the namespace does not start the demo or change
running effects. The example is retained for REPL use; it has not been promoted
to a production show template.

## Reproduced and repaired behavior

The first compatibility regression run had **7 failures and 19 errors** across
8 tests. The fixes are exercised against the actual fork, without replacing
dependency namespaces:

| Area | Before | Current behavior |
| --- | --- | --- |
| Color protocol | Extensions implemented `creator`, absent from the declared protocol | Declared `colorable?`, `colorize`, and `ensure-colorable` methods work |
| Dynamic colors | Saturation limiting attempted arithmetic on a parameter's missing saturation field | Parameters pass through construction unresolved |
| Numeric colors | `:fraction` returned the keyword instead of a color | `:fraction` constructs a thi.ng HSLA color; legacy `:full` remains an alias |
| Parameter defaults | Explicit `false` fell through to a true default; color type was invoked as a function | False/zero are retained; default specifications use the actual class and are called correctly |
| Cue variable metadata | False values disappeared because the filter returned the value rather than `some?` | False metadata survives and only nil is omitted |
| Variable definitions | A hash map was flattened into map entries while a small array map was accepted | Both map representations remain a single variable definition |
| Spatial helpers | An explicit show argument was ignored in favor of dynamic `*show*` | Each helper reads the supplied show's dimensions |
| Color fine channels | Wrong option name and reversed optional arguments lost fine offsets | Fine offsets reach Afterglow's channel constructor |
| Fixture prefix offsets | Pixel heads started after the count of top-level channel descriptions | Pixel heads start after the highest coarse/fine offset, including sparse modes |
| Global offsets | Fine channels and explicit color offsets could remain unshifted | All generated coarse/fine offsets shift together, with universe bounds checked |
| Single-pixel strips | Position interpolation divided by zero | A single pixel uses the midpoint of its bounds |
| Variable watchers | The callback argument list was registered instead of its function | The selected callback is registered and can be unregistered |

## Verification and boundaries

Run `./bin/test pure` and `./bin/test legacy` from the Tolglow repository with
the companion Afterglow checkout. The legacy lane includes
`tolglow.compatibility-test` alongside the original iterator and fixture tests.
At this compatibility checkpoint, the pure lane passed **10 tests / 57
assertions**, and the legacy lane passed **32 tests / 184 assertions**, both
with zero failures or errors. The compatibility namespace accounts for 12
tests / 81 assertions. `git diff --check` also passed.
In the managed environment, Clojure is installed under
`/workspace/.tools/clojure/bin`, with the workspace Java home and CLI config
described in the work log.

The new coverage reloads core and live code with a real in-memory show bound,
traps show start/effect launch during loading, and verifies that web/OSC/nREPL
server state is unchanged. It validates every configured fixture's generated
DMX offsets for bounds and overlap. It also creates a fixture, places and
launches an existing dimmer cue, resolves its real assigners into a captured
byte buffer, and ends the cue. A separate test renders a thi.ng color through
the active fork into an RGBW byte buffer. No OLA output or hardware is started.

This establishes headless namespace compatibility and these specific authoring
paths. It does **not** verify `activate-show`, every configured page builder,
the live demo's visual output, physical MIDI/OSC devices, or the graphics
experiments. Existing stateful effects and their lifecycle wrappers still need
the functional-runtime migration and trace coverage described in the roadmap.
These repairs are compatibility work, not a claim that all planned features
have shipped.
