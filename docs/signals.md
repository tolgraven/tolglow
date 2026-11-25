# Registered signals and derived modulation channels

[tolglow.signals](../src/tolglow/signals.clj) adds reusable waveform definitions,
selectors, derived channels and bounded previews. It lowers all signal work into
the same numeric IR used by effects. Registry entries and lowered expressions
are portable data; preview sampling uses the existing IR evaluator. There is no
second runtime that independently advances signal clocks.

The initial implementation includes versioned sine, triangle, saw and pulse
waveforms, composition through ordinary arithmetic, and numeric selector
metadata for a rich picker. Derived channels can drive other numeric effect
outputs immediately. General bindings that modulate arbitrary running parameter
values are a separate middleware integration; this module does not mutate a
show or install hidden global modulation state.

## Registry contract

```clojure
(require '[tolglow.signals :as signal]
         '[tolglow.effects :as fx])

(def waveforms (signal/new-registry))
```

A registry contains `:version`, `:revision`, `:waveforms` keyed by stable ID, and
`:versions` keyed by `[id version]`. `publish` returns a new registry. Identical
current definitions are idempotent; changed content requires a larger version.
`validate-registry` checks an EDN-restored registry and returns the same data,
including historical definitions. It rejects altered current/history pairs.

A waveform definition has:

```clojure
{:id :rounded-triangle
 :version 1
 :name "Rounded triangle"
 :category :composite
 :range [0.0 1.0]
 :params {}
 :expression ...}
```

`:expression` is a scalar numeric IR template. Its reserved input
`:signal/phase` is a cycle coordinate. Any other parameter reference must have a
scalar numeric schema in `:params`. Clocks and fixture positions are supplied
when lowering the waveform, rather than hidden inside its template. This keeps
derivatives explicitly about the template's phase input.

The declared range describes the authored waveform. It is validated as an
ordered finite pair, but is not a proof that every possible custom expression
stays within it. Numeric IR evaluation rejects nonfinite results; effect output
helpers provide the usual normalized output clamping.

## Compose and reuse waveforms

A custom waveform can contain lowered existing waveforms:

```clojure
(def phase (fx/param signal/phase-input))

(def rounded
  {:id :rounded-triangle :version 1 :name "Rounded triangle"
   :category :composite :range [0.0 1.0] :params {}
   :expression
   (fx/multiply
     (signal/sample waveforms :sine {:phase phase})
     (signal/sample waveforms :triangle {:phase phase}))})

(def extended (signal/publish waveforms rounded))
```

Use `fx/add`, `multiply`, `mix`, `clamp` and other existing expression helpers to
combine signals. Lowering copies the selected waveform body and resolves its
input bindings. It does not leave an executable closure or an unresolved
registry reference inside the effect. A previously lowered effect therefore
keeps its waveform version until the author rebuilds and publishes a new effect
revision.

`sample` returns a single channel expression. `channels` returns a map containing
all four channel expressions. The default phase follows one elapsed bar. An
explicit phase can use beats, bars, phrases, absolute transport or a spatial
coordinate:

```clojure
(signal/sample waveforms :sine
  {:phase (fx/cycles {:beats 2} {:phase-reference :transport})})

(signal/sample waveforms :pulse
  {:phase (fx/add (fx/cycles {:bars 1} {:phase-reference :launch})
                 (fx/divide (fx/position :x) 4.0))
   :params {:duty (fx/param :duty)}})
```

A supplied historical `:version` pins a template. Literal parameter bindings
are checked against their declared bounds. Expression bindings remain
expressions and receive the existing compiler/runtime validation when evaluated.

## Waveform selectors

```clojure
(def selector
  (signal/selector-schema waveforms [:sine :triangle :saw :pulse]
                          {:default-id :triangle}))

(def selected-wave
  (signal/select-waveform waveforms (fx/param :waveform) (:choices selector)
                          {:phase (fx/cycles {:bars 2})}))

(def selected-effect
  (fx/effect :user/selectable
    {:params {:waveform selector}}
    (fx/color [0.65 0.04 0.08])
    (fx/intensity selected-wave)))
```

The selector is a numeric parameter with `:semantic/type :waveform-selector`,
`:semantic/category :modulation`, `:step 1`, and ordered `:choices`. Every choice
has a numeric `:value`, stable waveform `:id`, pinned `:version`, human `:label`
and waveform `:category`. It defaults to the shared `:control/axis :slide`
annotation; callers can supply a different axis or `nil`.

Selection rounds to the nearest index and clamps to the available choices:
0.49 selects the first waveform, 0.5 selects the second. This explicit arithmetic
lowering works in the existing typed numeric compiler. Continuous controllers
can cross those boundaries, while a picker can send the exact choice values.
The schema bounds still apply to values supplied through an effect parameter.

Selectors accept one to sixteen choices. Each choice may be an ID or a map with
`:id`, optional `:version`, and optional per-waveform `:params`. For example,
only the pulse alternative needs a duty binding:

```clojure
[:sine :triangle {:id :pulse :version 1 :params {:duty (fx/param :duty)}}]
```

All alternatives are evaluated before their numeric selection masks are
combined. Custom expressions must therefore be defined for every supplied
input, even while unselected. This implementation does not promise lazy branch
evaluation. The pinned ordering is part of the definition; changing that order
changes the meaning of numeric controller values and should be an explicit
revision.

Waveform categories and effect category/modifier tags are separate from cue grid
swatches. A UI may color type/category tags without changing an operator's cue
color choices.

## Derived channels and their units

Let `f(p)` be the waveform at cycle phase `p`, and `h` the explicit derivative
window. The implementation lowers these finite differences into ordinary IR:

| Channel | Definition | Unit |
| --- | --- | --- |
| `:value` | `f(p)` | authored amplitude |
| `:slope` | `(f(p+h) - f(p-h)) / (2h)` | amplitude per cycle |
| `:angular-velocity` | `angle-span-deg × slope` | degrees per cycle |
| `:jagginess` | `abs((f(p+h) - 2f(p) + f(p-h)) / h²)` | amplitude per cycle squared |

The default window is 1/1024 of a cycle, with a supported range of 1e-6 through
0.25 cycles. The default angular span is 360 degrees. Angular velocity here is
the derivative of an angle obtained by linearly mapping signal amplitude across
that angular span. It is not the angular velocity of a waveform's graph tangent,
nor a measured moving-head motor velocity. Jagginess is this explicit curvature
measure, not a general perceptual or audio roughness classifier.

These are cycle-domain units. They are not silently labeled per beat or per
second. If a cycle spans `B` beats, slope per beat is `slope/B`, angular velocity
per beat is `angular-velocity/B`, and curvature magnitude per beat squared is
`jagginess/B²`. A per-second derivative additionally depends on transport rate.
A paused musical transport holds the sampled signal state; it does not mean the
waveform's cycle-domain slope becomes zero.

```clojure
(def derived
  (signal/channels waveforms :sine
                   {:phase (fx/cycles {:bars 1})
                    :window 0.0001
                    :angle-span-deg 180}))

(def response-effect
  (fx/effect :user/derived-response {}
    (fx/color [0.4 0.02 0.16])
    (fx/intensity (fx/add 0.5 (fx/multiply 0.1 (:slope derived))))
    (fx/zoom (fx/multiply 0.02 (:jagginess derived)))
    (fx/focus
      (fx/multiply 0.0001 {:op :abs :args [(:angular-velocity derived)]}))))
```

Derivative channels can be negative or exceed the normalized output range, so
choose a scale/offset/clamp appropriate to the destination parameter or output.
The output helpers above perform the final normalized clamping.

### Discontinuities are preserved

Saw and pulse signals have value discontinuities. Triangle has slope corners.
Builtin definitions retain this information in `:discontinuities`. Finite
sampling does not turn a jump into a physically finite derivative: reducing the
window increases the reported peak around a discontinuity. With a pulse window
of 1/128 cycle, the rising-edge slope is 64; at 1/256 it is 128. The associated
curvature peak also increases. Preview and effect execution show the same
chosen-window behavior rather than hiding the jump with an undocumented filter.

## Preview API

```clojure
(signal/preview waveforms :sine {:samples 129 :cycles 2 :window 0.0001})
(signal/preview waveforms {:selector 1.5 :choices (:choices selector)}
                {:samples 257 :cycles 1})
```

The pure result contains `:samples`, `:units`, `:window`, `:angle-span-deg`,
`:selection`, `:start-phase` and `:cycles`. Each sample row contains `:phase` plus
the four channels. Sample count is bounded to 2..2048, default 129, and includes
both interval endpoints. Parameter bindings for preview must resolve from the
supplied numeric values; the preview has no hidden show or fixture context.

This API supports a live chart without creating a separate browser oscillator.
The host exposes the current registry at `GET /api/waveforms`, this sampler at
`POST /api/waveforms/preview`, and guarded publication at
`POST /api/waveforms/publish` with an EDN `source`. Reading or previewing a
waveform does not launch a cue. Registry and historical definitions are included
in the saved show.

The Reagent [waveform picker](../src/studio/tolglow/studio/waveform.cljs) is shared
by the live inspector and composition slot parameters. It keeps exact numeric
choice values while displaying names and category tags separately from cue
swatches. Its expandable SVG chart uses only samples from the host API. Switching
between value, slope, angular velocity and jagginess reuses the same returned
samples. Cycles, sample detail, derivative window and angular span are editable;
the chart labels the cycle-domain units and retains discontinuity peaks.

Continuous selector changes round at the same half-index boundaries as the
compiler. Requests coalesce and only one request per open preview is in flight;
closing a preview prevents new sampling. Runtime/epoch and request identities
reject stale responses. Invalid input and host errors remain visible, with
explicit retry. The UI retains the previous trace while new samples arrive and
labels it as previous data until the replacement is ready.

Choice metadata preserves per-waveform `:params`. Literal numbers and constant
expressions can be previewed directly. A binding that depends on another effect
parameter requires explicit numeric preview inputs; the picker reports that
limitation instead of silently substituting defaults. This is a registered
waveform chart, not an animation of the whole effect or its current fixture
output. The preview does not advance or control the musical transport.

## Verification and extension boundaries

Tests compare sine slope and curvature against analytic values, check the
window-dependent pulse jump, serialize/reload registry history, lower composite
templates, and execute selector boundaries and derived-channel effects through
both real scalar and typed compilers, including final DMX-byte parity.

Audio-derived wavetable sources remain future work. A table-backed registry kind
needs explicit sample rate, cycle alignment, interpolation, normalization,
provenance and bounded storage; it must define how compiled effects snapshot a
table and how replacement revisions hot-reload. It should not introduce an
unowned audio callback or an unrelated waveform clock. General runtime parameter
bindings likewise need explicit target selection, range mapping, ownership and
composition order before they are integrated with manual/MPE/agent control.
