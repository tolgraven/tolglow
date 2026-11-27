# Lay out live effect controls

Open **Perform → Live** on a phone. On larger screens, scroll to **In the mix** beside or below the cue grid. Each running instance has its own parameter strip, output monitor and system controls. Editing a parameter addresses that instance. Two launches of the same cue can have independent values and monitor selections.

- Drag a rotary or focus it and use the arrow keys. Its bounds are printed beside its arc. Set `:ui/widget :fader` on a numeric parameter to draw a vertical fader instead, in the strip and in Detail. Press anywhere on the fader to jump there. An integer span of sixteen steps or fewer shows one tick per step, every fourth heavier. Detail also provides exact entries.
- A bounded two-component vector is an XY pad and a three-component vector an XY pad with a Z fader beside it. A group of X, Y and Z inputs works the same way. Up is high. Press or drag on the pad, or focus it and use the arrow keys: Left and Right move X, Up and Down move Y, Shift moves ten steps. The pointer marks the value you hear. When a source drives the parameter, a dashed ghost marks the base value you are editing. The component button below the pad opens exact entries.
- A four-component vector draws its components as nested rings in one knob. Set `:ui/widget :rings` to draw any bounded vector this way. Press the rings to open one knob per component. **Done** or Escape closes them. On Push, touching the parameter opens the component knobs across the free encoder slots after it. `:ui/widget :axes` keeps the component button only. A vector with an unbounded component also keeps the component button. Colours keep their swatch.
- **Widgets** above the running cards switches between the Ember and Standard widget styles. Ember is the default and the tolglow-web look: knobs, rings, pads, faders and the range sliders all get gently shaded faces, tracks and handles, and value fills are rust. Rust only shows how far a value is filled from its low end or zero. It does not mean edited or overridden; the dashed amber marker shows the base value under a source. Rings keep a colour per component. Standard is the flat style in each parameter's accent colour, and a saved Standard choice is kept. A fresh press in the bottom gap of an Ember knob leaves the value as it is, while a drag past either end lands exactly on that bound. Use the scroll wheel to turn an Ember knob once it has focus. A press on the knob also sets its angle, so to focus it without changing the value, Tab to it or press in the bottom gap. Turning follows vertical scroll distance. 1000 pixels cross the whole range, and an integer knob steps once per 60 pixels, a three-line mouse notch. When no knob has focus the page scrolls as usual, and horizontal scrolling or Shift+wheel always passes through to the strip. The choice is saved with your other Studio preferences.
- Swipe along a strip, or focus the strip and scroll horizontally, to reach later controls. Tab advances through the editors. Touch a vector to open its individual axes.
- Choose a parameter in the monitor to see its current position, orientation, color or four-beat history. A constant trace means the observed parameter is steady. A history starts collecting when its scope becomes visible.
- **Detail** opens the full control bank, with exact entries, modulation, parameter history and 3D controls. Select **History** to change the time window, or **3D** to return to a spatial view. **Compact** returns to the horizontal strip.
- Pause, zero, release and opacity remain available beside the bank. Category selectors above the cards still address multiple instances.

## Authoring a layout

An effect definition can contain a template keyword or a map under `:ui/layout`. A cue can override the fields it needs. Both are part of the normal show data and survive saving and loading. A generated cue uses the underlying effect's layout automatically.

```clojure
;; On a registered effect
:ui/layout {:template :movement
            :order [:aim :bearing :color :level]
            :monitor :aim}

;; On one cue for that effect
:ui/layout {:order [:color :level :aim]
            :monitor :color}
```

The built-in templates cover wash, movement, pulse, flash, optics and spatial volumes. Select them with `:wash`, `:movement`, `:pulse`, `:flash` `:optics` and `:volume`. A wash prioritizes tint and intensity, a pulse level and period, and movement aim and bearing. The demo library specifies its templates on the definitions, including the sweep's width monitor. The REPL examples, generated spatial volumes and procedural warm fill and beat pulse definitions also specify their layouts.

For a parameter shared between definitions, put `:ui/order` on its schema. Explicit cue/effect `:order` wins, followed by per-parameter order, expressive axes, the template order and the parameter identifier. Unlisted parameters stay editable. IDs can be namespaced. Unknown or repeated IDs in an authored order have no effect on the remaining controls. Without an authored template the resolver uses effect tags and parameter names, then stable schema ordering. The monitor falls back to the first visualizable parameter if its preferred parameter is unavailable. Enum, boolean and waveform controls retain their native selectors and previews.

Group three independent scalar inputs by using them as the X, Y and Z operands of the same aim or direction output. The grouped control still writes each component to its original parameter. A vector schema already presents as one control.

## Reproducing the browser check

Build the optimized Studio with `npm run studio:build`, then run `lein run -m tolglow.qa.performance-layout`. Set `CHROMIUM_PATH` when Chromium is installed outside `/usr/bin/chromium`. The test starts a preview-only Ring show, launches three demo effects and an eight-parameter spatial effect, checks real editing and scope changes at three viewport sizes, and writes PNGs and geometry measurements to `target/performance-layout`.

`lein run -m tolglow.qa.compound-widgets` checks the widgets in this section. It drags the XY pad with the mouse and with touch, and drags the Z fader and a vertical fader. It also opens the rings and steps a component knob, turns an Ember knob with the wheel, and reads every committed value back from `/api/state`. With the page still open, it submits REPL `param/set` forms and checks that they reach the view. It then renders the Push ring knob through the capture worker. Screenshots go to `target/compound-widgets-qa`.
