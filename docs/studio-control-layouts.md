# Lay out live effect controls

Open **Perform → Live** on a phone. On larger screens, scroll to **In the mix** beside or below the cue grid. Each running instance has its own parameter strip, output monitor and system controls. Editing a parameter addresses that instance. Two launches of the same cue can have independent values and monitor selections.

- Drag a rotary or focus it and use the arrow keys. Its bounds are printed beside its arc. Set `:ui/widget :fader` on a numeric parameter to use a horizontal slider in its compact slot. Detail also provides the range and exact entries.
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
