# Patching groups of fixtures

`:fixtures :patches` accepts a full vector of rig fixtures or Tolglow’s original
group map. Each group defines a list, its first address and its first universe.
Once expanded, each fixture and head has an explicit ID, position, universe,
address and channel map. The show stores those values in Transit and sends them
through Afterglow’s normal fixture validator, DMX packer and output sinks.

```clojure
{:fixtures
 {:definitions [{:id :rgbw :version 1 :name "RGBW pixel"
                 :footprint 4 :channels {:red 1 :green 2 :blue 3 :white 4}}]
  :patches {:back-wall {:universe 2 :offset 510
                        :list [:rgbw :rgbw [:rgbw -1 2 0] [:rgbw 1 2 0]]}}}}
```

The first pixel occupies universe 2, addresses 510 through 512, and universe
3, address 1. The second starts at 3/2, the third at 3/6 and the fourth at
3/10. IDs are `:back-wall-1` through `:back-wall-4`. A group's optional
`:type` adds a second group tag. Set `:fixtures :groups` to list group names
in your preferred order. Each group's own list always retains its order.

Use `:start-universe` to give each fixture the same starting address in its own
universe. An oversized fixture reserves enough consecutive universes to fit.
The full declared footprint counts, including fine channels and unused slots.

## Builders and pixel heads

In a Clojure venue config a list item can be a zero-argument fixture builder.
The positional form is `[fixture x y z & options]`. A nested
`[[builder argument ...] x y z]` calls a parameterized builder. Give an item
an options map to specify its `:id`, `:position` or `:rotation-deg`. Existing
`:x-rotation`, `:y-rotation` and `:z-rotation` options take radians.

An EDN venue can refer to registered fixture types by keyword. A Clojure
config can also supply `:fixture-types` for its existing definitions and
`:factories` for named builders. A quoted builder name resolves through the
supplied map. An executable factory runs on the submitting thread before
a rig enters the show owner.

To start with the fixtures from the included venue configuration, run this
in a Leiningen REPL:

```clojure
(require '[tolglow.application :as app] '[tolglow.config :as venue])
(def running (app/start! (assoc (app/default-config)
                                :web-server false
                                :fixtures (:fixtures venue/data))))
```

This uses the venue's original patch lists, fixture library, callable strip
builder and ordered moving, moving-mini, wash, COB, strip and tube groups.
Use `(app/stop! running)` when the session finishes.

A fixture builder may return the original vector or sequence of channel
records. Each independently addressable head becomes a rig target, named
`:back-wall-1-head-1`, `:back-wall-1-head-2` and so on. The head's
`:fixture-id` identifies its parent and its position includes the parent
mount's rotation and translation. Fixtures with global channels also keep a
parent target. A head's coarse and fine channels must fit within its span.

`tolglow.fixtures.patch/expand` returns the validated vector directly. The
configured fixture module and `(show/submit! runtime :rig {:fixtures groups})`
accept the same grouped representation. A rejected rig keeps the previous
patch and its physical outputs.

## Beam optics and brightness

Add optical properties to the fixture type in EDN or edit **Beam and brightness**
in Studio’s fixture library. A placement can override the type. For example,

```clojure
{:id :spot :version 1 :channels {:red 1 :green 2 :blue 3 :white 4}
 :beam-angle 12.5 :lens-radius-m 0.055 :max-watts 125 :pseudo-fixture true}
```

`:beam-angle` is the full opening angle in degrees. Both it and the two optional
`:zoom-range-deg` endpoints must be strictly between 0° and 180°. Give the zoom
angles in DMX order. The lens radius is in metres. Set it to zero for a point aperture. A head can have its own beam
angle, zoom range, lens radius and reflected-light setting. Omitting the
reflection setting preserves the preview default.

`:max-watts` means total rated electrical power. The preview estimates relative
full-white brightness assuming equal power per emitter and equal electrical
efficiency. It averages representative photopic weights for red (625 nm), green
(525 nm), blue (465 nm), amber (590 nm), broad white (300 lm per optical watt)
and UV (no visible output). A 100 W RGB fixture is 1.0. A dimmer-only white
fixture uses the broadband white assumption. Real emitter efficiency, drivers,
motor draw and optics can make nameplate estimates inaccurate.

Set `:measured-relative-brightness` to a positive measured full-white ratio
to override the estimate. A fixture without either input uses unit gain.
Type-wide wattage and measured output are shared equally over a compound
fixture’s emitting heads. A master-control group carries zero of that budget.
The editable rig and Transit show file retain the input properties. Studio
calculates `:relative-brightness` when a changed rig goes to the preview.
These properties affect the visual model. Physical channel packing continues
to use the authored cue and master levels.
