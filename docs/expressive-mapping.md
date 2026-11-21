# Expressive cue mappings

Push 3 in explicitly configured MPE User Mode and Seaboard M share the same pure
mapping functions. Expressions become ordinary cue/parameter commands, preserving
session, channel, note and activation ownership. The Studio editor uses the same
mapper through Ring; it does not implement a second matching algorithm.

## Policy and precedence

Policies are show data under `[:settings :expressive :devices device-id]`. A
device's `:mapping-id`, or its session ID when absent, selects this entry.
Settings changes apply on the next expressive input event, including held notes.

```clojure
{:devices
 {"push-main"
  {:enabled? true :auto? true :disabled-axes #{:glide}
   :params {:level {:axis :pressure :range [0.15 0.85]
                    :input-range [0.1 0.9] :curve :smoothstep :inverted? false}
            :position [{:axis :glide :component 0}
                       {:axis :slide :component 1}]
            :pattern nil}
   :cues {:ember {:params {:level {:axis :slide :range [0.2 0.6]}}}
          :sweep {:enabled? false}}}}}
```

Profile `:mapping-policy`, `:param-mappings` and `:cue-mappings` are defaults.
Saved device mappings override defaults; cue mappings override device entries.
Each manual entry wins over `:control/axis` annotations and semantic-name auto
matching. `nil` or `{:enabled? false}` explicitly disables one parameter. Removing
an override restores inheritance; it does not erase an inherited device mapping.

`:auto? false` stops inferred semantic-name assignments while retaining authored
schema annotations and manual entries. `:enabled? false` stops all expression
but retains cue launch and owned release. Device disable is a ceiling; a cue
cannot re-enable a disabled device. Disabled axes combine across levels.

Conventional level/dimmer/brightness/intensity names map to Pressure,
velocity/strike to Strike, pan/azimuth/rotation to Glide,
tilt/elevation/focus/zoom to Slide, and lift to Lift when available.
`:control/semantic` and words in a schema's display `:name` also express intent.
Unknown names remain unmapped. Explicit annotation can use any supported type.

## Typed mappings

Number and integer mappings use bounded ranges. Input endpoints clamp to a
selected interval before inversion and the response curve. Curves are linear,
ease-in, ease-out and smoothstep. Enum values use ordered bins and booleans use
a midpoint threshold; these do not accept custom numeric output ranges.
Color, vector and ordered-range parameters use explicit zero-based components.
Other components retain current instance values; range endpoints cannot cross.
Invalid targets or ranges are withheld and reported in the preview.

Each plan maps at most 62 parameters, reserving command-budget space for a release
and same-note relaunch. Each compound parameter accepts at most 16 component
bindings. Configurations contain at most 64 device entries and 4,096 cue overrides
per device. These limits bound event processing and validation.

## Ring API

`POST /api/settings` stores a complete `devices` map under its existing named
settings scope. Preserve other device entries when saving an edited device:

```clojure
{:scope :expressive :values {:devices {"push-main" {:auto? true :params {:level :pressure}}}}}
```

The server normalizes JSON axis/curve strings and disabled-axis arrays, validates
policy shape and rejects invalid writes before changing the show. Explicit nil
parameter entries survive JSON and show-file round trips.

`POST /api/expressive/preview` accepts:

```clojure
{:device-id "push-main" :cue-id :ember :device-kind :push-3
 :policy {:auto? true :params {:level {:axis :pressure :range [0.1 0.8]}}}}
```

`:policy` is optional; when provided it replaces that device's complete policy
for this preview only. `:device-kind` is `:push-3`, `:seaboard-m`, or generic
`:mpe`; generic MPE requires explicit `:axes`. An optional `:instance-id` previews
the matching running activation's captured schema. Otherwise, the cue's pinned
or current effect definition is used. Missing cues, mismatched instances and
malformed policies return HTTP400.

A matching connected device supplies its actual declared axes and captured
profile defaults, overriding the requested device kind. `:profile-source` is
`:connected` in that case and `:declared` otherwise. No preview opens a device.
The response includes:

- `:policy`, `:schemas`, `:axes` and `:curves` for the effective policy and constraints.
- `:mappings`, including parameter, axis, schema, origin and mapping options.
- `:disabled`, `:disabled-reasons` and `:diagnostics` for withheld controls.
- `:samples`, five normalized positions with axis input and mapped parameter values.
- Cue, effect/version, device and optional instance identities.

Samples vary all available axes together from minimum to maximum. They are
mapping previews, not captured hardware values. JSON uses keyword spellings as
strings. Neither preview nor settings writes advance effect state directly.

## Persistence and hardware boundary

Full show files include these policies and validate them before restore. They do
not reopen MIDI ports or restore held fingers. Existing session/epoch guards
continue rejecting stale callbacks. Running voices use their captured schemas;
a successful effect hot reload updates the captured schema, whereas merely
publishing a registry version does not retarget an older activation.

Push 3 uses Strike, continuing Pressure, horizontal pitch-bend Glide and vertical
CC74 Slide. Glide is not calibrated absolute pad X, and pressure/velocity are not
physical force. Lift is enabled for Seaboard M but not assumed for Push 3.
Explicit device zones and captured note-to-pad layouts remain necessary. Native
Push 3 display transport and physical iPad/controller verification are separate
gates. See the companion Afterglow `doc/architecture/expressive-devices.md` for
source references, setup and precise protocol limitations.

## Verification

The Afterglow parity suite passed 297 tests / 3,234 assertions, including the
12-test / 79-assertion expressive suite. Tolglow's focused Ring/host/persistence
suite passed 6 tests / 50 assertions. It checks draft isolation, pinned and
captured schemas, actual JSON settings, full show export/import, malformed-file
rejection, connected profile defaults and a real Seaboard session's commands
through the Studio host. External MIDI transport is injected; no hardware is
opened. UI interaction/browser evidence is tracked separately by its owner.
