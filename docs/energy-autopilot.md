# Perform with the energy autopilot

The autopilot takes two targets. **Brightness** is the fraction of the selected fixtures’ calibrated full-white light currently visible. **Visual activity** describes how busy that light looks, including flashing, coverage, lit movement and strobe rate. You can play cues alongside it. A hard [lighting budget](group-lighting-scenes.md) still sets the final wattage and source-count limits.

## Start with a two-bar look

In Studio’s **Energy autopilot** panel, choose the rig, a group, a cue page or explicit fixtures. A page covers the fixtures that its registered lighting cues can address. Code and show-control cues stay out of the candidate pool.

```clojure
(require '[tolglow.show :as show])

(show/submit! runtime :settings
  {:scope :autopilot
   :values {:enabled? true :scope {:group-id :front}
            :targets {:brightness 0.5 :activity 0.4}
            :window {:bars 2} :interval {:beats 1/2}
            :strategy :gradual :step 0.08 :tolerance 0.025
            :rotate? true :seed 41 :standby-level 0.02
            :preference :parameters :max-gain 2.0}})
```

The show measures the final light on every advancing frame. In this example it makes a decision every half-beat using two bars of history. A fixture at full brightness for half a beat in a four-beat bar averages one eighth. Closed shutters, zero alpha and the dark portion of an LFO all count toward the average. Use `:window {:ms 3000}` for a three-second window independent of tempo.

When the look is too bright, the conductor can take fixtures to standby and rotate which heads stay fully lit. When it is too dark it can raise gains, edit a suitable input or bring in another look. Changes already made are accounted for while the window catches up. Use `:instant` to request a full correction at the next decision, or `:gradual` to limit each correction by `:step`. `:tolerance` defines the target band. `:overshoot` allows a larger fixture to exceed the predicted target by the specified fraction when choosing a subset. Instant flashes can exceed a rolling target. Physical brightness, shutter state and the hard output caps determine what the rig can deliver.

| Preference | What changes |
| --- | --- |
| `:parameters` | Favor a compatible parameter on a conductor-owned cue or a reversible gain adjustment. |
| `:cues` | Launch eligible registered cues or retire earlier conductor-owned looks. |
| `:generate` | Create a transient declarative effect for the selected heads and run it once. |
| `:author` | Ask the configured show agent for a transient declarative effect. The provider works off the render thread and receives a single effect proposal tool. The show validates the result against the scope and current session. |

Use `:duration {:bars 1}` to set the length of a new temporary look. Disabling the conductor releases its controls and ends its owned instances. Human-owned cues remain playable. To use a pad as the switch, configure `:enabled? false :enable-cue :conduct` and launch that cue. The conductor runs for the lifetime of the cue. An effect marked `:show-control? true` can have empty `:outputs {}` for this purpose.

## Read the result

```clojure
(select-keys (:autopilot (show/snapshot runtime))
             [:targets :statistics :decision :last-action])
```

Each axis reports `:instant`, time-weighted `:mean`, `:min`, `:max`, `:stddev`, `:lit-fraction` and `:target-hit-fraction`. Its ten-bin `:histogram` records the fraction of observed song time spent at each range. For visual activity, `:lit-fraction` describes time above zero activity. `:coverage` tells you how much of the requested window has been observed. These readings are available to subscribers and effects under `:lighting/autopilot` in the show variables. Finite history bins keep memory bounded. At a fractional boundary in a full window, the oldest bin is prorated. Transport pause freezes elapsed musical history.

## Drive the targets

A numeric setting accepts a constant or a signal wherever Studio presents a binding editor. Use the existing `:audio/energy` signal to follow music. Declare a named oscillator to share its phase, or choose `:phase :local` for a fresh phase at launch. A cue can expose its numeric controls to other bindings via `{:cue-id :wash :param :level :default 0.3}`.

```clojure
(show/submit! runtime :settings
  {:scope :autopilot
   :values {:signals {:swell {:waveform :sine :period {:bars 4} :phase :global
                              :range [0.15 {:variable [:audio/energy] :default 0.6}]}}
            :targets {:brightness {:signal :swell}
                      :activity {:variable [:audio/energy] :default 0.25}}}})
```

Stack signals with expressions such as `{:op :mix :args [{:signal :swell} 0.5 0.25]}`. Tempo-synchronized sample-and-hold uses `{:random {:seed 97 :distribution :triangular} :period {:beats 1/2} :range [0.2 1.5]}`. Use that as a spill radius or as one input to another signal. Signal parameters and range endpoints may themselves be modulated.

## Overdrive and whiten highlights

A gain of two can bring an effect authored at 0.6 past full scale. HDR carries the excess through composition before DMX packing. Source whitening moves colored highlights toward white. Neighbor whitening controls the tint received by nearby heads and strip pixels. These controls also work with the conductor disabled.

```clojure
(show/submit! runtime :settings
  {:scope :hdr
   :values {:enabled? true :scope {:group-id :strip}
            :cue-gains {:chase {:cue-id :chase :param :drive :default 1}}
            :max-excess 1.0 :white-mix 1.0
            :spill 1.0 :spill-white-mix 1.0
            :radius 1.0 :falloff :linear}})
```

At 1.21, a channel has 0.21 of excess. Halfway to the edge of a linear spill radius, a receiver gets up to 0.105 before its own color mix, master and caps. Measure fixture positions in metres. Choose `:linear`, `:smoothstep` or `:quadratic` falloff. Spill is single-pass, so a receiving pixel does not start another halo. With explicit HDR enabled, the default whitening amounts are one and neighbor spill starts at zero. To keep the authored tint at the source or receiver, set its whitening amount to zero.

Put the same controls under the conductor’s `:hdr` entry to couple highlight behavior to that performance. When standalone HDR is enabled, it supplies the highlight treatment. The conductor still controls its selected sources. Fade overlapping effects with opacity. Use emission gain for a brighter contribution from the same effect. The normal `:instance/gain-set` command also accepts a per-instance gain.

[Output attack, release and per-frame blending](output-response.md) · [Brightness matching and hard budgets](group-lighting-scenes.md)
