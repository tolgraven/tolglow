# Browse cue pages on Push

Press **Session** on Push 2. Each miniature shows the arrangement of cues across a page. The outlined miniature is the page mapped to the pads. The eight buttons above the display select the corresponding page. Left and Right browse banks of eight. Selecting a page resets the pad origin. A pad already held retains ownership of its original cue until release.

**Shift + Session** cycles between authored cue colors, effect types and tags, and both together. In the combined view the diagonal divides the two palettes. Pages larger than 8×8 are sampled into an 8×8 miniature. Each occupied bucket uses its first occupied coordinate in sorted order. An empty bucket stays dark. Thumbnails read authored definitions and their pinned effect revisions, so browsing them does not evaluate inactive effects.

Configure the start view under `[:settings :controllers :push-display]` in the show. Use `:mode :pages`, `:page-colors :both` (or `:cue`, `:type`) and optionally `:tag-colors {:custom-tag "#d499c4"}`. Push 3 profiles can map captured controls to the same semantic actions. The offscreen Chromium worker feeds both Wayang on Push 2 and the [tethered Push 3 USB sink](push-display-packaging.md#tethered-push-3). The on-device trial checks screen ownership and physical output.

[960×160 capture](https://raw.githubusercontent.com/tolgraven/tolglow/c7f1a2de26d7105b0a9f7b69ec19abc966ce8e3a/pages/pages.png) · [Watch the page and palette changes](https://raw.githubusercontent.com/tolgraven/tolglow/c7f1a2de26d7105b0a9f7b69ec19abc966ce8e3a/pages/select-and-browse.gif)

## Cue variations

Connect Push 2 with `:cue-variations? true` to use the overlay for the
selected running cue. Press **Note** to cycle through full states, parameter
rows and the regular pads. The generated pads are amber. Parameter rows follow
the authored parameter order and Page Left and Right move through extra rows.
Press a generated pad to apply its prospective authored state to the selected
instance in one command.

The bottom row holds eight saved states in append order. Blue pads recall a
state. Hold **Record** and press **Note** to append current parameters, or hold
**Record** and press an occupied bottom-row pad to overwrite that slot. Once
all slots are full, append removes the oldest entry and shifts the others left.
Incompatible states appear pink after a schema change and can be overwritten.
The bank is part of the show file.

**Scales** toggles Preferred and Free beat ratios while the overlay is open.
Preferred uses the integer components 1, 2, 3, 4, 6 and 8. Free permits other
exact ratios inside the authored bounds and component limits. The encoder and
touch strip step through exact ratios for bounded beat-ratio parameters,
independent of Shift fine tuning. Studio's **Cue variations** panel under a
running cue's Detail controls also accepts numerator and denominator directly.

### Push 3 User Mode

For encoders alone, set `:cue-variations? true` on the connection and bind a
captured encoder to the beat-ratio parameter. Hold Accent before touching
the encoder to try a ratio and restore its previous value when you lift.

With a [tethered Push 3 display](push-display-packaging.md#tethered-push-3), use
Poly or Mono Aftertouch and add `:cue-variation-pads? true` to the connection
options. Set `:presentation-receipts? true` in the browser display options.
Map the following actions under `:button-bindings` to the IDs of dedicated
buttons in your captured `:layout`.

| Action | Control |
| --- | --- |
| `:variation/cycle` | Cycle full states, parameter rows and regular cues. |
| `:variation/record` | Hold while pressing Cycle to append, or while pressing a saved pad to overwrite. |
| `:variation/ratio-mode` | Toggle Preferred and Free. |
| `:variation/previous-page`, `:variation/next-page` | Move through extra parameter rows. |

The screen marks the captured pad positions and distinguishes generated
choices from saved states. Choose a pad once the corresponding frame arrives.
A cue pad held before entering the overlay keeps its release binding. This
route was exercised with injected MIDI and USB transfers.

```clojure
(let [options {:instance-id instance-id :layout :full :seed 0}
      preview (show/cue-variation-preview runtime options)
      selection (select-keys preview [:basis :layout :seed :row-page :instance-id])]
  (show/submit! runtime (assoc selection :op :cue-variation/apply :position [0 1])))
```

The REPL, Studio and Push use the same cue projection and commands. A projection
becomes stale when its instance, authored parameters or saved bank changes.
Refresh the preview before applying a stale selection. This workflow was
developed against the Push 2 source-derived protocol.
