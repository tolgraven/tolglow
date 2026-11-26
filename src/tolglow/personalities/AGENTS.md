# Track-feel personalities

A personality is versioned data: an ordered palette, concurrency limits, weights over a fixed catalogue of forms, and a motion character. `tolglow.personalities` validates it, blends it and owns the saved state under `:extensions`. Keep it free of closures, clocks and IO. [Operator guide and data contract](../../../docs/personalities.md)

- `forms.clj` is the shape language. Each form names the input it stands for in a sculpt morph, a spatial volume, an aim-path figure and a registered waveform. A new form needs an entry in `order`, a glyph, and a test against the validator that owns each input. Trace waveform figures from the engine's definitions.
- Weights are preferences. A family skips the forms it cannot take, and a reader with nothing to choose from keeps its authored input.
- Selection is a seeded draw over cumulative weights in catalogue order. The coordinate holds IDs and integers. Changing the catalogue order or the hash changes every saved show's choices, so treat both as a schema change.
- `icon.clj` draws from the name, palette, form weights and motion character alone. The same data must give the same bytes after a save and load. Print numbers through `number`, keep one seeded run of draws per shape, and regenerate the snapshots with `lein run -m tolglow.qa.personality-icons golden` when the picture is meant to change.
- Effects read the active personality through a declared input and `input-choice` on the frame's model. The show owner applies every change through `apply-operation`. A sharing package holds an explicit selection of definitions and nothing else from the show.

Run `lein test tolglow.personalities-test tolglow.personalities.icon-test tolglow.personalities-host-test`. Render the contact sheet with `CHROMIUM_PATH=… lein run -m tolglow.qa.personality-icons target/personality-icons` and look at it at 24 px before changing a glyph or a layout.
