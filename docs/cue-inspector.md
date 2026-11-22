# Inspect and patch a running cue

In Perform, open **Live** and expand the cue card. **Effects & connections** lists each effect instance belonging to that launch, including effects sharing a native composition layer. Each output shows its source parameters with current values, its expression and its destination. An effect can have several rows for the same destination when the rows target different fixtures or use different blend modes.

Open **Patch** on a destination and choose a parameter. A three-component destination can take one vector or three scalar parameters. Scale and offset apply in that order. Optional minimum and maximum bounds clamp the result. **Publish connection** creates a revision of the registered effect. Instances follow their cue’s reload policy. A pinned instance keeps its selected revision. A conflicting edit requires reopening the latest revision. Other outputs, target selectors and blend settings stay in the definition.

A vector parameter occupies one control slot. Tap it to open its component controls, then drag a rotary or enter a number. Aiming expressions that assemble X, Y and Z from three independent scalar parameters offer the same combined control. The three underlying parameters retain their original IDs, ranges and device mappings.

A red outline identifies a running cue with an attributed fault. Expand it and open the error to inspect the stage, message and available stack. **Show diagnostics** holds errors that could not be associated with a currently running cue.

The portable expression and output patch implementation is in [output_patch.cljc](../src/shared/tolglow/authoring/output_patch.cljc). The live browser check runs against a disposable Studio preview with `node dev/studio/active-inspector-qa.mjs http://127.0.0.1:8773`.
