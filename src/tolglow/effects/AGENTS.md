# Effect materials and observations

Tolglow owns material catalogs, attaching a generator or modulator to a show
value, and compiling authored effects. Afterglow owns the numeric IR, shader
emission, physical optics, and frame evaluation. Use its optical expressions
when adding a material that also appears in the room renderer.
[Effect building blocks](../../../docs/effect-blocks.md)

- Preserve the original source in the show model. Sample global chains once from
  an immutable frame, and pass resolved variables to each effect. Keep observation
  data scoped to the frame and epoch that produced it.
- Capture the referenced shared templates at publication. Editing a library
  entry updates global chains, while existing effect revisions retain the
  published defaults. Preserve local overrides and referenced templates on a
  Transit round trip.
- Use the effect's musical clock for stages and tweens. Declare consumed show
  variable paths on compiled steps so predictive rendering can refresh its
  origin and target. The inspector reads that frame's dry value, signed stage
  changes and result through an active subscription.
- Run `lein test-pure` with the pinned Afterglow fork. The focused host tests
  include `tolglow.material-inputs-test`, `tolglow.material-library-test` and
  `tolglow.block-bindings-test`. Changes to GPU fields also need the native
  show test. For Studio changes run `npm run studio:test` and
  `npm run studio:build` and exercise the affected control against Ring.
  Include the live Studio or Push trace in the visual review.
