# Studio frontend

This folder holds studio styles, its minimal mount page, generated ClojureScript
bundle, and specialized graphics/editor engines. The application UI belongs in
`src/studio/` as Reagent components and re-frame events/subscriptions. Read `docs/studio-ui.md` before
changing API assumptions, gestures, preview rendering or editor lifecycle.

* All show changes cross the semantic command/API boundary. A browser holds a
  projection, not authoritative effect state. Supply the current epoch and a
  stable session ID; never identify an instance to stop solely by cue ID.
* Keep one outstanding state poll. Rendering the WebGL preview has its own
  animation clock and must not block control polling. Bound graphics work and
  pause it when hidden. Fog controls affect preview state only.
* A held gesture owns its instance. Release on up, cancel, lost capture, blur,
  visibility change and page exit, including a launch acknowledgement arriving
  after release. The server lease is a final fallback, not the normal release.
* New controls must work, have accessible labels and reachable keyboard
  alternatives, and fit phone/iPad layouts. Do not hide required functionality
  only inside a desktop sidebar. Treat backend errors as visible UI outcomes.
* Render user text with textContent. EDN is validated and compiled server-side;
  never evaluate submitted JavaScript or arbitrary Clojure in the browser.
* The Neovim worker owns its real buffer. Read it before publishing or switching
  editors; terminate workers and observers when stopping. Preserve the textarea
  for assistive technology and unsupported isolated contexts. Do not describe a
  textarea as Neovim or syntax highlighting as linting.
* Do not hand-edit pinned `vendor/neovim/nvim.*` artifacts. Make adaptations in
  `studio-worker.js` and `transport.js`, and preserve licenses and provenance.

Run `node --check` on changed JavaScript engines and `npm run studio:build` for
the Reagent/re-frame application. Browser-test desktop, iPad and phone
widths against `./bin/studio`, using captured output. Check the console, actual
state changes, cancellation/held release, rejected edits, and editor teardown.
Do not claim Safari or physical MIDI/device behavior from Chromium alone.
