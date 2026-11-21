# Studio ClojureScript

The application is Reagent components over re-frame events/subscriptions.
Application state belongs in app-db. Keep asynchronous HTTP, timers, DOM focus,
pointer capture, files and device/graphics ownership at registered effect or
component lifecycle boundaries. Do not reintroduce a parallel imperative DOM
application in JavaScript. JavaScript WebGL and Neovim are specialized interop
engines only; they do not own lighting commands or application UI state.

Partial state projections may merge only when runtime identity, epoch and
revision match the accepted snapshot. Preserve local parameter drafts until
their acknowledged revision arrives. Every asynchronous authoring/chat result
must remain tied to the request/session that produced it; imported shows and
replacement server processes invalidate prior work. Restoring chat history must
never start generation automatically. Persist portable UI preferences through
the settings API, not provider credentials or editor drafts.

Use stable string property access at custom JavaScript interop boundaries; the
optimized Closure build can rename undeclared engine methods. Verify the release
bundle as well as development compilation. Treat musical duration maps as the
default authoring representation, with milliseconds only when explicitly chosen.

Coordinate event/db contracts between `events.cljs`, `views.cljs` and
`interop.cljs`. Preserve instance IDs, session leases and epochs across held
gesture acknowledgements/cancellations. See `docs/studio-ui.md` and
`resources/studio/AGENTS.md`. Build with `npm run studio:build`, then browser-test
actual semantic operations against the local captured-output Ring application.
