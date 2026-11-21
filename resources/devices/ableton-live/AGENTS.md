# Live-side bridge assets

This folder owns Max/Node platform adapters, not another web application.
`live_api.js` is ES5-compatible code for Max's `js` object; `max_adapter.js`
connects it to actual LiveAPI/Dict objects. All input must pass through
`deferlow`. `node_bridge.js` owns explicit loopback HTTP and ordered IPC only.
Never create LiveAPI objects in Node's worker context.

Keep the `.maxpat` as editable source. Do not manufacture or rename a compiled
`.amxd`. Preserve exact official Live Object Model references and distinguish
API-document evidence, injected tests and real Live/hardware acceptance.

Only write to a revalidated paired clip/track; reject stale IDs or expected-name
mismatches. IDs are Live-session identities, not durable Set references. Clip
note mutation and MIDI-wire recording are alternative routes, never both for
one accepted command. No invented DrumPad color or name setter, CC envelope API
or Push protocol is allowed.

Any failure after a possibly applied host write is indeterminate. Never retry
with another dictionary codec or fresh request ID automatically. Preserve
bounded request/command deduplication, callback order and late receipt handling.

Run the Node tests and syntax checks. Test substitutes belong only at the
external API boundary; HTTP lifecycle tests use real owned loopback listeners.
Keep actual host/device acceptance explicitly pending until observed.
