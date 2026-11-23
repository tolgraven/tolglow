# Studio Debug

Open **Debug** from the workspace or press **8** while the page has focus outside an editor. The direct URL is `/debug`. The page has three views.

## Event feed

Each row shows local wall-clock time with milliseconds, the event type, category, emitter and subscribers. Open a row for its timestamp with time zone, show epoch, frame, musical beat and emitted metadata. The delivery field distinguishes an observed callback, a matching subscriber and an unobserved delivery. **None** means the producer reported an empty subscriber list.

Open **Filters** to narrow the journal by event type, category, emitter or subscriber. Selectors combine, and the button shows how many are active while the controls are folded. **Clear filters** restores the full journal. **Pause feed** holds the displayed list so that new events cannot move a row while you inspect it. The client continues acknowledging batches. **Resume** displays the most recent events. The browser retains 320 rows and initially shows the latest 50 matching rows. **Show older events** expands that window. A missed-event count indicates the server's retained history was exhausted during a gap.

The Debug socket connects on entering this page. A reconnect supplies the last acknowledged runtime ID and cursor. A replacement runtime starts a fresh feed. Navigating away closes the socket. The studio show-state socket has its own lifecycle.

## Subscribers

Open **Subscribers** and expand **Registered event types** to inspect the running host's catalog. **New subscriber** accepts a stable ID, event type, event category and emitter. An empty match field admits any value for that field. Prefix a keyword emitter with a colon. Plain text identifies a string emitter.

Add one or more steps, reorder them, then save. A launch step can pick a random, first or all eligible matches in the cue/effect registry. Limit candidates by specific IDs, category and page. A one-shot can use beats, bars or phrases for its duration. Each step can start immediately or after a musical interval such as `1/4` beat. Release, kill and opacity actions use the same selector. Set opacity to 1 for full alpha. Callback steps support a registered function ID, a named Clojure var or a trusted source handler. The host resolves and authorizes callbacks.

An existing registration can be disabled, edited or removed. Editing keeps a local draft. Save and remove attach the original registration, process runtime and show epoch so the host can reject an edit made against a replaced show or changed definition. Push updates supply the current subscriber list to other open studio pages. The initial catalog and post-save refresh are single requests.

## DMX output

Select a patched universe to subscribe to the corresponding 512-byte output packet. The frame counter follows the last packet received. Each bank shows 64 numbered channels, their unsigned values from 0 through 255, and a level bar. Leaving the DMX view unsubscribes that universe. The display is also available on a preview-only host.

Run `lein run -m tolglow.qa.studio-browser event-feed-layout` after a production Studio build to exercise the compact journal at phone, iPad and desktop sizes.

The browser regression entry point is [debug_browser.cjs](../test/studio/debug_browser.cjs). Set `STUDIO_URL` to an isolated Studio host that includes the event journal. See the [Studio state stream](state-stream.md) for the show subscription used by the rest of Studio.
