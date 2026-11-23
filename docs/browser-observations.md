# Read the latest completed Studio observation

A cue, effect or REPL session can subscribe to the center color actually presented by a visible Studio viewport. The server uses the most recently completed observation at its next reaction graph publication. Browser drawing, GPU readback and the round trip run independently of the lighting frame.

```clojure
(require '[afterglow.reactive :as graph] '[tolglow.show :as show])

(def color-reader
  (graph/acquire! (show/reactions runtime)
                  [:browser/observation :preview.perform/center-rgb]
                  #(when % (println (:value %)))))

@(:signal color-reader)
;; => {:source :preview.perform/center-rgb, :value [38 25 64], ...}
((:close! color-reader))
```

Use `:preview.perform/center-rgb` for Perform and `:preview.rig/center-rgb` for Rig. Values are three display-encoded bytes in red, green, blue order. They include camera placement, work light, fog and postprocessing. A black pixel is a valid result. An absent producer or an observation still awaiting its first result yields `nil`. For positions, velocities and memberships of the show's bouncing bodies use the existing `[:spatial/body id]` and `[:spatial/members id]` subscriptions. They keep working without an open browser.

A second Studio component can consume the same signal. Include `:browser/observation` in the show's `[:settings :studio :public-queries]` and dereference `(rf/subscribe [:studio/remote [:browser/observation :preview.rig/center-rgb]])`. Mounting and disposing that component acquires and releases a real server graph consumer. The public-query setting controls which subscriptions other connected operators can read.

## Lifetime and time

- An active graph subscription starts its source, including subscriptions reached through a derived reaction. Additional consumers share the same capture.
- Visible Studio surfaces advertise on their existing same-origin state WebSocket. The oldest eligible producer for the requested view captures a single pixel from each eligible presented frame, at most once per 100 ms. One read may be in flight per source. Changed values travel as Transit messages. With no consumers the canvas has no sampling callback.
- Closing, leaving, scrolling the preview out of view or hiding the browser releases its provider. Another visible instance of the same view may take over. The host revokes the preceding grant. Reconnecting, remounting or restarting the show requires a fresh result. The last reader's release clears the retained sample even while show time is stopped.
- `:sampled-ms` and `:completed-ms` are monotonic times on the selected browser. `:received-ns` uses the host monotonic clock. Compare times only within their clock domain. `:frame-number` identifies the committed show frame used for the presented image. A faster display may produce several observations for the same show frame. The producer ID changes at remount. The reported timestamps describe the most recent **changed** value, so use connection/lease state to detect withdrawal. A stable color can stay current for an entire cue.
- The browser owns visual observations. They can change with monitor, camera or client choice. Treat the result as an optional input when designing effects. For non-browser consumers the lighting rig and its simulation remain available independently.

## Register another source

Pass a source map under `:browser-observations` to `show/create-runtime` or `show/start!`. The map replaces the defaults. Merge with `tolglow.observations/defaults` to retain both pixel readers. Each key names a show coeffect. The owner is `:rig` or `:perform`, the sampler names a browser reader, and `:valid?` checks its portable value. The host accepts at most eight sources with intervals of 25–60000 ms.

```clojure
(assoc tolglow.observations/defaults :installation/probe-position
       {:owner :rig :sampler :installation/local-position :interval-ms 50
        :valid? (fn [v] (and (vector? v) (= 3 (count v))
                             (every? #(and (number? %) (Double/isFinite (double %))) v)))})
```

In Studio initialization, call `tolglow.studio.observations/register-sampler!` with that sampler keyword and a function of the presented canvas. Return a value or a promise for the completed value. For a local simulation, read a copy of its latest completed coordinate vector. Return `nil` to skip a frame. Promise rejections skip that sample and a later eligible frame can retry. The same grant, deduplication, validation and cleanup apply to each registered sampler. Replacing a reader during live coding allows its successor to sample at the next presented frame, even if the old promise never settles.

For cancellable work register the two-argument reader with `(register-sampler! id {:abortable? true} (fn [canvas signal] ...))`. Pass that signal to a fetch or attach a listener to release local resources. Removing the final demand, replacing the grant or replacing the reader aborts an outstanding sample. A browser bitmap request that has already started can finish later. The built-in RGB reader closes the abandoned bitmap before any pixel readback.

## Rehearse both presentation paths

Build Studio with `npm run studio:build`. Then run `node dev/studio/browser-observations-qa.mjs /tmp/observations` with Leiningen, the paired Afterglow checkout and Chromium available. The rehearsal starts a capture-free preview show on loopback, opens the real Studio, acquires/releases a host graph reader and compares its result against a separate pixel read at presentation. It exercises the OffscreenCanvas worker and main-thread renderer, saves both camera views and writes the sampled values and read counts to `observations.json`. For the socket and graph regressions run `lein test tolglow.observations-test tolglow.studio.state-stream-test`.
