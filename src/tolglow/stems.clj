(ns tolglow.stems
  "REPL operations for named instrument stems (#675) against a running show.
  Each one is the same domain operation Studio's stem panel submits: audio
  capture with :stems, event subscribers for onsets and the signal patch for
  levels. See docs/stems.md.

    (require '[tolglow.stems :as stems])
    (stems/start! runtime {:default? true :channels 8 :input-channel 1}
                  [{:id :kick :channel 1 :preset :kick} {:id :bass :channel 3 :preset :bass}])
    @(:receipt (stems/route-onset! runtime :kick :strobe-hit))
    @(:enabled (stems/route-level! runtime :bass :wash :level))
    (stems/view runtime)

  Routing changes return receipt promises that the show's next frame
  delivers, like show/post-events!."
  (:require [tolglow.audio.capture :as capture]
            [tolglow.audio.stem-routes :as stem-routes]
            [tolglow.audio.stems :as detector]
            [tolglow.show :as show]))

(defn start!
  "Start capture with named stems. config is an ordinary capture
  configuration (input, :channels, :input-channel for the ordinary
  analyzer); stems is a vector of {:id :channel :preset ...}."
  [runtime config stems]
  (show/audio-action! runtime :start (assoc config :stems stems)))

(defn replay-file!
  "Replay a multichannel recording as live capture, paced in real time.
  The file's own sample rate and channel count are used."
  ([runtime path stems] (replay-file! runtime path stems {}))
  ([runtime path stems overrides]
   (show/audio-action! runtime :start (capture/file-config path (assoc overrides :stems stems)))))

(defn stop! [runtime] (show/audio-action! runtime :stop {}))

(defn status
  "Per-stem level, envelope, onset and bleed counts, gate and tempo, from the
  running capture."
  [runtime]
  (get-in (show/audio-action! runtime :status {}) [:features :stems]))

(defn- subscribers [runtime] (get-in (show/export-show runtime) [:model :event-subscribers :subscribers]))

(defn- patch-spec [runtime] (get-in (show/snapshot runtime) [:patch :spec]))

(defn routes
  "What each stem drives: onset subscribers and patch routes, grouped by stem."
  [runtime]
  (stem-routes/routes (subscribers runtime) (patch-spec runtime)))

(defn view
  "Configured stems with their live status and routes, as Studio shows them."
  [runtime]
  (let [audio (show/audio-action! runtime :status {}) routes (routes runtime)]
    (mapv (fn [{:keys [id] :as stem}]
            (merge (select-keys (detector/resolve-stem stem) [:id :channel :preset :label :band :min-interval])
                   {:overrides (dissoc stem :id :channel :preset)}
                   (select-keys (get-in audio [:features :stems id]) [:level :onset-count :bleed-count :gate-ms :bpm :last-onset-beat])
                   {:routes (get routes id {})}))
          (get-in audio [:config :stems]))))

(defn route-onset!
  "Launch cue-id on every onset of stem, through an event subscriber.
  Options: :grid {:beats 1/4} to quantize, :id for the subscriber identity."
  ([runtime stem cue-id] (route-onset! runtime stem cue-id {}))
  ([runtime stem cue-id options]
   (let [subscriber (stem-routes/onset-subscriber stem cue-id options)]
     {:subscriber subscriber :receipt (show/register-event-subscriber! runtime subscriber)})))

(defn unroute-onset!
  "Remove an onset subscriber. Returns its receipt promise."
  [runtime subscriber-id]
  (show/submit! runtime :event-subscriber {:op :remove :id subscriber-id}))

(defn route-level!
  "Drive a numeric cue parameter from stem's 0–1 level (or :feature :onset),
  by adding a route to the current signal patch and saving it. Routing is
  enabled unless :enable? false. Options as stem-routes/with-level-route.
  Returns {:spec :saved receipt :enabled receipt}."
  ([runtime stem cue-id param] (route-level! runtime stem cue-id param {}))
  ([runtime stem cue-id param {:keys [enable?] :or {enable? true} :as options}]
   (let [spec (stem-routes/with-level-route (patch-spec runtime) stem cue-id param options)]
     (cond-> {:spec spec :saved (show/submit! runtime :patch-save {:source (pr-str spec)})}
       enable? (assoc :enabled (show/submit! runtime :patch-enabled {:enabled? true}))))))
