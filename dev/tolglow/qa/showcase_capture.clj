(ns tolglow.qa.showcase-capture
  "Replay the showcase timeline and write what the Room preview receives.

  The show runs on a manual clock with a capture sink, so no Studio, network or
  device is involved. Each committed frame is projected the way Studio projects
  it for the preview: the bounded `[:output :values]` rows, the atmosphere and
  a rig whose fixtures carry `:relative-brightness`.

      lein run -m tolglow.qa.showcase-capture target/showcase 16 26

  writes `rig.json`, `frames.ndjson` (one frame per line) and `meta.json` for
  bars 16 up to 26. `node dev/preview/showcase-capture.mjs target/showcase`
  renders them."
  (:require [afterglow.render.photometry :as photometry]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.showcase :as showcase]))

(defn- wire-key [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- wire [value]
  (json/write-str value :key-fn wire-key :value-fn (fn [_ v] (if (keyword? v) (wire-key v) v))))

(defn- preview-rig [rig]
  (update (dissoc rig :pixel-layouts) :fixtures
          (partial mapv (fn [fixture]
                          (cond-> fixture
                            (or (:max-watts fixture) (:measured-relative-brightness fixture))
                            (assoc :relative-brightness (photometry/relative-brightness fixture)))))))

(defn- preview-frame [snapshot]
  (-> (select-keys snapshot [:show-id :epoch :revision :frame-number :time-ms :blackout? :master])
      (assoc :beat (get-in snapshot [:transport :beat])
             :running (mapv :cue-id (:instances snapshot))
             :output (select-keys (:output snapshot) [:values :atmosphere]))))

(defn capture!
  "Write the capture for bars `from-bar` up to `to-bar`. Returns the meta map."
  [directory {:keys [from-bar to-bar seed] :or {seed showcase/default-seed}}]
  (let [dir (doto (io/file directory) .mkdirs)
        rig (atom nil)
        faults (atom #{})
        frames (atom 0)]
    (with-open [out (io/writer (io/file dir "frames.ndjson"))]
      (let [{:keys [status dmx]}
            (showcase/replay
             {:seed seed :from-bar from-bar :to-bar to-bar
              :snapshot! (fn [snapshot]
                           (compare-and-set! rig nil (:rig snapshot))
                           (swap! faults into (keep (fn [instance]
                                                      (when-let [fault (:fault instance)]
                                                        [(:cue-id instance) (:message fault)])))
                                  (:instances snapshot))
                           (.write out ^String (wire (preview-frame snapshot)))
                           (.write out "\n")
                           (swap! frames inc)
                           nil)})
            meta {:timeline (:id showcase/timeline) :seed seed :bpm showcase/bpm :meter showcase/meter
                  :from-bar from-bar :to-bar to-bar :frame-ms 25 :frames @frames :dmx-frames (count dmx)
                  :universes (vec (keys (:universes (first dmx))))
                  :rejected (count (:rejected status)) :missed (count (:missed status))
                  :faults (vec (sort @faults)) :output "capture sink only"}]
        (spit (io/file dir "rig.json") (wire (preview-rig @rig)))
        (spit (io/file dir "meta.json") (wire meta))
        meta))))

(defn -main [directory & [from-text to-text seed-text]]
  (let [from-bar (if from-text (parse-double from-text) 0.0)
        to-bar (if to-text (parse-double to-text) 32.0)
        seed (if seed-text (parse-long seed-text) showcase/default-seed)]
    (when-not (and directory from-bar to-bar seed (<= 0 from-bar) (< from-bar to-bar))
      (throw (ex-info "Usage: showcase-capture <directory> [from-bar] [to-bar] [seed]" {})))
    (try
      (println (capture! directory {:from-bar from-bar :to-bar to-bar :seed seed}))
      (finally (shutdown-agents)))))
