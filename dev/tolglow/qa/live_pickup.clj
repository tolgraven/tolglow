(ns tolglow.qa.live-pickup
  "Inspect the playable pickup fixture or a MIDI clip exported after editing it in Live."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [tolglow.integrations.ableton :as live])
  (:import [javax.sound.midi MidiEvent MidiSystem Sequence ShortMessage Track]))

(def fixture (io/resource "devices/ableton-live/examples/pickup-automation.mid"))

(defn- note-event [ppq ^MidiEvent event]
  (let [message (.getMessage event)]
    (when (instance? ShortMessage message)
      (let [command (.getCommand ^ShortMessage message)]
        (when (#{ShortMessage/NOTE_ON ShortMessage/NOTE_OFF} command)
          {:beat (/ (.getTick event) ppq)
           :message {:type (if (= command ShortMessage/NOTE_ON) :note-on :note-off)
                     :note (.getData1 ^ShortMessage message) :velocity (.getData2 ^ShortMessage message)
                     :channel (inc (.getChannel ^ShortMessage message))}})))))

(defn clip-events [source]
  (with-open [in (io/input-stream source)]
    (let [sequence (MidiSystem/getSequence in)]
      (when-not (== Sequence/PPQ (.getDivisionType sequence))
        (throw (ex-info "This qualification clip uses musical ticks" {})))
      (->> (for [^Track track (.getTracks sequence), i (range (.size track))]
             (.get track i))
           (keep (partial note-event (.getResolution sequence)))
           (sort-by :beat) vec))))

(defn- playback-step [{:keys [bridge active trace]} [event-id {:keys [beat song-beat message]}]]
  (let [envelope {:command-id (str "event-" event-id) :time-ms (* 500 song-beat) :epoch 1 :show-id :pickup}
        {:keys [state commands]} (live/midi->commands bridge message envelope active)
        {:keys [op value] :as command} (first commands)
        next-id (case op :cue/launch (str "gate-" event-id) :instance/end nil active)
        ack {:status :accepted :command-id (:command-id command) :instance-id next-id}]
    {:bridge (if (= :cue/launch op) (:state (live/acknowledge-launch state (:note message) ack envelope)) state)
     :active next-id
     :trace (cond-> trace op (conj (cond-> {:song-beat song-beat :clip-beat beat :command op}
                                    (some? value) (assoc :level value :velocity (:velocity message)))))}))

(defn playback
  "Run the first pass and one repeat of the qualification clip's 2..6 loop."
  [events]
  (let [bridge (live/new-bridge {:mappings {36 {:cue-id :wash :mode :held}}
                                 :parameters {1 {:cue-id :wash :param :level :bounds [0 1] :lane-note 100}}})
        intro (for [event events :when (< (:beat event) 6)] (assoc event :song-beat (:beat event)))
        repeat-loop (for [event events :when (<= 2 (:beat event)) :while (< (:beat event) 6)]
                      (assoc event :song-beat (+ 4 (:beat event))))]
    (:trace (reduce playback-step {:bridge bridge :trace []} (map-indexed vector (concat intro repeat-loop))))))

(defn -main [& [path]]
  (let [rows (playback (clip-events (or path fixture)))]
    (pprint/print-table [:song-beat :clip-beat :command :velocity :level]
                        (map #(cond-> % (contains? % :level) (update :level (partial format "%.3f"))) rows))))
