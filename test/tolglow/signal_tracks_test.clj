(ns tolglow.signal-tracks-test
  "Signal tracks (#885) through the real show owner with captured output:
  a #670 rehearsal take records its stems and Push writes and replays from
  them alone, a transport stop records and replays without catching up, the
  writer never delays a frame, tracks are bounded, and full backups and
  sharing packages keep or leave them out."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.show :as show]
            [tolglow.signal-tracks :as signal-tracks]
            [tolglow.signal-tracks.recorder :as recorder]
            [tolglow.stems :as stems]))

(def frame-ms 125.0)

(def recorded-take
  (delay (rehearsal/run-take (passage/concert 670 :b) {:record-signals? true})))

(defn- digests [take] (mapv :dmx-hash (:frames take)))

(defn- frames [take] (mapv #(dissoc % :t) (:frames take)))

(deftest a-rehearsal-take-with-stems-and-push-gestures-writes-signal-tracks
  (let [{:keys [recording] :as bundle} (:signal-tracks @recorded-take)]
    (is (= #{"event/stem/kick/onset" "event/stem/snare/onset" "stem/bass/level" "stem/bass/onset"
             "control/push-3/chorus-wash/level@push-3-rehearsal" "control/push-3/sweep/span@push-3-rehearsal"}
           (set (keys (:tracks recording)))))
    (is (= {:kind :rehearsal-take :passage :concert :take :b :seed 670} (:attached-to recording)))
    (is (= [{:beat 0.0 :time-ms 0.0 :bpm 120.0 :playing? true}] (:tempo recording)) "one tempo segment at 120 BPM")
    (is (false? (:truncated? recording)))
    (is (= bundle (signal-tracks/validate-bundle bundle)) "every track matches its content hash")
    (testing "each source keeps its kind, unit, range and rows of beat, time, value and confidence"
      (let [kick (signal-tracks/track bundle "event/stem/kick/onset")
            bass (signal-tracks/track bundle "stem/bass/level")
            grab (signal-tracks/track bundle "control/push-3/chorus-wash/level@push-3-rehearsal")
            onsets (filter #(= :kick (:stem %)) (:onsets (passage/concert 670 :b)))]
        (is (= [:audio-event :event [0.0 1.0]] ((juxt :kind :unit :range) kick)))
        (is (= (mapv :beat onsets) (mapv #(nth % 4) (:rows kick))) "every kick onset once, at its own beat")
        (is (every? (fn [[resolved _ _ _ onset]] (= resolved (+ onset 0.25))) (:rows kick))
            "resolved by the frame after the capture block that carried it")
        (is (= [:audio :normalized [0.0 1.0] {:signal :stem/level :stem :bass :transport :audio}]
               ((juxt :kind :unit :range :address) bass)))
        (is (= (count (:levels (passage/concert 670 :b))) (count (:rows bass))))
        (is (= [{:beat 42.25 :time-ms 21125.0 :value 0.35 :confidence 1.0
                 :instance-id (:target (first (filter #(= :param/set (:op %)) (:gestures @recorded-take))))}]
               (mapv #(signal-tracks/row-map grab %) (:rows grab)))
            "the Push grab of the chorus level, on the frame that applied it, with the instance it addressed")))))

(deftest replaying-a-take-with-its-tracks-as-the-only-signal-input-reproduces-its-output
  (let [bundle (:signal-tracks @recorded-take)
        replay (rehearsal/run-take (passage/concert 670 :b) {:signal-tracks bundle})
        without (rehearsal/run-take (rehearsal/signal-input-passage (passage/concert 670 :b)))]
    (is (= (digests @recorded-take) (digests replay)) "every frame's output digest")
    (is (= (frames @recorded-take) (frames replay)) "every frame's instances, parameters, light and beat")
    (is (not= (digests @recorded-take) (digests without)) "without the tracks the take is not reproduced")
    (is (zero? (get-in replay [:signal-replay :undelivered])))
    (is (= (reduce + (map (comp count :rows) (vals (:tracks bundle)))) (count (get-in replay [:signal-replay :delivered]))))
    (is (every? (fn [[_ recorded replayed]] (< (Math/abs (- recorded replayed)) 1e-9)) (get-in replay [:signal-replay :delivered]))
        "each row is resolved by the frame whose beat it was recorded at")
    (is (= [:accepted :accepted] (keep #(when (re-find #"^Signal track" (:label %)) (:status %)) (:gestures replay))))))

(deftest across-a-transport-stop-tracks-gain-no-beats-and-the-replay-holds-and-resumes
  (let [stopped (assoc (passage/concert 670 :b) :stops [{:beat 66.0 :hold-ms 2000.0}])
        take (rehearsal/run-take stopped {:record-signals? true})
        {:keys [recording] :as bundle} (:signal-tracks take)
        [play stop resume] (:tempo recording)
        rows (mapcat :rows (vals (:tracks bundle)))
        replay (rehearsal/run-take stopped {:signal-tracks bundle})
        stopped-frames (filter #(false? (:playing? %)) (:frames take))]
    (is (= [true false true] (mapv :playing? [play stop resume])) "the stop is a tempo segment")
    (is (= (:beat stop) (:beat resume)) "beats stand still across it")
    (is (< 8 (count stopped-frames)) "two seconds of stopped frames")
    (is (= 2000.0 (- (:time-ms resume) (:time-ms stop))))
    (testing "the stop adds no row: every track has the beats it has without the stop"
      (let [plain (:signal-tracks @recorded-take)]
        (doseq [id (keys (:tracks recording))
                :let [stopped-rows (:rows (signal-tracks/track bundle id))
                      plain-rows (:rows (signal-tracks/track plain id))]]
          (is (= (mapv first plain-rows) (mapv first stopped-rows)) id)
          (is (every? (fn [[[beat t] [_ plain-t]]] (= t (if (> beat (:beat stop)) (+ plain-t 2000.0) plain-t)))
                      (map vector stopped-rows plain-rows))
              (str id ": capture times after the stop carry the hold, beats do not")))))
    (is (= (signal-tracks/beat->time-ms recording (+ (:beat stop) 1.0)) (+ (:time-ms resume) 500.0))
        "after the stop a beat maps to time from where play resumed")
    (is (= (digests take) (digests replay)))
    (let [beats (map :beat (:frames replay))
          steps (map - (rest beats) beats)]
      (is (every? #(<= -1e-9 % (+ 0.25 1e-9)) steps) "no frame catches up after the stop"))
    (is (= (count (filter #(= (:beat stop) (first %)) rows))
           (count (filter (fn [[_ _ replayed]] (= replayed (:beat stop))) (get-in replay [:signal-replay :delivered]))))
        "rows of the stop's frame are delivered once, not again on each stopped frame")))

;; ---------------------------------------------------------------------------
;; A small show with a counting stem source

(defn- harness []
  (let [rt (show/create-runtime {:model (passage/model 120) :sink {:send! (fn [_ _])} :workspace-options nil})]
    (show/tick! rt 0.0)
    (let [{:keys [saved enabled]} (stems/route-level! rt :bass :verse-wash :level)
          {:keys [receipt]} (stems/route-onset! rt :kick :kick-hit)]
      (show/tick! rt 0.0)
      (is (= [:accepted :accepted :accepted]
             (mapv #(:status (deref % 1000 nil)) [saved enabled receipt]))))
    rt))

(defn- counting-source
  "A stem level source that counts how often it is read; each read is a new value."
  []
  (let [reads (atom 0)]
    {:reads reads :read! #(/ (swap! reads inc) 1024.0)}))

(defn- play-frames!
  "Read the source once per frame and deliver it as capture would, then tick."
  [rt {:keys [read!]} from n]
  (let [callbacks (show/audio-callbacks rt)]
    (doseq [i (range from (+ from n))
            :let [t (* frame-ms (inc i))]]
      ((:features! callbacks) {:time-ms (* frame-ms i) :capture {:run-id "count"} :stems {:bass {:level (read!) :onset-count 0}}})
      (show/tick! rt t))))

(defn- counted-run [record?]
  (let [rt (harness) source (counting-source) samples (atom 0) sample parameter-sources/sample]
    (try
      (let [signals (when record? (recorder/attach! rt {:id "count" :attached-to {:kind :run-take :take-id "count"}}))]
        (with-redefs [parameter-sources/sample (fn [& args] (swap! samples inc) (apply sample args))]
          (play-frames! rt source 0 32))
        {:reads @(:reads source) :samples @samples :bundle (some-> signals recorder/finish!)})
      (finally (show/stop! rt)))))

(deftest a-counting-source-is-sampled-once-per-frame-with-recording-on-and-off
  (let [off (counted-run false) on (counted-run true)
        rows (:rows (signal-tracks/track (:bundle on) "stem/bass/level"))]
    (is (= 32 (:reads off) (:reads on)) "the source is read once per frame either way")
    (is (pos? (:samples off)))
    (is (= (:samples off) (:samples on)) "the frame's waveform sampler runs as often with recording on")
    (is (= (mapv #(/ % 1024.0) (range 1 33)) (mapv #(nth % 2) rows))
        "each value the frame resolved is recorded once, in order")))

(deftest past-its-bound-a-track-counts-what-it-dropped-and-is-truncated
  (let [rt (harness)]
    (try
      (let [signals (recorder/attach! rt {:id "bounded" :attached-to {:kind :run-take :take-id "b"} :max-rows 8})
            _ (play-frames! rt (counting-source) 0 20)
            {:keys [recording] :as bundle} (recorder/finish! signals)
            level (signal-tracks/track bundle "stem/bass/level")]
        (is (= [8 12 true] [(count (:rows level)) (:dropped level) (:truncated? level)]))
        (is (= (mapv #(/ % 1024.0) (range 1 9)) (mapv #(nth % 2) (:rows level))) "the first rows are kept")
        (is (true? (:truncated? recording))))
      (finally (show/stop! rt)))))

(deftest a-blocked-track-writer-does-not-delay-committed-frames
  (let [rt (harness) gate (java.util.concurrent.CountDownLatch. 1) entered (promise)]
    (try
      (let [signals (recorder/attach! rt {:id "blocked" :attached-to {:kind :run-take :take-id "blocked"}
                                          :queue-capacity 4
                                          :before-fold (fn [] (deliver entered true) (.await gate))})
            source (counting-source)
            ticks (future (play-frames! rt source 0 40) (show/snapshot rt) :done)]
        (is (= :done (deref ticks 20000 :blocked)) "40 frames commit while the writer is stalled")
        (is (true? (deref entered 1000 false)))
        (is (= 1 (.getCount gate)) "the writer was still stalled throughout")
        (let [{:keys [offered refused]} (recorder/progress signals)]
          (is (= 40 (+ offered refused)))
          (is (<= 35 refused) "frames the full queue refused are counted, not waited for"))
        (.countDown gate)
        (let [{:keys [recording]} (recorder/finish! signals)]
          (is (= (:refused (recorder/progress signals)) (:frames-dropped recording)))
          (is (true? (:truncated? recording)))))
      (finally (.countDown gate) (show/stop! rt)))))

(deftest a-full-backup-keeps-the-tracks-and-a-sharing-export-only-when-selected
  (let [file (java.io.File/createTempFile "signal-tracks" ".tolglow")
        bundle (:bundle (counted-run true))
        id (get-in bundle [:recording :id])]
    (try
      (let [rt (harness)]
        (try
          (let [receipt (recorder/store! rt bundle)]
            (show/tick! rt 5000.0)
            (is (= {:status :accepted :id id :tracks 2} (deref receipt 1000 nil))))
          (is (= bundle (recorder/bundle rt id)))
          (is (= [{:id "stem/bass/level" :rows 32 :kind :audio :hash (get-in bundle [:recording :tracks "stem/bass/level"])}]
                 (mapv #(select-keys (first (filter (comp #{"stem/bass/level"} :id) (:tracks %))) [:id :rows :kind :hash])
                       (recorder/recordings rt)))
              "the REPL lists what is stored")
          (show/save-show! rt (str file))
          (testing "a sharing export names its tracks explicitly"
            (let [model (:model @(:state rt))
                  bare (signal-tracks/export-package model id)
                  chosen (signal-tracks/export-package model id {:tracks ["stem/bass/level"]})
                  hash (get-in bundle [:recording :tracks "stem/bass/level"])]
              (is (= {} (:tracks bare) (get-in bare [:recording :tracks])))
              (is (= ["stem/bass/level" "stem/bass/onset"] (get-in bare [:recording :omitted])))
              (is (= {hash (signal-tracks/track bundle "stem/bass/level")} (:tracks chosen)))
              (is (= hash (signal-tracks/content-hash (get (:tracks chosen) hash))))
              (is (= [{:track "stem/bass/level" :identifies-music? true}] (:privacy chosen))
                  "an audio-derived track can identify the music")))
          (finally (show/stop! rt))))
      (let [rt (harness)]
        (try
          (show/tick! rt 50.0)
          (is (nil? (recorder/bundle rt id)))
          (let [reply (show/load-show! rt (str file))]
            (show/tick! rt 60.0)
            (is (= :accepted (:status (deref reply 1000 ::missing)))))
          (is (= bundle (recorder/bundle rt id)) "the restored show holds the same tracks")
          (let [receipt (recorder/remove! rt id)]
            (show/tick! rt 70.0)
            (is (= :accepted (:status (deref receipt 1000 nil)))))
          (is (= {:recordings {} :tracks {}} (signal-tracks/stored (:model @(:state rt)))))
          (finally (show/stop! rt))))
      (finally (io/delete-file file true)))))

;; ---------------------------------------------------------------------------
;; Pure parts

(deftest a-tampered-or-incomplete-bundle-is-refused
  (let [bundle (:bundle (counted-run true))
        [hash track] (first (:tracks bundle))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"content hash"
          (signal-tracks/validate-bundle (assoc-in bundle [:tracks hash] (update track :rows pop)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not hold"
          (signal-tracks/validate-bundle (update bundle :tracks dissoc hash))))))

(deftest a-grid-summary-is-derived-from-a-track
  (let [level {:id "stem/bass/level" :source :port :range [0.0 1.0]
               :rows [[0.0 0.0 0.2 1.0] [0.125 62.5 0.6 1.0] [0.5 250.0 0.3 1.0] [1.25 625.0 0.9 1.0]]}
        onsets {:id "event/stem/kick/onset" :source :event :range [0.0 1.0]
                :rows [[0.25 0.0 1.0 1.0 0.0] [1.25 500.0 1.0 1.0 1.0]]}]
    (is (= [{:beat 0.0 :level 0.4 :peak 0.6 :onset? true}
            {:beat 0.25 :level 0.6 :peak 0.6 :onset? false}
            {:beat 0.5 :level 0.3 :peak 0.3 :onset? false}
            {:beat 0.75 :level 0.3 :peak 0.3 :onset? false}
            {:beat 1.0 :level 0.3 :peak 0.3 :onset? false}
            {:beat 1.25 :level 0.9 :peak 0.9 :onset? true}]
           (signal-tracks/summary level)))
    (is (= [true false true false] (mapv :onset? (signal-tracks/summary onsets {:grid {:beats 1/2} :from 0.0 :to 2.0}))))
    (is (= 2 (count (signal-tracks/summary level {:grid {:bars 1} :from 0.0 :to 8.0}))))))

(deftest tempo-segments-turn-beats-back-into-time
  (let [recording {:tempo [{:beat 0.0 :time-ms 0.0 :bpm 120.0 :playing? true}
                           {:beat 8.0 :time-ms 4000.0 :bpm 120.0 :playing? false}
                           {:beat 8.0 :time-ms 4000.0 :bpm 90.0 :playing? true}]}]
    (is (= 2000.0 (signal-tracks/beat->time-ms recording 4.0)))
    (is (= 4000.0 (signal-tracks/beat->time-ms recording 8.0)))
    (is (= (+ 4000.0 (/ 60000.0 90.0)) (signal-tracks/beat->time-ms recording 9.0)))))
