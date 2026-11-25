(ns tolglow.legacy-frame-oracle.compare
  "Run the corresponding authored scenario with the functional runtime."
  (:require [afterglow.control.instance-order :as instance-order]
            [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.replay :as replay]
            [clojure.data :as data]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.legacy-frame-oracle.scenario :as scenario]))

(def rig [(assoc scenario/fixture :channels scenario/channels
                 :calibration scenario/calibration :rgbw-policy :extract-min)])

(def ^:private translated-rig
  ;; The original scale is for rotation about +X. The paired fixture expects
  ;; positive elevation, which is the opposite sign for this fixture.
  (update-in rig [0 :calibration :tilt 1] -))

(defn- definitions [authored-rgb]
  (for [[id {:keys [kind attribute aim blend dmx percent]}] (sort-by key scenario/cues)]
    {:id id :version 1 :targets [(:id scenario/fixture)]
     :outputs [(case kind
                 :color {:attribute :color :value (get authored-rgb id) :blend blend}
                 :aim {:attribute :aim :value aim}
                 :channel {:attribute attribute :value (/ (double dmx) 255.0)}
                 ;; The fixture's first shutter-open byte is 8 (the 0% end of 8–15).
                 :function {:attribute attribute :value (if (= attribute :shutter)
                                                          (/ 8.0 255.0)
                                                          (/ (double percent) 100.0))})]}))

(defn- command [model id payload expected-status]
  (let [result (control/apply-command
                model (merge {:command-id (str id) :show-id (:show-id model) :epoch (:epoch model)
                              :source :legacy-frame-oracle} payload))]
    (when-not (= expected-status (get-in result [:ack :status]))
      (throw (ex-info "Unexpected command status" {:command payload :ack (:ack result)})))
    result))

(defn- initial-show [rig authored-rgb]
  (reduce (fn [model {:keys [id] :as definition}]
            (let [published (:model (command model [:definition id]
                                             {:op :registry/publish :kind :effects :definition definition}
                                             :accepted))]
              (:model (command published [:cue id]
                               {:op :cue/put :cue {:id id :effect-id id :priority (:priority (scenario/cues id))}}
                               :accepted))))
          (control/new-show {:show-id :legacy-frame-oracle :rig {:fixtures rig}})
          (definitions authored-rgb)))

(defn- ordering [model]
  (mapv :cue-id (instance-order/ordered model)))

(defn- run-scenario [rig authored-rgb compile-effect]
  (loop [operations (map-indexed vector scenario/operations), model (initial-show rig authored-rgb)
         previous nil, launches {}, first-launches {}, history [], frames []]
    (if-let [[index [op time-ms label :as input]] (first operations)]
      (let [beat (/ time-ms 500.0)
            model (update model :transport assoc :time-ms time-ms :beat beat)
            next-operations (next operations)]
        (case op
          :frame
          (let [frame {:time-ms time-ms :show-id (:show-id model) :epoch (:epoch model) :fixtures rig}
                result (engine/advance model frame compile-effect)
                model (:model result)
                output (replay/render-output model frame (:contributions result) previous)
                ordering (ordering model)
                sources (mapv #(get-in % [:source :cue-id]) (:contributions result))]
            (when-not (= ordering sources)
              (throw (ex-info "Expected one ordered contribution per active effect"
                              {:frame label :sources sources :active ordering :events (:events result)})))
            (recur next-operations model output launches first-launches history
                   (conj frames {:label label :epoch (:epoch model) :time-ms time-ms :beat beat
                                 :active ordering :contributions sources
                                 :aim (:aim (first (render/project-compact (:compact output))))
                                 :intents (:intents result) :events (:events result)
                                 :universes (into (sorted-map)
                                                  (map (fn [[id bytes]] [id (scenario/unsigned-bytes bytes)]))
                                                  (:universes output))})))
          :launch
          (let [{:keys [model ack]} (command model index {:op :cue/launch :cue-id label :time-ms time-ms} :accepted)
                instance {:id (:instance-id ack) :epoch (:epoch model)}]
            (recur next-operations model previous (assoc launches label instance)
                   (if (contains? first-launches label) first-launches (assoc first-launches label instance))
                   (conj history {:input input :epoch (:epoch model) :instance (:id instance) :active (ordering model)}) frames))
          (:end :end-stale)
          (let [stale? (= op :end-stale)
                instance ((if stale? first-launches launches) label)
                {:keys [model ack]} (command model index {:op :instance/end :instance-id (:id instance)
                                                         :epoch (:epoch instance) :force? true :time-ms time-ms}
                                            (if stale? :rejected :accepted))]
            (recur next-operations model previous launches first-launches
                   (conj history (cond-> {:input input :epoch (:epoch model) :instance (:id instance)
                                          :outcome (if stale? :ignored :ended) :active (ordering model)}
                                   stale? (assoc :rejection (:code ack)))) frames))
          :restart
          (let [model (:model (command model index {:op :show/restart :time-ms time-ms} :accepted))]
            (recur next-operations model previous {} first-launches
                   (conj history {:input input :epoch (:epoch model) :active (ordering model)}) frames))))
      {:history history :frames frames})))

(defn- ensure-equal! [message expected observed]
  (when-not (= expected observed)
    (let [[only-expected only-observed] (data/diff expected observed)]
      (throw (ex-info message {:only-expected only-expected :only-observed only-observed
                               :expected expected :observed observed})))))

(defn- byte-differences [legacy paired]
  (ensure-equal! "Universe sets differ" (set (keys legacy)) (set (keys paired)))
  (vec (for [[universe old] legacy
             :let [current (paired universe)
                   _ (ensure-equal! "Every universe must contain 512 bytes" [512 512] [(count old) (count current)])]
             index (range 512)
             :let [before (nth old index) after (nth current index)]
             :when (not= before after)]
         [universe (inc index) before after])))

(def ^:private frame-fields [:label :epoch :time-ms :active])

(defn- compare-scenario [legacy rig]
  (let [paired (run-scenario rig (:authored-rgb legacy) (batch/compiler-for-rig rig))
        scalar (run-scenario rig (:authored-rgb legacy) compiler/compile-effect)
        history-fields [:input :epoch :active :outcome]
        project (fn [fields entries] (mapv #(select-keys % fields) entries))]
    (ensure-equal! "Scalar and typed execution must agree" scalar paired)
    (ensure-equal! "Lifecycle histories differ"
                   (project history-fields (:history legacy)) (project history-fields (:history paired)))
    (ensure-equal! "Frame order or active effects differ"
                   (project frame-fields (:frames legacy)) (project frame-fields (:frames paired)))
    (doseq [[old current] (map vector (:frames legacy) (:frames paired))]
      (let [{:keys [beat beat-phase]} (:snapshot old)]
        (ensure-equal! "Legacy and paired snapshots differ"
                       (+ (dec beat) beat-phase) (:beat current)))
      (doseq [{:keys [sources]} (:reductions old)]
        (ensure-equal! "Contribution order differs within a legacy assignment chain"
                       sources (filterv (set sources) (:contributions current)))))
    paired))

(defn- original-report [legacy paired]
  {:source scenario/paired-source
   :matching-lifecycle-operations (count (:history paired))
   :stale-rejection (:rejection (last (:history paired)))
   :frames (mapv (fn [old current]
                   (assoc (select-keys current frame-fields)
                          :legacy-reductions (:reductions old)
                          :legacy-resolution-order (:resolution-order old)
                          :paired-contributions (:contributions current)
                          :byte-differences (byte-differences (:universes old) (:universes current))))
                 (:frames legacy) (:frames paired))})

(defn- difference-reason [label [_ address :as change]]
  (case address
    (37 38) :pan-word-encoding
    (39 40) :tilt-word-encoding
    42 :implicit-dimmer
    (44 45 46 47)
    (cond
      (and (= 47 address) (#{:base-with-override :base-revealed :relaunch-survives} label))
      :white-rounding
      (and (#{44 46 47} address) (#{:overlapping-colors :back-aim-with-history} label))
      :htp-and-rgbw-extraction
      :else :rgbw-extraction)
    (throw (ex-info "Unexpected changed channel in translated render" {:frame label :change change}))))

(defn- translated-frame [fixture old original current]
  (let [{:keys [label aim universes]} current
        {:keys [address universe]} fixture
        tilt-indices (mapv #(+ (- address 2) %) (get-in fixture [:channels :tilt]))
        reachable? (when aim (render/aim-reachable? fixture aim))
        expected-reachable? (case label
                              (:back-aim-with-history :base-revealed) true
                              (:base-with-override :overlapping-colors :equal-priority-cover :relaunch-survives) false
                              (:idle :after-restart) nil)
        tilt-bytes (if expected-reachable? [15 12] [0 0])
        expected-universes (reduce (fn [output [index value]] (assoc-in output [universe index] value))
                                   (:universes original) (map vector tilt-indices tilt-bytes))]
    (ensure-equal! "Translated fixture changed frame state, contribution order, aim, or events"
                   (dissoc original :universes) (dissoc current :universes))
    (ensure-equal! "Unexpected translated aim reachability" expected-reachable? reachable?)
    ;; Prove the complete output by changing only the two predicted tilt bytes.
    (ensure-equal! "Translated universe must match the predicted tilt bytes and all other channels"
                   expected-universes universes)
    (assoc (select-keys current frame-fields)
           :tilt-bytes tilt-bytes :aim-reachable? reachable?
           :byte-differences (mapv #(conj % (difference-reason label %))
                                   (byte-differences (:universes old) universes)))))

(defn- translated-report [legacy original]
  (let [translated (compare-scenario legacy translated-rig)
        fixture (first translated-rig)
        frames (mapv (partial translated-frame fixture)
                     (:frames legacy) (:frames original) (:frames translated))]
    (ensure-equal! "Translated fixture changed command history" (:history original) (:history translated))
    {:source scenario/paired-source
     :calibration (:calibration fixture)
     :matching-lifecycle-operations (count (:history translated))
     :stale-rejection (:rejection (last (:history translated)))
     :difference-totals (into (sorted-map) (frequencies (map peek (mapcat :byte-differences frames))))
     :frames frames}))

(defn reports
  "Return the original paired comparison and optionally the fixture-local tilt translation."
  [legacy translate?]
  (ensure-equal! "Wrong legacy source" scenario/legacy-source (:source legacy))
  (ensure-equal! "Wrong legacy scenario seed" scenario/seed (:seed legacy))
  (let [paired (compare-scenario legacy rig)]
    (cond-> {:original (original-report legacy paired)}
      translate? (assoc :translated (translated-report legacy paired)))))

(defn- write-report! [report report-path expected-path]
  (scenario/write-edn! report-path report)
  (when expected-path
    (ensure-equal! "Parity evidence changed, review the emitted report"
                   (scenario/read-edn expected-path) report))
  (println (:matching-lifecycle-operations report) "lifecycle operations matched")
  (println (count (:frames report)) "complete frames compared, with"
           (transduce (map (comp count :byte-differences)) + (:frames report)) "changed bytes"))

(defn -main [& [legacy-path report-path expected-path translated-path translated-expected-path]]
  (when-not (and legacy-path report-path)
    (throw (ex-info "Supply legacy capture, report, and optional original and translated report paths" {})))
  (let [{:keys [original translated]} (reports (scenario/read-edn legacy-path) (some? translated-path))]
    (write-report! original report-path expected-path)
    (when translated-path
      (write-report! translated translated-path translated-expected-path))))
