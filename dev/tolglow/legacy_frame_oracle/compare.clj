(ns tolglow.legacy-frame-oracle.compare
  "Run the corresponding authored scenario with the functional runtime."
  (:require [afterglow.control.instance-order :as instance-order]
            [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.replay :as replay]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.legacy-frame-oracle.scenario :as scenario]))

(def rig [(assoc scenario/fixture :channels scenario/channels
                 :calibration scenario/calibration :rgbw-policy :extract-min)])

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

(defn- initial-show [authored-rgb]
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

(defn- run-scenario [authored-rgb compile-effect]
  (loop [operations (map-indexed vector scenario/operations), model (initial-show authored-rgb)
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
    (throw (ex-info message {:expected expected :observed observed}))))

(defn- byte-differences [legacy paired]
  (ensure-equal! "Universe sets differ" (set (keys legacy)) (set (keys paired)))
  (vec (for [[universe old] legacy
             :let [current (paired universe)
                   _ (ensure-equal! "Every universe must contain 512 bytes" [512 512] [(count old) (count current)])]
             index (range 512)
             :let [before (nth old index) after (nth current index)]
             :when (not= before after)]
         [universe (inc index) before after])))

(defn- compare-scenario [legacy]
  (ensure-equal! "Wrong legacy source" scenario/legacy-source (:source legacy))
  (ensure-equal! "Wrong legacy scenario seed" scenario/seed (:seed legacy))
  (let [paired (run-scenario (:authored-rgb legacy) (batch/compiler-for-rig rig))
        scalar (run-scenario (:authored-rgb legacy) compiler/compile-effect)
        frame-fields [:label :epoch :time-ms :active]
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
    {:source scenario/paired-source
     :matching-lifecycle-operations (count (:history paired))
     :stale-rejection (:rejection (last (:history paired)))
     :frames (mapv (fn [old current]
                     (assoc (select-keys current frame-fields)
                            :legacy-reductions (:reductions old)
                            :legacy-resolution-order (:resolution-order old)
                            :paired-contributions (:contributions current)
                            :byte-differences (byte-differences (:universes old) (:universes current))))
                   (:frames legacy) (:frames paired))}))

(defn -main [& [legacy-path report-path expected-path]]
  (when-not (and legacy-path report-path)
    (throw (ex-info "Supply legacy capture, report, and optional expected report paths" {})))
  (let [report (compare-scenario (scenario/read-edn legacy-path))]
    (scenario/write-edn! report-path report)
    (when expected-path
      (ensure-equal! "Parity evidence changed, review the emitted report"
                     (scenario/read-edn expected-path) report))
    (let [counts (mapv (comp count :byte-differences) (:frames report))]
      (println (:matching-lifecycle-operations report) "lifecycle operations matched")
      (println (count counts) "complete frames compared, with" (reduce + counts) "changed bytes"))))
