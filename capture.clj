(require '[afterglow.control.model :as control]
         '[afterglow.output.scheduler :as scheduler]
         '[clojure.data.json :as json]
         '[tolglow.show :as show]
         '[tolglow.studio :as studio])

(def heads
  (mapv (fn [i id]
          {:id id :position [i 3 0] :universe 1 :address (+ 1 (* 11 i))
           :beam-angle 20 :lens-radius-m 0.1 :pseudo-fixture true
           :channels {:pan [1 2] :tilt [3 4] :red 5 :green 6 :blue 7}})
        (range) [:one :two :three]))

(def registry
  {:effects {:beam {:id :beam :name "Beam" :version 1 :params {}
                    :targets [:one :two :three] :outputs {:color [1.0 0.4 0.2]}}
             :lamp {:id :lamp :name "Lamp" :version 1 :params {}
                    :targets [:outside] :outputs {:dimmer 1.0}}}
   :cues {:beam {:id :beam :effect-id :beam :version 1}
          :lamp {:id :lamp :effect-id :lamp :version 1}}})

(def envelope {:attack {:mode :linear :ms 100} :release {:mode :linear :ms 400}})

(defn bytes-copy [universes]
  (into {} (map (fn [[u bytes]] [u (mapv #(bit-and 255 %) bytes)])) universes))

(defn capture [batch? scope]
  (let [physical (atom 0)
        latest (atom nil)
        rig {:fixtures (conj heads {:id :outside :universe 1 :address 34 :channels {:dimmer 1}})
             :room {:width 20 :depth 20 :height 6}}
        model (assoc (control/new-show {:registry registry}) :rig rig
                     :settings {:output {:presentation {:mode :buffered-interpolation :latency-ms 50}}
                                :output-response (assoc scope :enabled? true)})
        runtime (show/create-runtime {:model model :workspace-options nil :batch? batch?
                                      :independent-output? true :physical-now-ms #(deref physical)
                                      :sink {:send! (fn [bytes _] (reset! latest (bytes-copy bytes)))}})
        tick! (fn [time]
                (reset! physical time)
                (show/tick! runtime time)
                (assert (every? nil? (map #(get-in @(:state runtime) [:status %]) [:error :output-error]))))
        submit! (fn [operation payload time]
                  (let [receipt (show/submit! runtime operation payload)]
                    (tick! time)
                    (assert (= :accepted (:status @receipt)) (pr-str @receipt))
                    @receipt))
        aim! (fn [target time]
               (submit! :surface-aim {:mode :edge-to-edge :targets [:one target] :surface-id :floor
                                      :point [0 0 0] :along :u} time))
        committed (fn [] (bytes-copy (get-in @(:state runtime) [:output :universes])))
        publish! (fn [time]
                   (reset! physical time)
                   (let [packet (scheduler/publish! (:publisher runtime))
                         bytes @latest]
                     (assert (= (:universes packet) bytes))
                     {:at time :sources (get-in packet [:presentation :source-times])
                      :weight (double (get-in packet [:presentation :weight])) :bytes bytes}))]
    (try
      (submit! :command {:op :cue/launch :cue-id :beam} 0)
      (let [lamp (:instance-id (submit! :command {:op :cue/launch :cue-id :lamp} 25))]
        (aim! :two 125)
        (submit! :command {:op :instance/remove :instance-id lamp} 150)
        (let [before (committed)]
          (publish! 175)
          (aim! :three 200)
          (let [reports (get-in (studio/browser-snapshot runtime {}) [:output :edge-tracking])
                rejected (committed)
                before-endpoint (publish! 200)
                entering (publish! 225)]
            (assert (= [nil :overlap] (mapv :reason reports)))
            (tick! 225)
            (let [still-rejected (committed)
                  rejected-endpoint (publish! 250)]
              (submit! :surface-aim {:mode :release :targets [:two]} 250)
              (let [recovered (committed)
                    held-endpoint (publish! 275)
                    leaving (publish! 287.5)
                    recovered-endpoint (publish! 300)]
                (tick! 325)
                (let [full (committed)
                      full-endpoint (publish! 375)
                      endpoints [before-endpoint rejected-endpoint held-endpoint recovered-endpoint full-endpoint]
                      results [before-endpoint entering rejected-endpoint held-endpoint leaving recovered-endpoint full-endpoint]
                      levels (mapv (fn [{:keys [bytes]}] (mapv (get bytes 1) [26 33])) results)]
                  (assert (= [before rejected still-rejected recovered full] (mapv :bytes endpoints)))
                  (assert (= [[150 200] [225 250]] (mapv :sources [entering leaving])))
                  (assert (= [0.5 0.5] (mapv :weight [entering leaving])))
                  (assert (= [[255 239] [0 223] [0 207] [0 191] [0 183] [64 175] [255 128]] levels)
                          (pr-str levels))
                  {:levels levels :samples results}))))))
      (finally (show/stop! runtime)))))

(let [global {:global envelope}
      local {:fixtures {:three envelope :outside envelope}}
      result (capture false global)]
  (assert (= result (capture true global) (capture false local) (capture true local)))
  (spit (first *command-line-args*)
        (json/write-str (assoc result :tolglow (second *command-line-args*)
                                      :afterglow (nth *command-line-args* 2))))
  (println "Identical rich/compact captures and global/fixture response" (:levels result)))
(shutdown-agents)
