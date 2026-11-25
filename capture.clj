(require '[afterglow.control.model :as control]
         '[afterglow.output.scheduler :as scheduler]
         '[clojure.data.json :as json]
         '[tolglow.show :as show])

(def inverted-lamp
  {:id :lamp :universe 1 :address 512
   :channels {:dimmer {:offset 1 :fine-offset 2 :inverted-from 0}}})

(def normal-lamp (assoc inverted-lamp :channels {:dimmer [1 2]}))
(def fog {:id :fog :universe 1 :address 2 :channels {:fog 1}
          :fogger {:dmx-range [196 0] :flow-m3-min 60}})

(def registry
  {:effects {:shine {:id :shine :name "Shine" :version 1 :params {}
                      :targets [:lamp] :outputs {:dimmer 1.0}}}
   :cues {:shine {:id :shine :effect-id :shine :version 1}}})

(def slots [[1 511] [2 0] [1 1]])

(defn capture-sink [clock]
  (let [sent (atom [])]
    {:sent sent
     :send! (fn [universes frame-id]
              (swap! sent conj {:at @clock :frame-id frame-id
                                :bytes (into {}
                                             (map (fn [[u bytes]] [u (mapv #(bit-and 255 %) bytes)]))
                                             universes)}))}))

(defn tick! [runtime clock time]
  (reset! clock time)
  (show/tick! runtime time)
  (assert (every? nil? (map #(get-in @(:state runtime) [:status %]) [:error :output-error]))))

(defn submit! [runtime clock time operation payload]
  (let [receipt (show/submit! runtime operation payload)]
    (tick! runtime clock time)
    (assert (= :accepted (:status @receipt)) (pr-str @receipt))))

(defn repatch! [runtime clock time fixtures]
  (let [model (:model @(:state runtime))]
    (submit! runtime clock time :rig (assoc (:rig model) :fixtures fixtures :expected-epoch (:epoch model)))))

(defn publication! [owner clock time & deadline]
  (reset! clock time)
  (apply scheduler/publish! owner deadline))

(defn samples [sent]
  (mapv #(assoc % :levels (mapv (fn [slot] (get-in (:bytes %) slot)) slots)) @sent))

(defn ordinary-capture []
  (let [clock (atom 0)
        {:keys [sent send!]} (capture-sink clock)
        model (-> (control/new-show {:registry registry :rig {:fixtures [inverted-lamp fog]}})
                  (assoc-in [:settings :output :presentation] {:mode :buffered-interpolation :latency-ms 25}))
        runtime (show/create-runtime {:model model :workspace-options nil :independent-output? true
                                      :physical-now-ms #(deref clock) :sink {:send! send!}})
        publish! (partial publication! (:publisher runtime) clock)]
    (try
      (submit! runtime clock 0 :command {:op :cue/launch :cue-id :shine})
      (assert (get-in (publish! 0) [:presentation :waiting?]))
      (publish! 25)
      (submit! runtime clock 50 :command {:op :master/set :value 0})
      (assert (get-in (publish! 50) [:presentation :blackout?]))
      (submit! runtime clock 75 :command {:op :master/set :value 1})
      (assert (get-in (publish! 75) [:presentation :waiting?]))
      (publish! 100)
      (repatch! runtime clock 125 [fog])
      (assert (get-in (publish! 125) [:presentation :waiting?]))
      (repatch! runtime clock 150 [normal-lamp fog])
      (assert (get-in (publish! 150) [:presentation :waiting?]))
      (publish! 175)
      (repatch! runtime clock 200 [inverted-lamp fog])
      (assert (get-in (publish! 200) [:presentation :waiting?]))
      (publish! 225)
      (reset! clock 250)
      (finally (show/stop! runtime)))
    (let [results (samples sent)]
      (mapv (fn [sample stage] (assoc sample :stage stage)) results
            [:first-wait :live :blackout :reopen-wait :reopened :withdrawn
             :reused-wait :reused :inverted-again-wait :inverted-again :shutdown]))))

(defn stale-safety-capture []
  (let [clock (atom 0)
        stall? (atom false)
        {:keys [sent send!]} (capture-sink clock)
        model (control/new-show {:rig {:fixtures [normal-lamp]}})
        runtime (show/create-runtime {:model model :workspace-options nil :independent-output? true
                                      :physical-now-ms #(deref clock) :sink {:send! send!}})
        owner (update (:publisher runtime) :prepare-output
                      (fn [prepare]
                        (fn [universes source]
                          (let [packet (prepare universes source)]
                            (when @stall? (swap! clock + 26))
                            packet))))
        publish! (partial publication! owner clock)]
    (try
      (tick! runtime clock 0)
      (publish! 25 25)
      (repatch! runtime clock 40 [inverted-lamp fog])
      (reset! stall? true)
      (let [late (publish! 50 50)]
        (assert (:safety-output? late))
        (assert (:expired-before-send? late)))
      (select-keys (peek (samples sent)) [:at :frame-id :levels :bytes])
      (finally (show/stop! runtime)))))

(let [[output tolglow afterglow] *command-line-args*
      ordinary (ordinary-capture)
      stale (stale-safety-capture)]
  (spit output
        (json/write-str {:tolglow tolglow :afterglow afterglow :slots slots
                         :ordinary ordinary :expired-stale-safety stale}))
  (prn (mapv (juxt :stage :levels) ordinary))
  (prn :expired-stale-safety (:levels stale)))
(shutdown-agents)
