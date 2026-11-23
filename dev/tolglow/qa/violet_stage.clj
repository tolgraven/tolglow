(ns tolglow.qa.violet-stage
  "A disconnected, deterministic small-stage reference using the normal cue and fixture pipeline."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

;; In metres, +Z faces the audience. Aim and colour are authored in cue space.
(def ^:private lights
  [{:position [-2.45 3.01 -1.82] :aim [0.12 1.45 -0.70] :angle 19 :rgb [0.28 0.009 1] :level 0.80}
   {:position [2.35 2.89 -1.62] :aim [0.12 1.45 -0.70] :angle 21 :rgb [0.86 0.009 0.45] :level 0.67}
   {:position [-2.22 0.47 -1.90] :aim [-1.83 1.57 -4.08] :angle 86 :rgb [0.24 0.015 1] :level 0.52}
   {:position [2.36 0.47 -2.01] :aim [1.88 1.44 -4.08] :angle 82 :rgb [0.70 0.014 0.38] :level 0.44}
   {:position [-1.84 0.66 -3.33] :aim [-2.52 0.04 -0.12] :angle 11 :rgb [1 0.17 0.018] :level 0.15 :radius 0.035}
   {:position [0.12 0.81 -3.54] :aim [0.40 0.04 0.74] :angle 10 :rgb [1 0.24 0.033] :level 0.12 :radius 0.032}
   {:position [1.70 0.69 -3.43] :aim [2.10 0.04 0.04] :angle 12 :rgb [1 0.15 0.015] :level 0.14 :radius 0.036}])

(defn- register-look [model {:keys [id] :as effect}]
  (-> model
      (assoc-in [:registry :effects id] effect)
      (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id})))

(defn stage-model []
  (let [fixtures (mapv (fn [fixture {:keys [position angle radius]}]
                         (assoc fixture :position position :beam-angle angle :lens-radius-m (or radius 0.085)))
                       (demo/fixtures) lights)
        looks (mapv (fn [{:keys [id]} {:keys [aim rgb level]}]
                      {:id id :version 1 :targets [id] :params {}
                       :outputs {:aim (demo/constant aim) :color (demo/constant rgb)
                                 :dimmer level :focus 0.08}})
                    fixtures lights)
        jet {:id :stage-fogger :position [-3.45 0.42 -2.14] :address 190 :universe 1
             :channels {:fog 1} :fogger {:flow-m3-min 105 :mixing-seconds 3.5}}
        wing {:id :stage-wing :point [-2.48 0.78 0.50] :size [0.79 1.56]
              :normal [0 0 1] :u-axis [1 0 0]}
        rig {:room {:width 9.4 :height 3.7 :depth 8.2}
             :fixtures (conj fixtures jet) :surfaces [wing]
             :trusses [{:id :stage-truss :position [0 3.43 -1.75] :length 6.9}]}
        haze {:id :pre-show-haze :version 1 :targets [:stage-fogger] :params {} :outputs {:fog 1}}]
    (reduce register-look (assoc (demo/model) :rig rig) (conj looks haze))))

(defn- take-look! [runtime]
  (let [fixture-ids (mapv :id (take (count lights) (get-in @(:state runtime) [:model :rig :fixtures])))
        launch #(show/submit! runtime {:op :cue/launch :cue-id %})
        receipts (mapv launch (conj fixture-ids :pre-show-haze))]
    (show/tick! runtime 0)
    (assert (every? #(= :accepted (:status @%)) receipts))
    (doseq [t (range 250 18750 250)] (show/tick! runtime t))
    (let [off (show/submit! runtime {:op :instance/end :instance-id (:instance-id @(peek receipts))})]
      (show/tick! runtime 18750)
      (assert (= :accepted (:status @off))))
    (doseq [t (range 19000 40250 250)] (show/tick! runtime t))
    (let [snapshot (show/snapshot runtime)]
      (-> (select-keys snapshot [:rig :epoch :time-ms :frame-number :revision])
          (assoc :show-id "violet-stage-reference"
                 :output (select-keys (:output snapshot) [:atmosphere :values]))))))

(defn -main [directory]
  (let [dir (doto (io/file directory) .mkdirs)
        runtime (show/create-runtime {:model (stage-model)})]
    (try
      (spit (io/file dir "show-frame.json") (json/write-str (take-look! runtime)))
      (finally (show/stop! runtime) (shutdown-agents)))))
