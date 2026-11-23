(ns tolglow.perf.musical-ceilings
  "Compare a mixed compound cue with full-rate ArtDMX through a real tempo change."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.composition :as composition]
            [tolglow.effects.cadence :as cadence]
            [tolglow.perf.multirate-quality :as quality]
            [tolglow.show :as show])
  (:import [java.security MessageDigest]
           [java.util Arrays HexFormat]))

(defn- op [kind & args] {:op kind :args (vec args)})

(defn- scene [rig pixels]
  (let [beat (op :multiply 0.00413 {:op :beat})
        field (fn [axis]
                (let [spatial (nth (iterate #(op :sin (op :add 1.2 %)) {:op :position :axis axis}) 8)]
                  (op :add 0.35 (op :multiply 0.25 spatial) beat)))
        wash {:id :wash :keyframes {:max-interval {:beats 1/8}} :targets (mapv :id (subvec rig 1 pixels))
              :outputs {:color (op :vec3 (field :x) (field :y) (field :z))}}
        steer {:id :steer :targets (mapv :id (subvec rig pixels))
               :outputs {:aim (op :vec3 (op :add 1.31 beat) 0.75 (op :add 3.27 beat))}}
        pulse {:id :pulse :targets [(:id (first rig))]
               :outputs {:dimmer (op :floor (op :add 0.18 (op :fract (op :add (op :multiply 2 {:op :beat}) 0.04))))
                         :color [1 1 1]}}
        put (fn [m {:keys [id] :as definition}]
              (-> m
                  (bench/command [id :publish] {:op :registry/publish :kind :effects
                                                :definition (assoc definition :version 1)})
                  (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id}})))
        effects (into [wash steer pulse] (filter #(= :shutter (:id %))) (quality/programs rig pixels))]
    (-> (reduce put (control/new-show {:show-id :ceilings :rig {:fixtures rig}}) effects)
        (composition/save {:id :stage :mode :macro :slots (mapv #(hash-map :cue-id %) [:wash :steer :pulse])})
        (composition/start :stage 0))))

(defn- measured-pressure [finish session model _]
  (let [cost {:wash 8.0 :steer 3.0 :pulse 20.0 :shutter 20.0}
        measured (into {} (map (fn [[id sample]]
                                 [id (assoc sample :total-ms (* (:calls sample)
                                                                (cost (get-in model [:instances id :effect-id]) 1.0)))]))
                       @(:samples session))]
    (finish (assoc session :samples (delay measured)) model 32.0)))

(def ^:private tempo-edit 40)

(defn- perform! [runtime i]
  (let [commands (cond-> []
                   (= i tempo-edit) (conj {:op :transport/set :transport {:bpm 240} :command-id "faster"})
                   (zero? (mod i 17)) (conj {:op :cue/launch :cue-id :shutter :command-id (str "shot-" i)
                                            :launch {:mode :one-shot :duration {:beats 1/4}}}))
        receipts (mapv #(show/submit! runtime %) commands)]
    (vreset! (get-in runtime [:sink :latest]) [])
    (show/tick! runtime (* quality/period-ms i))
    (when-not (and (every? #(= :accepted (:status @%)) receipts) (seq @(get-in runtime [:sink :latest])))
      (throw (ex-info "Missing ArtDMX or rejected command" {:frame i})))
    @(:state runtime)))

(defn- intervals [state]
  (into {} (map (fn [[id {:keys [effect-id]}]]
                  [effect-id (select-keys (get-in state [:cadence :state :effects id]) [:stride :max-stride])]))
        (get-in state [:model :instances])))

(defn- checked-frame! [reference candidate rig pixels digest i]
  (let [baseline (perform! reference i)
        actual (perform! candidate i)
        packets @(get-in candidate [:sink :latest])
        reference-packets @(get-in reference [:sink :latest])
        full (quality/read-frame baseline rig pixels)
        limited (quality/read-frame actual rig pixels)
        errors (quality/compare-frame rig pixels full limited)
        equal-bytes? #(Arrays/equals ^bytes %1 ^bytes %2)]
    (when-not (and (= (count packets) (count reference-packets))
                   (every? true? (map equal-bytes? packets reference-packets))
                   (= (get-in baseline [:model :instances]) (get-in actual [:model :instances]))
                   (= (get-in baseline [:status :last-intents]) (get-in actual [:status :last-intents]))
                   (empty? (:fault-events actual)) (empty? (:fault-events baseline))
                   (zero? (:dmx-max errors)) (< (:rgb-max errors) 1e-9) (< (:aim-max-m errors) 1e-9))
      (throw (ex-info "Musical sampling changed the full-rate show" {:frame i :errors errors})))
    (doseq [^bytes packet packets] (.update ^MessageDigest digest packet))
    (let [wash-id (some (fn [[id instance]] (when (= :wash (:effect-id instance)) id))
                        (get-in actual [:model :instances]))]
      {:frame i :beat (get-in actual [:model :transport :beat]) :bpm (get-in actual [:model :transport :bpm])
       :wash-window? (contains? (:keyframes actual) wash-id) :rates (intervals actual)
       :universes (count packets) :shutter (:shutter limited) :pulse (:pulse limited)
       :rgb-max (:rgb-max errors) :aim-max-m (:aim-max-m errors)})))

(defn measure [pixels movers]
  (let [fixtures (:fixtures (bench/corpus pixels movers :sculpture))
        rig (into (subvec fixtures 0 pixels) (map #(assoc-in % [:channels :shutter] 12)) (subvec fixtures pixels))
        model (scene rig pixels)
        runtime #(show/create-runtime {:model model :workspace-options nil :workers 2 :batch? true
                                       :worker-timeout-ms 10000 :sink (bench/capture-sink)
                                       :adaptive-keyframes {:enabled? % :pressure-frames 1}})
        full (runtime false) limited (runtime true)
        digest (MessageDigest/getInstance "SHA-256")]
    (try
      (with-redefs [cadence/finish (partial measured-pressure cadence/finish)]
        (let [frames (mapv (partial checked-frame! full limited rig pixels digest) (range 80))
              pre (nth frames (dec tempo-edit)) post (nth frames tempo-edit)]
          (when-not (and (= 2 (get-in pre [:rates :wash :stride])) (:wash-window? pre)
                         (= 1 (get-in post [:rates :wash :stride])) (not (:wash-window? post))
                         (= 8 (get-in post [:rates :steer :stride]))
                         (every? #(= 1 (get-in % [:rates :pulse :stride])) frames)
                         (every? (fn [period]
                                   (and (some #(> (:pulse %) 0.5) period)
                                        (some #(pos? (:shutter %)) period)))
                                 (split-at tempo-edit frames)))
            (throw (ex-info "Mixed compound did not exercise both ceilings and discrete output" {:before pre :after post})))
          {:pixels pixels :movers movers :tempo-change-frame tempo-edit :frames frames
           :artdmx-sha256 (.formatHex (HexFormat/of) (.digest digest))}))
      (finally (show/stop! limited) (show/stop! full)))))

(defn -main [file & args]
  (try
    (let [[pixels movers] (if (seq args) (mapv parse-long args) [1024 128])
          result (measure pixels movers)]
      (io/make-parents file) (spit file (pr-str result)) (prn (dissoc result :frames)))
    (finally (shutdown-agents))))
