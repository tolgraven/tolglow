(ns tolglow.perf.transient-scheduling
  "Compare actual show output under repeatable, injected overload/recovery cost."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.effects.cadence :as cadence]
            [tolglow.effects.sampling-quality :as quality]
            [tolglow.show :as show]))

(defn- scene []
  (let [fixtures [{:id :flash :universe 1 :address 1 :channels {:dimmer 1}}
                  {:id :wash :universe 1 :address 2 :channels {:dimmer 1}}]
        beat {:op :beat}
        flash {:op :floor :args [{:op :add :args [0.12 {:op :fract :args [{:op :add :args [0.04 {:op :multiply :args [2.0 beat]}]}]}]}]}
        wash {:op :add :args [0.5 {:op :multiply :args [0.45 {:op :sin :args [beat]}]}]}
        add (fn [model [id program]]
              (-> model
                  (bench/command [id :publish] {:op :registry/publish :kind :effects
                                                :definition {:id id :version 1 :targets [id] :outputs {:dimmer program}}})
                  (bench/command [id :launch] {:op :effect/launch :effect-id id})))]
    (reduce add (control/new-show {:rig {:fixtures fixtures}}) [[:flash flash] [:wash wash]])))

(defn replay [mode]
  (let [model (scene) ids (into {} (map (fn [[id instance]] [(:effect-id instance) id])) (:instances model))
        runtime (show/create-runtime {:model model :workspace-options nil :workers 2
                                      :adaptive-keyframes {:enabled? (not= mode :reference) :smoothing 0.5}})
        original-finish cadence/finish
        classify quality/transient-output? inspections (atom 0)
        ;; Raw sample admission is real. Only measured costs are replaced to
        ;; make the same pressure and recovery reproducible on different hosts.
        finish (fn [s m _actual-elapsed]
                 (let [frame (:frame-number m)
                       synthetic (fn [id {:keys [calls] :as event}]
                                   [id (assoc event :total-ms (* calls (if (= id (:flash ids)) 6.0 3.0)))])
                       measurements (into {} (map (fn [[id value]] (synthetic id value))) @(:samples s))]
                   (original-finish (assoc s :samples (delay measurements)) m
                                    (if (<= 17 frame 112) 32.0 8.0))))]
    (try
      (with-redefs [quality/transient-output? (fn [d] (swap! inspections inc)
                                                (and (not= mode :unchecked) (classify d)))
                    cadence/finish finish]
        (let [frames (mapv (fn [i]
                             (show/tick! runtime (* 25 i))
                             (let [{:keys [output status cadence]} @(:state runtime)
                                   _ (when (:error status) (throw (ex-info "Frame failed" (:error status))))
                                   b (get-in output [:universes 1])
                                   stride (fn [effect] (get-in cadence [:state :effects (ids effect) :stride] 1))]
                               {:frame i :flash (bit-and (aget ^bytes b 0) 255) :wash (bit-and (aget ^bytes b 1) 255)
                                :flash-stride (stride :flash) :wash-stride (stride :wash)}))
                           (range 320))]
          {:definition-inspections @inspections :frames frames}))
      (finally (show/stop! runtime)))))

(defn -main [& [file]]
  (when-not file (throw (ex-info "Supply an output EDN filename" {})))
  (try
    (let [results (into {} (map (fn [m] [m (replay m)]) [:reference :unchecked :protected]))
          flash (fn [lane] (mapv :flash (get-in results [lane :frames])))
          protected (get-in results [:protected :frames])]
      (when-not (and (= (flash :reference) (flash :protected)) (not= (flash :reference) (flash :unchecked))
                     (> (apply max (map :wash-stride protected)) 1)
                     (= 1 (:wash-stride (peek protected)))
                     (= 2 (get-in results [:protected :definition-inspections])))
        (throw (ex-info "The timed-edge, overload or recovery contract failed" {})))
      (io/make-parents file)
      (spit file (pr-str results))
      (prn {:frames (count protected) :protected-flash-mismatches 0
            :unchecked-flash-mismatches (count (filter false? (map = (flash :reference) (flash :unchecked))))
            :definition-inspections (get-in results [:protected :definition-inspections])
            :peak-wash-stride (apply max (map :wash-stride protected)) :ending-wash-stride (:wash-stride (peek protected))}))
    (finally (shutdown-agents))))
