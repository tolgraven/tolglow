(ns tolglow.predictive-keyframes-test
  (:require [afterglow.reactive :as reactive]
            [afterglow.runtime.keyframes :as keyframes]
            [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(def definition
  {:id :sampled :name "Beat endpoints" :version 1
   :params {:progress {:type :number :bounds [0 1] :default 0.0}}
   :keyframes {:interval {:beats 1} :progress {:mode :replace :param :progress}}
   :outputs {:color [1.0 0.1 0.05] :dimmer {:op :multiply :args [0.25 {:op :beat}]}}})

(defn- model []
  (-> (demo/model)
      (assoc-in [:registry :effects :sampled] definition)
      (assoc-in [:registry :cues :sampled] {:id :sampled :version 1 :effect-id :sampled})))

(defn- apply-at! [runtime kind command t]
  (let [receipt (show/submit! runtime kind command)]
    (show/tick! runtime t)
    (deref receipt 1000 ::missing)))

(defn- inspect [runtime id]
  (keyframes/inspect (get-in @(:state runtime) [:keyframes id]) :head-1 :dimmer))

(deftest compiled-show-obeys-progress-and-restores-the-active-segment
  (doseq [batch? [false true]]
    (let [runtime (show/create-runtime {:model (model) :batch? batch? :workspace-options nil})]
      (try
        (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :sampled} 0))
              get-window #(inspect runtime id)
              lease (reactive/acquire! (show/reactions runtime) [:runtime/keyframe id :head-1 :dimmer] (fn [_]))]
          (try
            (is (= [0.25] (:target (get-window))))
            (apply-at! runtime :command {:op :param/set :instance-id id :param :progress :value 0.8} 125)
            (is (= [0.2] (:result (get-window))))
            (is (= 51 (bit-and 255 (aget ^bytes (get-in @(:state runtime) [:output :universes 1]) 0))))
            (apply-at! runtime :command {:op :param/set :instance-id id :param :progress :value 0.4} 250)
            (is (= [0.1] (:result (get-window))))
            (is (= (get-window) @(:signal lease)))
            (is (nil? (get (show/snapshot runtime) :keyframes)))
            (let [source (show/export-show-source runtime)
                  saved (persistence/read-string source)]
              (is (map? (get-in saved [:model :instances id :tolglow.effects.keyframes/saved])))
              (is (= :accepted (:status (apply-at! runtime :import {:source source} 275))))
              (let [id' (first (keys (get-in @(:state runtime) [:model :instances])))]
                (is (not= id' id))
                (is (= [0.1] (:result (inspect runtime id'))))
                (show/tick! runtime 300)
                (is (= [0.1] (:result (inspect runtime id'))))
                (is (= [0.0] (:origin (inspect runtime id'))))
                (is (= [0.25] (:target (inspect runtime id'))))))
            (finally ((:close! lease)))))
        (finally (show/stop! runtime))))))

(deftest registry-refuses-discrete-keyframes-and-unknown-progress-parameters
  (doseq [invalid [(assoc definition :outputs {:strobe 1.0})
                   (assoc-in definition [:keyframes :progress :param] :bad)
                   (assoc-in definition [:keyframes :interval] {:beats 0})]]
    (is (seq (registry/validate-definition :effects invalid)))))

(deftest a-modulated-param-controls-the-current-frame-without-changing-the-endpoints
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})]
    (try
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :sampled} 0))
            options {:id :random-progress :seed 42 :every {:beats 1/4} :params [:progress]
                     :select {:instance-ids [id]} :ranges {:progress [0.1 0.9]}}
            initial (inspect runtime id)]
        (is (= :accepted (:status (apply-at! runtime :modulation-start options 100))))
        (doseq [t [125 250 375]]
          (show/tick! runtime t)
          (let [sample (get-in (show/snapshot runtime) [:modulation :overrides id :progress])
                window (inspect runtime id)]
            (is (<= 0.1 sample 0.9))
            (is (== sample (:progress window)))
            (is (= (:target-clock initial) (:target-clock window)))
            (is (= [(* 0.25 sample)] (:result window))))))
      (finally (show/stop! runtime)))))
