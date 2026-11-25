(ns tolglow.predictive-keyframes-test
  (:require [afterglow.reactive :as reactive]
            [afterglow.runtime.keyframes :as keyframes]
            [clojure.test :refer [deftest is]]
            [tolglow.compiler :as compiler]
            [tolglow.demo :as demo]
            [tolglow.effects.cadence :as cadence]
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

(def reactive-definition
  {:id :settling :version 1 :targets [:head-1]
   :reactive {:inputs {:level [:source ::level] :bias [:subscription [::bias]]}
              :transition {:beats 1/2}}
   :outputs {:dimmer {:op :add :args [{:op :reactive :key :level} {:op :reactive :key :bias}]}}})

(deftest an-authored-reactive-effect-uses-committed-inputs-and-holds-its-output
  (let [runtime (show/create-runtime {:workspace-options nil :adaptive-keyframes {:enabled? true}
                                     :capture-effect-samples? true})
        system (show/reactions runtime)
        level (reactive/ratom system ::level 0.125)
        bias (reactive/ratom system ::bias-source 0.125)
        bias-evaluations (atom 0)
        render-byte #(bit-and 255 (aget ^bytes (get-in @(:state runtime) [:output :universes 1]) 0))]
    (try
      (reactive/reg-sub! system ::bias (fn [_ _] (swap! bias-evaluations inc) @bias))
      (is (= :accepted (:status (apply-at! runtime :definition {:kind :effects :definition reactive-definition} 0))))
      (is (= :accepted (:status (apply-at! runtime :definition {:kind :cues :definition {:id :settling :effect-id :settling}} 0))))
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :settling} 0))
            sample #(inspect runtime id)
            evaluations #(some (fn [entry] (when (= id (:instance-id entry)) (:evaluations entry)))
                               (:effects (cadence/status runtime)))
            tick (fn [time] (show/tick! runtime time) (sample))
            read-query (reactive/acquire! system [:runtime/keyframe id :head-1 :dimmer] (constantly nil))]
        (try
          (is (nil? (sample)))
          (is (= #{[::bias]} (reactive/demanded-queries system #{::bias})))
          (is (= [0.25] (:result (tick 25))))
          (reset! level 0.75)
          (reset! bias 0.25)
          (is (= [0.25] (:result (tick 100))))
          (is (= [1.0] (:target (tick 125))))
          (is (= [0.625] (:result (tick 250))))
          (reset! level 0.125)
          (let [committed (:result (tick 275))
                retargeted (tick 300)]
            (is (= committed (:origin retargeted) (:result retargeted)))
            (is (= [0.375] (:target retargeted))))
          (let [settled (tick 550)
                held (get-in @(:state runtime) [:keyframes id :result])]
            (is (true? (:settled? settled)))
            (is (= [0.375] (:result settled)))
            (is (= 96 (render-byte)))
            (doseq [t (range 575 8075 25)] (tick t))
            (is (identical? held (get-in @(:state runtime) [:keyframes id :result])))
            (is (= 3 (evaluations)))
            (is (= 2 @bias-evaluations))
            (is (= 96 (render-byte)))
            (is (= (sample) @(:signal read-query))))
          (is (= :accepted (:status (apply-at! runtime :command {:op :instance/end :instance-id id :force? true} 8100))))
          (is (empty? (reactive/demanded-queries system #{::bias})))
          (finally ((:close! read-query)))))
      (finally (show/stop! runtime)))))

(deftest a-reactive-definition-constrains-bindings-and-expression-inputs
  (doseq [definition [(assoc reactive-definition :reactive true)
                      (assoc-in reactive-definition [:reactive :inputs :level] [:source "level"])
                      (assoc-in reactive-definition [:reactive :transition] {:ms 25})
                      (dissoc reactive-definition :targets)
                      (assoc reactive-definition :keyframes {:interval {:beats 1}})]]
    (is (seq (registry/validate-definition :effects definition))))
  (doseq [expression [{:op :reactive :key :unknown} {:op :beat} {:op :position :axis :x}
                      {:op :variable :key :unbound}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (compiler/compile-effect (assoc reactive-definition :outputs {:dimmer expression}))))))

(deftest distinct-sources-with-the-same-local-alias-preserve-exact-values
  (let [make-effect (fn [id source target]
                      {:id id :version 1 :targets [target]
                       :reactive {:inputs {:level [:source source]}}
                       :outputs {:dimmer {:op :reactive :key :level}}})
        left (make-effect :left ::left :head-1)
        right (make-effect :right ::right :head-2)
        model (-> (demo/model)
                  (update-in [:registry :effects] assoc :left left :right right)
                  (update-in [:registry :cues] assoc
                             :left {:id :left :version 1 :effect-id :left}
                             :right {:id :right :version 1 :effect-id :right}))
        runtime (show/create-runtime {:model model :workspace-options nil})
        system (show/reactions runtime)
        left-level (reactive/ratom system ::left -0.0)
        _ (reactive/ratom system ::right 0.5)]
    (try
      (let [left (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :left} 0))
            right (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :right} 0))
            value (fn [id target]
                    (let [window (get-in @(:state runtime) [:keyframes id])]
                      (first (:result (keyframes/inspect window target :dimmer)))))
            bits (fn [] (Double/doubleToRawLongBits (value left :head-1)))]
        (show/tick! runtime 25)
        (is (= Long/MIN_VALUE (bits)))
        (is (= 0.5 (value right :head-2)))
        (reset! left-level 0.0)
        (show/tick! runtime 50)
        (show/tick! runtime 75)
        (is (= 0 (bits)))
        (is (= 0.5 (value right :head-2)))
        (let [source (show/export-show-source runtime)
              saved (:model (persistence/read-string source))]
          (is (= 0.5 (get-in saved [:reactive :state :cells ::right])))
          (is (every? #(not= :tolglow.effects.reactive/input (:id %)) (get-in saved [:reactive :registrations])))
          (is (every? #(not (contains? % :tolglow.effects.keyframes/saved)) (vals (:instances saved))))
          (is (= :accepted (:status (apply-at! runtime :import {:source source} 100))))
          (show/tick! runtime 125)
          (let [restored-id (some (fn [[id instance]] (when (= :right (:effect-id instance)) id))
                                  (get-in @(:state runtime) [:model :instances]))]
            (is (not= right restored-id))
            (is (= 0.5 (value restored-id :head-2))))))
      (finally (show/stop! runtime)))))

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

(deftest adaptive-policy-observes-real-instances-and-turns-off-on-command
  (let [model (-> (model)
                  (assoc-in [:registry :effects :switch] {:id :switch :version 1 :outputs {:strobe 0.25}})
                  (assoc-in [:registry :cues :switch] {:id :switch :effect-id :switch}))
        rt (show/create-runtime {:model model :adaptive-keyframes {:enabled? true}
                                 :workspace-options nil :workers 2})]
    (try
      (let [s (apply-at! rt :command {:op :cue/launch :cue-id :sampled} 0)
            d (apply-at! rt :command {:op :cue/launch :cue-id :switch} 25)
            status (cadence/status rt)
            entries (into {} (map (juxt :instance-id identity)) (:effects status))]
        (is (= :accepted (:status s) (:status d)))
        (is (true? (:predictive? (entries (:instance-id s)))))
        (is (false? (:predictive? (entries (:instance-id d)))))
        (is (every? #(pos? (:evaluations %)) (vals entries)))
        (is (= :rejected (:status (apply-at! rt :settings {:scope :adaptive-keyframes :values {:headroom 3}} 50))))
        (is (= :accepted (:status (apply-at! rt :settings {:scope :adaptive-keyframes :values {:enabled? false}} 75))))
        (is (nil? (cadence/status rt))))
      (finally (show/stop! rt)))))
