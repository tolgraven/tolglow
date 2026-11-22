(ns tolglow.native-composition-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as control]
            [tolglow.composition :as composition]
            [tolglow.persistence :as persistence]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(def ratio-step {:attribute :beat-ratio :value {:op :divide :args [1 3]}})
(def compound
  {:id :combine :mode :macro :opacity 0.5
   :slots [{:cue-id :base}
           {:cue-id :tint
            :transforms [ratio-step
                         {:attribute :color :value {:op :rotate-hue
                                                    :args [{:op :previous :attribute :color}
                                                           {:op :field :attribute :beat-ratio}]}}
                         {:attribute :aim :value {:op :add :args [{:op :previous :attribute :aim}
                                                                  {:op :multiply :args [[3 0 0] {:op :field :attribute :beat-ratio}]}]}}]}]})

(defn- scene []
  (let [fixture {:id :head :universe 1 :address 1
                 :channels {:red 1 :green 2 :blue 3 :pan [4 5] :tilt [6 7] :focus 8}}
        defs [[:under -1 {:color [0 0 1] :aim [0 0 3]}]
              [:base 10 {:color [1 0 0] :aim [-2 1 5]}]
              [:tint 2 {:focus 0.6}]]
        model (control/new-show {:show-id "layer" :rig {:fixtures [fixture]}
                                 :transport {:beat 0 :bpm 120 :playing? true}})]
    (reduce (fn [m [id priority outputs]]
              (-> m
                  (assoc-in [:registry :effects id] {:id id :version 1 :params {} :targets [:head] :outputs outputs})
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority priority}))) model defs)))

(defn- submit-at! [rt t type data]
  (let [p (show/submit! rt type data)]
    (show/tick! rt t)
    (is (= :accepted (:status @p)) (str @p))))

(deftest compound-native-program-reaches-real-captured-channels
  (doseq [batch? [true false]]
    (let [rt (show/create-runtime {:model (scene) :batch? batch?})]
      (try
        (submit-at! rt 0 :command {:op :cue/launch :cue-id :under})
        (submit-at! rt 25 :composition compound)
        (submit-at! rt 50 :composition-run {:id :combine})
        (let [{:keys [model output status]} @(:state rt)
              owner (get-in model [:composition-runs :combine :owner-id])
              members (sort-by :member (keep #(get-in % [:source :native-layer]) (vals (:instances model))))
              target (get-in (show/snapshot rt) [:output :values :head])]
          (is (nil? (:error status)))
          (is (= [0 1] (mapv :member members)))
          (is (= [owner owner] (mapv :id members)))
          (is (= [(double -0.5) 0.5 4.0] (mapv double (:aim target))))
          (is (= [0 128 128] (mapv #(bit-and 255 %) (take 3 (get-in output [:universes 1])))))
          (let [restored (persistence/thaw model (persistence/read-string (persistence/write-string model)))
                new-owner (get-in restored [:composition-runs :combine :owner-id])
                layers (keep #(get-in % [:source :native-layer]) (vals (:instances restored)))]
            (is (not= owner new-owner))
            (is (= #{new-owner} (set (map :id layers))))
            (is (= (:transforms (second members)) (:transforms (second (sort-by :member layers)))))))
        (finally (show/stop! rt))))))

(deftest composition-validates-transforms-and-opacity-before-mutating-show
  (doseq [definition [(assoc compound :opacity 1.01)
                      (assoc-in compound [:slots 1 :transforms 0 :value :op] :typo)]]
    (is (thrown? clojure.lang.ExceptionInfo (composition/save (scene) definition)))))

(deftest sequence-launches-use-captured-slot-order-and-transforms
  (let [model (-> (scene)
                  (composition/save (assoc compound :mode :sequence :timebase :beats))
                  (composition/start :combine 0) (composition/advance 0))
        owner (get-in model [:composition-runs :combine :owner-id])
        next-step (-> model
                      (composition/save (assoc-in (assoc compound :mode :sequence) [:slots 1 :transforms] []))
                      (assoc-in [:transport :beat] 1)
                      (composition/advance 500))
        layer (some #(when (= (:cue-id %) :tint) (get-in % [:source :native-layer]))
                    (vals (:instances next-step)))]
    (is (= {:id owner :member 1 :opacity 0.5 :transforms (get-in compound [:slots 1 :transforms])} layer))))

(defn- rotate-hue [amount]
  {:attribute :color
   :value {:op :rotate-hue :args [{:op :field :attribute :color} amount]}})

(deftest reusable-modifier-runs-in-standalone-and-grouped-cues
  (doseq [batch? [true false] grouped? [true false]]
    (let [rt (show/create-runtime {:model (scene) :batch? batch?})
          modifier (cond-> {:id :filter :version 1
                            :layer {:transforms [(rotate-hue 1/3)]}}
                     grouped? (assoc-in [:layer :targets] :previous)
                     (not grouped?) (assoc :targets [:head]))
          publish! (fn [t definition] (submit-at! rt t :definition {:kind :effects :definition definition}))
          bytes! #(mapv (fn [b] (bit-and 255 b)) (take 3 (get-in @(:state rt) [:output :universes 1])))]
      (try
        (publish! 0 modifier)
        (submit-at! rt 25 :definition {:kind :cues :definition {:id :filter :effect-id :filter :priority 20}})
        (if grouped?
          (do (submit-at! rt 50 :composition {:id :filtered :mode :macro
                                               :slots [{:cue-id :base}
                                                       {:cue-id :filter :launch {:reload-policy :preserve-state}
                                                        :transforms [(rotate-hue 1/3)]}]})
              (submit-at! rt 75 :composition-run {:id :filtered}))
          (do (submit-at! rt 50 :command {:op :cue/launch :cue-id :base})
              (submit-at! rt 75 :command {:op :cue/launch :cue-id :filter :launch {:reload-policy :preserve-state}})))
        (is (= (if grouped? [0 0 255] [0 255 0]) (bytes!)))
        (let [view (show/snapshot rt)
              registered (some #(when (= :filter (:id %)) %) (:effects view))
              live (some #(when (= :filter (:cue-id %)) %) (:instances view))
              color-group (some #(when (= :color (:id %)) %) (:groups view))]
          (is (= [:color] (:categories registered) (:categories live)))
          (is (some #{(:id live)} (:instance-ids color-group))))
        (publish! 100 (-> modifier (assoc :version 2)
                          (assoc-in [:layer :transforms] [(rotate-hue 2/3)])))
        (is (= (if grouped? [255 0 0] [0 0 255]) (bytes!)))
        (finally (show/stop! rt))))))

(deftest reusable-modifiers-reject-malformed-programs-on-publication
  (doseq [layer [{:transforms []} {:transforms 4}
                 {:targets :other :transforms [ratio-step]}
                 {:transforms [{:attribute :color :value {:op :unknown}}]}]]
    (is (seq (registry/validate-definition :effects {:id :filter :version 1 :layer layer})))))

(deftest restored-release-tails-remain-in-their-layer-after-the-execution-stops
  (let [started (-> (scene) (composition/save compound) (composition/start :combine 0))
        owner (get-in started [:composition-runs :combine :owner-id])
        stopped (composition/stop started :combine 10)
        restored (persistence/thaw stopped (persistence/read-string (persistence/write-string stopped)))
        tails (vals (:instances restored))
        layers (sort-by :member (map #(get-in % [:source :native-layer]) tails))]
    (is (empty? (:composition-runs restored)))
    (is (= [:releasing :releasing] (mapv :phase tails)))
    (is (= [0 1] (mapv :member layers)))
    (is (apply = (map :id layers)))
    (is (not= owner (:id (first layers))))
    (is (= (:transforms (second layers)) (get-in compound [:slots 1 :transforms])))))
