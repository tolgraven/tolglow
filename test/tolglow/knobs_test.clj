(ns tolglow.knobs-test
  "Linked knob data, validation and resolution (#666) without a running show."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.knobs :as knobs]))

(def ^:private effect
  {:id :fx :version 1
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}
            :count {:type :integer :default 1 :bounds [1 8]}
            :tint {:type :color :default [0.0 0.0 0.0]}
            :mode {:type :enum :values [:a :b :c :d] :default :a}
            :strobe? {:type :boolean :default false}
            :rate {:type :number :default 1 :bounds [1/8 8] :semantic/type :beat-ratio}
            :span {:type :range :default [0.0 1.0] :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- model []
  (control/new-show {:show-id :knobs :transport {:playing? false :beat 0.0 :bpm 120.0}
                     :registry {:effects {:fx effect} :cues {:fx {:id :fx :effect-id :fx :version 1}}}
                     :rig {:fixtures []}}))

(defn- apply! [model command]
  (control/apply-domain-command model (merge {:command-id (str (random-uuid)) :show-id :knobs :epoch (:epoch model)
                                              :source {:adapter :repl :session-id "t"}} command)
                                knobs/apply-operation))

(defn- param [p & {:as more}] (merge {:id (name p) :destination {:kind :param :cue-id :fx :param p}} more))

(defn- schema [p] (get-in effect [:params p]))

(deftest link-validation-refuses-unknown-and-incompatible-destinations
  (let [m (model)
        refused (fn [link] (let [{:keys [ack]} (apply! m {:op :knob/create :knob-id :k :name "K" :links [link]})]
                             [(:status ack) (:message ack)]))]
    (is (= :accepted (get-in (apply! m {:op :knob/create :knob-id :k :name "K" :links [(param :level :range [0 1])]})
                             [:ack :status]))
        "a cue that is not running yet can be linked; it resolves once launched")
    (is (= [:rejected "The cue :nope no longer exists"]
           (refused {:id "x" :destination {:kind :param :cue-id :nope :param :level} :range [0 1]})))
    (is (= [:rejected "The effect has no parameter :gone"] (refused (param :gone :range [0 1]))))
    (is (= :rejected (first (refused (param :level :range [0 2])))) "endpoints outside the hard bounds")
    (is (= [:rejected "Musical ratio steps need a beat-ratio destination"]
           (refused (param :level :range [0.5 1] :quantize {:ratios :preferred}))))
    (is (= [:rejected "A knob cannot drive a range destination yet"] (refused (param :span :range [[0 1] [0 1]]))))
    (is (= :rejected (first (refused (param :level :range [0 1] :curve :wobble)))))
    (is (= :rejected (first (refused (param :level :range [0 1] :input [0.5 0.5])))))
    (is (= [:rejected "Choose your own show variable; runtime variables are reserved"]
           (refused {:id "v" :destination {:kind :variable :variable :audio/energy} :range [0 1]})))
    (is (= [:rejected "Bind a waveform to that parameter on a running launch first"]
           (refused {:id "w" :destination {:kind :lfo :cue-id :fx :param :level :input :duty} :range [0 1]})))
    (testing "duplicate knob IDs and link IDs"
      (let [m (:model (apply! m {:op :knob/create :knob-id :k :name "K"}))]
        (is (= "A knob with that ID already exists" (get-in (apply! m {:op :knob/create :knob-id :k :name "K"}) [:ack :message])))
        (is (= :rejected (get-in (apply! m {:op :knob/create :knob-id :j :name "J"
                                            :links [(param :level :range [0 1]) (param :level :range [1 0])]})
                                 [:ack :status])))))))

(deftest resolution-covers-every-supported-type
  (testing "number with input window, curve and direction"
    (let [link (param :level :range [0.2 0.6] :input [0.5 1.0] :direction :down)]
      (is (= 0.6 (knobs/resolve-value link (schema :level) 0.0)))
      (is (= 0.6 (knobs/resolve-value link (schema :level) 0.5)))
      (is (< (Math/abs (- 0.4 (knobs/resolve-value link (schema :level) 0.75))) 1e-12))
      (is (= 0.2 (knobs/resolve-value link (schema :level) 1.0)))))
  (testing "integers round and stay inside bounds"
    (is (= [1 3 5 8] (mapv #(knobs/resolve-value (param :count :range [1 8]) (schema :count) %) [0 0.25 0.6 1]))))
  (testing "colours interpolate component-wise"
    (is (= [0.5 0.25 0.0] (knobs/resolve-value (param :tint :range [[0.0 0.0 0.0] [1.0 0.5 0.0]]) (schema :tint) 0.5))))
  (testing "enums step through declared values between the endpoints, in endpoint order"
    (is (= [:b :b :c :d :d] (mapv #(knobs/resolve-value (param :mode :range [:b :d]) (schema :mode) %) [0 0.33 1/3 0.99 1])))
    (is (= [:d :c :b] (mapv #(knobs/resolve-value (param :mode :range [:d :b]) (schema :mode) %) [0 0.5 1]))))
  (testing "booleans switch at the halfway boundary"
    (is (= [false false true true] (mapv #(knobs/resolve-value (param :strobe? :range [false true]) (schema :strobe?) %)
                                         [0 0.4999 0.5 1]))))
  (testing "free ratios use denominators up to 16 and stay exact"
    (let [link (param :rate :range [1 2] :quantize {:ratios :free})
          values (mapv #(knobs/resolve-value link (schema :rate) %) [0 0.5 1])]
      (is (= [1 2] [(first values) (last values)]))
      (is (ratio? (second values)))
      (is (<= (denominator (second values)) 16))))
  (testing "preferred ratios within the destination bounds; exact boundaries select the upper step"
    (let [link (param :rate :range [1/8 8] :quantize {:ratios :preferred})
          steps (count (filter #(<= 1/8 % 8) (distinct (for [n [1 2 3 4 6 8] d [1 2 3 4 6 8]] (/ n d)))))]
      (is (= 1/8 (knobs/resolve-value link (schema :rate) 0)))
      (is (= 8 (knobs/resolve-value link (schema :rate) 1)))
      (is (= steps (:steps (knobs/link-view (model) {:id :k :value 0.5} link {}))))
      (is (= 1 (knobs/step-index 1/2 2)))
      (is (= 0 (knobs/step-index 0.4999 2)))
      (is (= 3 (knobs/step-index (/ 3.0 7) 7)) "a decimal boundary behaves like its exact fraction")))
  (testing ":direction :down over [a b] steps exactly like :up over [b a], boundaries included"
    (let [period {:type :number :bounds [1/64 64] :semantic/type :beat-ratio}
          reversed (param :rate :range [2 1/2] :quantize {:ratios :preferred})
          down (param :rate :range [1/2 2] :direction :down :quantize {:ratios :preferred})
          positions (sort (distinct (concat (map #(/ % 7.0) (range 8)) (map #(- (/ % 7.0) 1e-6) (range 1 8))
                                            (map #(+ (/ % 7.0) 1e-6) (range 7)) (map #(/ % 100.0) (range 101)))))]
      (doseq [k positions]
        (is (= (knobs/resolve-value reversed period k) (knobs/resolve-value down period k)) (str "knob " k)))
      (is (= 3/2 (knobs/resolve-value down period (/ 1 7.0))) "a boundary selects the step above it")
      (is (= 2 (knobs/resolve-value down period (- (/ 1 7.0) 1e-6))))
      (testing "booleans and enums too"
        (is (= [false true true] (mapv #(knobs/resolve-value (param :strobe? :range [true false] :direction :down)
                                                            (schema :strobe?) %) [0.4999 0.5 1])))
        (is (= (mapv #(knobs/resolve-value (param :mode :range [:d :b]) (schema :mode) %) [0 1/3 2/3 1])
               (mapv #(knobs/resolve-value (param :mode :range [:b :d] :direction :down) (schema :mode) %) [0 1/3 2/3 1]))))))
  (testing "display text keeps ratios exact and adds units"
    (is (= {:value 3/2 :text "3/2 beats" :unit :beats :ratio [3 2]}
           (knobs/display 3/2 knobs/period-schema nil)))
    (is (= {:value 0.25 :text "0.250 dmx" :unit :dmx} (knobs/display 0.25 {:type :number} :dmx)))))

(deftest knob-commands-edit-links-and-values
  (let [m (:model (apply! (model) {:op :knob/create :knob-id :k :name "K" :links [(param :level :range [0 1])]}))
        m (:model (apply! m {:op :knob/link :knob-id :k :link (param :level :range [1 0])}))
        m (:model (apply! m {:op :knob/link :knob-id :k :link (param :count :range [1 8])}))
        m (:model (apply! m {:op :knob/rename :knob-id :k :name "Look"}))
        {:keys [ack] m :model} (apply! m {:op :knob/set :knob-id :k :value 0.25})]
    (is (= [[1 0] [1 8]] (mapv :range (get-in m [:knobs :knobs :k :links]))) "linking an existing ID edits it in place")
    (is (= "Look" (get-in m [:knobs :knobs :k :name])))
    (is (= [{:link-id "level" :status :skipped :reason :not-running}
            {:link-id "count" :status :skipped :reason :not-running}] (:links ack))
        "links to a cue without a running launch are skipped, not failed")
    (is (= 1.0 (get-in (apply! m {:op :knob/nudge :knob-id :k :delta 2}) [:model :knobs :knobs :k :value])))
    (is (= 0.0 (get-in (apply! m {:op :knob/nudge :knob-id :k :delta -2}) [:model :knobs :knobs :k :value])))
    (is (= :rejected (get-in (apply! m {:op :knob/set :knob-id :k :value 1.5}) [:ack :status])))
    (is (= ["count"] (mapv :id (get-in (apply! m {:op :knob/unlink :knob-id :k :link-id "level"}) [:model :knobs :knobs :k :links]))))
    (is (= :rejected (get-in (apply! m {:op :knob/unlink :knob-id :k :link-id "nope"}) [:ack :status])))
    (is (= {} (get-in (apply! m {:op :knob/remove :knob-id :k}) [:model :knobs :knobs])))
    (testing "saved state validation"
      (is (= (:knobs m) (knobs/validate-state (:knobs m))))
      (is (thrown? Exception (knobs/validate-state (assoc-in (:knobs m) [:knobs :k :value] 2))))
      (is (thrown? Exception (knobs/validate-state (assoc-in (:knobs m) [:knobs :k :links 0 :range] :x))))
      (is (thrown? Exception (knobs/validate-state (assoc (:knobs m) :schema-version 9)))))))
