(ns tolglow.knob-typed-destinations-test
  "Generated macros for every parameter type (#667): numbers, integers, grid
  numbers, colours, enums, booleans, vectors and beat ratios. The generator
  drafts each within its schema, and a knob built from those drafts is swept
  through the real show owner: every committed value satisfies its schema,
  stepped destinations move monotonically, and a knob dithering on a boundary
  never makes a destination chatter."
  (:require [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.agent :as agent]
            [tolglow.knob-candidates :as knob-candidates]
            [tolglow.knob-candidates.generate :as generate]
            [tolglow.knobs :as knobs]
            [tolglow.show :as show]))

(def ^:private effect
  {:id :typed :version 1
   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}
            :bands {:type :integer :default 3 :bounds [1 8]}
            :zoom {:type :number :default 1.0 :bounds [0.0 4.0] :step 0.25}
            :tint {:type :color :default [0.2 0.4 1.0]}
            :shape {:type :enum :values [:dot :bar :ring :star :wave] :default :bar}
            :strobe {:type :boolean :default false}
            :centre {:type :vector :size 3 :default [0.0 1.0 2.0] :bounds [-4.0 4.0]
                     :semantic/type :position :unit :m}
            :extent {:type :vector :size 3 :default [1.0 0.5 2.0] :bounds [0.0 6.0] :step 0.5 :unit :m}
            :beats {:type :number :default 1 :bounds [1/8 8] :unit :beats :semantic/type :beat-ratio}}
   :outputs {:dimmer {:op :param :key :level}}})

(def ^:private schemas (:params effect))

(defn- base-model []
  (control/new-show {:show-id :typed :transport {:playing? true :beat 0.0 :bpm 120.0}
                     :registry {:effects {:typed effect} :cues {:typed {:id :typed :effect-id :typed :version 1}}}
                     :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(defn- running-model []
  (let [{:keys [model ack]} (control/apply-command (base-model)
                                                   {:op :cue/launch :cue-id :typed :launch {:mode :persistent}
                                                    :command-id "launch" :show-id :typed :epoch 0
                                                    :source {:adapter :test} :time-ms 0})]
    (assert (= :accepted (:status ack)) (pr-str ack))
    model))

(def ^:private studio {:adapter :studio :session-id "typed-test"})

(defn- submit! [rt time payload]
  (let [reply (show/submit! rt :command (merge {:source studio} payload))]
    (show/tick! rt time)
    (deref reply 5000 ::missing)))

(defn- param-of [link] (get-in link [:destination :param]))

(defn- drafts
  "Every link the generator drafts over 300 seeds, by destination parameter."
  []
  (let [eligibility (generate/eligibility (running-model))]
    (group-by param-of (for [seed (range 300)
                             :let [c (generate/generate eligibility nil {:seed seed :size [4 8]})]
                             :when (= :ready (:status c))
                             link (:links c)]
                         link))))

(defn- on-grid? [step x] (< (Math/abs (- (/ (double x) step) (Math/rint (/ (double x) step)))) 1e-9))

(deftest every-parameter-type-is-generated-within-its-schema
  (let [eligibility (generate/eligibility (running-model))
        by-param (drafts)]
    (testing "every type is eligible with a role"
      (is (= {:level :intensity :bands :spread :zoom :spread :tint :colour :shape :mode :strobe :toggle
              :centre :position :extent :spread :beats :period}
             (into {} (map (juxt param-of :role)) (:eligible eligibility)))))
    (testing "some seed drafts every destination"
      (is (= (set (keys schemas)) (set (keys by-param)))))
    (doseq [[param links] by-param
            :let [schema (schemas param)]
            {[a b] :range :as link} (distinct links)]
      (is (empty? (knobs/link-errors link)) (pr-str link))
      (is (nil? (knobs/compatibility-error link schema)) (pr-str link))
      (is (and (params/valid-value? schema a) (params/valid-value? schema b)) "both endpoints satisfy the schema")
      (is (generate/playable? link schema))
      (when (knobs/stepped? link schema)
        (is (< 0 (:hysteresis link) 0.0501) (str "a stepped link holds its steps: " (pr-str link))))
      (case param
        :shape (let [values (:values schema) i (.indexOf ^java.util.List values a) j (.indexOf ^java.util.List values b)]
                 (is (< i j) "a choice steps up through its declared values")
                 (is (<= 2 (inc (- j i)) 5))
                 (is (= :linear (:curve link)) "each value gets an equal share of the knob"))
        :strobe (do (is (= [false true] (:range link)))
                    (is (some #{(:input link)} [[0 1] [0.5 1] [0.25 1]]) "a seeded switching point"))
        :zoom (do (is (= {:step 0.25} (:quantize link)))
                  (is (and (on-grid? 0.25 a) (on-grid? 0.25 b)) "endpoints on the schema's grid"))
        :extent (do (is (= {:step 0.5} (:quantize link)))
                    (is (every? #(on-grid? 0.5 %) (concat a b)) "every component on the grid"))
        :centre (do (is (nil? (:quantize link)))
                    (is (nil? (:hysteresis link)) "a continuous curve through space needs no hysteresis"))
        :bands (is (= :integer (:quantize link)))
        :beats (is (= {:ratios :preferred} (:quantize link)))
        nil))
    (testing "explanations name choices, switches and held steps"
      (let [whys (map :why (for [seed (range 300)
                                 :let [c (generate/generate eligibility nil {:seed seed :size [4 8]})]
                                 e (:explain c)]
                             e))]
        (is (some #(re-find #"^Choice \w+ → \w+" %) whys))
        (is (some #(re-find #"^Switch off → on .* switching at knob 0\.(5|75|625)" %) whys))
        (is (some #(re-find #"each step holds until the knob is 0\.\d+ past its boundary" %) whys))))))

(deftest a-type-selection-restricts-the-destinations-and-is-recorded-in-the-recipe
  (let [eligibility (generate/eligibility (running-model) {:types #{:number :enum :boolean}})
        c (generate/generate eligibility nil {:seed 3})]
    (is (= #{:level :zoom :shape :strobe :beats} (set (map param-of (:eligible eligibility)))))
    (is (= #{:type-not-selected} (set (map :reason (:excluded eligibility)))))
    (is (= :ready (:status c)))
    (is (= [:boolean :enum :number] (get-in c [:recipe :types])))
    (is (every? #(#{:number :enum :boolean} (:type (schemas (param-of %)))) (:links c)))))

(deftest older-recipes-replay-without-the-new-types
  (let [eligibility (generate/eligibility (running-model))]
    (doseq [version [1 2 3] seed (range 40)
            :let [c (generate/generate eligibility nil {:seed seed :generator version})]]
      (is (not-any? #(#{:shape :strobe :centre :extent} (param-of %)) (:links c)))
      (is (not-any? :hysteresis (:links c)) "no hysteresis before version 4"))))

;; ---------------------------------------------------------------------------
;; Sweeping a knob through the real owner

(defn- one-link-per-type
  "The first drafted link for every destination, with a distinct ID."
  [by-param]
  (vec (for [param (sort (keys by-param))] (assoc (first (sort-by pr-str (by-param param))) :id (name param)))))

(defn- committed [rt]
  (let [model (:model @(:state rt))
        instance (some #(when (= :typed (:cue-id %)) %) (vals (:instances model)))]
    (select-keys (:params instance) (keys schemas))))

(defn- turn! [rt t value]
  (let [ack (submit! rt t {:op :knob/set :knob-id "typed" :value (double value)})]
    (assert (= :accepted (:status ack)) (pr-str ack))
    (committed rt)))

(defn- order-of
  "Position of a stepped value on its link, as the sweep checks monotonicity."
  [link value]
  (let [schema (schemas (param-of link))]
    (case (:type schema)
      :enum (.indexOf ^java.util.List (:values schema) value)
      :boolean (if value 1 0)
      :vector (vec value)
      (double value))))

(defn- changes [xs] (count (filter (fn [[a b]] (not= a b)) (partition 2 1 xs))))

(defn- monotone? [link xs ascending?]
  (let [schema (schemas (param-of link))
        ords (map #(order-of link %) xs)
        ok? (fn [cmp] (every? (fn [[a b]] (if (vector? a) (every? true? (map #(or (= %1 %2) (cmp %1 %2)) a b))
                                              (or (= a b) (cmp a b))))
                              (partition 2 1 ords)))]
    (if (= :vector (:type schema))
      ;; Each component of a straight path moves one way; which way is per component.
      (every? (fn [i] (let [c (map #(nth % i) xs)] (or (apply <= c) (apply >= c)))) (range (count (first xs))))
      (ok? (if (= ascending? (= :up (:direction link :up))) < >)))))

(defn- boundaries
  "Knob positions where a stepped link's plain resolution changes step, from a fine sweep."
  [link schema]
  (let [xs (map #(/ % 2048.0) (range 2049))]
    (keep (fn [[a b]] (when (not= (knobs/resolve-value link schema a) (knobs/resolve-value link schema b)) b))
          (partition 2 1 xs))))

(deftest a-knob-sweep-writes-only-valid-values-and-never-chatters
  (let [links (one-link-per-type (drafts))
        rt (show/create-runtime {:model (running-model) :workspace-options nil})]
    (try
      (is (= (set (keys schemas)) (set (map param-of links))) "one link for every parameter type")
      (is (= :accepted (:status (submit! rt 10 {:op :knob/create :knob-id "typed" :name "Typed" :links links}))))
      (let [up (mapv #(turn! rt (+ 100 %) (/ % 256.0)) (range 257))
            down (mapv #(turn! rt (+ 400 %) (- 1.0 (/ % 256.0))) (range 257))]
        (testing "every committed value satisfies its schema"
          (doseq [values (concat up down) [param value] values]
            (is (params/valid-value? (schemas param) value) (str param " " (pr-str value)))))
        (testing "stepped destinations move one way through their steps on each sweep"
          (doseq [link links
                  :let [schema (schemas (param-of link))]
                  :when (knobs/stepped? link schema)]
            (is (monotone? link (map (param-of link) up) true) (str (param-of link) " rising"))
            (is (monotone? link (map (param-of link) down) false) (str (param-of link) " falling"))
            (when-not (= :vector (:type schema))
              (is (= (dec (knobs/step-count link schema)) (changes (map (param-of link) up)))
                  (str (param-of link) " visits each step once rising")))))
        (testing "the knob's ends reach the link's endpoints"
          (doseq [link links :let [schema (schemas (param-of link))]]
            (is (= (knobs/resolve-value link schema 0.0) (get (first up) (param-of link))))
            (is (= (knobs/resolve-value link schema 1.0) (get (last up) (param-of link)))))))
      (testing "a knob dithering on any boundary inside the hysteresis never chatters"
        (doseq [link links
                :let [schema (schemas (param-of link))
                      h (:hysteresis link)]
                :when (knobs/stepped? link schema)
                b (boundaries link schema)
                [settle wobble] [[(- b (* 2 h)) [(+ b (* 0.45 h)) (- b (* 0.45 h))]]
                                 [(+ b (* 2 h)) [(- b (* 0.45 h)) (+ b (* 0.45 h))]]]
                :let [settle (max 0.0 (min 1.0 settle))
                      start (get (turn! rt 900 settle) (param-of link))
                      seen (for [i (range 12)] (get (turn! rt (+ 901 i) (nth wobble (mod i 2))) (param-of link)))]]
          ;; Moving to the boundary may cross a neighbouring step (another component of
          ;; a vector, or a narrow zone under a curve) first; after that it holds.
          (is (= 1 (count (distinct (drop 2 seen))))
              (str (param-of link) " at boundary " b " from " settle ": " (pr-str (distinct seen))))
          (when (and (not= :vector (:type schema)) (= :linear (:curve link :linear)))
            (is (every? #(= start %) seen)
                (str (param-of link) " holds the step it came from at boundary " b)))))
      (testing "continuous destinations move in proportion to the knob"
        (doseq [link links
                :let [schema (schemas (param-of link))]
                :when (not (knobs/stepped? link schema))
                :let [[a b] (:range link)
                      span (if (vector? a) (reduce max (map #(Math/abs (- (double %1) (double %2))) a b))
                               (Math/abs (- (double a) (double b))))
                      before (get (turn! rt 950 0.5) (param-of link))
                      after (get (turn! rt 951 0.502) (param-of link))
                      moved (if (vector? before) (reduce max (map #(Math/abs (- (double %1) (double %2))) before after))
                                (Math/abs (- (double before) (double after))))]]
          ;; No easing curve is steeper than 2, and a window at least halves the knob.
          (is (<= moved (* 4 span 0.002 1.0001)) (str (param-of link) " moved " moved))))
      (finally (show/stop! rt)))))

(deftest hysteresis-latches-through-the-knob-and-a-hand-edit-still-wins-until-it-moves
  (let [link {:id "shape" :destination {:kind :param :cue-id :typed :param :shape} :range [:dot :wave]
              :hysteresis 0.05}
        rt (show/create-runtime {:model (running-model) :workspace-options nil})]
    (try
      (is (= :accepted (:status (submit! rt 10 {:op :knob/create :knob-id "typed" :name "Typed" :links [link]}))))
      (testing "five zones of 0.2; a step holds until the knob is 0.05 past the boundary"
        (is (= [:dot :bar :bar :bar :ring :ring :bar :bar :dot]
               (mapv #(:shape (turn! rt (+ 20 %2) %1)) [0.0 0.25 0.22 0.17 0.46 0.35 0.34 0.16 0.14] (range)))))
      (testing "the projection shows the held step and the knob keeps it in a full backup"
        (let [view (some #(when (= "typed" (:id %)) %) (:knobs (:knobs (show/snapshot rt))))]
          (is (= {"shape" :dot} (get-in @(:state rt) [:model :knobs :knobs "typed" :latched])))
          (is (= "dot" (get-in view [:links 0 :resolved :text])))
          (is (= 0 (get-in view [:links 0 :step])))))
      (testing "a hand edit holds until the knob moves, as for any destination"
        (let [id (some #(when (= :typed (:cue-id %)) (:id %)) (vals (:instances (:model @(:state rt)))))]
          (is (= :accepted (:status (submit! rt 40 {:op :param/set :instance-id id :param :shape :value :star}))))
          (is (= :star (:shape (committed rt))))
          (is (= :bar (:shape (turn! rt 41 0.3))))))
      (testing "replacing the link forgets its latch"
        (is (= :accepted (:status (submit! rt 50 {:op :knob/link :knob-id "typed" :link (assoc link :range [:bar :star])}))))
        (is (nil? (get-in @(:state rt) [:model :knobs :knobs "typed" :latched "shape"]))))
      (finally (show/stop! rt)))))

(deftest knob-links-check-grids-hysteresis-and-text-enum-endpoints
  (let [model (running-model)
        create (fn [link] (control/apply-domain-command model {:op :knob/create :knob-id "k" :name "K" :links [link]
                                                               :command-id (str (random-uuid)) :show-id :typed :epoch 0
                                                               :source {:adapter :test}}
                                                        knobs/apply-operation))
        link (fn [param & {:as more}] (merge {:id (name param) :destination {:kind :param :cue-id :typed :param param}} more))
        refused (fn [l] (get-in (create l) [:ack :message]))]
    (is (= "Hysteresis needs a stepped destination: enum, boolean, integer, beat ratios or a {:step size} grid"
           (refused (link :level :range [0.0 1.0] :hysteresis 0.05))))
    (is (re-find #"Hysteresis is a knob distance" (refused (link :shape :range [:dot :wave] :hysteresis 0.3))))
    (is (= "An integer destination steps by whole numbers" (refused (link :bands :range [1 8] :quantize {:step 0.5}))))
    (is (= "Grid steps need a number, integer or vector destination"
           (refused (link :shape :range [:dot :wave] :quantize {:step 1}))))
    (is (= [0.0 0.25 0.5 0.75 1.0]
           (mapv #(knobs/resolve-value (link :zoom :range [0.0 1.0] :quantize {:step 0.25}) (schemas :zoom) %)
                 [0.0 0.2 0.4 0.6 0.99]))
        "a grid number steps through equal knob zones")
    (is (= [:ring :wave] (get-in (create (link :shape :range ["ring" "wave"])) [:model :knobs :knobs "k" :links 0 :range]))
        "a JSON surface names enum values as text")
    (is (= [[0.0 0.0 0.0] [0.5 0.5 0.5] [1.0 1.0 1.0]]
           (mapv #(knobs/resolve-value (link :extent :range [[0.0 0.0 0.0] [1.0 1.0 1.0]] :quantize {:step 0.5})
                                       (schemas :extent) %)
                 [0.0 0.5 1.0])))))

(deftest the-knob-agent-tool-proposes-auditions-rerolls-keeps-and-turns-typed-mappings
  (let [tool (some #(when (= "knob" (:name %)) %) agent/default-tools)
        model (running-model)]
    (is tool)
    (is (empty? (agent/validate-arguments (:parameters tool) {:op "propose" :types ["choice" "switch" "number"] :seed 3})))
    (is (seq (agent/validate-arguments (:parameters tool) {:op "propose" :types ["gradient"]})))
    (testing "each op maps to the shared command"
      (is (= {:op :knob-candidate/generate :seed 3 :types ["choice" "switch"]}
             (knob-candidates/knob-tool-command model {:op :propose :seed 3 :types ["choice" "switch"]})))
      (is (= {:op :knob-candidate/audition :value 0.4} (knob-candidates/knob-tool-command model {:op :audition :value 0.4})))
      (is (= {:op :knob-candidate/reroll} (knob-candidates/knob-tool-command model {:op "reroll"})))
      (is (= :knob-candidate/link (:op (knob-candidates/knob-tool-command model {:op :link :link {:id "x"}}))))
      (is (= {:op :knob/link :knob-id "k" :link {:id "x" :direction :down}}
             (knob-candidates/knob-tool-command model {:op :link :knob-id "k" :link {:id "x" :direction "down"}})))
      (is (= {:op :knob/nudge :knob-id "k" :delta 0.1} (knob-candidates/knob-tool-command model {:op :nudge :knob-id "k" :delta 0.1}))))
    (testing "through the show's agent dispatcher"
      (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
            call! (fn [time args]
                    (let [result (future (show/dispatch-tool! rt "knob" args {:command-id (str "knob-" time) :run-id "agent-run"}))]
                      (loop [t time]
                        (show/tick! rt t)
                        (if (realized? result) @result (do (Thread/sleep 2) (recur (+ t 1)))))))]
        (try
          (let [proposed (call! 10 {:op "propose" :seed 5 :types ["choice" "switch" "number"]})
                candidate (get-in @(:state rt) [:model :knob-candidates :candidate])]
            (is (= [:accepted :ready] ((juxt :status :candidate-status) proposed)) (pr-str proposed))
            (is (every? #(#{:number :enum :boolean} (:type (schemas (param-of %)))) (:links candidate)))
            (is (= :accepted (:status (call! 20 {:op "audition" :value 0.7}))))
            (is (= :accepted (:status (call! 30 {:op "reroll"}))))
            (is (not= (:links candidate) (get-in @(:state rt) [:model :knob-candidates :candidate :links])))
            (let [kept (call! 40 {:op "keep" :knob-id "agent-knob" :name "Agent knob"})]
              (is (= :accepted (:status kept)) (pr-str kept)))
            (is (= :accepted (:status (call! 50 {:op "set" :knob-id "agent-knob" :value 1.0}))))
            (is (= 1.0 (get-in @(:state rt) [:model :knobs :knobs "agent-knob" :value])))
            (is (= :accepted (:status (call! 60 {:op "link" :knob-id "agent-knob"
                                                 :link {:id "strobe" :destination {:kind "param" :cue-id "typed" :param "strobe"}
                                                        :range [false true] :hysteresis 0.05}}))))
            (is (= :accepted (:status (call! 70 {:op "nudge" :knob-id "agent-knob" :delta -0.4}))))
            ;; The switch was linked after the last turn, so it holds no step yet and 0.6 is above its threshold.
            (is (= true (:strobe (committed rt))))
            (is (= :accepted (:status (call! 75 {:op "nudge" :knob-id "agent-knob" :delta -0.12}))))
            (is (= true (:strobe (committed rt))) "0.48 is inside the hysteresis below the 0.5 threshold, so it stays on")
            (is (= :accepted (:status (call! 78 {:op "nudge" :knob-id "agent-knob" :delta -0.04}))))
            (is (= false (:strobe (committed rt))) "0.44 is past it")
            (is (= :rejected (:status (call! 80 {:op "set" :knob-id "nope" :value 0.5})))))
          (finally (show/stop! rt)))))))

(deftest a-candidate-sweep-in-the-room-holds-steps-like-a-kept-knob
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil :worker-timeout-ms 60000
                                 :audition {:max-live-share ##Inf}})
        shape {:id "shape" :destination {:kind :param :cue-id :typed :param :shape} :range [:dot :wave]
               :hysteresis 0.05}
        room-shape (fn [] (some #(when (= :typed (:cue-id %)) (get-in % [:params :shape]))
                                (vals (:instances (get-in @(:scene-draft rt) [:frame :model])))))
        audition! (fn [t value]
                    (is (= :accepted (:status (submit! rt t {:op :knob-candidate/audition :value value}))))
                    (doseq [f (range 5)]
                      (show/tick! rt (+ t 1 f))
                      (is (show/await-audition rt 5000)))
                    [(room-shape) (get-in (show/snapshot rt) [:knob-candidates :candidate :links])])]
    (try
      (is (= :ready (:candidate-status (submit! rt 10 {:op :knob-candidate/generate :seed 2
                                                       :types ["choice" "number"]}))))
      (let [existing (some #(when (= :shape (param-of %)) (:id %))
                           (get-in @(:state rt) [:model :knob-candidates :candidate :links]))]
        (is (= :accepted (:status (submit! rt 20 {:op :knob-candidate/link :link (assoc shape :id (or existing "shape"))})))))
      (let [live (:shape (committed rt))
            sweep (mapv #(audition! (+ 100 (* 10 %2)) %1) [0.0 0.25 0.22 0.17 0.46 0.35 0.34 0.16 0.14] (range))]
        (is (= [:dot :bar :bar :bar :ring :ring :bar :bar :dot] (mapv first sweep))
            "the Room holds each step until the sweep is 0.05 past its boundary")
        (is (= ["dot" "bar" "bar" "bar" "ring" "ring" "bar" "bar" "dot"]
               (mapv (fn [[_ links]] (some #(when (= :shape (param-of %)) (get-in % [:resolved :text])) links)) sweep))
            "Studio, the REPL and agents read the same held step")
        (is (= live (:shape (committed rt))) "the live show is unchanged"))
      (finally (show/stop! rt)))))
