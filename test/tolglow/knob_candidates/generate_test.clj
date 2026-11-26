(ns tolglow.knob-candidates.generate-test
  "The knob candidate generator (#667) as pure data → data: eligibility and
  its reasons, seeded determinism, hard constraints, scoring, correlated
  choices, track-feel shaping and the too-few-parameters explanation."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.knob-candidates.generate :as generate]
            [tolglow.knobs :as knobs]
            [tolglow.personalities :as personalities]))

(def ^:private effects
  {:pulse {:id :pulse :version 1
           :params {:level {:type :number :default 1.0 :bounds [0.0 1.0]}
                    :duty {:type :number :default 0.35 :bounds [0.05 1.0]}
                    :rate {:type :number :default 4.0 :bounds [0.5 8.0]}
                    :tint {:type :color :default [0.42 0.27 0.79]}
                    :seed {:type :number :default 12.0 :bounds [0.0 9999.0]}}
           :outputs {:dimmer {:op :param :key :level}}}
   :wash {:id :wash :version 1
          :params {:hue {:type :number :default 0.7 :bounds [0.0 1.0]}
                   :saturation {:type :number :default 0.9 :bounds [0.0 1.0]}
                   :level {:type :number :default 0.5 :bounds [0.0 1.0]}
                   :origin {:type :vector :size 3 :default [0.0 0.0 0.0]}
                   :mystery {:type :number :default 0.5 :bounds [0.0 1.0]}
                   :loose {:type :number :default 0.0}}
          :outputs {:dimmer {:op :param :key :level}}}
   :aim {:id :aim :version 1
         :params {:pan {:type :number :default 0.5 :bounds [0.0 1.0]}
                  :tilt {:type :number :default 0.4 :bounds [0.0 1.0]}
                  :spread {:type :number :default 10.0 :bounds [-45.0 45.0]}
                  :bars {:type :number :default 2.0 :bounds [0.25 16.0]}}
         :outputs {:pan {:op :param :key :pan}}}
   :empty {:id :empty :version 1 :params {} :outputs {:dimmer {:op :const :value 0.0}}}})

(def ^:private rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1 :pan 2}}]})

(defn- command [model op]
  (let [{:keys [model ack]} (control/apply-command model (merge {:command-id (str (gensym "c")) :show-id :gen
                                                                 :epoch (:epoch model) :source {:adapter :test} :time-ms 0}
                                                                op))]
    (assert (= :accepted (:status ack)) (pr-str ack))
    [model ack]))

(defn- launch [model cue-id]
  (let [[model ack] (command model {:op :cue/launch :cue-id cue-id :launch {:mode :persistent}})]
    [model (:instance-id ack)]))

(defn show
  "A show with pulse, wash and aim running, and a square LFO bound to the
  wash level, built through the shared command path."
  ([] (show [:pulse :wash :aim]))
  ([cue-ids]
   (let [base (control/new-show {:show-id :gen :rig rig :transport {:playing? true :beat 0.0 :bpm 120.0}
                                 :registry {:effects effects
                                            :cues (into {} (for [id (keys effects)] [id {:id id :effect-id id :version 1}]))}})
         [model ids] (reduce (fn [[model ids] cue] (let [[model id] (launch model cue)] [model (assoc ids cue id)]))
                             [base {}] cue-ids)]
     (if-let [wash (:wash ids)]
       (parameter-sources/bind model {:instance-id wash :param :level
                                      :binding {:type :lfo :waveform :pulse :period {:beats 1} :phase-reference :transport
                                                :inputs {:duty 0.5} :from 0.0 :to 0.8}})
       model))))

(defn- by-label [entries] (into {} (map (juxt :label identity)) entries))

(deftest eligibility-types-each-destination-and-says-why-the-rest-are-left-out
  (let [{:keys [eligible excluded selection]} (generate/eligibility (show))
        eligible (by-label eligible)
        excluded (into {} (map (juxt :label :reason)) excluded)]
    (testing "roles come from the schema type and the parameter name"
      (is (= {"pulse · level" :intensity "pulse · duty" :duty "pulse · rate" :rate "pulse · tint" :colour
              "wash · hue" :hue "wash · saturation" :saturation
              "aim · pan" :position "aim · tilt" :position "aim · spread" :spread "aim · bars" :period
              "wash · level LFO gain" :intensity "wash · level LFO period" :period "wash · level LFO duty" :duty}
             (update-vals eligible :role))))
    (testing "destinations are addressed by cue so they follow relaunches"
      (is (= {:kind :param :cue-id :pulse :param :level} (:destination (eligible "pulse · level"))))
      (is (= {:kind :lfo :cue-id :wash :param :level :field :period} (:destination (eligible "wash · level LFO period"))))
      (is (= knobs/period-schema (:schema (eligible "wash · level LFO period")))))
    (testing "every parameter left out has a reason"
      (is (= {"pulse · seed" :seed-parameter "wash · origin" :unsupported-type "wash · mystery" :no-role
              "wash · loose" :unbounded "wash · level" :modulated}
             excluded)))
    (is (= {:running 3} selection))))

(deftest a-selection-limits-the-cues-and-reports-missing-ones
  (let [{:keys [eligible excluded selection]} (generate/eligibility (show) {:cue-ids ["pulse" :empty :nope]})]
    (is (= #{:pulse} (set (map #(get-in % [:destination :cue-id]) eligible))))
    (is (= [:pulse :empty :nope] (:cue-ids selection)) "text cue IDs resolve to the registry's keywords")
    (is (= {"empty" :not-running ":nope" :cue-missing}
           (into {} (map (juxt :label :reason)) (filter #(#{:not-running :cue-missing} (:reason %)) excluded))))))

(defn- entries-of [eligibility candidate]
  (let [by-destination (into {} (map (juxt :destination identity)) (:eligible eligibility))]
    (into {} (map (fn [link] [(:id link) (by-destination (:destination link))])) (:links candidate))))

(deftest the-same-seed-gives-the-same-candidate-and-every-candidate-meets-the-constraints
  (let [eligibility (generate/eligibility (show))
        candidates (mapv #(generate/generate eligibility nil {:seed %}) (range 60))]
    (is (= (generate/generate eligibility nil {:seed 17}) (generate/generate eligibility nil {:seed 17})))
    (is (every? #(= :ready (:status %)) candidates))
    (is (< 10 (count (distinct (map generate/candidate-signature candidates)))) "seeds explore different mappings")
    (is (= #{:energy :colour :motion} (set (map :character candidates))))
    (doseq [{:keys [links character] :as candidate} candidates
            :let [entries (entries-of eligibility candidate)
                  polarity (:polarity (some #(when (= character (:id %)) %) generate/characters))]]
      (is (empty? (generate/constraint-errors candidate entries)) (pr-str candidate))
      (is (<= generate/min-links (count links) generate/max-links))
      (doseq [{:keys [range direction quantize] :as link} links
              :let [{:keys [schema role default]} (entries (:id link))]]
        (is (= direction (polarity role)) "directions follow the character")
        (is (generate/playable? link schema) "both knob ends give different values")
        (when (and (#{:number :integer} (:type schema)) (not= :period role) (not= :rate role) (not quantize))
          (let [[lo hi] (:bounds schema)]
            (is (<= lo (first range) (second range) hi) "a range stays inside the schema bounds")
            (when-not (#{:position :hue} role)
              (is (<= (first range) default (second range)) "the authored value lies inside the range"))))
        (when quantize
          (is (= {:ratios :preferred} quantize))
          (is (every? rational? range) "beat ratios stay exact")
          (is (= :linear (:curve link))))))))

(deftest correlated-roles-on-one-cue-share-a-window-brighter-but-shorter-pulses
  (let [eligibility (generate/eligibility (show [:pulse]))
        candidates (for [seed (range 40)
                         :let [c (generate/generate eligibility nil {:seed seed :character :energy :size [3 4]})]
                         :when (every? (set (map :id (:links c))) ["pulse-level" "pulse-duty"])]
                     c)
        {:keys [links explain]} (first candidates)
        link #(some (fn [l] (when (= % (:id l)) l)) links)]
    (is (seq candidates) "energy picks the level and duty of one pulse together")
    (is (= :up (:direction (link "pulse-level"))))
    (is (= :down (:direction (link "pulse-duty"))) "brighter but shorter")
    (is (= (:input (link "pulse-level")) (:input (link "pulse-duty"))) "both move in the same stretch of the knob")
    (is (= "pulse-duty" (:correlated-with (some #(when (= "pulse-level" (:link-id %)) %) explain))))
    (let [{:keys [schema]} (some #(when (= "pulse · duty" (:label %)) %) (:eligible eligibility))
          level-schema (:schema (some #(when (= "pulse · level" (:label %)) %) (:eligible eligibility)))]
      (is (< (knobs/resolve-value (link "pulse-duty") schema 1.0) (knobs/resolve-value (link "pulse-duty") schema 0.0)))
      (is (> (knobs/resolve-value (link "pulse-level") level-schema 1.0)
             (knobs/resolve-value (link "pulse-level") level-schema 0.0))))))

(deftest waveform-periods-step-through-preferred-beat-ratios
  (let [eligibility (generate/eligibility (show [:wash]))
        candidate (some #(let [c (generate/generate eligibility nil {:seed % :character :energy})]
                           (when (some (fn [l] (= :period (get-in l [:destination :field]))) (:links c)) c))
                        (range 40))
        period (some #(when (= :period (get-in % [:destination :field])) %) (:links candidate))
        steps (map #(knobs/resolve-value period knobs/period-schema (/ % 20.0)) (range 21))]
    (is (= :down (:direction period)) "more energy, shorter period")
    (is (apply >= steps) "the period only shortens as the knob rises")
    (is (every? #(some #{%} (for [n [1 2 3 4 6 8] d [1 2 3 4 6 8]] (/ n d))) steps))))

(deftest the-track-feel-shapes-colours-and-curves
  (let [eligibility (generate/eligibility (show))
        library (personalities/library {})
        smooth (library :squircle) shaky (library :zigzag-shaky)
        palette (set (:palette (generate/feel smooth)))
        colour (for [seed (range 30)
                     :let [c (generate/generate eligibility smooth {:seed seed :character :colour})]
                     link (:links c) :when (= :tint (get-in link [:destination :param]))]
                 link)
        curves (fn [personality] (set (for [seed (range 30)
                                            link (:links (generate/generate eligibility personality {:seed seed}))
                                            :when (not (:quantize link))]
                                        (:curve link))))]
    (is (seq colour))
    (is (every? #(contains? palette (second (:range %))) colour) "a colour link ends on a palette colour")
    (testing "without a feel a colour link still ends clearly away from the authored colour"
      (let [plain (for [seed (range 30)
                        link (:links (generate/generate eligibility nil {:seed seed :character :colour}))
                        :when (= :tint (get-in link [:destination :param]))]
                    (:range link))
            distance (fn [[a b]] (Math/sqrt (reduce + (map #(* (- %1 %2) (- %1 %2)) a b))))]
        (is (seq plain))
        (is (every? #(> (distance %) 0.5) plain))))
    (is (every? #{:ease-in-out :sine :smoothstep} (curves smooth)) "a smooth feel eases")
    (is (every? #{:linear :ease-in} (curves shaky)) "a nervous feel moves straight")
    (is (not= (generate/generate eligibility smooth {:seed 3}) (generate/generate eligibility shaky {:seed 3})))))

(deftest a-selection-with-too-few-parameters-explains-itself
  (testing "one cue with no parameters and nothing else running"
    (let [result (generate/generate (generate/eligibility (show [:empty])) nil {:seed 1})]
      (is (= :insufficient (:status result)))
      (is (= 0 (:eligible result)))
      (is (re-find #"No parameter in the selection can be generated; a knob needs at least 2" (:message result)))
      (is (= [:no-parameters] (map :reason (:excluded result))))))
  (testing "a pinned character without its anchor roles"
    (let [result (generate/generate (generate/eligibility (show [:aim])) nil {:seed 1 :character :colour})]
      (is (= :insufficient (:status result)))
      (is (re-find #"The Colour character needs at least 2 parameters including one of colour, hue, saturation" (:message result)))))
  (testing "a bad seed or size is an error, not a candidate"
    (is (thrown? clojure.lang.ExceptionInfo (generate/generate (generate/eligibility (show)) nil {:seed 1.5})))
    (is (thrown? clojure.lang.ExceptionInfo (generate/generate (generate/eligibility (show)) nil {:seed 1 :size [1 9]})))))

(deftest constraints-reject-unplayable-mappings-and-the-score-prefers-variety
  (let [eligibility (generate/eligibility (show))
        entry (by-label (:eligible eligibility))
        link (fn [label range direction]
               (let [{:keys [destination group]} (entry label)]
                 {:id (str group "-" (name (:param destination))) :destination destination :range range
                  :direction direction :curve :linear :input [0 1]}))
        entries (fn [links] (into {} (map (fn [l] [(:id l) (some #(when (= (:destination l) (:destination %)) %)
                                                                 (:eligible eligibility))])) links))
        good [(link "pulse · level" [0.4 1.0] :up) (link "wash · saturation" [0.5 1.0] :up)
              (link "aim · spread" [0.0 40.0] :up)]]
    (is (empty? (generate/constraint-errors {:character :energy :links good} (entries good))))
    (is (= ["A knob needs 2 to 8 links"]
           (generate/constraint-errors {:character :energy :links (take 1 good)} (entries (take 1 good)))))
    (is (some #(re-find #"same destination" %)
              (generate/constraint-errors {:character :energy :links (conj good (assoc (first good) :id "again"))}
                                          (assoc (entries good) "again" (entry "pulse · level")))))
    (is (some #(re-find #"runs against the Energy direction" %)
              (let [links (assoc good 0 (link "pulse · level" [0.4 1.0] :down))]
                (generate/constraint-errors {:character :energy :links links} (entries links)))))
    (is (some #(re-find #"does not change its destination" %)
              (let [links (assoc good 1 (link "wash · saturation" [0.7 0.7] :up))]
                (generate/constraint-errors {:character :energy :links links} (entries links)))))
    (is (some #(re-find #"needs one of" %)
              (let [links [(link "wash · saturation" [0.5 1.0] :up) (link "aim · spread" [0.0 40.0] :up)]]
                (generate/constraint-errors {:character :energy :links links} (entries links)))))
    (testing "variety of roles and owners scores higher than one cue's look-alike links"
      (let [varied (generate/score {:links good :pairs []} (entries good) (:eligible eligibility))
            narrow-links [(link "aim · pan" [0.3 0.7] :up) (link "aim · tilt" [0.3 0.7] :up)
                          (link "aim · spread" [0.0 40.0] :up)]
            narrow (generate/score {:links narrow-links :pairs []} (entries narrow-links) (:eligible eligibility))]
        (is (> (:total varied) (:total narrow)))
        (is (= 1.0 (:roles varied) (:owners varied)))
        (is (< (:owners narrow) 0.5))))))

(deftest rerolls-give-distinct-mappings
  (let [eligibility (generate/eligibility (show))
        first-candidate (generate/generate eligibility nil {:seed 5})
        second-candidate (generate/reroll eligibility nil {} 5 #{(generate/candidate-signature first-candidate)})
        third-candidate (generate/reroll eligibility nil {} (:seed second-candidate)
                                         (set (map generate/candidate-signature [first-candidate second-candidate])))]
    (is (= 3 (count (distinct (map generate/candidate-signature [first-candidate second-candidate third-candidate])))))
    (is (< 5 (:seed second-candidate) (:seed third-candidate)))
    (is (= second-candidate (generate/generate eligibility nil {:seed (:seed second-candidate)}))
        "a reroll is reproducible from its own seed")))
