(ns tolglow.knob-candidates.generate-test
  "The knob candidate generator (#667) as pure data → data: eligibility and
  its reasons, seeded determinism, hard constraints, scoring, correlated
  choices, track-feel shaping and the too-few-parameters explanation."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.knob-candidates :as knob-candidates]
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

(deftest a-reroll-that-stops-after-a-repeat-says-it-repeated
  (let [reroll-from #'generate/reroll-from
        repeat {:status :ready :seed 6 :character :energy
                :links [{:id "pulse-level" :destination {:kind :param :cue-id :pulse :param :level}}]}
        seen #{(generate/candidate-signature repeat)}
        too-few {:status :insufficient :message "No playable mapping"}]
    (testing "seed 6 repeats and seed 7 has no playable mapping: the repeat comes back marked"
      (is (= (assoc repeat :repeated? true) (reroll-from {6 repeat 7 too-few} 5 seen))))
    (testing "a first seed with no playable mapping is reported as it is"
      (is (= too-few (reroll-from {6 too-few} 5 seen))))
    (testing "a new mapping after a repeat is not marked"
      (let [fresh (assoc repeat :seed 7 :character :motion)]
        (is (= fresh (reroll-from {6 repeat 7 fresh} 5 seen)))))
    (testing "sixteen repeats in a row"
      (is (= (assoc repeat :repeated? true) (reroll-from (constantly repeat) 5 seen))))))

(deftest refused-options-carry-a-code
  (let [eligibility (generate/eligibility (show))
        refusal (fn [options]
                  (try (generate/generate eligibility nil options) nil
                       (catch clojure.lang.ExceptionInfo error
                         [(:control/error (ex-data error)) (ex-message error)])))]
    (is (= :invalid-seed (first (refusal {:seed 1.5}))))
    (is (= [:unknown-character "Unknown character; choose one of energy, colour, motion"]
           (refusal {:seed 1 :character :sparkle})))
    (doseq [size [[1 9] [8 2] [3] 3 ["2" "4"]]]
      (is (= :invalid-size (first (refusal {:seed 1 :size size}))) (pr-str size)))
    (is (nil? (refusal {:seed 1 :size [2 8]})))))

(defn- knob [model knob-id links]
  (:model (knobs/apply-operation model {:op :knob/create :knob-id knob-id :name knob-id :links links})))

(deftest destinations-existing-knobs-drive-are-left-out-unless-shared
  (let [level {:kind :param :cue-id :pulse :param :level}
        rate {:kind :param :cue-id :pulse :param :rate}
        base (show)
        pulse-id (some #(when (= :pulse (:cue-id %)) (:id %)) (vals (:instances base)))
        model (-> base
                  (knob "lift" [{:id "l" :destination level :range [0.2 0.9]}])
                  ;; Addressed to the running launch, so it also covers the cue's rate.
                  (knob "push" [{:id "r" :destination (assoc (dissoc rate :cue-id) :instance-id pulse-id) :range [1.0 6.0]}]))
        driven (generate/knob-driven model)
        avoided (generate/eligibility model {:avoid driven})
        shared (generate/eligibility model {:shared driven})
        on? (fn [destination] #(= destination (:destination %)))]
    (is (= {level ["lift"] rate ["push"] {:kind :param :instance-id pulse-id :param :rate} ["push"]} driven))
    (testing "left out, with the knob that drives each"
      (is (not-any? (some-fn (on? level) (on? rate)) (:eligible avoided)))
      (is (= #{["pulse · level" ["lift"]] ["pulse · rate" ["push"]]}
             (set (for [x (:excluded avoided) :when (= :knob-driven (:reason x))] [(:label x) (:knob-ids x)]))))
      (is (= (dec (dec (count (:eligible (generate/eligibility model))))) (count (:eligible avoided)))))
    (testing "no candidate drives them, and its recipe records exactly what it avoided"
      (doseq [seed (range 30)
              :let [c (generate/generate avoided nil {:seed seed})]]
        (is (= :ready (:status c)))
        (is (not-any? (some-fn (on? level) (on? rate)) (:links c)))
        (is (= (sort-by pr-str [level rate]) (get-in c [:recipe :avoid])))))
    (testing "shared on request, and explained"
      (is (= ["lift"] (:driven-by (some #(when ((on? level) %) %) (:eligible shared)))))
      (let [shared-links (for [seed (range 60)
                               :let [c (generate/generate shared nil {:seed seed :character :energy})]
                               e (:explain c) :when (:shared-with e)]
                           e)]
        (is (seq shared-links))
        (is (every? #(#{["lift"] ["push"]} (:shared-with %)) shared-links))
        (is (every? #(nil? (get-in (generate/generate shared nil {:seed % :character :energy}) [:recipe :avoid]))
                    (range 10)))))))

(defn- namespaced-show []
  (let [cues {:a/pulse {:id :a/pulse :effect-id :pulse :version 1}
              :b/pulse {:id :b/pulse :effect-id :pulse :version 1}}
        base (control/new-show {:show-id :gen :rig rig :transport {:playing? true :beat 0.0 :bpm 120.0}
                                :registry {:effects (select-keys effects [:pulse]) :cues cues}})]
    (reduce (fn [model cue] (first (launch model cue))) base [:a/pulse :b/pulse])))

(deftest cues-that-differ-only-by-namespace-keep-distinct-link-ids
  (let [model (namespaced-show)
        eligibility (generate/eligibility model)
        candidates (map #(generate/generate eligibility nil {:seed % :size [4 8]}) (range 40))
        ids (set (mapcat #(map :id (:links %)) candidates))]
    (is (= #{"a/pulse · level" "b/pulse · level"} (set (filter #(re-find #"level$" %) (map :label (:eligible eligibility))))))
    (is (every? #(= :ready (:status %)) candidates))
    (is (every? ids ["a/pulse-level" "b/pulse-level"]) "each cue's link keeps its namespace")
    (doseq [{:keys [links explain]} candidates]
      (is (apply distinct? (map :id links)))
      (is (some? (knob model "kept" links)) "keeping never collides on a link ID")
      (doseq [{:keys [link-id correlated-with]} explain :when correlated-with]
        (is (= (subs link-id 0 7) (subs correlated-with 0 7)) "correlated pairs stay on one cue")))
    (testing "a selection names a namespaced cue by its full text; a bare shared name is ambiguous"
      (is (= [:a/pulse] (get-in (generate/eligibility model {:cue-ids ["a/pulse"]}) [:selection :cue-ids])))
      (is (= #{:a/pulse} (set (map #(get-in % [:destination :cue-id]) (:eligible (generate/eligibility model {:cue-ids ["a/pulse"]}))))))
      (is (= [:cue-missing] (map :reason (:excluded (generate/eligibility model {:cue-ids ["pulse"]}))))))))

(defn- rig-show
  "pulse, wash and aim running under namespaced cue IDs with distinct names."
  []
  (let [cues (into {} (for [id [:pulse :wash :aim]] [(keyword "rig" (name id)) {:id (keyword "rig" (name id)) :effect-id id :version 1}]))
        base (control/new-show {:show-id :gen :rig rig :transport {:playing? true :beat 0.0 :bpm 120.0}
                                :registry {:effects effects :cues cues}})]
    (reduce (fn [model cue] (first (launch model cue))) base (sort (keys cues)))))

(defn- candidate-command [model command]
  (let [{:keys [model ack]} (control/apply-domain-command
                             model (merge {:command-id (str (gensym "k")) :show-id :gen :epoch (:epoch model)
                                           :source {:adapter :test}} command)
                             knob-candidates/apply-operation)]
    [model ack (get-in model [:knob-candidates :candidate])]))

(deftest version-1-recipes-replay-with-their-own-link-ids-and-stale-edits-are-refused
  (let [model (rig-show)
        [_ fresh-ack fresh] (candidate-command model {:op :knob-candidate/generate :seed 4 :size [4 6]})
        [_ v1-ack v1] (candidate-command model {:op :knob-candidate/generate :recipe {:generator 1 :seed 4 :size [4 6]}})
        plain #(subs % 4)]
    (is (= [:accepted :accepted] [(:status fresh-ack) (:status v1-ack)]))
    (is (= [:ready :ready] [(:status fresh) (:status v1)]))
    (is (<= 4 (count (:links fresh))))
    (testing "a fresh recipe is the current version with namespaced link IDs"
      (is (= 3 generate/generator-version (get-in fresh [:recipe :generator])))
      (is (every? #(re-find #"^rig/" %) (map :id (:links fresh)))))
    (testing "a version 1 recipe regenerates the same links under its plain-name IDs"
      (is (= 1 (get-in v1 [:recipe :generator])))
      (is (= (map plain (map :id (:links fresh))) (map :id (:links v1))))
      (is (= (map #(dissoc % :id) (:links fresh)) (map #(dissoc % :id) (:links v1)))))
    (let [[dropped kept] (:links v1)]
      (testing "its recorded edits still apply"
        (let [edits [{:op :unlink :link-id (:id dropped)} {:op :link :link (assoc kept :curve :sine)}]
              [_ ack c] (candidate-command model {:op :knob-candidate/generate
                                                  :recipe {:generator 1 :seed 4 :size [4 6] :edits edits}})]
          (is (= :accepted (:status ack)) (pr-str ack))
          (is (not-any? #(= (:id dropped) (:id %)) (:links c)))
          (is (= :sine (:curve (some #(when (= (:id kept) (:id %)) %) (:links c)))))))
      (testing "an edit naming a link the recipe does not make is refused, not dropped or doubled"
        (let [refusal (fn [recipe] ((juxt :status :code) (second (candidate-command model {:op :knob-candidate/generate :recipe recipe}))))]
          (is (= [:rejected :unknown-link]
                 (refusal {:generator 2 :seed 4 :size [4 6] :edits [{:op :unlink :link-id (:id dropped)}]}))
              "a plain-name unlink against version 2 IDs")
          (is (= [:rejected :duplicate-destination]
                 (refusal {:generator 2 :seed 4 :size [4 6] :edits [{:op :link :link (assoc kept :curve :sine)}]}))
              "a plain-name link on a destination the candidate already drives")
          (is (= [:rejected :unsupported-generator] (refusal {:generator 4 :seed 4}))))))
    (testing "live edits follow the same rules"
      (let [[model] (candidate-command model {:op :knob-candidate/generate :seed 4 :size [4 6]})
            [x y] (:links fresh)
            [_ ack] (candidate-command model {:op :knob-candidate/unlink :link-id "nope"})
            [_ dup] (candidate-command model {:op :knob-candidate/link :link (assoc x :id "again")})
            [_ onto] (candidate-command model {:op :knob-candidate/link
                                                :link (assoc y :id (:id x))})
            [_ same] (candidate-command model {:op :knob-candidate/link :link (assoc x :curve :sine)})]
        (is (= [:rejected :unknown-link] ((juxt :status :code) ack)))
        (is (= [:rejected :duplicate-destination] ((juxt :status :code) dup)))
        (is (= [:rejected :duplicate-destination] ((juxt :status :code) onto))
            "replacing a link by its ID onto another link's destination")
        (is (= :accepted (:status same)) "replacing a link on its own destination")))
    (testing "a recipe's sharing flag is a boolean"
      (is (= [:rejected :invalid-knob-candidate]
             ((juxt :status :code) (second (candidate-command model {:op :knob-candidate/generate
                                                                     :recipe {:generator 2 :seed 4 :include-knob-driven? "yes"}}))))))
    (testing "a recipe's reversed size is normalised like a command's"
      (let [[_ ack c] (candidate-command model {:op :knob-candidate/generate :recipe {:generator 2 :seed 4 :size [6 4]}})]
        (is (= :accepted (:status ack)))
        (is (= [4 6] (get-in c [:recipe :size])))
        (is (= (:links fresh) (:links c)))))))

(deftest a-version-1-recipe-groups-cues-by-plain-name-as-version-1-did
  ;; Version 1 named owners by plain name, so :a/pulse and :b/pulse were one
  ;; owner for correlated pairs and owner variety, and shared link-ID prefixes.
  ;; The reference is the generator run on entries grouped that way.
  (let [model (namespaced-show)
        eligibility (generate/eligibility model)
        plain (update eligibility :eligible
                      (partial mapv #(assoc % :group (name (get-in % [:destination :cue-id])))))
        compared (for [seed (range 60) size [nil [2 4] [4 6]]
                       :let [options (cond-> {:seed seed} size (assoc :size size))
                             replayed (generate/generate eligibility nil (assoc options :generator 1))
                             reference (generate/generate plain nil options)]]
                   [(select-keys replayed [:status :links :score :explain :character])
                    (select-keys reference [:status :links :score :explain :character])])]
    (is (every? (fn [[a b]] (= a b)) compared))
    (is (some (fn [[a]] (some :correlated-with (:explain a))) compared) "correlations are exercised")
    (is (not= (map first compared)
              (for [seed (range 60) size [nil [2 4] [4 6]]]
                (select-keys (generate/generate eligibility nil (cond-> {:seed seed} size (assoc :size size)))
                             [:status :links :score :explain :character])))
        "version 2 groups the two cues apart")))

(defn- integer-show
  "One cue whose number level and rate sit beside integer parameters, as in #747."
  []
  (let [effect {:id :scatter :version 1
                :params {:level {:type :number :default 0.8 :bounds [0.0 1.0]}
                         :rate {:type :number :default 4.0 :bounds [0.5 8.0]}
                         :spread {:type :integer :default 2 :bounds [0 16]}
                         :gain {:type :integer :default 3 :bounds [1 4]}
                         :step {:type :integer :default 2 :bounds [1 16] :unit :beats}}
                :outputs {:dimmer {:op :param :key :level}}}
        base (control/new-show {:show-id :gen :rig rig :transport {:playing? true :beat 0.0 :bpm 120.0}
                                :registry {:effects {:scatter effect}
                                           :cues {:scatter {:id :scatter :effect-id :scatter :version 1}}}})]
    (first (launch base :scatter))))

(deftest integer-parameters-link-with-whole-endpoints-inside-their-bounds
  (let [eligibility (generate/eligibility (integer-show))
        schemas (into {} (map (juxt #(get-in % [:destination :param]) :schema)) (:eligible eligibility))
        candidates (mapv #(generate/generate eligibility nil {:seed % :size [4 4]}) (range 200))
        integer-links (for [c candidates link (:links c)
                            :when (= :integer (:type (schemas (get-in link [:destination :param]))))]
                        link)
        params (set (map #(get-in % [:destination :param]) integer-links))]
    (is (= #{:spread :gain :step} (set (keep #(when (= :integer (:type (val %))) (key %)) schemas))))
    (is (every? #(= :ready (:status %)) candidates) "integer parameters fill out the four links")
    (is (= #{:spread :gain :step} params) "every eligible integer parameter is linked by some seed")
    (doseq [{[a b] :range :keys [destination quantize] :as link} integer-links
            :let [{:keys [bounds default] :as schema} (schemas (:param destination))]]
      (is (and (int? a) (int? b) (< a b)) (pr-str link))
      (is (<= (first bounds) a b (second bounds)) "whole endpoints inside the bounds")
      (when-not (= :step (:param destination))
        (is (<= a default b) "the authored value lies inside the range"))
      (is (= :integer quantize))
      (is (generate/playable? link schema))
      (let [values (map #(knobs/resolve-value link schema (/ % 32.0)) (range 33))]
        (is (every? int? values))
        (is (= #{a b} (set [(first values) (last values)])) "the knob's ends reach both endpoints")))
    (is (some #(re-find #", in whole steps" (:why %)) (mapcat :explain candidates)) "explained as whole steps")
    (testing "one number and one integer intensity make a knob"
      (let [effect {:id :pair :version 1
                    :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}
                             :dimmer {:type :integer :default 100 :bounds [0 255]}}
                    :outputs {:dimmer {:op :param :key :level}}}
            model (first (launch (control/new-show {:show-id :gen :rig rig :transport {:playing? true :beat 0.0 :bpm 120.0}
                                                    :registry {:effects {:pair effect}
                                                               :cues {:pair {:id :pair :effect-id :pair :version 1}}}})
                                 :pair))
            pair (generate/eligibility model)]
        (is (every? #(= :ready (:status (generate/generate pair nil {:seed %}))) (range 100)))
        (testing "a version 2 recipe still leaves the integer out, as version 2 did"
          (is (every? #(= :insufficient (:status (generate/generate pair nil {:seed % :generator 2}))) (range 100))))))
    (testing "older recipes replay without integer links"
      (doseq [version [1 2]
              seed (range 60)
              :let [c (generate/generate eligibility nil {:seed seed :size [2 4] :generator version})]]
        (is (not-any? #(= :integer (:type (schemas (get-in % [:destination :param])))) (:links c)))))))

(deftest version-3-drafts-number-and-colour-links-exactly-as-version-2
  ;; The integer change consumes the same draws, so a show without integer
  ;; parameters generates the same candidate under either version.
  (let [eligibility (generate/eligibility (show))]
    (doseq [seed (range 60) size [nil [2 4] [4 8]]
            :let [options (cond-> {:seed seed} size (assoc :size size))
                  strip #(-> % (dissoc :generator) (update :recipe dissoc :generator))]]
      (is (= (strip (generate/generate eligibility nil (assoc options :generator 2)))
             (strip (generate/generate eligibility nil options)))))))
