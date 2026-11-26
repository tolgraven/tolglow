(ns tolglow.knob-candidates-test
  "Generated knob candidates (#667) on the showcase rig through the real show
  owner: three distinct candidates auditioned in the Room without touching the
  live show, keep and repeat from a saved recipe, edits, a selection with too
  few parameters, discard, replay and the agent tool."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.agent :as agent]
            [tolglow.knob-candidates.generate :as generate]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.knobs :as knobs]
            [tolglow.persistence :as files]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.looks :as looks]))

(def ^:private studio {:adapter :studio :session-id "candidate-test"})

(defn- runtime
  ([] (runtime (showcase/model)))
  ([model] (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 60000
                                 :audition {:max-live-share ##Inf}})))

(defn- submit! [rt time payload]
  (let [reply (show/submit! rt :command (merge {:source studio} payload))]
    (show/tick! rt time)
    (deref reply 5000 ::missing)))

(defn- message! [rt time type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt time)
    (deref reply 5000 ::missing)))

(def ^:private cues [:beam-shafts :strobe :can-can :mover-orbit :wash-tide])

(defn- setup!
  "Launch five showcase cues with their authored waveforms bound."
  [rt]
  (let [ids (into {} (map-indexed (fn [i cue]
                                    [cue (:instance-id (submit! rt (* 10 (inc i)) {:op :cue/launch :cue-id cue
                                                                                   :launch {:mode :persistent}}))])
                                  cues))]
    (doseq [[cue id] ids [param binding] (get looks/sources cue)]
      (assert (= :accepted (:status (message! rt 80 :modulation-bind {:instance-id id :param param :binding binding})))))
    ids))

(defn- model [rt] (:model @(:state rt)))

(defn- candidate [rt] (get-in (model rt) [:knob-candidates :candidate]))

(defn- view [rt] (:knob-candidates (show/snapshot rt)))

(defn- entries
  "Eligible destination of each candidate link, by link ID."
  [model candidate]
  (let [by-destination (into {} (map (juxt :destination identity)) (:eligible (generate/eligibility model)))]
    (into {} (map (fn [link] [(:id link) (by-destination (:destination link))])) (:links candidate))))

(defn- live-state
  "What a candidate must never write: every launch's parameters and waveform
  bindings, the knobs and the show variables."
  [model]
  {:params (update-vals (:instances model) #(select-keys % [:params :parameter-bindings]))
   :knobs (:knobs model)
   :variables (into {} (remove (comp subscribers/reserved-variable? key)) (:variables model))})

(defn- room-shows!
  "The Room's private model holds every candidate link resolved at value."
  [rt value]
  (let [room (get-in @(:scene-draft rt) [:frame :model])
        c (candidate rt)
        live (model rt)]
    (doseq [{:keys [destination] :as link} (:links c)
            :let [{:keys [schema]} ((entries live c) (:id link))
                  expected (knobs/resolve-value link schema value)
                  ids (keep #(when (= (:cue-id destination) (:cue-id %)) (:id %)) (vals (:instances room)))]]
      (is (seq ids) (:id link))
      (doseq [id ids]
        (case (:kind destination)
          :param (is (= expected (get-in room [:instances id :params (:param destination)])) (:id link))
          :lfo (let [binding (get-in room [:instances id :parameter-bindings (:param destination) :binding])]
                 (is (= expected (case (:field destination)
                                   :period (get-in binding [:period :beats])
                                   (:from :to) (get binding (:field destination))
                                   nil (get-in binding [:inputs (:input destination)])))
                     (:id link))))))))

(defn- room-bytes [rt]
  (into {} (map (fn [[u b]] [u (vec b)])) (get-in @(:scene-draft rt) [:frame :output :universes])))

(defn- frames! [rt t0 n]
  (doseq [t (range t0 (+ t0 (* 20 n)) 20)]
    (show/tick! rt t)
    (is (show/await-audition rt 5000))))

(defn- slow-audition
  "The real audition frame, ms later, so a test can act while a frame is in flight."
  [ms]
  (let [render @#'show/render-audition-frame]
    (fn [& args] (Thread/sleep (long ms)) (apply render args))))

(defn- audition-frames! [rt t0]
  (doseq [t (range t0 (+ t0 100) 20)]
    (show/tick! rt t)
    (is (show/await-audition rt 5000))))

(deftest three-distinct-candidates-audition-in-the-room-and-leave-the-live-show-alone
  (let [rt (runtime)]
    (try
      (setup! rt)
      (let [before (live-state (model rt))
            generated (submit! rt 100 {:op :knob-candidate/generate :seed 11})
            first-candidate (candidate rt)
            _ (submit! rt 110 {:op :knob-candidate/reroll})
            second-candidate (candidate rt)
            _ (submit! rt 120 {:op :knob-candidate/reroll})
            third-candidate (candidate rt)
            candidates [first-candidate second-candidate third-candidate]]
        (is (= [:accepted :ready] ((juxt :status :candidate-status) generated)))
        (testing "three distinct usable candidates"
          (is (= 3 (count (distinct (map generate/candidate-signature candidates)))))
          (is (= [11 12] [(:seed first-candidate) (:seed second-candidate)]) "rerolls step the seed")
          (doseq [c candidates]
            (is (= :ready (:status c)))
            (is (empty? (generate/constraint-errors c (entries (model rt) c))) (pr-str (:links c)))
            (is (= (count (:links c)) (count (:explain c))) "every link is explained")))
        (is (= before (live-state (model rt))) "generating and rerolling write nothing")
        (doseq [[i c] (map-indexed vector candidates)
                :let [t (+ 200 (* 200 i))]]
          (when (pos? i) (submit! rt t {:op :knob-candidate/reroll}))
          (when (pos? i) (is (= (:links c) (:links (candidate rt))) "a reroll is reproducible from its seed"))
          (submit! rt t {:op :knob-candidate/generate :seed (:seed c)})
          (let [ack (submit! rt (+ t 10) {:op :knob-candidate/audition :value 1.0})]
            (is (= :accepted (:status ack)))
            (is (= "knob-candidate" (:draft-id ack))))
          (audition-frames! rt (+ t 20))
          (let [room (get-in @(:scene-draft rt) [:frame :model])
                c (candidate rt)
                live (model rt)]
            (testing "the Room renders the candidate at the auditioned knob value"
              (doseq [{:keys [destination] :as link} (:links c)
                      :let [{:keys [schema]} ((entries live c) (:id link))
                            expected (knobs/resolve-value link schema 1.0)
                            ids (keep #(when (= (:cue-id destination) (:cue-id %)) (:id %)) (vals (:instances room)))]]
                (doseq [id ids]
                  (case (:kind destination)
                    :param (is (= expected (get-in room [:instances id :params (:param destination)])) (:id link))
                    :lfo (let [binding (get-in room [:instances id :parameter-bindings (:param destination) :binding])]
                           (is (= expected (case (:field destination)
                                             :period (get-in binding [:period :beats])
                                             (:from :to) (get binding (:field destination))
                                             nil (get-in binding [:inputs (:input destination)])))
                               (:id link)))))))
            (testing "the live show, its knobs and its values are unchanged"
              (is (= before (live-state live))))
            (testing "Studio and the REPL read the same audition"
              (is (= {:value 1.0 :status :auditioning}
                     (select-keys (:audition (view rt)) [:value :status])))
              (is (= (get-in (show/scene-draft rt) [:draft :revision]) (get-in (view rt) [:audition :draft-revision])))
              (is (every? (comp :text :resolved) (get-in (view rt) [:candidate :links]))))))
        (testing "a candidate audition cannot be applied as a scene"
          (let [ack (submit! rt 900 {:op :scene-draft/apply :draft-id "knob-candidate"
                                     :expected-draft-revision (get-in (show/scene-draft rt) [:draft :revision])})]
            (is (= [:rejected :preview-draft] ((juxt :status :code) ack))))
          (is (= before (live-state (model rt))))))
      (finally (show/stop! rt)))))

(deftest a-kept-candidate-is-an-ordinary-knob-that-repeats-from-its-saved-recipe
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 21})
      (submit! rt 110 {:op :knob-candidate/audition :value 0.75})
      (let [kept (candidate rt)
            before (live-state (model rt))
            ack (submit! rt 120 {:op :knob-candidate/keep :name "Lift" :value 0.75})
            knob (get-in (model rt) [:knobs :knobs (:knob-id ack)])]
        (is (= :accepted (:status ack)))
        (is (= "generated-1" (:knob-id ack)))
        (testing "keeping creates a #666 knob with the candidate's links and recipe and ends the draft"
          (is (= {:name "Lift" :value 0.75 :links (:links kept) :recipe (:recipe kept)}
                 (select-keys knob [:name :value :links :recipe])))
          (is (= {:generator generate/generator-version :seed 21 :character (:character kept)} (:recipe kept)))
          (is (nil? (candidate rt)))
          (is (nil? (show/scene-draft rt)) "the Room audition ends")
          (is (= (:params before) (:params (live-state (model rt)))) "keeping writes nothing until the knob moves")
          (is (= (:recipe kept) (get-in (show/snapshot rt) [:knobs :knobs 0 :recipe]))))
        (testing "turning the kept knob writes its destinations like any knob"
          (let [turned (submit! rt 130 {:op :knob/set :knob-id "generated-1" :value 1.0})]
            (is (= :accepted (:status turned)))
            (is (every? #{:applied} (map :status (:links turned))))))
        (testing "an unkept draft is not saved; the kept knob and its recipe are"
          (submit! rt 140 {:op :knob-candidate/generate :seed 99})
          (let [saved (files/freeze (model rt))
                restored (files/thaw (showcase/model) saved)]
            (is (some? (candidate rt)))
            (is (not (contains? restored :knob-candidates)))
            (is (= (:recipe kept) (get-in restored [:knobs :knobs "generated-1" :recipe])))
            (testing "after a reload, the recipe gives the same links"
              (let [reloaded (runtime restored)]
                (try
                  (let [ack (submit! reloaded 1000 {:op :knob-candidate/generate :from-knob "generated-1"})]
                    (is (= [:accepted :ready 21] ((juxt :status :candidate-status :seed) ack)))
                    (is (= (:links kept) (:links (candidate reloaded))))
                    (is (= (:recipe kept) (:recipe (candidate reloaded)))))
                  (finally (show/stop! reloaded))))))))
      (finally (show/stop! rt)))))

(deftest edits-are-validated-and-replayed-from-the-recipe
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 4 :size [4 6]})
      (let [{:keys [links]} (candidate rt)
            [a b] links
            dropped (submit! rt 110 {:op :knob-candidate/unlink :link-id (:id b)})
            edited-link (assoc a :direction (if (= :up (:direction a)) :down :up) :curve :sine)
            edited (submit! rt 120 {:op :knob-candidate/link :link (update edited-link :direction name)})
            refused (submit! rt 130 {:op :knob-candidate/link
                                     :link (assoc a :range [-5.0 5.0] :destination {:kind :param :cue-id :strobe :param :duty})})
            current (candidate rt)]
        (is (= :accepted (:status dropped) (:status edited)))
        (is (= :rejected (:status refused)) "an edit outside the destination's bounds is refused")
        (is (= (dec (count links)) (count (:links current))))
        (is (= :sine (:curve (first (:links current)))))
        (is (:edited? current))
        (is (= [{:op :unlink :link-id (:id b)} {:op :link :link (first (:links current))}]
               (get-in current [:recipe :edits])))
        (let [kept (submit! rt 140 {:op :knob-candidate/keep})
              _ (submit! rt 150 {:op :knob-candidate/generate :from-knob (:knob-id kept)})]
          (is (= (:links current) (:links (candidate rt))) "the recipe replays the edits"))
        (testing "a recipe's edits are checked like live edits before they replay"
          (let [out-of-bounds (assoc a :range [-50.0 50.0] :destination {:kind :param :cue-id :strobe :param :duty})
                recipe {:generator generate/generator-version :seed 4 :size [4 6]}
                refused (submit! rt 160 {:op :knob-candidate/generate
                                         :recipe (assoc recipe :edits [{:op :link :link out-of-bounds}])})
                malformed (submit! rt 170 {:op :knob-candidate/generate :recipe (assoc recipe :edits [{:op :bogus}])})]
            (is (= [:rejected :invalid-knob] ((juxt :status :code) refused)))
            (is (= [:rejected :invalid-knob-candidate] ((juxt :status :code) malformed)))
            (is (= (:links current) (:links (candidate rt))) "a refused recipe leaves the candidate alone"))))
      (finally (show/stop! rt)))))

(deftest a-selection-with-too-few-parameters-says-why
  (let [rt (runtime)]
    (try
      (submit! rt 10 {:op :cue/launch :cue-id :gather-path :launch {:mode :persistent}})
      (submit! rt 20 {:op :cue/launch :cue-id :colour-drift :launch {:mode :persistent}})
      (let [ack (submit! rt 30 {:op :knob-candidate/generate :cue-ids [:gather-path :colour-drift :haze]})
            {:keys [status candidate]} (view rt)]
        (is (= [:accepted :insufficient 0 2] ((juxt :status :candidate-status :eligible :needed) ack)))
        (is (= :insufficient status))
        (is (re-find #"No parameter in the selection can be generated" (:message candidate)))
        (is (= {"gather-path" :no-parameters "colour-drift" :no-parameters "haze" :not-running}
               (into {} (map (juxt :label :reason)) (:excluded candidate))))
        (is (= [:rejected :no-candidate] ((juxt :status :code) (submit! rt 40 {:op :knob-candidate/audition :value 0.5}))))
        (is (= :rejected (:status (submit! rt 50 {:op :knob-candidate/keep}))) "nothing to keep"))
      (finally (show/stop! rt)))))

(deftest discard-ends-the-audition-and-a-repeated-command-replays-its-receipt
  (let [rt (runtime)]
    (try
      (setup! rt)
      (let [first-ack (submit! rt 100 {:op :knob-candidate/generate :seed 8 :command-id "gen-8"})
            replay (submit! rt 110 {:op :knob-candidate/generate :seed 8 :command-id "gen-8"})]
        (is (= first-ack replay))
        (is (= "candidate-1" (:id (candidate rt))) "a replay generates nothing new"))
      (submit! rt 120 {:op :knob-candidate/audition :value 0.25})
      (is (= "knob-candidate" (get-in (show/scene-draft rt) [:draft :id])))
      (testing "another Room draft waits for the candidate audition"
        (let [refused (submit! rt 130 {:op :scene-draft/stage :draft-id "scene"
                                       :operations [{:op :cue/launch :cue-id :haze :launch {:mode :persistent}}]})]
          (is (= [:rejected :draft-active] ((juxt :status :code) refused)))))
      (is (= :accepted (:status (submit! rt 140 {:op :knob-candidate/discard}))))
      (is (nil? (candidate rt)))
      (is (nil? (show/scene-draft rt)))
      (is (nil? (:candidate (view rt))))
      (is (= [{:id :energy :name "Energy"} {:id :colour :name "Colour"} {:id :motion :name "Motion"}]
             (:characters (view rt))))
      (finally (show/stop! rt)))))

(defn- tool! [rt time tool arguments command-id]
  (let [result (future (show/dispatch-tool! rt tool arguments {:command-id command-id :run-id "agent-run"}))]
    (loop [t time]
      (show/tick! rt t)
      (if (realized? result) @result (do (Thread/sleep 2) (recur (+ t 5)))))))

(deftest an-agent-uses-the-same-generate-audition-and-keep-operations
  (let [rt (runtime)
        declaration (some #(when (= "knob_candidate" (:name %)) %) agent/default-tools)]
    (try
      (setup! rt)
      (is (empty? (agent/validate-arguments (:parameters declaration) {"op" "generate" "seed" 5 "character" "motion"})))
      (is (seq (agent/validate-arguments (:parameters declaration) {"op" "apply"})))
      (let [generated (tool! rt 100 "knob_candidate" {:op "generate" :seed 5 :character "motion"} "agent-1")
            auditioned (tool! rt 200 "knob_candidate" {:op "audition" :value 0.6} "agent-2")
            kept (tool! rt 300 "knob_candidate" {:op "keep" :name "Agent motion"} "agent-3")]
        (is (= [:accepted :ready :motion] ((juxt :status :candidate-status :character) generated)))
        (is (= [:accepted 0.6] ((juxt :status :value) auditioned)))
        (is (= :accepted (:status kept)))
        (is (= "Agent motion" (get-in (model rt) [:knobs :knobs (:knob-id kept) :name])))
        (is (= {:adapter :agent :session-id "agent-run"}
               (get-in (model rt) [:control :history "agent-1" :command :source]))))
      (finally (show/stop! rt)))))

(deftest refusals-carry-codes-and-the-agent-schema-matches-the-command
  (let [rt (runtime)
        declaration (some #(when (= "knob_candidate" (:name %)) %) agent/default-tools)]
    (try
      (setup! rt)
      (is (= [:rejected :unknown-character] ((juxt :status :code) (submit! rt 100 {:op :knob-candidate/generate :character "sparkle"}))))
      (doseq [[i size] (map-indexed vector [[1 9] [3] "4" ["2" "4"]])]
        (is (= [:rejected :invalid-size] ((juxt :status :code) (submit! rt (+ 110 i) {:op :knob-candidate/generate :size size})))
            (pr-str size)))
      (is (= [:rejected :invalid-seed] ((juxt :status :code) (submit! rt 120 {:op :knob-candidate/generate :seed 1.5}))))
      (testing "the two size bounds come in either order, as the tool schema allows"
        (is (empty? (agent/validate-arguments (:parameters declaration) {"op" "generate" "size" [8 2]})))
        (is (seq (agent/validate-arguments (:parameters declaration) {"op" "generate" "size" [1 9]})))
        (is (empty? (agent/validate-arguments (:parameters declaration) {"op" "generate" "include-knob-driven?" true})))
        (let [ack (tool! rt 130 "knob_candidate" {:op "generate" :seed 4 :size [6 4]} "agent-size")]
          (is (= [:accepted :ready] ((juxt :status :candidate-status) ack)))
          (is (= [4 6] (get-in (candidate rt) [:recipe :size])))
          (is (= [4 6] (get-in (view rt) [:session :options :size])))
          (is (<= 4 (count (:links (candidate rt))) 6))))
      (finally (show/stop! rt)))))

(deftest sweeping-an-audition-keeps-its-private-timeline-and-compile-caches
  (let [rt (runtime)]
    (try
      (let [ids (setup! rt)
            before (live-state (model rt))]
        (submit! rt 100 {:op :knob-candidate/generate :seed 11})
        (is (= :accepted (:status (submit! rt 110 {:op :knob-candidate/audition :value 0.2}))))
        (audition-frames! rt 120)
        (let [staged @(:scene-draft rt)
              revision (get-in staged [:draft :revision])
              outputs (atom [(room-bytes rt)])]
          (doseq [[i value] (map-indexed vector [0.4 0.6 0.8 1.0])
                  :let [t (+ 300 (* 200 i))
                        ack (submit! rt t {:op :knob-candidate/audition :value value})]]
            (is (= [:accepted true (+ revision i 1) value] ((juxt :status :retuned? :draft-revision :value) ack)))
            (audition-frames! rt (+ t 10))
            (room-shows! rt value)
            (testing "the rendered Room output is a frame of the new value"
              (is (= (+ revision i 1) (:frame-revision @(:scene-draft rt))
                     (get-in (view rt) [:audition :frame-revision]) (get-in (show/snapshot rt) [:draft-output :frame-revision])))
              (is (not= (peek @outputs) (room-bytes rt)) "the Room packets change with the value")
              (swap! outputs conj (room-bytes rt))))
          (let [swept @(:scene-draft rt)]
            (is (identical? (:timeline staged) (:timeline swept)) "one private timeline for the whole sweep")
            (is (identical? (:caches staged) (:caches swept)) "the private compile caches are kept")
            (is (< (get-in staged [:frame :model :time-ms]) (get-in swept [:frame :model :time-ms]))
                "the private timeline keeps running")
            (is (= {:value 1.0 :status :auditioning :draft-revision (+ revision 4)}
                   (select-keys (:audition (view rt)) [:value :status :draft-revision])))
            (is (= (get-in (show/scene-draft rt) [:draft :revision]) (+ revision 4)))
            (is (= before (live-state (model rt))) "the live show is unchanged"))
          (testing "a committed live change restages a fresh private copy"
            (let [[cue id] (first ids)
                  _ (is (= :accepted (:status (submit! rt 1200 {:op :instance/opacity-set :instance-id id :value 0.5}))))
                  ack (submit! rt 1210 {:op :knob-candidate/audition :value 0.5})]
              (is (= :accepted (:status ack)) (pr-str cue))
              (is (not (:retuned? ack)))
              (is (not (identical? (:timeline staged) (:timeline @(:scene-draft rt)))))
              (audition-frames! rt 1220)
              (room-shows! rt 0.5)))
          (testing "an edit restages too, so a removed link leaves the Room"
            (let [timeline (:timeline @(:scene-draft rt))
                  link (first (:links (candidate rt)))]
              (is (= :accepted (:status (submit! rt 1400 {:op :knob-candidate/unlink :link-id (:id link)}))))
              (is (not (identical? timeline (:timeline @(:scene-draft rt)))))
              (audition-frames! rt 1410)
              (room-shows! rt 0.5)
              (is (:retuned? (submit! rt 1600 {:op :knob-candidate/audition :value 0.7})) "and sweeps coalesce again")))))
      (finally (show/stop! rt)))))

(deftest a-restart-drops-the-candidate-and-an-audition-needs-a-running-cue
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 11})
      (submit! rt 110 {:op :knob-candidate/audition :value 0.5})
      (is (some? (show/scene-draft rt)))
      (is (= :accepted (:status (submit! rt 120 {:op :show/restart}))))
      (testing "the candidate and its session go with the launches it came from"
        (is (nil? (candidate rt)))
        (is (nil? (show/scene-draft rt)))
        (is (= {:characters (:characters (view rt))} (view rt)))
        (is (= [:rejected :no-candidate] ((juxt :status :code) (submit! rt 130 {:op :knob-candidate/audition :value 0.5}))))
        (is (= :rejected (:status (submit! rt 140 {:op :knob-candidate/reroll}))) "nothing to reroll"))
      (testing "a later candidate does not reuse an earlier ID"
        (setup! rt)
        (submit! rt 200 {:op :knob-candidate/generate :seed 11})
        (is (= "candidate-2" (:id (candidate rt)))))
      (testing "released cues leave nothing to audition"
        (doseq [id (keys (:instances (model rt)))]
          (submit! rt 300 {:op :instance/end :instance-id id :force? true}))
        (is (empty? (filter #(= :running (:phase %)) (vals (:instances (model rt))))))
        (is (some? (candidate rt)) "a cue release keeps the candidate, like a knob keeps its links")
        (is (= [:rejected :no-running-cues]
               ((juxt :status :code) (submit! rt 310 {:op :knob-candidate/audition :value 0.5})))))
      (finally (show/stop! rt)))))

(deftest a-fresh-candidate-leaves-out-destinations-a-kept-knob-drives
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 21})
      (let [first-links (:links (candidate rt))
            first-destinations (set (map knobs/destination-key first-links))
            kept (submit! rt 110 {:op :knob-candidate/keep :name "Lift"})
            _ (submit! rt 120 {:op :knob-candidate/generate :seed 21})
            second-candidate (candidate rt)
            excluded (filter #(= :knob-driven (:reason %)) (:excluded (generate/eligibility (model rt) {:avoid (generate/knob-driven (model rt))})))]
        (is (= "generated-1" (:knob-id kept)))
        (is (= :ready (:status second-candidate)))
        (is (not-any? first-destinations (map knobs/destination-key (:links second-candidate))))
        (is (= (sort-by pr-str first-destinations) (get-in second-candidate [:recipe :avoid])))
        (is (= (count first-links) (count excluded)))
        (is (every? #(= ["generated-1"] (:knob-ids %)) excluded))
        (testing "the second knob repeats from its recipe although both knobs now drive destinations"
          (submit! rt 130 {:op :knob-candidate/keep :name "Second"})
          (submit! rt 140 {:op :knob-candidate/generate :from-knob "generated-2"})
          (is (= (:links second-candidate) (:links (candidate rt))))
          (is (= (:recipe second-candidate) (:recipe (candidate rt)))))
        (testing "sharing is an explicit opt-in that each shared link explains"
          (let [shared (for [seed (range 30)
                             :let [ack (submit! rt (+ 200 seed) {:op :knob-candidate/generate :seed seed
                                                                 :include-knob-driven? true})]
                             e (get-in (candidate rt) [:explain])
                             :when (:shared-with e)]
                         [ack e])]
            (is (seq shared))
            (is (every? (fn [[_ e]] (some #{"generated-1" "generated-2"} (:shared-with e))) shared))
            (is (= [:rejected :invalid-knob-candidate]
                   ((juxt :status :code) (submit! rt 300 {:op :knob-candidate/generate :include-knob-driven? "yes"})))))))
      (finally (show/stop! rt)))))

(deftest retunes-behind-an-in-flight-frame-land-on-the-last-value
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 11})
      (submit! rt 110 {:op :knob-candidate/audition :value 0.2})
      (frames! rt 120 3)
      (let [revision (get-in @(:scene-draft rt) [:draft :revision])]
        (with-redefs [show/render-audition-frame (slow-audition 150)]
          (show/tick! rt 300)
          (is (:in-flight @(:scene-draft rt)) "a slow frame at 0.2 is rendering")
          (let [a (submit! rt 310 {:op :knob-candidate/audition :value 0.4})
                b (submit! rt 320 {:op :knob-candidate/audition :value 0.9})]
            (is (= [true true] [(:retuned? a) (:retuned? b)]))
            (is (:in-flight @(:scene-draft rt)) "both retunes queued behind the in-flight frame"))
          (is (show/await-audition rt 5000))
          (room-shows! rt 0.2)
          (testing "the in-flight frame is labelled with the revision it rendered, not the new one"
            (is (= revision (:frame-revision @(:scene-draft rt))))
            (is (= [(+ revision 2) revision]
                   ((juxt :draft-revision :frame-revision) (:audition (view rt))))))
          (frames! rt 400 2))
        (room-shows! rt 0.9)
        (is (= (+ revision 2) (get-in (view rt) [:audition :frame-revision])) "the last value wins")
        (is (= (get-in @(:scene-draft rt) [:retune :seq]) (:retuned @(:scene-draft rt)))))
      (finally (show/stop! rt)))))

(deftest a-reroll-racing-an-in-flight-sweep-frame-never-installs-the-old-candidate
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 11})
      (submit! rt 110 {:op :knob-candidate/audition :value 0.3})
      (frames! rt 120 3)
      (is (:retuned? (submit! rt 200 {:op :knob-candidate/audition :value 0.6})))
      (let [timeline (:timeline @(:scene-draft rt))]
        (with-redefs [show/render-audition-frame (slow-audition 150)]
          (show/tick! rt 300)
          (is (:in-flight @(:scene-draft rt)))
          (submit! rt 310 {:op :knob-candidate/reroll})
          (is (not (identical? timeline (:timeline @(:scene-draft rt)))))
          (Thread/sleep 300)
          (is (zero? (or (:rendered @(:scene-draft rt)) 0)) "the old timeline's frame did not install")
          (frames! rt 400 2))
        (room-shows! rt 0.6))
      (finally (show/stop! rt)))))

(deftest a-stopped-transport-holds-the-room-across-retunes
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 11})
      (frames! rt 120 5)
      (submit! rt 300 {:op :transport/set :transport {:playing? false}})
      (frames! rt 310 3)
      (submit! rt 370 {:op :knob-candidate/audition :value 0.3})
      (frames! rt 375 10)
      (let [held (room-bytes rt)
            beat (get-in @(:scene-draft rt) [:frame :model :transport :beat])]
        (frames! rt 600 10)
        (is (= held (room-bytes rt)) "a stopped Room holds without a value change")
        (is (:retuned? (submit! rt 900 {:op :knob-candidate/audition :value 0.8})))
        (frames! rt 910 3)
        (let [retuned (room-bytes rt)]
          (is (not= held retuned) "the value reaches the stopped Room")
          (frames! rt 1000 10)
          (is (= retuned (room-bytes rt)) "and the Room stays held after the retune")
          (is (= beat (get-in @(:scene-draft rt) [:frame :model :transport :beat])) "no catch-up")
          (is (:retuned? (submit! rt 1300 {:op :knob-candidate/audition :value 0.3})))
          (frames! rt 1310 3)
          ;; Back at the first value the Room model matches it again. The packets may
          ;; differ by a moving head's pan wrapping 360°: pan continuity follows the
          ;; previous packet, as it would on a live sweep.
          (room-shows! rt 0.3)
          (is (= beat (get-in @(:scene-draft rt) [:frame :model :transport :beat])))))
      (finally (show/stop! rt)))))

(deftest rerolls-after-a-recipe-repeat-still-leave-out-knob-driven-destinations
  (let [rt (runtime)]
    (try
      (setup! rt)
      (submit! rt 100 {:op :knob-candidate/generate :seed 21})
      (let [kept-destinations (set (map knobs/destination-key (:links (candidate rt))))]
        (submit! rt 110 {:op :knob-candidate/keep :name "Lift"})
        (is (nil? (get-in (model rt) [:knobs :knobs "generated-1" :recipe :avoid])) "the first knob's recipe avoided nothing")
        (is (= [:accepted :ready] ((juxt :status :candidate-status)
                                   (submit! rt 120 {:op :knob-candidate/generate :from-knob "generated-1"}))))
        (is (= kept-destinations (set (map knobs/destination-key (:links (candidate rt))))) "the repeat is the kept knob")
        (doseq [i (range 6)
                :let [ack (submit! rt (+ 130 (* 10 i)) {:op :knob-candidate/reroll})
                      c (candidate rt)]]
          (is (= [:accepted :ready] ((juxt :status :candidate-status) ack)))
          (is (not-any? kept-destinations (map knobs/destination-key (:links c))) (str "reroll " i))
          (is (not-any? :shared-with (:explain c)))
          (is (= (sort-by pr-str kept-destinations) (sort-by pr-str (get-in c [:recipe :avoid])))))
        (testing "a recipe made with sharing keeps sharing on its rerolls"
          (submit! rt 300 {:op :knob-candidate/generate :seed 5 :include-knob-driven? true})
          (is (true? (get-in (candidate rt) [:recipe :include-knob-driven?])))
          (submit! rt 310 {:op :knob-candidate/keep :name "Shared"})
          (submit! rt 320 {:op :knob-candidate/generate :from-knob "generated-2"})
          (is (true? (get-in (view rt) [:session :options :include-knob-driven?])))
          (is (some :shared-with (for [i (range 12)
                                       :let [_ (submit! rt (+ 330 (* 10 i)) {:op :knob-candidate/reroll})]
                                       e (:explain (candidate rt))]
                                   e)))))
      (finally (show/stop! rt)))))
