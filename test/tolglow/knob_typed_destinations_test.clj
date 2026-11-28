(ns tolglow.knob-typed-destinations-test
  "Generated macros for every parameter type (#667): numbers, integers, grid
  numbers, colours, enums, booleans, vectors and beat ratios. The generator
  drafts each within its schema, and a knob built from those drafts is swept
  through the real show owner: every committed value satisfies its schema,
  stepped destinations move monotonically, and a knob dithering on a boundary
  never makes a destination chatter. Stepped links are smoothed by an
  envelope in musical time and their step crossings are scheduled, which
  commits exactly what per-frame evaluation would."
  (:require [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [clojure.test :refer [deftest is testing]]
            [tolglow.clock :as show-clock]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.integrations.agent :as agent]
            [tolglow.knob-candidates :as knob-candidates]
            [tolglow.knob-candidates.generate :as generate]
            [tolglow.knob-presets :as knob-presets]
            [tolglow.knobs :as knobs]
            [tolglow.modulation :as modulation]
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
   ;; The dimmer follows the grid number, so captured frames show a stepped destination.
   :outputs {:dimmer {:op :multiply :args [{:op :param :key :zoom} {:op :const :value 0.25}]}}})

(def ^:private schemas (:params effect))

(def ^:private wide
  "A 64-step integer and a 12-value choice, for relative encoder moves."
  {:id :wide :version 1
   :params {:count {:type :integer :default 1 :bounds [1 64]}
            :mode {:type :enum :values (vec (map #(keyword (str "m" %)) (range 12))) :default :m0}}
   :outputs {:dimmer {:op :const :value 0.0}}})

(defn- base-model
  ([] (base-model 120.0))
  ([bpm]
   (control/new-show {:show-id :typed :transport {:playing? true :beat 0.0 :bpm bpm}
                      :registry {:effects {:typed effect :wide wide}
                                 :cues {:typed {:id :typed :effect-id :typed :version 1}
                                        :wide {:id :wide :effect-id :wide :version 1}}}
                      :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}})))

(defn- running-model
  ([] (running-model 120.0))
  ([bpm]
   (let [{:keys [model ack]} (control/apply-command (base-model bpm)
                                                    {:op :cue/launch :cue-id :typed :launch {:mode :persistent}
                                                     :command-id "launch" :show-id :typed :epoch 0
                                                     :source {:adapter :test} :time-ms 0})]
     (assert (= :accepted (:status ack)) (pr-str ack))
     model)))

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
      (is (not (contains? link :hysteresis)) "version 4 never emits hysteresis")
      (is (= (when (knobs/stepped? link schema) knobs/stepped-envelope) (:envelope link))
          (str "stepped links are smoothed, continuous ones follow at once: " (pr-str link)))
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
                    (is (nil? (:envelope link)) "a continuous curve through space follows the knob at once"))
        :bands (is (= :integer (:quantize link)))
        :beats (is (= {:ratios :preferred} (:quantize link)))
        nil))
    (testing "explanations name choices, switches and smoothing"
      (let [whys (map :why (for [seed (range 300)
                                 :let [c (generate/generate eligibility nil {:seed seed :size [4 8]})]
                                 e (:explain c)]
                             e))]
        (is (some #(re-find #"^Choice \w+ → \w+" %) whys))
        (is (some #(re-find #"^Switch off → on .* switching at knob 0\.(5|75|625)" %) whys))
        (is (some #(re-find #"smoothed over 1 beat for the whole range" %) whys))))))

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
      (is (not-any? #(or (:hysteresis %) (:envelope %)) (:links c)) "no smoothing before version 4"))))

;; ---------------------------------------------------------------------------
;; Envelopes and scheduled steps through the real owner

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

(defn- monotone?
  "Whether a stepped destination moves one way through its steps: each
  component of a vector one way, any other value one way in its link order."
  [link xs _]
  (let [one-way? (fn [ords] (or (apply <= ords) (apply >= ords)))]
    (if (vector? (first xs))
      (every? #(one-way? (map (fn [x] (double (nth x %))) xs)) (range (count (first xs))))
      (one-way? (map #(double (order-of link %)) xs)))))


(def ^:private stepped-links
  "One hand-made link per stepped type, all with the stepped default envelope,
  plus a continuous level link smoothed the same way."
  (mapv #(assoc % :envelope knobs/stepped-envelope)
        [{:id "shape" :destination {:kind :param :cue-id :typed :param :shape} :range [:dot :wave]}
         {:id "strobe" :destination {:kind :param :cue-id :typed :param :strobe} :range [false true]}
         {:id "bands" :destination {:kind :param :cue-id :typed :param :bands} :range [1 8] :curve :ease-in}
         {:id "zoom" :destination {:kind :param :cue-id :typed :param :zoom} :range [0.0 4.0] :quantize {:step 0.25}}
         {:id "beats" :destination {:kind :param :cue-id :typed :param :beats} :range [2 1/2]
          :quantize {:ratios :preferred}}
         {:id "extent" :destination {:kind :param :cue-id :typed :param :extent}
          :range [[0.0 0.0 0.0] [6.0 3.0 1.5]] :quantize {:step 0.5}}
         {:id "level" :destination {:kind :param :cue-id :typed :param :level} :range [0.0 1.0]}]))

(def ^:private stepped-params [:shape :strobe :bands :zoom :beats :extent])

(defn- packets-runtime
  "A runtime with a capture sink. Returns [rt packets]."
  ([] (packets-runtime (running-model)))
  ([model]
   (let [packets (atom [])
         sink {:send! (fn [universes _] (swap! packets conj (vec (get universes 1))))}]
     [(show/create-runtime {:model model :sink sink :workspace-options nil}) packets])))

(defn- model-of [rt] (:model @(:state rt)))

(defn- committed [rt]
  (let [instance (some #(when (= :typed (:cue-id %)) %) (vals (:instances (model-of rt))))]
    (select-keys (:params instance) (keys schemas))))

(defn- turn! [rt t value]
  (let [ack (submit! rt t {:op :knob/set :knob-id "k" :value (double value)})]
    (assert (= :accepted (:status ack)) (pr-str ack))
    ack))

(defn- level-of [rt link-id]
  (let [model (model-of rt) knob (get-in model [:knobs :knobs "k"])]
    (when knob (knobs/output-level model knob (some #(when (= link-id (:id %)) %) (:links knob))))))

(defn- frames!
  "Tick from t in 20 ms frames n times, returning [t beat committed level] per frame."
  [rt t n]
  (vec (for [i (range n) :let [time (+ t (* 20 i))]]
         (do (show/tick! rt time)
             [time (get-in (model-of rt) [:transport :beat]) (committed rt) (level-of rt "shape")]))))

(defn- create! [rt t links]
  (is (= :accepted (:status (submit! rt t {:op :knob/create :knob-id "k" :name "K" :links links})))))

(defn- param-writes
  "Committed :param/set commands on param, from the shared receipt history."
  [rt param]
  (let [model (model-of rt)]
    (filter #(and (= :param/set (:op %)) (= param (:param %)))
            (map #(get-in model [:control :history % :command]) (get-in model [:control :history-order])))))

(defn- counting
  "Run f with knobs/resolve-value counted for stepped links. Returns [result count]."
  [f]
  (let [n (atom 0) resolve knobs/resolve-value]
    (with-redefs [knobs/resolve-value (fn [link & args]
                                        (when-not (= "level" (:id link)) (swap! n inc))
                                        (apply resolve link args))]
      [(f) @n])))

(deftest scheduled-steps-commit-exactly-what-per-frame-evaluation-commits
  ;; Run A: one knob turn with envelopes; the owner commits stepped links from
  ;; each move's crossing schedule. Run B: the same links without envelopes,
  ;; turned every frame to run A's smoothed level, so every link is evaluated
  ;; every frame. Both runs see identical beats, so their committed values and
  ;; captured frames must match frame for frame.
  (let [plan (fn [frame] (cond (= frame 5) 1.0 (= frame 22) 0.35 (= frame 40) 0.9 :else nil))
        run-a (fn []
                (let [[rt packets] (packets-runtime)]
                  (try
                    (create! rt 0 stepped-links)
                    (turn! rt 20 0.0)
                    (let [rows (vec (for [frame (range 90) :let [t (+ 100 (* 20 frame))]]
                                      (do (if-let [v (plan frame)] (turn! rt t v) (show/tick! rt t))
                                          {:levels (into {} (for [id ["shape" "level"]] [id (level-of rt id)]))
                                           :committed (committed rt) :packet (peek @packets)})))]
                      {:rows rows :writes (count (mapcat #(param-writes rt %) stepped-params))})
                    (finally (show/stop! rt)))))
        [a a-evals] (counting run-a)
        run-b (fn []
                (let [[rt packets] (packets-runtime)]
                  (try
                    ;; An empty envelope opts out of the stepped default.
                    (create! rt 0 (mapv #(assoc % :envelope {}) stepped-links))
                    (turn! rt 20 0.0)
                    (vec (for [[frame {:keys [levels]}] (map-indexed vector (:rows a))
                               :let [t (+ 100 (* 20 frame))]]
                           (do (turn! rt t (get levels "shape"))
                               {:committed (committed rt) :packet (peek @packets)})))
                    (finally (show/stop! rt)))))
        [b b-evals] (counting run-b)]
    (testing "every frame commits the same values and renders the same packet"
      ;; A frame whose smoothed level lies on a step boundary may take either side
      ;; (a crossing fires at its beat; per-frame evaluation takes the upper step),
      ;; so those frames are not compared exactly.
      (let [schema #(schemas (param-of %))
            on-boundary? (fn [level] (some (fn [link] (some #(< (Math/abs (- level %)) 1e-6)
                                                           (knobs/step-boundaries link (schema link))))
                                           (butlast stepped-links)))]
        (doseq [[frame ra rb] (map vector (range) (:rows a) b)
                :when (not (on-boundary? (get-in ra [:levels "shape"])))]
          (is (= (:committed ra) (:committed rb)) (str "frame " frame))
          (is (= (:packet ra) (:packet rb)) (str "packet " frame)))))
    (testing "the moves actually crossed steps both ways"
      (is (< 3 (count (distinct (map (comp :shape :committed) (:rows a)))))))
    (testing "scheduled steps evaluate far less than per-frame evaluation"
      ;; A: each stepped move evaluates a segment once when a frame first reaches
      ;; it, and at frames within 1e-6 of a boundary. B resolves every link on every frame.
      (is (< (* 3 a-evals) b-evals) (str a-evals " vs " b-evals))
      (println "stepped-link evaluations, scheduled vs per frame:" a-evals b-evals)
      (is (< (:writes a) (* 90 (count stepped-params))) "stepped writes only where a step changed"))))

(deftest a-noisy-input-shorter-than-the-attack-never-changes-a-step
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
        links (concat (butlast stepped-links)
                      [{:id "level" :destination {:kind :param :cue-id :typed :param :level} :range [0.0 1.0]}])]
    (try
      (create! rt 0 (vec links))
      (turn! rt 20 0.3)
      (frames! rt 40 40)
      (let [settled (select-keys (committed rt) stepped-params)
            writes (count (mapcat #(param-writes rt %) stepped-params))
            rng (java.util.Random. 667)
            levels (atom #{})]
        ;; Twenty one-frame spikes (1/25 beat) to 0.95, at seeded gaps.
        (loop [t 900 n 0]
          (when (< n 20)
            (turn! rt t 0.95)
            (swap! levels conj (:level (committed rt)))
            (turn! rt (+ t 20) 0.3)
            (frames! rt (+ t 40) (+ 2 (.nextInt rng 6)))
            (recur (+ t 400) (inc n))))
        (frames! rt 9000 40)
        (is (contains? @levels 0.95) "the unsmoothed level link saw every spike")
        (is (= settled (select-keys (committed rt) stepped-params)) "no stepped destination moved")
        (is (= writes (count (mapcat #(param-writes rt %) stepped-params))) "nothing was committed for them")
        (testing "a sustained move does change them"
          (turn! rt 10000 0.95)
          (frames! rt 10020 60)
          (is (not= settled (select-keys (committed rt) stepped-params)))))
      (finally (show/stop! rt)))))

(deftest a-sweep-is-monotone-and-settles-on-the-plain-step
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
    (try
      (create! rt 0 stepped-links)
      (frames! rt 20 5)
      (testing "one turn to the top"
        (turn! rt 200 1.0)
        (let [rows (frames! rt 220 60)]
          (doseq [link (butlast stepped-links)
                  :let [param (param-of link) schema (schemas param)
                        seen (map #(get (nth % 2) param) rows)]]
            (is (every? #(params/valid-value? schema %) seen))
            (is (monotone? link seen true) (str param))
            (is (= (knobs/resolve-value link schema 1.0) (last seen)) (str param " settles on the plain step at 1.0"))
            (when-not (= :vector (:type schema))
              (is (= (dec (knobs/step-count link schema)) (changes (cons (knobs/resolve-value link schema 0.0) seen)))
                  (str param " visits each step once"))))))
      (testing "a slow hand sweep back down, turned every frame"
        (let [rows (vec (for [i (range 60) :let [t (+ 2000 (* 20 i))]]
                          (do (turn! rt t (- 1.0 (/ i 59.0))) (committed rt))))
              rows (into rows (map #(nth % 2) (frames! rt 3300 60)))]
          (doseq [link (butlast stepped-links) :let [param (param-of link) seen (map param rows)]]
            (is (monotone? link seen false) (str param " falling"))
            (is (= (knobs/resolve-value link (schemas param) 0.0) (last seen)) (str param " at 0")))))
      (finally (show/stop! rt)))))

(deftest a-retarget-takes-over-the-move-from-where-it-is
  (doseq [[label first-target second-target] [["up then one step further" 0.5 0.7]
                                              ["up then several steps further" 0.3 1.0]
                                              ["up then one step back" 0.62 0.45]
                                              ["up then several steps back" 0.9 0.05]]]
    (testing label
      (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
            shape (first stepped-links)]
        (try
          (create! rt 0 [shape])
          (turn! rt 20 0.0)
          (frames! rt 40 3)
          (turn! rt 100 1.0)
          ;; Run until the smoothed level passes first-target, then retarget.
          (let [before (loop [t 120 rows []]
                         (let [[row] (frames! rt t 1) rows (conj rows row)]
                           (if (or (>= (nth row 3) first-target) (> t 2000)) rows (recur (+ t 20) rows))))
                at (peek before)
                level-before (nth at 3)
                t (+ 20 (first at))
                _ (turn! rt t second-target)
                turned [t nil (committed rt) (level-of rt "shape")]
                after (frames! rt (+ t 20) 80)
                rows (concat before [turned] after)
                levels (map #(nth % 3) rows)
                shapes (map #(:shape (nth % 2)) rows)
                values (:values (schemas :shape))
                index #(.indexOf ^java.util.List values %)]
            (is (every? (fn [[x y]] (< (Math/abs (- x y)) 0.0401)) (partition 2 1 levels))
                "the smoothed level carries on from where it was: no jump, never back to the old start")
            (is (< (Math/abs (- (nth (first after) 3) level-before)) 0.0801))
            (is (every? (fn [[x y]] (<= (Math/abs (- (index x) (index y))) 1)) (partition 2 1 shapes))
                "no step is skipped")
            (is (= (knobs/resolve-value shape (schemas :shape) second-target) (last shapes)) "it settles on the new target")
            (let [writes (map :value (param-writes rt :shape))]
              (is (= :dot (first writes)) "the turn to 0 wrote the first step")
              (is (every? (fn [[x y]] (not= x y)) (partition 2 1 writes)) "no step is committed twice in a row")
              (is (= (changes (cons :dot shapes)) (dec (count writes))) "each change was committed once")))
          (finally (show/stop! rt)))))))

(deftest relative-encoder-moves-reach-both-ends
  (let [model (:model (control/apply-command (running-model)
                                             {:op :cue/launch :cue-id :wide :launch {:mode :persistent}
                                              :command-id "wide" :show-id :typed :epoch 0 :source {:adapter :test} :time-ms 0}))
        rt (show/create-runtime {:model model :workspace-options nil})
        wide-params (fn [rt] (let [i (some (fn [x] (when (= :wide (:cue-id x)) x)) (vals (:instances (model-of rt))))]
                               (select-keys (:params i) [:count :mode])))
        links [{:id "count" :destination {:kind :param :cue-id :wide :param :count} :range [1 64]
                :envelope knobs/stepped-envelope}
               {:id "mode" :destination {:kind :param :cue-id :wide :param :mode} :range [:m0 :m11]
                :envelope knobs/stepped-envelope}]]
    (try
      (create! rt 0 links)
      (let [up (vec (for [i (range 110) :let [t (+ 20 (* 20 i))]]
                      (let [ack (submit! rt t (knobs/encoder-command "k" 1 false))]
                        (assert (= :accepted (:status ack)) (pr-str ack))
                        (wide-params rt))))
            up (into up (map (fn [t] (show/tick! rt t) (wide-params rt)) (range 2300 3500 20)))]
        (is (= {:count 64 :mode :m11} (last up)) "Push coarse detents reach the top endpoints")
        (is (apply <= (map :count up)))
        (is (= 63 (changes (cons 1 (map :count up)))) "every whole number on the way")
        (let [down (vec (for [i (range 110) :let [t (+ 4000 (* 20 i))]]
                          (do (submit! rt t (knobs/encoder-command "k" -1 false)) (wide-params rt))))
              down (into down (map (fn [t] (show/tick! rt t) (wide-params rt)) (range 6300 7500 20)))]
          (is (= {:count 1 :mode :m0} (last down)) "and the bottom ones")
          (is (apply >= (map :count down)))))
      (finally (show/stop! rt)))))

(deftest envelopes-stretch-with-the-tempo-and-freeze-on-stop
  (let [crossings (fn [bpm]
                    (let [rt (show/create-runtime {:model (running-model bpm) :workspace-options nil})]
                      (try
                        (create! rt 0 [(first stepped-links)])
                        (frames! rt 20 3)
                        (turn! rt 100 1.0)
                        (let [start (get-in (model-of rt) [:knobs :knobs "k" :motion "shape" :start])
                              rows (frames! rt 120 (long (* 2 (/ 60000 bpm) (/ 1 20))))]
                          ;; Beats since the move started.
                          (keep (fn [[[_ _ a] [_ beat b]]] (when (not= (:shape a) (:shape b)) [(:shape b) (- beat start)]))
                                (partition 2 1 rows)))
                        (finally (show/stop! rt)))))
        slow (crossings 70.0) fast (crossings 180.0)]
    (testing "the same steps cross at the same beats at 70 and 180 BPM"
      (is (= [:bar :ring :star :wave] (map first slow) (map first fast)))
      (doseq [[[_ a] [_ b]] (map vector slow fast)]
        ;; Within one frame of beats at the faster tempo.
        (is (< (Math/abs (- a b)) 0.0601)))))
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
    (try
      (create! rt 0 [(first stepped-links)])
      (frames! rt 20 3)
      (turn! rt 100 1.0)
      (frames! rt 120 12)
      (is (= :accepted (:status (submit! rt 360 {:op :transport/set :transport {:playing? false}}))))
      (let [level (level-of rt "shape") shape (:shape (committed rt))
            writes (count (param-writes rt :shape))]
        (let [held (frames! rt 380 50)]
          (is (every? #(= level (nth % 3)) held) "the move holds while the transport is stopped")
          (is (every? #(= shape (:shape (nth % 2))) held))
          (is (= writes (count (param-writes rt :shape)))))
        (is (= :accepted (:status (submit! rt 1400 {:op :transport/set :transport {:playing? true}}))))
        (let [[[_ _ _ resumed]] (frames! rt 1420 1)]
          (is (< (- resumed level) 0.0801) "it resumes from where it stopped, without catching up")))
      (finally (show/stop! rt)))))

(deftest knob-looks-recall-the-captured-step
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
        shape (first stepped-links)]
    (try
      (create! rt 0 [shape])
      (turn! rt 20 0.25)
      (frames! rt 40 40)
      (is (= :bar (:shape (committed rt))))
      (is (= :accepted (:status (submit! rt 900 {:op :knob-preset/capture :slot 0}))))
      (turn! rt 920 0.0)
      (frames! rt 940 40)
      (is (= :dot (:shape (committed rt))))
      (is (= :accepted (:status (submit! rt 1800 {:op :knob-preset/recall :slot 0}))))
      (frames! rt 1820 40)
      (is (= (knobs/resolve-value shape (schemas :shape) 0.25) (:shape (committed rt)))
          "the recalled look settles on the step at the captured position")
      (finally (show/stop! rt)))))

(defn- shape-move!
  "A knob with the shape link, turned from 0 to 1 at t 200 and run n frames."
  [rt n]
  (create! rt 0 [(first stepped-links)])
  (turn! rt 20 0.0)
  (frames! rt 40 5)
  (turn! rt 200 1.0)
  (frames! rt 220 n))

(defn- shape-writes [rt] (mapv :value (param-writes rt :shape)))

(deftest envelope-state-is-saved-validated-and-dropped-per-link
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
    (try
      (create! rt 0 [(first stepped-links) (second stepped-links)])
      (turn! rt 20 1.0)
      (frames! rt 40 5)
      (let [state (get-in (model-of rt) [:knobs])
            motion (get-in state [:knobs "k" :motion])
            beat (get-in (model-of rt) [:transport :beat])]
        (is (= 2 (:schema-version state)))
        (is (= #{"shape" "strobe"} (set (keys motion))) "both links are moving")
        (is (every? :crossings (vals motion)) "each stepped move has its crossing beats")
        (testing "a restore keeps well-formed moves for links that still exist, as fresh moves"
          (let [stale (-> state
                          (assoc-in [:knobs "k" :motion "gone"] {:from 0.2 :start 1.0 :beats 1.0})
                          (update-in [:knobs "k" :links] (partial filterv #(not= "strobe" (:id %)))))
                restored (knobs/restore stale {} 1 beat)
                entry (get-in restored [:knobs "k" :motion "shape"])]
            (is (= #{"shape"} (set (keys (get-in restored [:knobs "k" :motion])))))
            (is (= #{:from :start :beats :written :turn} (set (keys entry)))
                "crossings, counters and the recorded source are planned or supplied again")))
        (testing "a malformed move is dropped per link, never refused or thrown every frame"
          (doseq [bad [{:from 0.0 :start 1e9 :beats 1.0 :commits "x" :source "not-a-map"}
                       {:from 2.0 :start 1.0 :beats 1.0}
                       {:from 0.5 :start ##NaN :beats 1.0}
                       {:from 0.5 :start 1.0 :beats -1.0}
                       {:from 0.5 :start 1.0 :beats 1.0 :source {:adapter :studio :password "x"}}]]
            (let [restored (knobs/restore (assoc-in state [:knobs "k" :motion "shape"] bad) {} 1 beat)]
              (is (nil? (get-in restored [:knobs "k" :motion "shape"])) (pr-str bad))))
          (let [future (knobs/restore (assoc-in state [:knobs "k" :motion "shape"] {:from 0.1 :start 1e9 :beats 1.0}) {} 1 beat)]
            (is (= beat (get-in future [:knobs "k" :motion "shape" :start])) "a future start is clamped to the restored beat"))
          (testing "a move that cannot be advanced is dropped instead of failing every frame"
            (swap! (:state rt) assoc-in [:model :knobs :knobs "k" :motion "shape" :crossings] ["not" "beats"])
            (frames! rt 200 3)
            (is (nil? (get-in (model-of rt) [:knobs :knobs "k" :motion "shape"])))
            (is (not (:error (:status @(:state rt)))) "the frame still renders")
            (is (= [:move-failed "k" "shape"] ((juxt :code :knob-id :link-id) (last (:diagnostics (:knobs (show/snapshot rt))))))
                "and the drop is recorded where Studio, the REPL and agents read knobs")))
        (testing "version 1 state restores as version 2, and a backup without version 2 fields writes version 1"
          (let [plain (-> state (assoc :schema-version 1)
                          (update-in [:knobs "k"] dissoc :motion :created-by)
                          (update-in [:knobs "k" :links] (partial mapv #(dissoc % :envelope))))]
            (is (= 2 (:schema-version (knobs/restore plain {} 1))))
            (is (= 1 (:schema-version (knobs/saved-state (assoc plain :schema-version 2)))))
            (is (= 2 (:schema-version (knobs/saved-state state)))))))
      (testing "unlinking a link drops its move"
        (is (= :accepted (:status (submit! rt 220 {:op :knob/unlink :knob-id "k" :link-id "strobe"}))))
        (is (nil? (get-in (model-of rt) [:knobs :knobs "k" :motion "strobe"]))))
      (finally (show/stop! rt)))))

(deftest a-seek-loop-or-link-resume-keeps-the-remaining-musical-time
  (doseq [offset [-8.0 8.0 -0.3]]
    (testing (str "rebase by " offset " beats mid-move")
      (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
        (try
          (shape-move! rt 12)
          (let [level (level-of rt "shape") writes (shape-writes rt) m (model-of rt)]
            (swap! (:state rt) update :model show-clock/rebase {:beat (+ (get-in m [:transport :beat]) offset)
                                                               :time-ms (:time-ms m)})
            (is (< (Math/abs (- level (level-of rt "shape"))) 1e-9) "the level is where it was")
            (let [rows (frames! rt 460 60)]
              (is (every? (fn [[x y]] (<= 0 (- y x) 0.0401)) (partition 2 1 (cons level (map #(nth % 3) rows))))
                  "it carries on at its rate: no snap back, no catch-up")
              (let [after (shape-writes rt)]
                (is (= writes (subvec after 0 (count writes))))
                (is (apply distinct? (drop (dec (count writes)) after)) "no past step fires again")
                (is (= :wave (last after))))))
          (finally (show/stop! rt))))))
  (testing "a Link-following resume after a stop"
    (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
      (try
        (shape-move! rt 12)
        (submit! rt 460 {:op :transport/set :transport {:playing? false}})
        (let [level (level-of rt "shape") writes (shape-writes rt) m (model-of rt)]
          (frames! rt 480 10)
          ;; A Link session resumes at another beat; the owner rebases the show onto it.
          (swap! (:state rt) update :model show-clock/rebase {:beat (+ 37.5 (get-in m [:transport :beat])) :playing? true
                                                             :time-ms 780 :source :link :source-epoch 1})
          (let [[[_ _ _ resumed]] (frames! rt 800 1)]
            (is (<= 0 (- resumed level) 0.0401) "resume without catch-up"))
          (is (= writes (shape-writes rt)) "nothing re-fired on resume"))
        (finally (show/stop! rt))))))

(deftest another-write-ends-the-move-and-shared-destinations-do-not-alternate
  (testing "a hand edit mid-move stands"
    (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
      (try
        (shape-move! rt 6)
        (let [id (some #(when (= :typed (:cue-id %)) (:id %)) (vals (:instances (model-of rt))))]
          (is (= :accepted (:status (submit! rt 340 {:op :param/set :instance-id id :param :shape :value :wave}))))
          (frames! rt 360 60)
          (is (= :wave (:shape (committed rt))) "the edit is not reverted")
          (is (nil? (get-in (model-of rt) [:knobs :knobs "k" :motion "shape"])) "the move ended")
          (is (= :wave (last (shape-writes rt))))
          (testing "an equal-value write also ends a move: holding the current step works"
            (turn! rt 1500 0.0)
            (frames! rt 1520 3)
            (turn! rt 1600 1.0)
            (frames! rt 1620 8)
            (let [held (:shape (committed rt))]
              (is (= :accepted (:status (submit! rt 1800 {:op :param/set :instance-id id :param :shape :value held}))))
              (is (nil? (get-in (model-of rt) [:knobs :knobs "k" :motion "shape"])) "the move ended at once"
                  )
              (frames! rt 1820 60)
              (is (= held (:shape (committed rt))))))
          (testing "turning the knob again moves the destination, as for any knob"
            (turn! rt 3200 0.5)
            (frames! rt 3220 60)
            (is (= :ring (:shape (committed rt))))))
        (finally (show/stop! rt)))))
  (testing "two enveloped knobs on one destination"
    (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
          shape (first stepped-links)]
      (try
        (create! rt 0 [shape])
        (is (= :accepted (:status (submit! rt 10 {:op :knob/create :knob-id "j" :name "J" :links [shape]}))))
        (turn! rt 20 0.0)
        (frames! rt 40 3)
        (turn! rt 200 1.0)
        (is (= :accepted (:status (submit! rt 220 {:op :knob/set :knob-id "j" :value 0.0}))))
        (let [before (count (param-writes rt :shape))
              rows (frames! rt 240 80)
              writes (drop before (shape-writes rt))]
          (is (every? (fn [[x y]] (not= x y)) (partition 2 1 writes)) "no ping-pong between the knobs")
          (is (< (count writes) 8) (str "a handful of commits, not one per frame: " (vec writes)))
          (is (empty? (mapcat #(keys (get-in (model-of rt) [:knobs :knobs % :motion])) ["k" "j"]))
              "the knob whose destination was taken ends its move"))
        (finally (show/stop! rt))))))

(deftest replays-at-different-frame-rates-commit-the-same-sequence
  (let [run (fn [frame-ms]
              (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
                (try
                  (create! rt 0 (vec (butlast stepped-links)))
                  (turn! rt 200 0.0)
                  ;; Turns at the same musical times: 1 beat = 500 ms at 120 BPM.
                  (doseq [[t v] [[400 1.0] [800 0.3] [1100 0.9] [1300 0.0]]]
                    (doseq [frame (range (+ 200 frame-ms) t frame-ms)] (show/tick! rt frame))
                    (turn! rt t v))
                  (doseq [frame (range 1300 3500 frame-ms)] (show/tick! rt frame))
                  (into {} (for [p stepped-params] [p (mapv :value (param-writes rt p))]))
                  (finally (show/stop! rt)))))
        a (run 25) b (run 40)]
    (doseq [p stepped-params]
      (is (= (get a p) (get b p)) (str p " commits the same steps at 25 ms and 40 ms frames")))
    (is (< 3 (count (:shape a))))))

(deftest tempo-meter-and-envelope-edits-mid-move
  (testing "a tempo change mid-move keeps the beat schedule"
    (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
      (try
        (shape-move! rt 6)
        (submit! rt 340 {:op :transport/set :transport {:bpm 70.0}})
        (let [rows (frames! rt 360 120)
              levels (map #(nth % 3) rows)]
          (is (every? (fn [[x y]] (<= 0 (- y x) 0.0401)) (partition 2 1 levels)))
          (is (= :wave (:shape (committed rt)))))
        (finally (show/stop! rt)))))
  (testing "a bar envelope is fixed when its move starts, so a meter change does not jump"
    (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
          shape (assoc (first stepped-links) :envelope {:attack {:bars 1/2} :release {:bars 1/2}})]
      (try
        (create! rt 0 [shape])
        (turn! rt 20 0.0)
        (turn! rt 200 1.0)
        (frames! rt 220 5)
        (let [level (level-of rt "shape")]
          (swap! (:state rt) assoc-in [:model :transport :meter] {:beats-per-bar 7 :bars-per-phrase 4})
          (is (< (Math/abs (- level (level-of rt "shape"))) 1e-9)))
        (finally (show/stop! rt)))))
  (testing "editing a moving link's envelope carries the move on from where it is"
    (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})]
      (try
        (shape-move! rt 8)
        (let [level (level-of rt "shape") shape (:shape (committed rt))
              ack (submit! rt 380 {:op :knob/link :knob-id "k"
                                   :link (assoc (first stepped-links) :envelope {:attack {:beats 2} :release {:beats 2}})})]
          (is (= :accepted (:status ack)))
          (is (< (Math/abs (- level (level-of rt "shape"))) 0.0401) "the new link starts from the old one's level")
          (is (= shape (:shape (committed rt))))
          (frames! rt 400 120)
          (is (= :wave (:shape (committed rt)))))
        (testing "replacing it with an unsmoothed link writes its plain value"
          (turn! rt 3000 0.0)
          (frames! rt 3020 3)
          (submit! rt 3100 {:op :knob/link :knob-id "k" :link (assoc (first stepped-links) :envelope {})})
          (is (= :dot (:shape (committed rt))))
          (is (nil? (get-in (model-of rt) [:knobs :knobs "k" :motion]))))
        (finally (show/stop! rt))))))

(deftest a-link-from-an-older-backup-keeps-no-envelope-when-edited
  (let [model (running-model)
        old {:schema-version 1 :next-order 1
             :knobs {"k" {:id "k" :name "Old" :value 0.0 :order 0
                          :links [{:id "s" :destination {:kind :param :cue-id :typed :param :shape} :range [:dot :wave]
                                   :direction :up :curve :linear :input [0 1]}]}}}
        restored (assoc model :knobs (knobs/restore old {} 0 0.0))
        apply! (fn [model command] (:model (control/apply-domain-command model (merge {:command-id (str (random-uuid))
                                                                                         :show-id :typed :epoch 0
                                                                                         :source {:adapter :studio}} command)
                                                                           knobs/apply-operation)))
        link (get-in restored [:knobs :knobs "k" :links 0])]
    (is (not (contains? link :envelope)) "a link restored from version 1 has none and steps at once")
    (testing "an edit that carries {} keeps it unsmoothed (Studio's edited-link sends {})"
      (let [edited (apply! restored {:op :knob/link :knob-id "k" :link (assoc link :direction :down :envelope {})})]
        (is (= {} (get-in edited [:knobs :knobs "k" :links 0 :envelope])))
        (is (empty? (get-in (apply! edited {:op :knob/set :knob-id "k" :value 1.0}) [:knobs :knobs "k" :motion]))
            "a turn steps at once")))
    (testing "an empty envelope on a continuous link is dropped, so plain backups stay version 1"
      (let [with-level (apply! restored {:op :knob/link :knob-id "k"
                                         :link {:id "l" :destination {:kind :param :cue-id :typed :param :level}
                                                :range [0.0 1.0] :envelope {}}})]
        (is (not (contains? (get-in with-level [:knobs :knobs "k" :links 1]) :envelope)))
        (is (= 1 (:schema-version (knobs/saved-state (:knobs with-level)))))))))

(deftest repl-and-agent-links-get-the-typed-default-envelope
  (let [model (running-model)
        created (control/apply-domain-command model {:op :knob/create :knob-id "k" :name "K" :command-id "c" :show-id :typed
                                                     :epoch 0 :source {:adapter :repl :session-id "r"}
                                                     :links [{:id "s" :destination {:kind :param :cue-id :typed :param :shape}
                                                              :range [:dot :wave]}
                                                             {:id "z" :destination {:kind :param :cue-id :typed :param :zoom}
                                                              :range [0.0 4.0] :quantize {:step 0.25}}
                                                             {:id "l" :destination {:kind :param :cue-id :typed :param :level}
                                                              :range [0.0 1.0]}
                                                             {:id "o" :destination {:kind :param :cue-id :typed :param :strobe}
                                                              :range [false true] :envelope {}}]}
                                                    knobs/apply-operation)
        links (into {} (map (juxt :id :envelope)) (get-in created [:model :knobs :knobs "k" :links]))]
    (is (= {"s" knobs/stepped-envelope "z" knobs/stepped-envelope "l" nil "o" {}} links)
        "stepped destinations (a grid number included) get the default; an empty envelope opts out")
    (is (nil? (get-in created [:model :knobs :knobs "k" :created-by])) "only an agent's knob records its creator")))

(deftest grids-are-bounded-and-vector-values-stay-on-them
  (let [model (running-model)
        create (fn [link] (control/apply-domain-command model {:op :knob/create :knob-id "k" :name "K" :links [link]
                                                               :command-id (str (random-uuid)) :show-id :typed :epoch 0
                                                               :source {:adapter :test}}
                                                        knobs/apply-operation))
        link (fn [param & {:as more}] (merge {:id (name param) :destination {:kind :param :cue-id :typed :param param}} more))
        refused (fn [l] (get-in (create l) [:ack :message]))]
    (is (re-find #"^A grid step of 1.0E-300 puts no value, or more than 1024" (refused (link :zoom :range [0.0 4.0] :quantize {:step 1e-300}))))
    (is (re-find #"wider than the range" (refused (link :zoom :range [0.0 1.0] :quantize {:step 2.0}))))
    (is (re-find #"gives a component more than 1024 values"
                 (refused (link :extent :range [[0.0 0.0 0.0] [1.0 1.0 1.0]] :quantize {:step 1e-300}))))
    (is (re-find #"wider than every component's range"
                 (refused (link :extent :range [[0.0 0.0 0.0] [1.0 1.0 1.0]] :quantize {:step 1e300}))))
    (is (re-find #"envelope is" (refused (link :shape :range [:dot :wave] :envelope {:attack {:ms 100}}))))
    (is (= "An integer destination steps by whole numbers" (refused (link :bands :range [1 8] :quantize {:step 0.5}))))
    (is (= [0.0 0.25 0.5 0.75 1.0]
           (mapv #(knobs/resolve-value (link :zoom :range [0.0 1.0] :quantize {:step 0.25}) (schemas :zoom) %)
                 [0.0 0.2 0.4 0.6 0.99])))
    (testing "vector values snap inside the bounds and on the grid"
      (let [l (link :extent :range [[0.0 0.0 0.0] [6.0 5.9 0.3]] :quantize {:step 0.5})]
        (doseq [k (range 0 1.0001 0.01) :let [v (knobs/resolve-value l (schemas :extent) k)]]
          (is (every? #(on-grid? 0.5 %) v) (pr-str v))
          (is (params/valid-value? (schemas :extent) v)))))
    (is (= [:ring :wave] (get-in (create (link :shape :range ["ring" "wave"])) [:model :knobs :knobs "k" :links 0 :range]))
        "a JSON surface names enum values as text")))

(deftest a-stepped-ramp-schedules-its-steps-and-takes-over-from-where-it-is
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
        id (some #(when (= :typed (:cue-id %)) (:id %)) (vals (:instances (running-model))))
        n (atom 0)
        interpolate modulation/interpolate-value
        overlay #(get-in (subscribers/frame-overlays (model-of rt) %) [:params id :bands])]
    (try
      (with-redefs [modulation/interpolate-value (fn [& args] (swap! n inc) (apply interpolate args))]
        (submit! rt 0 {:op :param/set :instance-id id :param :bands :value 1})
        (submit! rt 100 {:op :param/ramp :event-id "climb" :instance-id id :param :bands :to 8 :over {:beats 2}
                         :source {:adapter :repl :session-id "r"}})
        (let [ramp (first (get-in (model-of rt) [:event-subscribers :ramps]))
              evaluated-at-start @n
              rows (vec (for [i (range 30) :let [t (+ 120 (* 20 i))]]
                          (do (show/tick! rt t)
                              (let [beat (get-in (model-of rt) [:transport :beat])
                                    p (max 0.0 (min 1.0 (/ (- beat (:start ramp)) (- (:end ramp) (:start ramp)))))]
                                [(overlay t) p]))))
              per-frame @n]
          (is (some? (:crossings ramp)) "a linear integer ramp gets a crossing schedule")
          (doseq [[value p] rows]
            (is (= (interpolate (schemas :bands) 1 8 p) value) (str "lookup equals evaluation at " p)))
          (is (< (- per-frame evaluated-at-start) 30) "frames look values up instead of evaluating each")
          (testing "a new ramp takes over from the exact position, never repeating a step"
            (let [before (first (peek rows))]
              (submit! rt 720 {:op :param/ramp :event-id "back" :instance-id id :param :bands :to 2 :over {:beats 1}
                               :source {:adapter :repl :session-id "r"}})
              (let [after (vec (for [i (range 40) :let [t (+ 740 (* 20 i))]] (do (show/tick! rt t) (overlay t))))
                    seen (cons before (filter some? after))]
                (is (every? (fn [[x y]] (<= -1 (- y x) 0)) (partition 2 1 seen)) "down one step at a time from where it was")
                (is (= 2 (get-in (model-of rt) [:instances id :params :bands])) "it commits its target"))))))
      (finally (show/stop! rt)))))

(deftest the-room-audition-shows-where-a-candidate-settles
  (let [rt (show/create-runtime {:model (running-model) :workspace-options nil :worker-timeout-ms 60000
                                 :audition {:max-live-share ##Inf}})
        shape (first stepped-links)
        room-shape (fn [] (some #(when (= :typed (:cue-id %)) (get-in % [:params :shape]))
                                (vals (:instances (get-in @(:scene-draft rt) [:frame :model])))))]
    (try
      (is (= :ready (:candidate-status (submit! rt 10 {:op :knob-candidate/generate :seed 2 :types ["choice" "number"]}))))
      (let [existing (some #(when (= :shape (param-of %)) (:id %))
                           (get-in (model-of rt) [:knob-candidates :candidate :links]))]
        (is (= :accepted (:status (submit! rt 20 {:op :knob-candidate/link :link (assoc shape :id (or existing "shape"))})))))
      (doseq [[i value] (map-indexed vector [0.0 0.25 0.17 0.46 0.14])]
        (let [t (+ 100 (* 50 i))]
          (is (= :accepted (:status (submit! rt t {:op :knob-candidate/audition :value value}))))
          (doseq [f (range 5)] (show/tick! rt (+ t 1 f)) (is (show/await-audition rt 5000)))
          (is (= (knobs/resolve-value shape (schemas :shape) value) (room-shape)) (str "at " value))))
      (finally (show/stop! rt)))))

(deftest the-knob-agent-tool-proposes-auditions-rerolls-keeps-and-turns-typed-mappings
  (let [tool (some #(when (= "knob" (:name %)) %) agent/default-tools)
        model (running-model)
        agent-source {:adapter :agent :session-id "agent-run"}
        command #(knob-candidates/knob-tool-command model % agent-source)]
    (is tool)
    (is (empty? (agent/validate-arguments (:parameters tool) {:op "propose" :types ["choice" "switch" "number"] :seed 3})))
    (is (seq (agent/validate-arguments (:parameters tool) {:op "propose" :types ["gradient"]})))
    (testing "each op maps to the shared command"
      (is (= {:op :knob-candidate/generate :seed 3 :types ["choice" "switch"]}
             (command {:op :propose :seed 3 :types ["choice" "switch"]})))
      (is (= {:op :knob-candidate/audition :value 0.4} (command {:op :audition :value 0.4})))
      (is (= {:op :knob-candidate/reroll} (command {:op "reroll"})))
      (is (= :knob-candidate/link (:op (command {:op :link :link {:id "x"}}))))
      (is (= {:op :knob/link :knob-id "k" :link {:id "x" :direction :down}}
             (command {:op :link :knob-id "k" :link {:id "x" :direction "down"}})))
      (is (= {:op :knob/nudge :knob-id "k" :delta 0.1} (command {:op :nudge :knob-id "k" :delta 0.1}))))
    (testing "through the show's agent dispatcher"
      (let [rt (show/create-runtime {:model (running-model) :workspace-options nil})
            call! (fn [time args]
                    (let [result (future (show/dispatch-tool! rt "knob" args {:command-id (str "knob-" time) :run-id "agent-run"}))]
                      (loop [t time]
                        (show/tick! rt t)
                        (if (realized? result)
                          ;; The agent loop turns a thrown refusal into {:ok? false}, as here.
                          (try @result (catch java.util.concurrent.ExecutionException e
                                         {:ok? false :message (ex-message (.getCause e))}))
                          (do (Thread/sleep 2) (recur (+ t 1)))))))]
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
            (is (= {:adapter :agent :session-id "agent-run"} (get-in @(:state rt) [:model :knobs :knobs "agent-knob" :created-by])))
            (is (= :accepted (:status (call! 50 {:op "set" :knob-id "agent-knob" :value 1.0}))))
            (is (= 1.0 (get-in @(:state rt) [:model :knobs :knobs "agent-knob" :value])))
            (is (= :accepted (:status (call! 60 {:op "link" :knob-id "agent-knob"
                                                 :link {:id "strobe" :destination {:kind "param" :cue-id "typed" :param "strobe"}
                                                        :range [false true] :envelope {:attack {:beats 1} :release {:beats 1}}}}))))
            (is (= :accepted (:status (call! 70 {:op "nudge" :knob-id "agent-knob" :delta -0.7}))))
            (frames! rt 100 60)
            (is (= false (:strobe (committed rt))) "the switch settles off once its envelope has moved below 0.5")
            (is (= :accepted (:status (call! 2000 {:op "unlink" :knob-id "agent-knob" :link-id "strobe"})))
                "an agent may unlink its own knob's links")
            (is (= :rejected (:status (call! 2010 {:op "set" :knob-id "nope" :value 0.5})))))
          (testing "the operator's knobs can be linked, renamed and turned, never removed by an agent"
            (is (= :accepted (:status (submit! rt 2100 {:op :knob/create :knob-id "mine" :name "Mine"
                                                        :links [(first stepped-links)]}))))
            (is (= :accepted (:status (call! 2120 {:op "rename" :knob-id "mine" :name "Renamed"}))))
            (is (= :accepted (:status (call! 2140 {:op "set" :knob-id "mine" :value 0.5}))))
            (let [refused (call! 2160 {:op "remove" :knob-id "mine"})]
              (is (= [:rejected :not-agent-knob] ((juxt :status :code) refused)) (pr-str refused))
              (is (re-find #"only on knobs it created" (str (:message refused)))))
            (is (= :rejected (:status (call! 2180 {:op "unlink" :knob-id "mine" :link-id "shape"}))))
            (is (= :rejected (:status (call! 2190 {:op "link" :knob-id "mine" :link (assoc (first stepped-links) :range [:bar :star])})))
                "nor replace one of its links")
            (is (= :accepted (:status (call! 2195 {:op "link" :knob-id "mine"
                                                   :link {:id "added" :destination {:kind "param" :cue-id "typed" :param "level"}
                                                          :range [0 1]}})))
                "adding a link is allowed")
            (is (contains? (get-in @(:state rt) [:model :knobs :knobs]) "mine"))
            (is (= :accepted (:status (call! 2200 {:op "remove" :knob-id "agent-knob"}))) "its own knob it may remove"))
          (testing "tool fields cannot replace the command envelope"
            (let [merged (merge (knob-candidates/knob-tool-command model {:op :set :knob-id "k" :value 1.0
                                                                          :source {:adapter :studio} :command-id "x"}
                                                                   agent-source)
                                {:source agent-source :command-id "run/1"})]
              (is (= [agent-source "run/1"] [(:source merged) (:command-id merged)]))))
          (finally (show/stop! rt)))))))
