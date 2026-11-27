(ns tolglow.effects.reload-compile-test
  "Programs for published revisions and first launches compile off the frame
  (#774). A revision is adopted by all its instances together once every
  program they need is ready; until then they run their captured definition,
  and an instance that never rendered waits. The compile worker is stepped by
  hand here: assertions count frames, versions and compilations, never clocks."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.show.programs :as programs]))

(defn- param [key] {:op :param :key key})

(defn- probe-id [i] (keyword (str "probe-" i)))

(defn- head [i] (keyword (str "head-" (inc i))))

(defn- head-dimmer
  "Index of head-(i+1)'s dimmer in the captured universe."
  [i]
  (* 12 i))

(defn- probe
  "Version `version` of probe `i`, a dimmer on head-(i+1)."
  ([i version] (probe i version {}))
  ([i version {:keys [bounds outputs] :or {bounds [0.0 1.0]}}]
   {:id (probe-id i) :version version :name (str "Probe " i) :targets [(head i)]
    :params {:level {:type :number :default 0.5 :bounds bounds}}
    :outputs (or outputs {:dimmer (param :level)})}))

(defn- show-model [n]
  (reduce (fn [model i]
            (let [id (probe-id i)]
              (-> model
                  (assoc-in [:registry :effects id] (probe i 1))
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority 20
                                                 :reload-policy :preserve-state}))))
          (demo/model) (range n)))

(defn- show
  "A show of `n` probes with a capture sink and a hand-stepped compile worker.
  :compiled records every call of the rig compiler as [phase id version
  targets], where phase is :frame inside a tick and :worker in `work!`."
  ([n] (show n {}))
  ([n {:keys [model fail inline?]}]
   (let [output (atom []) tasks (atom []) compiled (atom []) phase (atom :setup)
         rt (show/create-runtime (cond-> {:model (or model (show-model n)) :workspace-options nil :effect-log! (fn [_])
                                          :workers 2
                                          :sink {:send! (fn [universes _]
                                                          (swap! output conj (mapv #(bit-and 255 %) (get universes 1))))}}
                                   (not inline?) (assoc :background-compile (fn [task] (swap! tasks conj task)))))]
     (swap! (:compiler-context rt) update :compiler
            (fn [compiler]
              (fn [{:keys [id version targets] :as program}]
                (swap! compiled conj [@phase id version targets])
                (when (= fail [id version]) (throw (ex-info "Compiler rejected this revision" {:effect-id id})))
                (compiler program))))
     {:rt rt :output output :tasks tasks :compiled compiled :phase phase})))

(defn- work!
  "Run up to `n` queued compile tasks (all by default), first in first out."
  ([s] (work! s Integer/MAX_VALUE))
  ([{:keys [tasks phase]} n]
   (let [[run keep] (split-at n @tasks)]
     (reset! tasks (vec keep))
     (reset! phase :worker)
     (doseq [task run] (task))
     (reset! phase :setup)
     (count run))))

(defn- tick! [{:keys [rt phase]} t]
  (reset! phase :frame)
  (show/tick! rt t)
  (reset! phase :setup))

(defn- request! [s t type payload]
  (let [reply (show/submit! (:rt s) type payload)]
    (tick! s t)
    (deref reply 2000 {:status :timeout})))

(defn- publish!
  "Publish every revision in one owner cycle and render that frame."
  [s definitions t]
  (let [replies (mapv #(show/submit! (:rt s) :definition {:kind :effects :definition %}) definitions)]
    (tick! s t)
    (mapv #(:status (deref % 2000 {:status :timeout})) replies)))

(defn- model [s] (:model @(:state (:rt s))))

(defn- instances [s]
  (into {} (map (juxt :cue-id identity)) (vals (:instances (model s)))))

(defn- versions [s n]
  (let [by-cue (instances s)]
    (mapv #(get-in by-cue [(probe-id %) :effect-version]) (range n))))

(defn- rendered
  "[effect-version reload-policy] of each instance of `effect-id` that has
  rendered, in launch order."
  [s effect-id]
  (->> (vals (:instances (model s)))
       (filter #(and (= effect-id (:effect-id %)) (get-in % [:system :last-frame-number])))
       (sort-by :order)
       (mapv (juxt :effect-version :reload-policy))))

(defn- launch-all!
  "Launch probes 0..n-1 in that order, compile their programs, bind `binding`
  to each level when given, and return the next free time."
  [s n binding]
  (doseq [i (range n)]
    (let [t (* 25 (inc i))
          ack (request! s t :command {:op :cue/launch :cue-id (probe-id i)})]
      (is (= :accepted (:status ack)))
      (work! s)
      (when binding
        (is (= :accepted (:status (request! s t :modulation-bind
                                            {:instance-id (:instance-id ack) :param :level :binding binding})))))))
  (tick! s (* 25 (inc n)))
  (* 25 (+ 2 n)))

(defn- compiled-revisions
  "The [phase id version] of compilations above version 1."
  [s]
  (into [] (comp (filter #(< 1 (nth % 2))) (map #(subvec % 0 3))) @(:compiled s)))

(defn- frame-compiles [s]
  (filterv #(= :frame (first %)) (compiled-revisions s)))

(defn- dimmers [s n] (mapv #(nth (peek @(:output s)) (head-dimmer %)) (range n)))

(defn- last-intents [s] (get-in @(:state (:rt s)) [:status :last-intents]))

(defn- stop! [& shows] (doseq [s shows] (show/stop! (:rt s))))

(deftest a-revision-is-adopted-once-its-programs-are-compiled-off-the-frame
  (let [s (show 5)]
    (try
      (let [t (launch-all! s 5 nil)
            frame (fn [] (get-in (model s) [:frame-number]))
            acks (publish! s (mapv #(probe % 2 {:outputs {:dimmer {:op :const :value 1.0}}}) (range 5)) t)
            published-frame (frame)]
        (is (= (repeat 5 :accepted) acks))
        (is (= [[1 1 1 1 1] [128 128 128 128 128] 5] [(versions s 5) (dimmers s 5) (count @(:tasks s))])
            "The publication frame runs version 1 and queues one compile per revision")
        (tick! s (+ t 25))
        (is (= [[1 1 1 1 1] 5] [(versions s 5) (count @(:tasks s))]) "A waiting revision is queued once")
        (is (= 5 (work! s)))
        (tick! s (+ t 50))
        (is (= [[2 2 2 2 2] [255 255 255 255 255]] [(versions s 5) (dimmers s 5)]) "Every probe adopts in one frame")
        (is (= [] (frame-compiles s)) "No revision compiles inside a frame")
        (is (= (set (map #(vector :worker (probe-id %) 2) (range 5))) (set (compiled-revisions s))))
        (testing "the reload events carry the frame each revision was published and adopted"
          (let [reloaded (filter #(= :effect/reloaded (:type %)) (last-intents s))]
            (is (= 5 (count reloaded)))
            (is (every? #(= [published-frame (+ 2 published-frame)] [(:pending-since-frame %) (:adopted-frame %)]) reloaded))))
        (is (every? (comp nil? :reload-fault) (vals (instances s))))
        (is (nil? (get-in @(:state (:rt s)) [:status :error]))))
      (finally (stop! s)))))

(deftest one-revision-launched-per-fixture-switches-all-its-instances-together
  ;; Spatial actions launch one instance per fixture with its own :targets, so
  ;; one revision needs one program per head. None adopts before all compile.
  (let [wide (fn [version] (assoc (probe 0 version) :targets (mapv head (range 8))))
        s (show 1 {:model (assoc-in (show-model 1) [:registry :effects :probe-0] (wide 1))})]
    (try
      (doseq [i (range 8)]
        (is (= :accepted (:status (request! s (* 25 (inc i)) :command
                                            {:op :effect/launch :effect-id :probe-0
                                             :launch {:reload-policy :preserve-state :targets [(head i)]}}))))
        (work! s))
      (tick! s 225)
      (is (= (repeat 8 [1 :preserve-state]) (rendered s :probe-0)))
      (publish! s [(wide 2)] 250)
      (is (= 8 (count @(:tasks s))) "One program per launch target set")
      (work! s 5)
      (tick! s 275)
      (is (= (repeat 8 1) (map first (rendered s :probe-0))) "Five of eight programs are not enough")
      (work! s)
      (tick! s 300)
      (is (= (repeat 8 [2 :preserve-state]) (rendered s :probe-0)))
      (is (= [] (frame-compiles s)))
      (is (= 8 (count (filter #(= [:worker :probe-0 2] %) (compiled-revisions s)))))
      (finally (stop! s)))))

(deftest a-launch-while-its-revision-compiles-waits-for-the-whole-effect
  ;; Probe 1 is relaunched in the cycle that publishes it. The new instance
  ;; captures version 2; it waits unrendered while the running instance keeps
  ;; version 1, and both render version 2 from the same frame.
  (let [s (show 2)]
    (try
      (let [t (launch-all! s 2 nil)
            replies [(show/submit! (:rt s) :definition {:kind :effects :definition (probe 1 2)})
                     (show/submit! (:rt s) :command {:op :cue/launch :cue-id :probe-1})]
            _ (tick! s t)
            waiting (rendered s :probe-1)
            _ (tick! s (+ t 25))
            still (rendered s :probe-1)
            _ (work! s)
            _ (tick! s (+ t 50))]
        (is (= [:accepted :accepted] (mapv #(:status (deref % 2000 {:status :timeout})) replies)))
        (is (= [[1 :preserve-state]] waiting still) "Only the running instance renders, on version 1")
        (is (= [[2 :preserve-state] [2 :preserve-state]] (rendered s :probe-1)))
        (is (= [] (frame-compiles s))))
      (finally (stop! s)))))

(deftest a-superseded-revision-is-never-compiled
  (let [s (show 1)]
    (try
      (let [t (launch-all! s 1 nil)]
        (publish! s [(probe 0 2)] t)
        (publish! s [(probe 0 3)] (+ t 25))
        (work! s)
        (tick! s (+ t 50))
        (is (= [3] (versions s 1)))
        (is (= [[:worker :probe-0 3]] (compiled-revisions s)) "The queued version 2 compile is skipped"))
      (finally (stop! s)))))

(deftest a-revision-that-fails-to-compile-off-the-frame-is-refused-once
  (let [s (show 2 {:fail [:probe-0 2]})]
    (try
      (let [t (launch-all! s 2 nil)]
        (publish! s [(probe 0 2) (probe 1 2)] t)
        (work! s)
        (tick! s (+ t 25))
        (tick! s (+ t 50))
        (let [refused (get (instances s) :probe-0)
              events (get-in (model s) [:variables :runtime :events])]
          (is (= [1 2] (versions s 2)))
          (is (= [:compile 2] [(get-in refused [:reload-fault :stage]) (get-in refused [:reload-fault :effect-version])]))
          (is (= [:effect/reload-failed] (mapv :type events)))
          (is (= #{[:worker :probe-0 2] [:worker :probe-1 2]} (set (compiled-revisions s)))
              "The failure is kept, so neither the frame nor a retry compiles it again")))
      (finally (stop! s)))))

(deftest compiles-run-first-in-first-out-so-republishing-cannot-starve-others
  ;; The worker manages two compiles per frame. Probes 0 and 1 are republished
  ;; every frame; 2 and 3 were published once, so their compiles are older
  ;; and run first, and they adopt on the third frame.
  (let [s (show 4)]
    (try
      (let [t (launch-all! s 4 nil)
            _ (publish! s (mapv #(probe % 2) (range 4)) t)
            frames (mapv (fn [k]
                           (work! s 2)
                           (publish! s [(probe 0 (+ 3 k)) (probe 1 (+ 3 k))] (+ t (* 25 (inc k))))
                           (versions s 4))
                         (range 4))]
        (is (= [1 1 2 2] (nth frames 1)) "Version 2 of probes 0 and 1 was superseded before adoption")
        (is (= [[1 1 1 1] [1 1 2 2] [1 1 2 2] [1 1 2 2]] frames))
        (work! s)
        (tick! s (+ t 150))
        (is (= [6 6 2 2] (versions s 4)) "Once publishing stops, the latest revisions adopt"))
      (finally (stop! s)))))

(def ^:private level-binding
  {:type :lfo :waveform :saw :period {:beats 1} :phase-reference :transport :from 0.6 :to 0.9})

(deftest a-waiting-revision-keeps-its-waveform-on-the-captured-definition
  ;; Probe 2's revision narrows the bound level below the waveform's range.
  ;; While it compiles, the frame must sample the captured bounds, as a twin
  ;; that never published it does.
  (let [s (show 3) twin (show 3)]
    (try
      (let [t (launch-all! s 3 level-binding)
            _ (launch-all! twin 3 level-binding)]
        (publish! s [(probe 2 2 {:bounds [0.0 0.5]})] t)
        (tick! twin t)
        (is (= [1 1 1] (versions s 3)))
        (is (= (dimmers twin 3) (dimmers s 3)))
        (is (< 128 (nth (dimmers twin 3) 2)) "The waveform lifts the captured level above its default")
        (work! s)
        (tick! s (+ t 25))
        (tick! twin (+ t 25))
        (is (= [1 1 2] (versions s 3)))
        (is (not= (nth (dimmers twin 3) 2) (nth (dimmers s 3) 2)) "The adopted revision samples the narrowed bounds"))
      (finally (stop! s twin)))))

(deftest a-renderer-rejection-while-another-revision-compiles-keeps-it-pending
  ;; The renderer refuses probe 0's revision after the engine accepted it, so
  ;; the frame is evaluated again on the reload fallback (#663) while probe
  ;; 1's revision is still compiling. Its policy and the registry must survive.
  (let [s (show 2)
        position (get-in (model s) [:rig :fixtures 0 :position])
        registry-versions #(mapv (fn [i] (get-in (model s) [:registry :effects (probe-id i) :version])) (range 2))
        policies #(mapv (fn [i] (:reload-policy (get (instances s) (probe-id i)))) (range 2))]
    (try
      (let [t (launch-all! s 2 level-binding)]
        (publish! s [(assoc-in (probe 0 2 {:bounds [0.0 0.5]}) [:outputs :aim] {:op :const :value position})] t)
        (work! s)
        (publish! s [(probe 1 2 {:bounds [0.0 0.5]})] (+ t 25))
        (let [refusal-frame [(versions s 2) (registry-versions) (policies)]
              refused (get (instances s) :probe-0)]
          (is (= [[1 1] [2 2] [:preserve-state :preserve-state]] refusal-frame))
          (is (= [:output 2] [(get-in refused [:reload-fault :stage]) (get-in refused [:reload-fault :effect-version])])))
        (work! s)
        (tick! s (+ t 50))
        (is (= [[1 2] [2 2] [:preserve-state :preserve-state]] [(versions s 2) (registry-versions) (policies)]))
        (is (nil? (get-in @(:state (:rt s)) [:status :error]))))
      (finally (stop! s)))))

(defn- start-ramp! [s t cue param to]
  (let [instance-id (:id (get (instances s) cue))]
    (:status (request! s t :command {:op :param/ramp :instance-id instance-id :param param :to to
                                     :over {:beats 4} :source {:adapter :repl :session-id "ramps"}}))))

(deftest a-waiting-revision-keeps-the-ramps-its-captured-definition-reads
  ;; Probe 0 ramps a gain that its waiting revision drops. While that revision
  ;; compiles, version 1 still reads the ramped gain: the frame and the
  ;; resolved overrides Studio reads must equal a twin that never published.
  (let [gained (-> (probe 0 1)
                   (assoc-in [:params :gain] {:type :number :default 1.0 :bounds [0.0 1.0]})
                   (assoc :outputs {:dimmer {:op :multiply :args [(param :level) (param :gain)]}}))
        run (fn [definitions]
              (let [s (show 1 {:model (assoc-in (show-model 1) [:registry :effects :probe-0] gained)})]
                (try
                  (let [t (launch-all! s 1 nil)]
                    (is (= :accepted (start-ramp! s t :probe-0 :gain 0.0)))
                    (publish! s definitions (+ t 200))
                    {:versions (versions s 1) :dimmer (first (dimmers s 1))
                     :overrides (get-in @(:state (:rt s)) [:resolved-param-overrides (:id (get (instances s) :probe-0))])
                     :error (get-in @(:state (:rt s)) [:status :error])})
                  (finally (stop! s)))))
        published (run [(probe 0 2 {:outputs {:dimmer {:op :const :value 1.0}}})])
        twin (run [])]
    (is (= [1] (:versions published)) "The revision waits for its compile")
    (is (< 0 (:dimmer twin) 128) "The ramp has lowered the gain")
    (is (= (:dimmer twin) (:dimmer published)))
    (is (contains? (:overrides twin) :gain))
    (is (= (:overrides twin) (:overrides published)) "The resolved overrides keep the ramped gain")
    (is (nil? (:error published)))))

(deftest without-a-worker-the-publication-frame-adopts-as-before
  (let [s (show 2 {:inline? true})]
    (try
      (let [t (launch-all! s 2 nil)]
        (publish! s [(probe 0 2) (probe 1 2)] t)
        (is (= [2 2] (versions s 2)))
        (is (= #{[:frame :probe-0 2] [:frame :probe-1 2]} (set (compiled-revisions s)))))
      (finally (stop! s)))))

(deftest an-owned-worker-compiles-off-the-frame-and-stops-with-the-show
  (let [rt (show/create-runtime {:model (show-model 1) :workspace-options nil :effect-log! (fn [_])
                                 :background-compile true})
        tick (atom 0)
        next! #(show/tick! rt (swap! tick + 25))
        settle! (fn []
                  ;; A bound on the wait, not a timing assertion.
                  (loop [n 0]
                    (next!)
                    (when (and (< n 400) (seq @(:queued @(:compiler-context rt))))
                      (Thread/sleep 5)
                      (recur (inc n)))))]
    (try
      (show/submit! rt {:op :cue/launch :cue-id :probe-0})
      (settle!)
      (next!)
      (show/submit! rt :definition {:kind :effects :definition (probe 0 2)})
      (settle!)
      (next!)
      (is (= 2 (:effect-version (first (vals (get-in @(:state rt) [:model :instances]))))))
      (is (realized? (get-in rt [:compile-worker :owned])))
      (finally (show/stop! rt)))
    (is (not-any? #(= "tolglow-program-compiler" (.getName ^Thread %)) (keys (Thread/getAllStackTraces)))
        "Stopping the show stops its compile worker")))

(deftest background-compile-is-a-boolean-or-a-function
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Background compile"
                        (show/create-runtime {:workspace-options nil :background-compile :yes}))))

(deftest a-full-compile-cache-keeps-running-programs-compiled
  (let [calls (atom {})
        context (programs/context (fn [definition] (swap! calls update definition (fnil inc 0)) [:step definition]) nil)
        running (probe 0 1)
        others (mapv #(probe 1 (+ 2 %)) (range 900))]
    (programs/compile! context running)
    (doseq [definition (take 300 others)] (programs/compile! context definition) (programs/compile! context running))
    (is (= 1 (get @calls running)) "A program used every frame survives the cache filling up")
    (is (= [:step running] (programs/compile! context running)))
    (doseq [definition (drop 300 others)] (programs/compile! context definition))
    (programs/compile! context running)
    (is (= 2 (get @calls running)) "An unused program still leaves the cache")
    (is (every? #(= 1 %) (map @calls others)) "Every published revision is compiled once")))

(deftest a-single-first-launch-renders-at-once-and-a-wide-one-waits-for-the-worker
  ;; A frame compiles up to four first-launch programs itself, so one launch
  ;; renders on the frame that admits it. Six launches of one effect on six
  ;; heads need six programs: they wait for the worker and appear together.
  (let [wide (assoc (probe 0 1) :targets (mapv head (range 6)))
        s (show 1 {:model (assoc-in (show-model 1) [:registry :effects :probe-0] wide)})
        launch (fn [i] (show/submit! (:rt s) :command {:op :effect/launch :effect-id :probe-0
                                                         :launch {:reload-policy :preserve-state :targets [(head i)]}}))]
    (try
      (launch 0)
      (tick! s 25)
      (is (= [[1 :preserve-state]] (rendered s :probe-0)) "One launch renders on its admission frame")
      (is (= 0 (count @(:tasks s))))
      (doseq [i (range 1 6)] (launch i))
      (tick! s 50)
      (is (= 1 (count (rendered s :probe-0))) "Five new programs are more than the frame compiles itself")
      (is (= 5 (count @(:tasks s))))
      (work! s)
      (tick! s 75)
      (is (= 6 (count (rendered s :probe-0))) "All five appear on the same frame")
      (is (= 1 (count (filter #(= :frame (first %)) @(:compiled s)))) "Only the single launch compiled in a frame")
      (finally (stop! s)))))
