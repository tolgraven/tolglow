(ns tolglow.effects.reload-compile-test
  "Programs compile ahead of launches and of pending revisions, off the frame
  (#774). A revision is adopted by all its instances together once every
  program they need is ready; until then they run their captured definition.
  A launch renders on its start: its program is warmed ahead, or compiled in
  the frame as a fallback. The compile worker is stepped by hand here:
  assertions count frames, versions and compilations, never clocks."
  (:require [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is testing]]
            [tolglow.spatial.targets :as targets]
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
  "Run up to `n` queued worker tasks, first in first out, or by default every
  task until none is left (a warm rebuild queues its compiles). Returns how
  many ran."
  ([s] (loop [ran 0]
         (let [n (work! s Integer/MAX_VALUE)]
           (if (zero? n) ran (recur (+ ran n))))))
  ([{:keys [tasks phase]} n]
   (let [[run keep] (split-at n @tasks)]
     (reset! tasks (vec keep))
     (reset! phase :worker)
     (doseq [task run] (task))
     (reset! phase :setup)
     (count run))))

(defn- check-publications!
  "Run the publication checks queued on the worker. They run ahead of its
  compiles, before the owner cycle that commits them (#784)."
  [{:keys [tasks phase]}]
  (let [run (filterv programs/ahead? @tasks)]
    (swap! tasks #(into [] (remove programs/ahead?) %))
    (reset! phase :worker)
    (doseq [task run] (task))
    (reset! phase :setup)))

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
    (check-publications! s)
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
        (is (= [[1 1 1 1 1] [128 128 128 128 128]] [(versions s 5) (dimmers s 5)])
            "The publication frame runs version 1")
        (tick! s (+ t 25))
        (is (= [1 1 1 1 1] (versions s 5)))
        (work! s)
        (is (= 5 (count (compiled-revisions s))) "Each waiting revision compiles once")
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
      (is (= 9 (count @(:tasks s))) "One untargeted program for later launches, and one per launch target set")
      (work! s 5)
      (tick! s 275)
      (is (= (repeat 8 1) (map first (rendered s :probe-0))) "Five of eight programs are not enough")
      (work! s)
      (tick! s 300)
      (is (= (repeat 8 [2 :preserve-state]) (rendered s :probe-0)))
      (is (= [] (frame-compiles s)))
      (is (= 9 (count (filter #(= [:worker :probe-0 2] %) (compiled-revisions s)))))
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
        (is (= [1 1] (subvec (peek frames) 0 2)) "Probes 0 and 1 keep being superseded before adoption")
        (is (some #(= [2 2] (subvec % 2 4)) (take 3 frames)) "Probes 2 and 3 adopt within three frames")
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
      (let [reply (show/submit! rt :definition {:kind :effects :definition (probe 0 2)})]
        ;; The worker checks the publication before an owner cycle commits it.
        (loop [n 0]
          (next!)
          (when (and (< n 400) (not (realized? reply)))
            (Thread/sleep 5)
            (recur (inc n))))
        (is (= :accepted (:status (deref reply 0 nil)))))
      (settle!)
      (next!)
      (is (= 2 (:effect-version (first (vals (get-in @(:state rt) [:model :instances]))))))
      (is (realized? (get-in rt [:compile-worker :owned])))
      (finally (show/stop! rt)))
    (is ((:terminated? @(get-in rt [:compile-worker :owned]))) "Stopping the show stops its compile worker")))

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


(deftest five-short-one-shots-launched-in-one-frame-all-render-on-their-start
  ;; 20 ms one-shots end before the next 25 ms frame. Each must render on the
  ;; frame that admits it even though none of their programs is warm yet.
  (let [s (show 5)
        lit (atom #{})]
    (try
      (tick! s 0)
      (reset! (:tasks s) [])
      (let [replies (mapv #(show/submit! (:rt s) :command {:op :cue/launch :cue-id (probe-id %)
                                                          :launch {:mode :one-shot :duration-ms 20}})
                          (range 5))]
        (tick! s 25)
        (is (every? #(= :accepted (:status (deref % 2000 {:status :timeout}))) replies))
        (doseq [i (range 5) :when (pos? (nth (peek @(:output s)) (head-dimmer i)))] (swap! lit conj i))
        (tick! s 50)
        (is (= #{0 1 2 3 4} @lit) "Every one-shot rendered on its first frame")
        (is (empty? (:instances (model s))) "And all of them have ended"))
      (finally (stop! s)))))

(deftest a-burst-of-grid-launches-after-a-publish-lands-on-the-beat
  ;; Like firing a Live scene: one effect, revised just before, launched on
  ;; five heads for the next beat, each launch with its own targets (five
  ;; programs). Scheduling them warms those programs, so with the worker run
  ;; before the boundary they render on the beat without compiling in a frame;
  ;; with the worker stalled they still render on the beat.
  (doseq [worker? [true false]]
    (testing (if worker? "warmed ahead" "worker stalled")
      (let [wide (fn [version] (assoc (probe 0 version) :targets (mapv head (range 5))))
            s (show 1 {:model (assoc-in (show-model 1) [:registry :effects :probe-0] (wide 1))})
            source {:adapter :repl :session-id "burst"}]
        (try
          (tick! s 0)
          (work! s)
          (publish! s [(wide 2)] 25)
          (work! s)
          (reset! (:compiled s) [])
          ;; 120 BPM: a beat lasts 500 ms, so the boundary at beat 1 is t = 500.
          (let [replies (mapv #(show/submit! (:rt s) :command {:op :cue/launch :cue-id :probe-0 :grid {:beats 1}
                                                              :launch {:targets [(head %)]}
                                                              :command-id (str "burst-" %) :source source})
                              (range 5))]
            (tick! s 50)
            (is (every? #(#{:pending} (:status (deref % 2000 {:status :timeout}))) replies))
            (when worker? (work! s))
            (let [frames (mapv (fn [t] (tick! s t) [t (count (rendered s :probe-0))]) (range 75 551 25))]
              (is (= [500 5] (first (filter #(pos? (second %)) frames))) "All five render first on the beat")
              (is (= (repeat 5 2) (map first (rendered s :probe-0))) "They launch the published revision")
              (if worker?
                (is (= [] (filterv #(= :frame (first %)) @(:compiled s))) "No program compiled in a frame")
                (is (= 5 (count (filterv #(= :frame (first %)) @(:compiled s)))) "The frame compiled them as a fallback"))))
          (finally (stop! s)))))))

(deftest a-stack-overflow-while-compiling-refuses-the-revision-once
  ;; A StackOverflowError while compiling one revision must not leave the
  ;; effect silently pinned, stop every frame, or compile again each frame.
  (doseq [inline? [false true]]
    (testing (if inline? "inline" "worker")
      (let [s (show 1 {:inline? inline?})
            attempts (atom 0)]
        (try
          (let [t (launch-all! s 1 nil)]
            (swap! (:compiler-context (:rt s)) update :compiler
                   (fn [compiler]
                     (fn [program]
                       (when (= [:probe-0 2] [(:id program) (:version program)])
                         (swap! attempts inc)
                         (throw (StackOverflowError. "deep")))
                       (compiler program))))
            (publish! s [(probe 0 2)] t)
            (dotimes [k 5] (work! s) (tick! s (+ t (* 25 (inc k)))))
            (let [refused (get (instances s) :probe-0)]
              (is (= 1 @attempts) "Compiled once, never retried")
              (is (= [1 :compile 2] [(:effect-version refused) (get-in refused [:reload-fault :stage])
                                     (get-in refused [:reload-fault :effect-version])]))
              (is (re-find #"StackOverflowError" (str (get-in refused [:reload-fault :message]))))
              (is (nil? (get-in @(:state (:rt s)) [:status :error])) "The other frames keep running")))
          (finally (stop! s)))))))

(deftest results-no-frame-needs-are-dropped
  (let [tasks (atom [])
        context (programs/context (fn [program] [:step program]) (fn [task] (swap! tasks conj task)))
        [a b] [(probe 0 2) (probe 0 3)]]
    (programs/request! context [a])
    (doseq [task @tasks] (task))
    (reset! tasks [])
    (is (programs/ready? context a))
    (programs/request! context [b])
    (is (not (programs/ready? context a)) "A superseded result is not kept")))

(deftest a-retired-context-compiles-and-stores-nothing
  ;; A rig change or stop! retires the context while compiles are queued.
  (let [tasks (atom []) calls (atom 0)
        context (programs/context (fn [program] (swap! calls inc) [:step program]) (fn [task] (swap! tasks conj task)))
        program (probe 0 2)]
    (programs/request! context [program])
    (programs/retire! context)
    (doseq [task @tasks] (task))
    (is (= 0 @calls))
    (is (not (programs/ready? context program)))))

(deftest a-rig-change-while-compiles-wait-adopts-on-the-new-rig
  (let [s (show 2)]
    (try
      (let [t (launch-all! s 2 nil)
            old-context @(:compiler-context (:rt s))]
        (publish! s [(probe 0 2)] t)
        (is (seq @(:tasks s)) "The revision's compile is queued")
        ;; Move head-8 by a centimetre: a new rig, so a new compiler and cache.
        (let [reply (show/submit! (:rt s) :rig (update-in (:rig (model s)) [:fixtures 7 :position 0] + 0.01))]
          (tick! s (+ t 25))
          (is (= :accepted (:status (deref reply 2000 {:status :timeout}))) "The rig edit is admitted"))
        (is (not (identical? old-context @(:compiler-context (:rt s)))))
        (work! s)
        (tick! s (+ t 50))
        (is (= [2 1] (versions s 2)) "The revision adopts once the new rig's compiler has it")
        (is (nil? (get-in @(:state (:rt s)) [:status :error]))))
      (finally (stop! s)))))

(deftest stopping-with-a-compile-queued-is-harmless
  (let [s (show 1)]
    (let [t (launch-all! s 1 nil)]
      (publish! s [(probe 0 2)] t)
      (is (seq @(:tasks s)) "The revision's compile is queued")
      (stop! s)
      (reset! (:compiled s) [])
      (is (pos? (work! s)) "The queued tasks still run after stop!")
      (is (= [] @(:compiled s)) "It compiles nothing for a stopped show"))))

(deftest the-program-key-matches-the-engine
  ;; The plan must key programs exactly as the engine compiles them, or a
  ;; revision would wait on one key while the engine compiles another. The
  ;; engine compiles one program per distinct narrowed definition; record
  ;; what it compiles and compare with the keys the plan uses.
  (let [authored {:id :e :version 1 :targets [:a :b :c] :params {} :outputs {:dimmer 1.0}}
        untargeted (assoc (dissoc authored :targets) :id :u)
        ;; [untargeted []] is its own program: an engine reading [] as
        ;; untargeted would compile `untargeted` instead.
        cases [[authored nil] [untargeted nil] [untargeted [:c :a]] [untargeted []]
               [authored [:b]] [authored [:c :a]] [authored [:x]] [authored []]]
        instances (map-indexed (fn [i [definition targets]]
                                 (cond-> {:id (str "i" i) :cue-id (:id definition) :effect-id (:id definition)
                                          :effect-version 1 :definition definition :params {} :state {}
                                          :started-at-ms 0 :priority 0 :order i :phase :running
                                          :reload-policy :pinned :release-duration-ms 0}
                                   (some? targets) (assoc :targets targets)))
                               cases)
        model {:show-id "programs" :epoch 1 :revision 1 :time-ms 0
               :registry {:effects {:e authored :u untargeted} :cues {} :fixtures {}}
               :instances (into {} (map (juxt :id identity)) instances)
               :variables {} :master 1.0 :blackout? false :transport {:bpm 120 :playing? true}}
        compiled (atom [])]
    (engine/advance model {:time-ms 1} (fn [program] (swap! compiled conj program) identity))
    (is (= (set (map #(programs/program (:definition %) %) instances)) (set @compiled)))
    (is (= 7 (count @compiled)) "Once per program: [:x] and [] both narrow the authored targets to []")
    (is (some #{(assoc untargeted :targets [])} @compiled))
    (doseq [[definition targets] cases]
      (is (= (engine/launch-program definition targets)
             (programs/program definition (cond-> {} (some? targets) (assoc :targets targets))))
          (pr-str definition targets)))))

(deftest a-cue-selection-is-compiled-before-its-first-launch
  ;; The cue selects two of the effect's four heads. Publishing it warms that
  ;; program, so its first launch compiles nothing in the frame.
  (let [wide (assoc (probe 0 1) :targets (mapv head (range 4)))
        s (show 1 {:model (-> (show-model 1)
                              (assoc-in [:registry :effects :probe-0] wide)
                              (assoc-in [:registry :cues :probe-0 :selector] {:fixture-ids [(head 1) (head 2)]}))})]
    (try
      (tick! s 0)
      (work! s)
      (reset! (:compiled s) [])
      (request! s 25 :command {:op :cue/launch :cue-id :probe-0})
      (is (= [[1 :preserve-state]] (rendered s :probe-0)) "It renders on its first frame")
      (is (= [] (filterv #(= :frame (first %)) @(:compiled s))) "Its program was warmed ahead")
      (testing "a new revision of the cue's effect warms the selection on that revision"
        (publish! s [(assoc (probe 0 2) :targets (mapv head (range 4)))] 50)
        (work! s)
        (is (programs/ready? @(:compiler-context (:rt s))
                             (programs/program (assoc (probe 0 2) :targets (mapv head (range 4)))
                                               {:targets [(head 1) (head 2)]}))))
      (finally (stop! s)))))

(deftest a-recorded-take-is-compiled-before-it-replays
  ;; Live-run playback replays a take with its recorded targets. The take
  ;; below stands in for one recorded earlier; its program is warmed, so the
  ;; replayed launch compiles nothing in the frame.
  (let [wide (assoc (probe 0 1) :targets (mapv head (range 4)))
        s (show 1 {:model (assoc-in (show-model 1) [:registry :effects :probe-0] wide)})]
    (try
      (swap! (:live-runs (:rt s)) assoc-in [:record :takes "take-1"]
             {:take-id "take-1" :cue-id :probe-0 :launch {:mode :persistent :targets [(head 3)]}})
      (tick! s 0)
      (work! s)
      (reset! (:compiled s) [])
      (request! s 25 :command {:op :cue/launch :cue-id :probe-0 :launch {:targets [(head 3)]}})
      (is (= [[1 :preserve-state]] (rendered s :probe-0)))
      (is (= [] (filterv #(= :frame (first %)) @(:compiled s))) "The take's program was warmed ahead")
      (finally (stop! s)))))

(deftest a-launch-while-its-revision-compiles-starts-on-the-running-revision
  ;; An effect runs on 32 heads, one instance per head, and its revision is
  ;; compiling. One more launch on one of those heads starts on version 1,
  ;; the revision its siblings run, with the program they already use, and
  ;; adopts with them: nothing of the revision compiles in a frame.
  (let [wide (fn [version] (assoc (probe 0 version) :targets (mapv #(keyword (str "head-" (inc %))) (range 32))))
        heads (mapv #(keyword (str "head-" (inc %))) (range 32))
        s (show 1 {:model (-> (show-model 1)
                              (assoc-in [:registry :effects :probe-0] (wide 1))
                              (assoc-in [:rig :fixtures] (mapv (fn [i] (merge (nth (get-in (demo/model) [:rig :fixtures]) (mod i 8))
                                                                               {:id (nth heads i) :universe (inc (quot i 40))
                                                                                :address (inc (* 12 (mod i 40)))}))
                                                                (range 32))))})
        launch (fn [head] (show/submit! (:rt s) :command {:op :effect/launch :effect-id :probe-0
                                                          :launch {:reload-policy :preserve-state :targets [head]}}))]
    (try
      (doseq [head heads] (launch head))
      (tick! s 25)
      (work! s)
      (tick! s 50)
      (is (= 32 (count (rendered s :probe-0))))
      (reset! (:compiled s) [])
      (publish! s [(wide 2)] 75)
      (launch (nth heads 3))
      (tick! s 100)
      (is (= (repeat 33 1) (map first (rendered s :probe-0))) "The new launch renders on version 1 with its siblings")
      (is (= [] (frame-compiles s)) "Nothing of the revision compiles in a frame")
      (work! s)
      (tick! s 125)
      (is (= (repeat 33 [2 :preserve-state]) (rendered s :probe-0)) "All 33 adopt version 2 on the same frame")
      (is (= [] (frame-compiles s)))
      (finally (stop! s)))))

(deftest a-pinned-launch-keeps-the-revision-it-captured
  ;; A pinned launch never reloads, so it cannot start on the old revision and
  ;; catch up; it renders the revision it captured, compiled in the frame.
  (let [s (show 1)]
    (try
      (let [t (launch-all! s 1 nil)]
        (publish! s [(probe 0 2)] t)
        (show/submit! (:rt s) :command {:op :cue/launch :cue-id :probe-0 :launch {:reload-policy :pinned}})
        (tick! s (+ t 25))
        (is (= [[1 :preserve-state] [2 :pinned]] (rendered s :probe-0))))
      (finally (stop! s)))))

(deftest the-warm-tier-is-built-off-the-owner-and-kept-apart-from-the-cache
  (let [tasks (atom []) calls (atom {}) built (atom [])
        context (programs/context (fn [program] (swap! calls update program (fnil inc 0)) [:step program])
                                  (fn [task] (swap! tasks conj task)))
        run! #(loop [] (when-let [queued (seq @tasks)] (reset! tasks []) (doseq [t queued] (t)) (recur)))
        warm (probe 0 1)]
    (programs/warm! context :registry-1 (fn [] (swap! built conj (Thread/currentThread)) [warm]))
    (is (= [] @built) "Building the list waits for the worker")
    (run!)
    (is (= 1 (count @built)))
    (programs/warm! context :registry-1 (fn [] (swap! built conj :again) [warm]))
    (run!)
    (is (= 1 (count @built)) "An unchanged key builds nothing")
    (doseq [i (range 600)] (programs/compile! context (probe 1 (+ 2 i))))
    (is (= [:step warm] (programs/compile! context warm)) "Six hundred other programs do not evict it")
    (is (= 1 (get @calls warm)))))

(deftest a-program-a-reload-queued-first-still-lands-in-the-warm-tier
  ;; A frame's request queues the program first; the warm rebuild then sees it
  ;; queued and leaves it. When the request result is dropped, the warm tier
  ;; must still hold it for the next launch.
  (let [tasks (atom [])
        context (programs/context (fn [program] [:step program]) (fn [task] (swap! tasks conj task)))
        run! #(loop [] (when-let [queued (seq @tasks)] (reset! tasks []) (doseq [t queued] (t)) (recur)))
        program (probe 0 2)]
    (testing "requested, then the rebuild"
      (programs/request! context [program])
      (programs/warm! context :registry-2 (fn [] [program]))
      (run!)
      (programs/request! context [])
      (is (programs/ready? context program)))
    (testing "the rebuild queued first, then requested in the same frame"
      (let [context (programs/context (fn [program] [:step program]) (fn [task] (swap! tasks conj task)))]
        (programs/warm! context :registry-3 (fn [] [program]))
        (programs/request! context [program])
        (run!)
        (programs/request! context [])
        (is (programs/ready? context program))))))

(deftest a-bulk-republish-never-recompiles-running-programs
  ;; 600 effects are republished while three instances keep running. The warm
  ;; tier churns through 600 programs; the running programs stay compiled.
  (let [extra (fn [i version] {:id (keyword (str "bulk-" i)) :version version :targets [:head-8]
                               :params {} :outputs {:dimmer {:op :const :value (/ (mod (+ i version) 10) 10.0)}}})
        bulk-model (reduce (fn [m i] (assoc-in m [:registry :effects (keyword (str "bulk-" i))] (extra i 1)))
                           (show-model 3) (range 600))
        s (show 3 {:model bulk-model})]
    (try
      (let [t (launch-all! s 3 nil)
            running (set (keep #(when (#{:probe-0 :probe-1 :probe-2} (:effect-id %)) [(:effect-id %) (:effect-version %)])
                               (vals (:instances (model s)))))]
        (work! s)
        (reset! (:compiled s) [])
        (publish! s (mapv #(extra % 2) (range 600)) t)
        (work! s)
        (dotimes [k 3] (tick! s (+ t (* 25 (inc k)))))
        (is (= 3 (count running)))
        (is (= [] (filterv #(and (= :frame (first %)) (contains? running [(nth % 1) (nth % 2)])) @(:compiled s)))
            "No running program recompiles in a frame")
        (is (= [1 1 1] (versions s 3))))
      (finally (stop! s)))))

(deftest a-cancelled-compile-is-retried-once
  (let [tasks (atom []) attempts (atom 0)
        context (programs/context (fn [program] (swap! attempts inc) (throw (java.util.concurrent.CancellationException. "cancelled")))
                                  (fn [task] (swap! tasks conj task)))
        run! #(loop [] (when-let [queued (seq @tasks)] (reset! tasks []) (doseq [t queued] (t)) (recur)))
        program (probe 0 2)]
    (programs/request! context [program])
    (run!)
    (is (not (programs/ready? context program)) "A first cancellation leaves it uncompiled")
    (programs/request! context [program])
    (run!)
    (is (= 2 @attempts))
    (is (thrown? java.util.concurrent.CancellationException (programs/compile! context program))
        "A second one is kept and rethrown on the owner")))

(deftest an-out-of-memory-error-while-compiling-fails-one-frame-then-recovers
  ;; Not attributed to the revision: the owner meets the original error once,
  ;; as an inline compile would, and the next request compiles again.
  (doseq [inline? [false true]]
    (testing (if inline? "inline" "worker")
      (let [s (show 1 {:inline? inline?})
            failures (atom 1)]
        (try
          (let [t (launch-all! s 1 nil)]
            (swap! (:compiler-context (:rt s)) update :compiler
                   (fn [compiler] (fn [program]
                                    (when (and (= [:probe-0 2] [(:id program) (:version program)]) (pos? @failures))
                                      (swap! failures dec)
                                      (throw (OutOfMemoryError. "test")))
                                    (compiler program))))
            (show/submit! (:rt s) :definition {:kind :effects :definition (probe 0 2 {:outputs {:dimmer {:op :const :value 1.0}}})})
            (let [outcomes (mapv (fn [k] (work! s)
                                   (try (tick! s (+ t (* 25 k))) :ok (catch OutOfMemoryError _ :oom)))
                                 (range 8))]
              (is (= 1 (count (filter #{:oom} outcomes))) "One frame fails")
              (is (every? #{:ok} (drop 3 outcomes)) "Later frames render")
              ;; Inline, the error escapes the owner cycle that admitted the
              ;; publication, which is then not committed, as on master.
              (if inline?
                (is (= [(get-in (model s) [:registry :effects :probe-0 :version])] (versions s 1)))
                (do (is (= [2] (versions s 1)) "The revision adopts once it compiles")
                    (is (= 255 (first (dimmers s 1))))))
              (is (nil? (get-in (instances s) [:probe-0 :reload-fault])) "No permanent refusal")))
          (finally (stop! s)))))))

(deftest a-second-cancellation-fails-one-frame-then-recovers
  (let [s (show 1)
        cancels (atom 2)]
    (try
      (let [t (launch-all! s 1 nil)]
        (swap! (:compiler-context (:rt s)) update :compiler
               (fn [compiler] (fn [program]
                                (when (and (= [:probe-0 2] [(:id program) (:version program)]) (pos? @cancels))
                                  (swap! cancels dec)
                                  (throw (java.util.concurrent.CancellationException. "test")))
                                (compiler program))))
        (publish! s [(probe 0 2 {:outputs {:dimmer {:op :const :value 1.0}}})] t)
        (let [frames (mapv (fn [k] (work! s) (tick! s (+ t (* 25 (inc k))))
                             [(get-in (model s) [:frame-number]) (some? (get-in @(:state (:rt s)) [:status :error]))])
                           (range 10))]
          (is (= 1 (count (filter second frames))) "One frame fails")
          (is (apply < (map first (drop 3 frames))) "Later frames advance")
          (is (= [2] (versions s 1)))
          (is (= 255 (first (dimmers s 1))))))
      (finally (stop! s)))))

(deftest a-pad-press-resolves-no-cue-selection-on-the-owner
  ;; Twenty selector cues. Scheduling a grid launch must not resolve them on
  ;; the frame owner, and editing one cue re-resolves only that cue.
  (let [cues (into {} (for [i (range 20)]
                        [(keyword (str "sel-" i)) {:id (keyword (str "sel-" i)) :version 1 :effect-id :probe-0
                                                   :selector {:fixture-ids [(head (mod i 8))]}}]))
        s (show 1 {:model (-> (show-model 1)
                              (assoc-in [:registry :effects :probe-0] (assoc (probe 0 1) :targets (mapv head (range 8))))
                              (update-in [:registry :cues] merge cues))})
        resolve-command targets/resolve-command
        resolutions (atom [])]
    (with-redefs [targets/resolve-command (fn [model command]
                                            (swap! resolutions conj [@(:phase s) (:cue-id command)])
                                            (resolve-command model command))]
      (try
        (tick! s 0)
        (work! s)
        (is (= 20 (count (filter #(= :worker (first %)) @resolutions))) "The first build resolves each cue once, on the worker")
        (reset! resolutions [])
        (show/submit! (:rt s) :command {:op :cue/launch :cue-id :sel-3 :grid {:beats 1} :command-id "pad"
                                        :source {:adapter :repl :session-id "pad"}})
        (tick! s 25)
        (work! s)
        (is (= [] (filterv #(= :frame (first %)) @resolutions)) "The pad press resolved nothing on the owner")
        (is (= [[:worker :sel-3]] @resolutions) "Only the scheduled launch is resolved, on the worker")
        (reset! resolutions [])
        (is (= :accepted (:status (request! s 50 :command {:op :cue/put :cue (-> (get cues :sel-5)
                                                                                  (dissoc :version)
                                                                                  (assoc :selector {:fixture-ids [(head 1)]}))}))))
        (work! s)
        (is (= [[:worker :sel-5] [:worker :sel-3]] @resolutions)
            "Editing one cue re-resolves only that cue, plus the launch still scheduled")
        (finally (stop! s))))))

(deftest a-running-program-served-from-the-warm-tier-survives-a-rebuild
  ;; Probe 0 launches on its warmed program. Publishing a revision rebuilds
  ;; the warm tier without version 1; the running instance's program must not
  ;; leave with it, or the frames after would compile it again, here as the
  ;; fallback of a revision rejected at evaluation.
  (let [s (show 1)]
    (try
      (tick! s 0)
      (work! s)
      (reset! (:compiled s) [])
      (request! s 25 :command {:op :cue/launch :cue-id :probe-0})
      (is (= [] (filterv #(= :frame (first %)) @(:compiled s))) "It launched on the warm program")
      (publish! s [(-> (probe 0 2)
                       (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                       (assoc-in [:outputs :dimmer] {:op :divide :args [(param :level) (param :divisor)]}))] 50)
      (work! s)
      (dotimes [k 3] (tick! s (+ 75 (* 25 k))))
      (is (= :evaluate (get-in (instances s) [:probe-0 :reload-fault :stage])))
      (is (= [] (filterv #(= :frame (first %)) @(:compiled s))) "Nothing compiled in a frame")
      (finally (stop! s)))))

(deftest an-unrelated-settings-change-resolves-no-cue-again
  (let [cues (into {} (for [i (range 20)]
                        [(keyword (str "sel-" i)) {:id (keyword (str "sel-" i)) :version 1 :effect-id :probe-0
                                                   :selector {:fixture-ids [(head (mod i 8))]}}]))
        s (show 1 {:model (-> (show-model 1)
                              (assoc-in [:registry :effects :probe-0] (assoc (probe 0 1) :targets (mapv head (range 8))))
                              (update-in [:registry :cues] merge cues))})
        resolve-command targets/resolve-command
        resolutions (atom [])]
    (with-redefs [targets/resolve-command (fn [model command]
                                            (swap! resolutions conj (:cue-id command))
                                            (resolve-command model command))]
      (try
        (tick! s 0)
        (work! s)
        (reset! resolutions [])
        (is (= :accepted (:status (request! s 25 :settings {:scope :output :values {:presentation {:mode :hold-fade :fade-ms 100}}}))))
        (work! s)
        (tick! s 50)
        (work! s)
        (is (= [] @resolutions) "No cue selection is resolved again")
        (finally (stop! s))))))

(deftest a-launch-keeps-values-the-older-revision-cannot-hold
  ;; Version 2 adds :gain. A launch asking for gain 0.2, or a cue whose saved
  ;; values set it, cannot start on version 1 without losing it, so it keeps
  ;; the revision it captured; a launch that only uses the defaults starts on
  ;; version 1 and adopts with its sibling.
  (let [v2 (-> (probe 0 2) (assoc-in [:params :gain] {:type :number :default 1.0 :bounds [0.0 1.0]})
               (assoc :outputs {:dimmer {:op :multiply :args [(param :level) (param :gain)]}}))]
    (doseq [[label launch cue-update expected] [["launch value" {:params {:gain 0.2}} identity [1 2]]
                                                ["saved cue value" {} #(assoc % :saved-params {:gain 0.3}) [1 2]]
                                                ["defaults only" {} identity [1 1]]]]
      (testing label
        (let [s (show 1)]
          (try
            (let [t (launch-all! s 1 nil)]
              (publish! s [v2] t)
              (when-not (= identity cue-update)
                (is (= :accepted (:status (request! s (+ t 25) :command {:op :cue/put :cue (dissoc (cue-update (get-in (model s) [:registry :cues :probe-0])) :version)})))))
              (show/submit! (:rt s) :command {:op :cue/launch :cue-id :probe-0 :launch (assoc launch :reload-policy :preserve-state)})
              (tick! s (+ t 50))
              (is (= expected (map first (rendered s :probe-0))))
              (work! s)
              (tick! s (+ t 75))
              (is (= [2 2] (map first (rendered s :probe-0))) "Both run version 2 once it is compiled")
              (let [launched (last (sort-by :order (filter #(= :probe-0 (:effect-id %)) (vals (:instances (model s))))))]
                (is (= (case label "launch value" 0.2 "saved cue value" 0.3 "defaults only" 1.0)
                       (get-in launched [:params :gain])) "The launch keeps its value")))
            (finally (stop! s))))))))

(deftest a-cancellation-is-forgotten-once-no-frame-wants-the-program
  (let [tasks (atom []) attempts (atom 0)
        context (programs/context (fn [program] (swap! attempts inc) (throw (java.util.concurrent.CancellationException. "cancelled")))
                                  (fn [task] (swap! tasks conj task)))
        run! #(loop [] (when-let [queued (seq @tasks)] (reset! tasks []) (doseq [t queued] (t)) (recur)))
        program (probe 0 2)]
    (programs/request! context [program])
    (run!)
    (programs/request! context [])
    (programs/request! context [program])
    (run!)
    (is (= 2 @attempts))
    (is (not (programs/ready? context program))
        "A superseded program's earlier cancellation does not count against a later request")))

(deftest a-launch-on-a-head-only-the-new-revision-targets-keeps-that-revision
  ;; Version 2 widens the effect from head-1 to head-1 and head-2. A launch on
  ;; head-2 while version 2 compiles would reach no fixture on version 1, so
  ;; it keeps version 2 and lights head-2 on its first frame.
  (let [s (show 1)]
    (try
      (let [t (launch-all! s 1 nil)]
        (publish! s [(assoc (probe 0 2) :targets [(head 0) (head 1)])] t)
        (show/submit! (:rt s) :command {:op :cue/launch :cue-id :probe-0
                                        :launch {:reload-policy :preserve-state :targets [(head 1)]}})
        (tick! s (+ t 25))
        (is (= [1 2] (map first (rendered s :probe-0))) "The launch keeps the revision that reaches its head")
        (is (= 128 (nth (peek @(:output s)) (head-dimmer 1))) "Head-2 renders the launch's level on its first frame"))
      (finally (stop! s)))))

(deftest a-program-that-keeps-failing-is-reported-and-retried
  ;; Six out-of-memory failures in a row, then the compiler recovers. After
  ;; five, the frame status names the program; once it compiles, the revision
  ;; is adopted and the report clears.
  (let [s (show 1)
        failures (atom 6)
        reported (atom nil)]
    (try
      (let [t (launch-all! s 1 nil)]
        (swap! (:compiler-context (:rt s)) update :compiler
               (fn [compiler] (fn [program]
                                (when (and (= [:probe-0 2] [(:id program) (:version program)]) (pos? @failures))
                                  (swap! failures dec)
                                  (throw (OutOfMemoryError. "test")))
                                (compiler program))))
        (publish! s [(probe 0 2 {:outputs {:dimmer {:op :const :value 1.0}}})] t)
        (dotimes [k 24]
          (work! s)
          (try (tick! s (+ t (* 25 (inc k)))) (catch OutOfMemoryError _ nil))
          (when-let [report (get-in @(:state (:rt s)) [:status :compile-failures [:probe-0 2]])]
            (reset! reported report)))
        (is (<= 5 (:failures @reported)) "The repeated failure was reported")
        (is (= "java.lang.OutOfMemoryError" (:error @reported)))
        (is (= [2] (versions s 1)) "It kept retrying and adopted the revision once it compiled")
        (is (nil? (get-in @(:state (:rt s)) [:status :compile-failures])) "The report clears"))
      (finally (stop! s)))))
