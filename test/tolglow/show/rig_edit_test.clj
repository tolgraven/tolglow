(ns tolglow.show.rig-edit-test
  "A rig edit compiles off the frame (#811). The owner holds a message that
  changes the patched fixtures while the programs worker compiles the new
  patch and every running effect's program for the new rig, then applies it
  in a cycle whose frame renders the new rig with them. The worker is
  stepped by hand on the mixed 2,048-pixel/64-head rig with 32 running
  effects: assertions count compiles, owner cycles and captured DMX, never
  clocks."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.bench-show :as bench]
            [tolglow.compiler.batch :as batch]
            [tolglow.show :as show]
            [afterglow.render.frame :as render]
            [tolglow.show.programs :as programs])
  (:import [java.util.concurrent RejectedExecutionException]))

(def ^:private probes
  "Probe effects launched beside the corpus's four, for 32 running effects."
  28)

(defn- probe-id [i] (keyword (str "probe-" i)))

(defn- mixed-model
  "The mixed corpus (spatial, gain, accent and movement running) with a
  dimmer probe launched on each of the first 28 moving heads."
  []
  (let [{:keys [model fixtures]} (bench/corpus 2048 64)
        heads (filterv #(str/starts-with? (:id %) "mover-") fixtures)]
    (reduce (fn [model i]
              (let [id (probe-id i)]
                (-> model
                    (bench/command [id :definition]
                                   {:op :registry/publish :kind :effects
                                    :definition {:id id :version 1 :targets [(:id (nth heads i))]
                                                 :params {:level {:type :number :default 0.6 :bounds [0.0 1.0]}}
                                                 :outputs {:dimmer {:op :param :key :level}}}})
                    (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id :priority 20 :reload-policy :preserve-state}})
                    (bench/command [id :launch] {:op :cue/launch :cue-id id}))))
            model (range probes))))

(defn- counting-compilers
  "Wrap every rig compiler made while `f` runs, including those a rig edit
  makes, so each call records [phase effect-id]: phase is [:frame show] in
  a tick and [:worker show] in `work!`."
  [compiled phase f]
  (let [make batch/compiler-for-rig]
    (with-redefs [batch/compiler-for-rig (fn [& args]
                                           (let [compiler (apply make args)]
                                             (fn [program]
                                               (swap! compiled conj [@phase (:id program)])
                                               (compiler program))))]
      (f))))

(defn- show
  "A runtime named `label` on `model` with a capture sink keeping every
  frame's universes. With `worker?` its compile worker is a queue stepped by
  `work!`; while :rejecting is true it refuses every task, as a closed worker
  does."
  [label model worker?]
  (let [frames (atom []) tasks (atom []) rejecting (atom false)
        rt (show/create-runtime (cond-> {:model model :workspace-options nil :effect-log! (fn [_]) :workers 2
                                         :sink {:send! (fn [universes _]
                                                         (swap! frames conj (into (sorted-map)
                                                                                  (map (fn [[u bytes]] [u (vec bytes)]))
                                                                                  universes)))}}
                                  worker? (assoc :background-compile
                                                 (fn [task]
                                                   (when @rejecting (throw (RejectedExecutionException. "worker closed")))
                                                   (swap! tasks conj task)))))]
    {:label label :rt rt :frames frames :tasks tasks :rejecting rejecting}))

(defn- work!
  "Run every queued worker task, including those the tasks queue; returns
  how many ran."
  [{:keys [tasks label]} phase]
  (reset! phase [:worker label])
  (try
    (loop [ran 0]
      (if-let [run (seq @tasks)]
        (do (reset! tasks [])
            (doseq [task run] (task))
            (recur (+ ran (count run))))
        ran))
    (finally (reset! phase nil))))

(defn- tick! [{:keys [rt label]} phase t]
  (reset! phase [:frame label])
  (try (show/tick! rt t) (finally (reset! phase nil))))

(defn- model [s] (:model @(:state (:rt s))))

(defn- compiles
  "Effect IDs compiled in `kind` (:frame or :worker) for show `s`."
  [compiled kind s]
  (into [] (keep (fn [[phase id]] (when (= phase [kind (:label s)]) id))) @compiled))

(defn- moved-rig
  "The rig with mover-5 a metre further back, so the movement effect aims
  it differently."
  [model]
  (update (:rig model) :fixtures
          (fn [fixtures] (mapv #(if (= "mover-5" (:id %)) (update-in % [:position 2] + 1.0) %) fixtures))))

(def ^:private mover-5-pan
  "[universe index] of mover-5's coarse pan in the captured frames."
  (let [offset (+ (* 4 2048) (* 12 5))]
    [(inc (quot offset 512)) (+ (mod offset 512) 5)]))

(defn- running-effects [s] (set (map :effect-id (vals (:instances (model s))))))

(defn- warm-up!
  "Tick `n` frames from t = 25, running the worker after each."
  [s phase n]
  (doseq [i (range n)]
    (tick! s phase (* 25 (inc i)))
    (work! s phase)))

(defn- first-difference
  "The index of the first captured frame that differs between `a` and `b`."
  [a b]
  (first (keep-indexed (fn [i [x y]] (when (not= x y) i)) (map vector a b))))

(defn- stop! [& shows] (doseq [s shows] (show/stop! (:rt s))))

(deftest a-rig-edit-compiles-nothing-in-the-frame-and-adopts-the-next-cycle
  (let [compiled (atom []) phase (atom nil) base (mixed-model)]
    (counting-compilers compiled phase
      (fn []
        (let [worker (show :worker base true) inline (show :inline base false) unedited (show :unedited base false)
              shows [worker inline unedited]]
          (try
            (doseq [s shows] (warm-up! s phase 6))
            (is (= 32 (count (running-effects worker))) "32 running effects on the mixed rig")
            (reset! compiled [])
            (let [edited (moved-rig (model worker))
                  reply (show/submit! (:rt worker) :rig edited)]
              (doseq [s shows] (tick! s phase 175))
              (testing "the cycle it arrives in holds it while the worker compiles"
                (is (nil? (deref reply 0 nil)))
                (is (= [:rig] (mapv :type @(:held-messages (:rt worker)))))
                (is (not= (:fixtures edited) (get-in (model worker) [:rig :fixtures]))))
              (work! worker phase)
              (is (= (running-effects worker) (set (take 32 (compiles compiled :worker worker))))
                  "The worker compiles the 32 running programs for the new rig first")
              (let [inline-reply (show/submit! (:rt inline) :rig (moved-rig (model inline)))]
                (doseq [s [worker inline unedited]] (tick! s phase 200))
                (is (= :accepted (:status (deref reply 0 nil))) "It applies in the next cycle: adoption latency 1 frame")
                (is (= :accepted (:status (deref inline-reply 0 nil))) "Without a worker it applies at once, as before"))
              (is (= (:fixtures edited) (get-in (model worker) [:rig :fixtures])))
              (is (empty? @(:held-messages (:rt worker))))
              (doseq [i (range 4)]
                (doseq [s shows] (tick! s phase (+ 225 (* 25 i))))
                (work! worker phase))
              (is (= [] (compiles compiled :frame worker)) "No compile inside a frame")
              (is (= 32 (count (compiles compiled :frame inline))) "The inline show compiled all 32 in its edit frame")
              (is (nil? (get-in @(:state (:rt worker)) [:status :error])))
              (testing "output is continuous: every frame equals the show that compiled the edit inline"
                (let [w @(:frames worker) i @(:frames inline) u @(:frames unedited)
                      edit-frame 7]
                  (is (= 12 (count w) (count i) (count u)))
                  (is (nil? (first-difference w i)) "The same DMX in every frame, so no blank or mixed frame")
                  (is (= edit-frame (first-difference u i)) "Identical to the unedited show until the edit frame")
                  (is (not= (get-in (nth u edit-frame) mover-5-pan) (get-in (nth i edit-frame) mover-5-pan))
                      "The edit frame already aims mover-5 from its new position")
                  (is (every? (fn [frame] (some pos? (mapcat val frame))) w) "No blank frame"))))
            (finally (apply stop! shows))))))))

;; The worker steps below run only the tasks a test names; `stall` keeps the
;; rig edit's compiles queued while publication checks may run.

(defn- run-checks!
  "Run only the publication checks queued on the worker."
  [{:keys [tasks]}]
  (let [checks (filterv programs/ahead? @tasks)]
    (swap! tasks #(into [] (remove (set checks)) %))
    (doseq [task checks] (task))))

(defn- status [s] (:status @(:state (:rt s))))

(defn- reply-status [reply] (:status (deref reply 0 nil)))

(defn- publish-fixture-type!
  "Publish an RGBW fixture type, so `:fixture-patch` can patch fixtures."
  [s phase t]
  (let [reply (show/submit! (:rt s) :definition {:kind :fixtures
                                                 :definition {:id :rgbw :name "RGBW" :version 1 :footprint 4
                                                              :channels {:red 1 :green 2 :blue 3 :white 4}}})]
    (work! s phase) (tick! s phase t) (work! s phase) (tick! s phase (+ t 5)) (work! s phase)
    (is (= :accepted (reply-status reply)))))

(deftest launches-go-ahead-of-a-held-rig-edit-and-join-what-it-waits-for
  ;; Master, parameter, binding and launch input go ahead of a rig edit the
  ;; worker has not compiled for; a publication waits behind it (#784). A
  ;; launch, immediate or quantized, runs on the current rig and the edit
  ;; waits for its program on the new rig too.
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              rt (:rt s)]
          (try
            (warm-up! s phase 6)
            (reset! compiled [])
            (let [probe (some #(when (= :probe-0 (:cue-id %)) (:id %)) (vals (:instances (model s))))
                  edited (moved-rig (model s))
                  replies {:rig (show/submit! rt :rig edited)
                           :publication (show/submit! rt :definition
                                                      {:kind :effects
                                                       :definition (assoc (get-in (model s) [:registry :effects :probe-1]) :version 2
                                                                          :name "Probe 1 again")})
                           :launch (show/submit! rt {:op :cue/launch :cue-id :accent})
                           :quantized (show/submit! rt {:op :cue/launch :cue-id :gain :grid {:beats 1} :command-id "on-the-beat"
                                                        :source {:adapter :repl :session-id "test"}})
                           :master (show/submit! rt {:op :master/set :value 0.5})
                           :param (show/submit! rt {:op :param/set :instance-id probe :param :level :value 0.3})
                           :bind (show/submit! rt :modulation-bind {:instance-id probe :param :level
                                                                    :binding {:type :lfo :waveform :sine :period {:bars 2}
                                                                              :from 0.2 :to 0.4}})}
                  statuses #(update-vals replies reply-status)]
              (run-checks! s)
              (tick! s phase 175)
              (is (= {:rig nil :publication nil :launch :accepted :quantized :pending :master :accepted :param :accepted
                      :bind :accepted}
                     (statuses))
                  "Launches, master, parameter and binding input apply while the rig edit waits")
              (is (= [:rig :definition] (mapv :type @(:held-messages rt))))
              (let [due (:due-beat (deref (:quantized replies) 0 nil))]
                ;; Tick to the quantized launch's beat with the worker stalled.
                (loop [t 200]
                  (when (< (get-in (model s) [:transport :beat]) due)
                    (tick! s phase t)
                    (recur (+ t 25))))
                (is (= 2 (count (filter #(= :gain (:cue-id %)) (vals (:instances (model s))))))
                    "The quantized launch fired on its beat, on the current rig")
                (is (nil? (reply-status (:rig replies))) "The rig edit still waits")
                (is (= (count (running-effects s)) 32))
                (is (= (count (distinct (map (juxt :effect-id :targets) (vals (:instances (model s))))))
                       (get-in (status s) [:rig-edit-pending :programs]))
                    "The launches joined what the edit waits for"))
              (work! s phase)
              (tick! s phase 1000)
              (is (every? #{:accepted} (vals (dissoc (statuses) :quantized))) (pr-str (statuses)))
              (is (= :fired (:status (show/quantized-cue-status rt "on-the-beat"))) "The quantized launch kept its beat")
              (is (nil? (:rig-edit-pending (status s))))
              (is (= (:fixtures edited) (get-in (model s) [:rig :fixtures])))
              (doseq [t [1025 1050]] (work! s phase) (tick! s phase t))
              (is (= 2 (some #(when (= :probe-1 (:cue-id %)) (:effect-version %)) (vals (:instances (model s)))))
                  "The publication adopts on the new rig")
              (is (= [] (compiles compiled :frame s)) "Nothing compiles inside a frame, the launches included")
              (is (nil? (:error (status s)))))
            (finally (stop! s))))))))

(defn- other-work!
  "Submit an unrelated task through the show's compile worker and run only
  it, as a worker busy with other compiles finishes them."
  [{:keys [rt tasks]}]
  ((#'show/compile-submit (:compile-worker rt)) (fn []))
  (let [task (peek @tasks)]
    (swap! tasks pop)
    (task)))

(deftest a-busy-worker-holds-a-rig-edit-without-a-bound-and-compiles-nothing-in-frames
  ;; The worker finishes another task every cycle but not the edit's: the edit
  ;; waits however long that takes, past the stall watchdog's bound.
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              stall-cycles (quot @#'show/rig-edit-stall-ms 25)]
          (try
            (warm-up! s phase 6)
            (reset! compiled [])
            (let [reply (show/submit! (:rt s) :rig (moved-rig (model s)))
                  blackout (show/submit! (:rt s) {:op :blackout/set :value true})]
              (doseq [i (range (* 2 stall-cycles))]
                (tick! s phase (+ 175 (* 25 i)))
                (other-work! s))
              (is (= :accepted (reply-status blackout)) "Blackout went ahead in the first cycle")
              (is (nil? (deref reply 0 nil)) "Still waiting after twice the stall bound: there is no budget")
              (is (= {:since-frame 7 :patch-ready? false :programs 32 :programs-ready 0}
                     (:rig-edit-pending (status s)))
                  "The status shows the pending edit")
              (is (= [] (compiles compiled :frame s)))
              (work! s phase)
              (tick! s phase 10000)
              (is (= :accepted (reply-status reply)) "Applied in the cycle after the worker finished")
              (is (= [] (compiles compiled :frame s)))
              (is (nil? (:error (status s)))))
            (finally (stop! s))))))))

(deftest a-hung-worker-never-locks-the-queue-and-the-edit-resolves-after-the-stall-bound
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              rt (:rt s)
              stall-cycles (quot @#'show/rig-edit-stall-ms 25)]
          (try
            (warm-up! s phase 6)
            (reset! compiled [])
            ;; From here the worker runs nothing, as if its current task hung.
            (let [reply (show/submit! rt :rig (moved-rig (model s)))
                  _ (tick! s phase 175)
                  ;; Registry writes pile up behind the held edit, up to the queue's capacity.
                  writes (mapv #(show/submit! rt {:op :cue/put :cue {:id (keyword (str "c" %)) :effect-id :gain}}) (range 300))
                  _ (tick! s phase 200)
                  refused (filterv #(= :rejected (reply-status %)) writes)
                  blackout (show/submit! rt {:op :blackout/set :value true})
                  master (show/submit! rt {:op :master/set :value 0.4})]
              (is (seq refused) "Dependent writes past the queue's 256 slots are refused")
              (is (= 256 (count @(:held-messages rt))) "The held edit and 255 writes fill it")
              (tick! s phase 225)
              (is (= [:accepted :accepted] (mapv reply-status [blackout master]))
                  "Blackout and master are never crowded out by held messages")
              (is (nil? (deref reply 0 nil)))
              (doseq [i (range stall-cycles)] (tick! s phase (+ 250 (* 25 i))))
              (is (= :accepted (reply-status reply)) "The stall watchdog applied it through the worker-failure path")
              (is (= 32 (count (compiles compiled :frame s))) "and that frame compiled the running programs")
              (is (every? #(#{:accepted} (reply-status %)) (remove (set refused) writes)) "The writes behind it landed")
              (is (empty? @(:held-messages rt)))
              (is (nil? (:error (status s)))))
            (finally (stop! s))))))))

(defn- with-wash
  "`model` with a :wash dimmer effect and its cue, selecting `selector`."
  [model selector]
  (-> model
      (bench/command [:wash :definition]
                     {:op :registry/publish :kind :effects
                      :definition {:id :wash :version 1
                                   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
                                   :outputs {:dimmer {:op :param :key :level}}}})
      (bench/command [:wash :cue] {:op :cue/put :cue (cond-> {:id :wash :effect-id :wash :priority 5}
                                                       selector (assoc :selector selector))})))

(deftest a-launch-during-the-hold-does-not-delay-the-release
  ;; Its program is queued at the end of the cycle it launched in.
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              rt (:rt s)]
          (try
            (warm-up! s phase 6)
            ;; Targets no warm program has: the launch frame compiles its
            ;; old-rig program (a launch never waits), the edit its new one.
            (let [reply (show/submit! rt :rig (moved-rig (model s)))
                  launch (show/submit! rt {:op :cue/launch :cue-id :accent :launch {:targets ["pixel-7" "pixel-8"]}})]
              (tick! s phase 175)
              (is (= [nil :accepted] (mapv reply-status [reply launch])))
              (reset! compiled [])
              (work! s phase)
              (tick! s phase 200)
              (is (= :accepted (reply-status reply)) "Released in the next cycle")
              (is (= [] (compiles compiled :frame s)) "with the launch's new-rig program compiled on the worker"))
            (finally (stop! s))))))))

(deftest a-launch-during-the-hold-is-re-targeted-for-the-edited-rig
  ;; A cue selecting every fixture is launched while an edit that removes
  ;; mover-5 and adds mover-new waits; at release its targets are those a
  ;; launch after the edit resolves, as in an inline show.
  (let [compiled (atom []) phase (atom nil)
        base (with-wash (mixed-model) :all)
        edit (fn [model]
               (update (:rig model) :fixtures
                       (fn [fixtures]
                         (let [mover (some #(when (= "mover-5" (:id %)) %) fixtures)]
                           (conj (filterv #(not= "mover-5" (:id %)) fixtures)
                                 (assoc mover :id "mover-new" :universe 30 :address 1))))))
        wash (fn [s] (some #(when (= :wash (:cue-id %)) (:targets %)) (vals (:instances (model s)))))]
    (counting-compilers compiled phase
      (fn []
        (let [w (show :worker base true) i (show :inline base false)]
          (try
            (doseq [s [w i]] (warm-up! s phase 6))
            (reset! compiled [])
            (let [rw (show/submit! (:rt w) :rig (edit (model w)))
                  ri (show/submit! (:rt i) :rig (edit (model i)))]
              (doseq [s [w i]] (tick! s phase 175))
              (let [lw (show/submit! (:rt w) {:op :cue/launch :cue-id :wash})
                    li (show/submit! (:rt i) {:op :cue/launch :cue-id :wash})]
                (doseq [s [w i]] (tick! s phase 200))
                (is (= [nil :accepted :accepted :accepted] (mapv reply-status [rw ri lw li])))
                (is (some #{"mover-5"} (wash w)) "Launched on the old rig while the edit waits")
                (work! w phase)
                (doseq [s [w i]] (tick! s phase 225))
                (is (= :accepted (reply-status rw)))
                (is (= 2112 (count (wash i))))
                (is (= (wash i) (wash w)) "The same targets, in the same order, as a launch after the edit")
                (is (not-any? #{"mover-5"} (wash w)))
                (is (some #{"mover-new"} (wash w)))
                (doseq [t [250 275]] (doseq [s [w i]] (tick! s phase t)) (work! w phase))
                (is (= [] (compiles compiled :frame w)) "Its program for the new targets compiled on the worker")
                (let [fw @(:frames w) fi @(:frames i)]
                  (is (nil? (first-difference (drop 9 fw) (drop 9 fi))) "The same DMX as the inline show from the release frame on"))
                (is (every? nil? (map #(:error (status %)) [w i])))))
            (finally (stop! w i))))))))

(deftest a-worker-that-refuses-the-edit-compiles-its-patch-at-once
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)]
          (try
            (warm-up! s phase 6)
            (reset! compiled [])
            (reset! (:rejecting s) true)
            (let [edited (moved-rig (model s))
                  reply (show/submit! (:rt s) :rig edited)]
              (tick! s phase 175)
              (is (= :accepted (reply-status reply)) "Applied in its own cycle")
              (is (= (:fixtures edited) (get-in (model s) [:rig :fixtures])))
              (is (= 32 (count (compiles compiled :frame s))) "That frame compiles the running programs")
              (is (nil? @(:rig-preparation (:rt s))))
              (is (nil? (:error (status s)))))
            (finally (stop! s))))))))

(deftest a-second-fixture-change-in-the-release-cycle-keeps-the-first-edits-programs
  ;; Review of #881: the edit released in a cycle keeps its compiled context
  ;; when a later message in that cycle changes the fixtures again.
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              rt (:rt s)]
          (try
            (warm-up! s phase 6)
            (publish-fixture-type! s phase 160)
            (reset! compiled [])
            (let [edited (moved-rig (model s))
                  a (show/submit! rt :rig edited)]
              (tick! s phase 175)
              (work! s phase)
              (let [b (show/submit! rt :fixture-patch {:id "extra-1" :fixture-type-id :rgbw :universe 40 :address 1})]
                (tick! s phase 200)
                (is (= :accepted (reply-status a)))
                (is (nil? (deref b 0 nil)) "The fixture patch waits for its own compiles")
                (is (= [:fixture-patch] (mapv :type @(:held-messages rt))))
                (is (= (:fixtures edited) (get-in (model s) [:rig :fixtures])))
                (is (= [] (compiles compiled :frame s)) "The released edit's programs are used, not compiled again")
                (work! s phase)
                (tick! s phase 225)
                (is (= :accepted (reply-status b)))
                (is (= "extra-1" (:id (peek (get-in (model s) [:rig :fixtures])))))
                (is (= [] (compiles compiled :frame s)))
                (is (nil? (:error (status s))))))
            (finally (stop! s))))))))

(deftest a-held-edit-refused-on-re-apply-drops-its-preparation
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              rt (:rt s)]
          (try
            (warm-up! s phase 6)
            (let [reply (show/submit! rt :rig (moved-rig (model s)))]
              (tick! s phase 175)
              (let [{:keys [context]} @(:rig-preparation rt)]
                (is (some? context))
                ;; Another writer replaces the rig the edit was based on.
                (swap! (:state rt) update-in [:model :rig] assoc :note "edited elsewhere")
                (tick! s phase 200)
                (is (= :rejected (reply-status reply)))
                (is (re-find #"Rig changed while editing" (:message (deref reply 0 nil))))
                (is (nil? @(:rig-preparation rt)) "The preparation is dropped")
                (is (programs/worker-failed? context) "and its context retired, so its queued work stores nothing")
                (is (nil? (:rig-edit-pending (status s))))))
            (finally (stop! s))))))))

(defn- poison-patch
  "`render/compile-patch` refusing any rig with a fixture named poison."
  [compile-patch]
  (fn [fixtures]
    ;; A single fixture's own check, as fixture instantiation runs it, passes.
    (when (and (next fixtures) (some #(= "poison" (:id %)) fixtures)) (throw (ex-info "bad patch" {})))
    (compile-patch fixtures)))

(deftest a-patch-the-worker-cannot-compile-refuses-only-that-edit
  (doseq [rejecting? [false true]]
    (testing (if rejecting? "the worker refuses the task: refused in its own cycle" "refused once the worker compiled it")
      (let [compiled (atom []) phase (atom nil)]
        (counting-compilers compiled phase
          (fn []
            (with-redefs [render/compile-patch (poison-patch render/compile-patch)]
              (let [s (show :worker (mixed-model) true)
                    unedited (show :unedited (mixed-model) false)
                    rt (:rt s)]
                (try
                  (doseq [x [s unedited]] (warm-up! x phase 6))
                  (publish-fixture-type! s phase 160)
                  (doseq [t [160 165]] (tick! unedited phase t))
                  (reset! compiled [])
                  (reset! (:rejecting s) rejecting?)
                  (let [before (get-in (model s) [:rig :fixtures])
                        poison (show/submit! rt :fixture-patch {:id "poison" :fixture-type-id :rgbw :universe 40 :address 1})
                        cue (show/submit! rt {:op :cue/put :cue {:id :after :effect-id :gain}})
                        launch (show/submit! rt {:op :cue/launch :cue-id :accent})
                        master (show/submit! rt {:op :master/set :value 0.5})]
                    (doseq [x [s unedited]] (tick! x phase 175))
                    (when-not rejecting?
                      (is (= [nil nil :accepted :accepted] (mapv reply-status [poison cue launch master])))
                      (work! s phase)
                      (doseq [x [s unedited]] (tick! x phase 200)))
                    (is (= :rejected (reply-status poison)))
                    (is (= "bad patch" (:message (deref poison 0 nil))))
                    (is (every? #{:accepted} (map reply-status [cue launch master])) "The rest of the cycle lands")
                    (is (< (:revision (deref poison 0 nil) 0) (:revision (deref cue 0 nil))) "in order")
                    (is (identical? before (get-in (model s) [:rig :fixtures])) "The fixtures are unchanged")
                    (is (nil? @(:rig-preparation rt)))
                    (is (nil? (:error (status s))))
                    (is (= [] (compiles compiled :frame s)) "No running program compiled again")
                    (is (identical? before (:fixtures @(:patch rt))) "The patched rig is the one before")
                    (is (= 2 (count (filter #(= :accent (:cue-id %)) (vals (:instances (model s))))))))
                  (finally (stop! s unedited)))))))))))
