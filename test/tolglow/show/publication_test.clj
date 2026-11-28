(ns tolglow.show.publication-test
  "A published definition is validated and compiled once, and with a worker
  before the frame owner sees it (#784). The owner holds a publication whose
  check is not ready, and every message after it, for a later cycle. The
  worker is stepped by hand: assertions count validations, compiles and
  owner cycles, never clocks."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.audio-lfo :as audio-lfo]
            [tolglow.authoring.variation-control :as variations]
            [tolglow.compiler :as compiler]
            [tolglow.demo :as demo]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.knob-candidates :as knob-candidates]
            [tolglow.knob-presets :as knob-presets]
            [tolglow.knobs :as knobs]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.show.programs :as programs]
            [tolglow.show.scene-draft :as scene-draft]
            [tolglow.surface.link :as surface])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- probe
  ([version] (probe version {}))
  ([version extra]
   (merge {:id :probe :version version :name "Probe"
           :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
           :outputs {:dimmer {:op :param :key :level}}}
          extra)))

(defn- show-model []
  (-> (demo/model)
      (assoc-in [:registry :effects :probe] (probe 1))
      (assoc-in [:registry :cues :probe] {:id :probe :version 1 :effect-id :probe})))

(def ^:private ^:dynamic *program*
  "True while the rig compiler builds a launch's program, which is not part
  of checking a publication."
  false)

(defn- show
  "A runtime with no running instances. With `worker?` its compile worker is
  a queue stepped by `work!`. :calls records each validation and effect
  compile plan outside the rig's program compiles as [where kind], where is
  :owner on the thread that ticks and :worker inside `work!`."
  [worker?]
  (let [tasks (atom []) calls (atom []) phase (atom :owner)
        rt (show/create-runtime (cond-> {:model (show-model) :workspace-options nil :effect-log! (fn [_]) :workers 1}
                                  worker? (assoc :background-compile (fn [task] (swap! tasks conj task)))))]
    (swap! (:compiler-context rt) update :compiler
           (fn [compiler] (fn [program] (binding [*program* true] (compiler program)))))
    {:rt rt :tasks tasks :calls calls :phase phase}))

(defmacro ^:private counting
  "Run `body` recording each `registry/validate-definition` of an effect and
  each `compiler/compile-plan` into `s`'s :calls."
  [s & body]
  `(let [{calls# :calls phase# :phase} ~s
         validate# registry/validate-definition
         plan# compiler/compile-plan]
     (with-redefs [registry/validate-definition (fn [kind# definition#]
                                                  (when (and (= :effects kind#) (not *program*))
                                                    (swap! calls# conj [@phase# :validate]))
                                                  (validate# kind# definition#))
                   compiler/compile-plan (fn [& args#]
                                           (when-not *program* (swap! calls# conj [@phase# :compile]))
                                           (apply plan# args#))]
       ~@body)))

(defn- work!
  "Run the publication checks queued on the worker, as it does between owner
  cycles ahead of its compiles. Program compiles stay queued, so the counts
  here are the publications' own."
  [{:keys [tasks phase]}]
  (let [run (filterv programs/ahead? @tasks)]
    (swap! tasks #(into [] (remove programs/ahead?) %))
    (reset! phase :worker)
    (try (doseq [task run] (task)) (finally (reset! phase :owner)))
    (count run)))

(defn- tick! [{:keys [rt]} t] (show/tick! rt t))

(defn- model [s] (:model @(:state (:rt s))))

(defn- receipt
  "The reply's receipt, or nil while the owner has not answered it."
  [reply]
  (deref reply 0 nil))

(defn- ack [reply] (some-> (receipt reply) (dissoc :revision :command-id)))

(defn- calls-since! [{:keys [calls]}]
  (let [seen @calls] (reset! calls []) (frequencies seen)))

(deftest an-accepted-publication-is-validated-and-compiled-once
  (testing "without a worker, on the owner"
    (let [s (show false)]
      (try
        (counting s
          (let [reply (show/submit! (:rt s) :definition {:kind :effects :definition (probe 2)})]
            (tick! s 25)
            (is (= :accepted (:status (ack reply))))
            (is (= {[:owner :validate] 1 [:owner :compile] 1} (calls-since! s)))
            (is (= 2 (get-in (model s) [:registry :effects :probe :version])))))
        (finally (show/stop! (:rt s))))))
  (testing "with a worker, before the owner sees it; the owner validates and compiles nothing"
    (let [s (show true)]
      (try
        (counting s
          (let [reply (show/submit! (:rt s) :definition {:kind :effects :definition (probe 2)})]
            (is (= 1 (count @(:tasks s))))
            (is (programs/ahead? (first @(:tasks s))) "The check runs ahead of queued compiles")
            (work! s)
            (is (= {[:worker :validate] 1 [:worker :compile] 1} (calls-since! s)))
            (tick! s 25)
            (is (= :accepted (:status (ack reply))))
            (is (= {:kind :effects :id :probe :version 2} (select-keys (ack reply) [:kind :id :version])))
            (is (= {} (calls-since! s)) "No validation or compile on the owner")
            (is (= 2 (get-in (model s) [:registry :effects :probe :version])))))
        (finally (show/stop! (:rt s)))))))

(deftest a-definition-error-check-compiles-once
  (let [calls (atom 0)
        plan compiler/compile-plan
        validate registry/validate-definition]
    (with-redefs [registry/validate-definition (fn [kind definition] (swap! calls inc) (validate kind definition))
                  compiler/compile-plan (fn [& args] (swap! calls + 100) (apply plan args))]
      (is (= [] (compiler/definition-errors :effects (probe 2) {:publication? true})))
      (is (= 101 @calls) "One validation and one compile plan"))))

(defn- behind-publication!
  "Submit a publication of probe v2, then a cue pinning v2 and a launch of
  the probe's cue, which depend on it, and a launch of an unrelated cue, a
  blackout and a master change, which do not."
  [s]
  (let [submit (partial show/submit! (:rt s))]
    {:published (submit :definition {:kind :effects :definition (probe 2)})
     :pinned (submit {:op :cue/put :cue {:id :pinned :effect-id :probe :effect-version 2}})
     :probe-launch (submit {:op :cue/launch :cue-id :probe})
     :other-launch (submit {:op :cue/launch :cue-id :moon})
     :blackout (submit {:op :blackout/set :value true})
     :master (submit {:op :master/set :value 0.25})}))

(defn- statuses [replies] (update-vals replies #(:status (ack %))))

(deftest only-what-depends-on-a-waiting-publication-waits-with-it
  (doseq [worker-runs? [false true]]
    (testing (if worker-runs? "the worker checks it before the next cycle" "the worker is stalled")
      (let [s (show true)]
        (try
          (counting s
            (let [replies (behind-publication! s)]
              (tick! s 25)
              (is (= {:published nil :pinned nil :probe-launch nil
                      :other-launch :accepted :blackout :accepted :master :accepted}
                     (statuses replies))
                  "Blackout, master and other launches apply in the publication's cycle")
              (is (true? (:blackout? (model s))))
              (is (= 0.25 (:master (model s))))
              (is (= 3 (count @(:held-messages (:rt s)))))
              (is (= {} (calls-since! s)))
              (when worker-runs? (work! s))
              (tick! s 50)
              (is (= {:published :accepted :pinned :accepted :probe-launch :accepted
                      :other-launch :accepted :blackout :accepted :master :accepted}
                     (statuses replies))
                  "The publication and what depends on it land on the next cycle")
              (is (apply < (map #(:revision (receipt (get replies %))) [:published :pinned :probe-launch]))
                  "in submission order")
              (is (= 2 (get-in (model s) [:registry :cues :pinned :effect-version])))
              (is (= 2 (some #(when (= :probe (:cue-id %)) (:effect-version %)) (vals (:instances (model s)))))
                  "The waiting launch starts on the new revision")
              (is (= (if worker-runs?
                       {[:worker :validate] 1 [:worker :compile] 1}
                       {[:owner :validate] 1 [:owner :compile] 1})
                     (calls-since! s))
                  "A check the worker has not run by the next cycle runs once on the owner")
              (is (empty? @(:held-messages (:rt s))))))
          (finally (show/stop! (:rt s))))))))

(deftest registry-readers-without-a-listed-type-wait-behind-a-publication
  ;; A lighting-scene vary copies a registry effect into a scene effect, and a
  ;; surface cue publishes an effect and cue: in submission order both see the
  ;; held revision, so both wait for it (review of #809).
  (let [s (show true)
        submit (partial show/submit! (:rt s))]
    (try
      (let [published (submit :definition {:kind :effects :definition (probe 2)})
            scene (submit :lighting-scene {:op :vary :group-id :front :target 0.72 :effect-ids [:probe]})
            surface-cue (submit :video-mapping {:op :surface-cue :surface :wall :priority 10})
            blackout (submit {:op :blackout/set :value true})]
        (tick! s 25)
        (is (= [nil nil nil :accepted] (mapv #(:status (ack %)) [published scene surface-cue blackout])))
        (is (= [:definition :lighting-scene :video-mapping] (mapv :type @(:held-messages (:rt s)))))
        (tick! s 50)
        (is (= :accepted (:status (ack published))))
        (is (every? #(some? (ack %)) [scene surface-cue]) "Both are answered on the next cycle, after the publication")
        (is (empty? @(:held-messages (:rt s)))))
      (finally (show/stop! (:rt s))))))

(defn- source-forms
  "The top-level forms of a source file on the classpath, read as data in
  `ns-name` so its auto-resolved keywords resolve."
  [path ns-name]
  (binding [*ns* (the-ns ns-name) *read-eval* false]
    (with-open [reader (java.io.PushbackReader. (io/reader (io/resource path)))]
      (doall (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof :read-cond :allow} reader)))))))

(defn- case-keys
  "The keyword test constants of the first `(case dispatch …)` in `defn-name`."
  [path ns-name defn-name dispatch]
  (let [form (some #(when (and (seq? %) (= defn-name (second %))) %) (source-forms path ns-name))
        case-form (some #(when (and (seq? %) (= 'case (first %)) (= dispatch (second %))) %) (tree-seq coll? seq form))]
    (into #{} (comp (map first) (mapcat #(if (seq? %) % [%])) (filter keyword?))
          (partition 2 (drop 2 case-form)))))

(defn- set-in-defn
  "The keywords of the set literal in `defn-name`'s body."
  [path ns-name defn-name]
  (let [form (some #(when (and (seq? %) (= defn-name (second %))) %) (source-forms path ns-name))]
    (into #{} (comp (filter set?) (mapcat seq) (filter keyword?)) (tree-seq coll? seq form))))

(def ^:private classified-types
  "Every message type and how it relates to a held publication. A new type
  must be added here, and so classified (review of #809)."
  {:command :per-op :cue-schedule :launch :cue-cancel :cancel
   :rdm-update :independent :sensor-audience-zone :independent :sensor-configure :independent
   :sensor-arm :independent :sensor-calibrate :independent :surface-aim :independent
   :modulation-bind :independent :modulation-unbind :independent :modulation-hold :independent
   :modulation-resume :independent
   :spatial-cue :dependent :autopilot-authored :dependent :lighting-scene :dependent :video-mapping :dependent
   :personality :dependent :event-subscriber :dependent :venue-surface :dependent :fog-calibration :dependent
   :photometry :dependent :calibration :dependent :patch-save :dependent :patch-enabled :dependent
   :definition :dependent :material-edit :dependent :agent-cue :dependent :rig :dependent :fixture-patch :dependent
   :composition :dependent :composition-append :dependent :composition-cards :dependent :composition-run :dependent
   :composition-stop :dependent :template-group :dependent :template-group-remove :dependent
   :template-group-launch :dependent :template-group-stop :dependent :template-group-relaunch :dependent :template-group-cards :dependent
   :response :dependent :response-release :dependent :response-delete :dependent
   :page-generate :dependent :modulation-start :dependent :waveform :dependent :modulation-stop :dependent
   :aim-path :dependent :modulation-cue :dependent :variable-block :dependent :audio-return-configure :dependent
   :settings :dependent :auto :dependent :import :dependent})

(def ^:private independent-ops
  #{:blackout/set :master/set :transport/set :param/set :param/ramp :variable/set
    :gesture/begin :gesture/sample :gesture/end :gesture/cancel :history/undo :history/redo :history/select
    :instance/end :instance/remove :instance/set-priority :instance/move-after :instances/shuffle
    :instance/pause :instance/resume :instance/opacity-set :instance/gain-set :instance/opacity-toggle
    :instance/envelope-set :group/apply :effect-selection/configure :effect-selection/toggle
    :effect-groups/configure :effect-groups/overlay-set :effect-groups/level-set :effect-groups/assign-instance
    :knob/create :knob/link :knob/nudge :knob/remove :knob/rename :knob/set :knob/unlink
    :knob-preset/capture :knob-preset/clear :knob-preset/recall :audio-lfo/configure :audio-lfo/remove
    :control-surface/attach :control-surface/bank :control-surface/detach :control-surface/layout
    :control-surface/link :control-surface/remove :control-surface/unlink})

(def ^:private dependent-ops
  #{:registry/publish :cue/put :cue/save-params :cue/clear-saved-params :cue/delete :instance/retry
    :page/put :page/delete :show/restart :schedule/post :schedule/cancel :schedule/move :macro/run :macro/stop
    :cue-variation/append :cue-variation/apply :cue-variation/overwrite :cue-variation/ratio-mode
    :cue-variation/ratio-set :scene-draft/apply :scene-draft/discard :scene-draft/stage
    :knob-candidate/discard :knob-candidate/generate :knob-candidate/keep :knob-candidate/link
    :knob-candidate/reroll :knob-candidate/unlink :knob-candidate/audition})

(def ^:private classified-ops
  "Every command op and how it relates to a held publication. A new op must
  be added here, and so classified."
  (merge (zipmap independent-ops (repeat :independent)) (zipmap dependent-ops (repeat :dependent))
         {:cue/launch :launch :effect/launch :launch :cue/cancel-launch :cancel}))

(deftest every-message-is-classified-against-a-held-publication
  (let [message-class @#'show/message-class
        types (case-keys "tolglow/show.clj" 'tolglow.show 'apply-message 'type)
        ops (into #{:cue/cancel-launch :param/ramp :history/undo :history/redo :history/select}
                  cat [(case-keys "afterglow/control/model.clj" 'afterglow.control.model 'apply-operation 'op)
                       @#'control/gesture-operations
                       (set-in-defn "tolglow/effects/group_channels.clj" 'tolglow.effects.group-channels 'operation?)
                       variations/operations surface/operations subscribers/operations knobs/operations
                       audio-lfo/operations knob-candidates/operations knob-candidates/audition-operations
                       knob-presets/operations scene-draft/operations])]
    (is (< 40 (count types)) "The message types were read from apply-message")
    (is (< 80 (count ops)) "The command ops were collected")
    (is (= types (set (keys classified-types))) "Classify each new message type here, and drop removed ones")
    (is (= #{} (reduce disj ops (keys classified-ops))) "Classify each new command op here")
    (doseq [[type expected] classified-types :when (not= :per-op expected)]
      (is (= expected (message-class type nil)) (str type)))
    (doseq [[op expected] classified-ops]
      (is (= expected (message-class :command op)) (str op)))
    (is (= :dependent (message-class :some-future-type nil)) "Unknown messages wait")
    (is (= :dependent (message-class :command :some/future-op)))))

(deftest held-messages-count-against-the-queue
  (let [s (show true)
        submit (partial show/submit! (:rt s))
        cue-put (fn [i] (submit {:op :cue/put :cue {:id (keyword (str "c" i)) :effect-id :probe}}))]
    (try
      (submit :definition {:kind :effects :definition (probe 2)})
      (let [first-wave (mapv cue-put (range 200))]
        (tick! s 25)
        (is (= 201 (count @(:held-messages (:rt s)))) "The cue edits wait behind the publication")
        (let [second-wave (mapv cue-put (range 200 300))
              refused (filterv #(= :rejected (:status (ack %))) second-wave)]
          (is (= 45 (count refused)) "Only the 55 slots the held messages leave are taken")
          (is (every? #(re-find #"queue is full" (:message (ack %) "")) refused))
          (tick! s 50)
          (is (every? #(= :accepted (:status (ack %))) first-wave))))
      (finally (show/stop! (:rt s))))))

(deftest a-stale-check-is-prepared-again-against-the-owner-model
  ;; Both publications name no version, so each takes the next one. The worker
  ;; checks both against the committed model, where both would be version 2.
  (let [s (show true)
        unversioned (dissoc (probe 1 {:name "Again"}) :version)]
    (try
      (counting s
        (let [first-reply (show/submit! (:rt s) :definition {:kind :effects :definition (dissoc (probe 1) :version)})
              second-reply (show/submit! (:rt s) :definition {:kind :effects :definition unversioned})]
          (work! s)
          (tick! s 25)
          (is (= [:accepted nil] [(:status (ack first-reply)) (:status (ack second-reply))]))
          (is (= 1 (count (filter programs/ahead? @(:tasks s)))) "The second publication is checked again as version 3")
          (work! s)
          (tick! s 50)
          (is (= :accepted (:status (ack second-reply))))
          (is (= [2 3] [(:version (receipt first-reply)) (:version (receipt second-reply))]))
          (is (= "Again" (get-in (model s) [:registry :effects :probe :name])))
          (is (= {[:worker :validate] 3 [:worker :compile] 3} (calls-since! s))
              "Nothing is validated or compiled on the owner")))
      (finally (show/stop! (:rt s))))))

(deftest a-publication-stale-twice-is-checked-on-the-owner
  (let [s (show true)
        rt (:rt s)]
    (try
      (counting s
        (let [first-reply (show/submit! rt :definition {:kind :effects :definition (dissoc (probe 1) :version)})
              second-reply (show/submit! rt :definition {:kind :effects :definition (dissoc (probe 1 {:name "Again"}) :version)})]
          (work! s)
          (tick! s 25)
          (is (= [:accepted nil] [(:status (ack first-reply)) (:status (ack second-reply))]))
          (work! s)
          (is (= {[:worker :validate] 3 [:worker :compile] 3} (calls-since! s))
              "Both checks, then the second again as version 3")
          ;; Another writer commits version 3 before the next cycle, so the
          ;; second check is stale too.
          (swap! (:state rt) update :model
                 #(-> % (assoc-in [:registry :effects :probe] (probe 3))
                      (assoc-in [:registry :versions [:effects :probe 3]] (probe 3))))
          (tick! s 50)
          (is (= :accepted (:status (ack second-reply))) "It lands in this cycle instead of waiting again")
          (is (= 4 (:version (receipt second-reply))))
          (is (= {[:owner :validate] 1 [:owner :compile] 1} (calls-since! s)) "checked once on the owner")))
      (finally (show/stop! rt)))))

(def ^:private refusals
  "Publications refused at each check, in the owner's order."
  {:validation {:kind :effects :definition (probe 2 {:params {:level {:type :number :default 2.0 :bounds [0.0 1.0]}}})}
   :compile {:kind :effects :definition (probe 2 {:params {:other {:type :number :default 0.5 :bounds [0.0 1.0]}}})}
   :period {:kind :effects :definition (probe 2 {:modulation {:every {:beats 1/2048}}})}
   :stale-version {:kind :effects :definition (probe 7) :expected-version 4}
   :version-order {:kind :effects :definition (probe 1 {:name "Same version"})}
   :cue {:kind :cues :definition {:id :bad :effect-id :probe :priority "high"}}})

(defn- refusal [worker? payload]
  (let [s (show worker?)]
    (try
      (let [reply (show/submit! (:rt s) :definition payload)]
        (work! s)
        (tick! s 25)
        [(ack reply) (get-in (model s) [:registry :effects :probe])])
      (finally (show/stop! (:rt s))))))

(deftest refusals-are-the-same-with-and-without-a-worker
  (doseq [[label payload] refusals]
    (testing label
      (let [[inline inline-probe] (refusal false payload)
            [worker worker-probe] (refusal true payload)]
        (is (= :rejected (:status inline)) (pr-str inline))
        (is (= inline worker) "Same status, code, message and errors")
        (is (= (probe 1) (dissoc inline-probe :imported-prefabs) (dissoc worker-probe :imported-prefabs))
            "The last valid definition keeps running")))))

(deftest stopping-the-show-answers-a-held-publication
  (let [s (show true)
        reply (show/submit! (:rt s) :definition {:kind :effects :definition (probe 2)})]
    (tick! s 25)
    (is (not (realized? reply)))
    (show/stop! (:rt s))
    (is (= {:status :rejected :message "Show stopped"} (deref reply 1000 nil)))))

(deftest the-owned-worker-runs-checks-ahead-of-compiles
  (let [{:keys [submit close!]} (programs/worker)
        started (CountDownLatch. 1) release (CountDownLatch. 1) done (CountDownLatch. 4)
        order (atom [])
        task (fn [label] (fn [] (swap! order conj label) (.countDown done)))]
    (try
      (submit (fn [] (.countDown started) (.await release 10 TimeUnit/SECONDS)))
      (is (.await started 10 TimeUnit/SECONDS))
      (submit (task :compile-1))
      (submit (task :compile-2))
      (submit (programs/ahead (task :check-1)))
      (submit (programs/ahead (task :check-2)))
      (.countDown release)
      (is (.await done 10 TimeUnit/SECONDS))
      (is (= [:check-1 :check-2 :compile-1 :compile-2] @order))
      (finally (close!)))))
