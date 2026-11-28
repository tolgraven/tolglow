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
            [tolglow.show.programs :as programs]))

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
  `work!`."
  [label model worker?]
  (let [frames (atom []) tasks (atom [])
        rt (show/create-runtime (cond-> {:model model :workspace-options nil :effect-log! (fn [_]) :workers 2
                                         :sink {:send! (fn [universes _]
                                                         (swap! frames conj (into (sorted-map)
                                                                                  (map (fn [[u bytes]] [u (vec bytes)]))
                                                                                  universes)))}}
                                  worker? (assoc :background-compile (fn [task] (swap! tasks conj task)))))]
    {:label label :rt rt :frames frames :tasks tasks}))

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

(deftest commands-behind-a-held-rig-edit-follow-the-selective-hold
  ;; Master, parameter and parameter-source input go ahead of a rig edit the
  ;; worker has not compiled for; a launch, whose targets resolve against
  ;; the rig, and a publication wait behind it in submission order (#784).
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
                           :master (show/submit! rt {:op :master/set :value 0.5})
                           :param (show/submit! rt {:op :param/set :instance-id probe :param :level :value 0.3})
                           :bind (show/submit! rt :modulation-bind {:instance-id probe :param :level
                                                                    :binding {:type :lfo :waveform :sine :period {:bars 2}
                                                                              :from 0.2 :to 0.4}})}
                  statuses #(update-vals replies (fn [reply] (:status (deref reply 0 nil))))]
              ;; The publication's check may run: only the rig edit's compiles stall.
              (let [checks (filterv programs/ahead? @(:tasks s))]
                (swap! (:tasks s) #(into [] (remove (set checks)) %))
                (doseq [task checks] (task)))
              (doseq [t [175 200 225]] (tick! s phase t))
              (is (= {:rig nil :publication nil :launch nil :master :accepted :param :accepted :bind :accepted} (statuses))
                  "Only master, parameter and binding input applied while the rig edit waits")
              (is (= 0.5 (:master (model s))))
              (is (= [:rig :definition :command] (mapv :type @(:held-messages rt))))
              (work! s phase)
              (tick! s phase 250)
              (is (every? #{:accepted} (vals (statuses))) (pr-str (statuses)))
              (is (apply < (map #(:revision (deref (get replies %) 0 nil)) [:publication :launch]))
                  "The publication and the launch land after the rig edit, in submission order")
              (is (= (:fixtures edited) (get-in (model s) [:rig :fixtures])))
              (is (= 2 (count (filter #(= :accent (:cue-id %)) (vals (:instances (model s)))))))
              (doseq [t [275 300]] (work! s phase) (tick! s phase t))
              (is (= 2 (some #(when (= :probe-1 (:cue-id %)) (:effect-version %)) (vals (:instances (model s)))))
                  "The publication adopts on the new rig")
              (is (= [] (compiles compiled :frame s)) "Nothing compiles inside a frame, the waiting launch included")
              (is (nil? (get-in @(:state rt) [:status :error]))))
            (finally (stop! s))))))))

(deftest a-stalled-worker-holds-a-rig-edit-up-to-its-limit
  (let [compiled (atom []) phase (atom nil)]
    (counting-compilers compiled phase
      (fn []
        (let [s (show :worker (mixed-model) true)
              limit @#'show/rig-edit-hold-limit]
          (try
            (warm-up! s phase 6)
            (reset! compiled [])
            (let [reply (show/submit! (:rt s) :rig (moved-rig (model s)))
                  blackout (show/submit! (:rt s) {:op :blackout/set :value true})]
              (doseq [i (range limit)] (tick! s phase (+ 175 (* 25 i))))
              (is (= :accepted (:status (deref blackout 0 nil))) "Blackout went ahead in the first cycle")
              (is (nil? (deref reply 0 nil)) "Still waiting after the limit's cycles")
              (is (= [] (compiles compiled :frame s)))
              (tick! s phase (+ 175 (* 25 limit)))
              (is (= :accepted (:status (deref reply 0 nil))) "Applied in the next cycle")
              (is (= 32 (count (compiles compiled :frame s))) "That frame compiles what the stalled worker did not")
              (is (nil? (get-in @(:state (:rt s)) [:status :error]))))
            (finally (stop! s))))))))
