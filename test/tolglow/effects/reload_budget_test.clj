(ns tolglow.effects.reload-budget-test
  "One frame attempts a bounded number of newly published effect revisions
  (#774). The rest take over on later frames, in launch order, and read their
  captured definitions everywhere until then. Assertions count frames,
  versions and compilations; none reads a clock."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(defn- param [key] {:op :param :key key})

(defn- probe-id [i] (keyword (str "probe-" i)))

(defn- head-dimmer
  "Index of head-(i+1)'s dimmer in the captured universe."
  [i]
  (* 12 i))

(defn- probe
  "Version `version` of probe `i`, a dimmer on head-(i+1)."
  ([i version] (probe i version {}))
  ([i version {:keys [bounds outputs] :or {bounds [0.0 1.0]}}]
   {:id (probe-id i) :version version :name (str "Probe " i) :targets [(keyword (str "head-" (inc i)))]
    :params {:level {:type :number :default 0.5 :bounds bounds}}
    :outputs (or outputs {:dimmer (param :level)})}))

(defn- rejected [i version]
  (-> (probe i version)
      (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
      (assoc-in [:outputs :dimmer] {:op :divide :args [(param :level) (param :divisor)]})))

(defn- show-model [n]
  (reduce (fn [model i]
            (let [id (probe-id i)]
              (-> model
                  (assoc-in [:registry :effects id] (probe i 1))
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority 20
                                                 :reload-policy :preserve-state}))))
          (demo/model) (range n)))

(defn- runtime
  "A show of `n` probes with a capture sink. Compilations are recorded at the
  runtime's own compiler boundary as [effect-id version], one list per tick."
  [n output compiled options]
  (-> (show/create-runtime (merge {:model (show-model n) :workspace-options nil :effect-log! (fn [_]) :workers 2
                                   :sink {:send! (fn [universes _]
                                                   (swap! output conj (mapv #(bit-and 255 %) (get universes 1))))}}
                                  options))
      (update :compile (fn [compile-effect]
                         (fn [{:keys [id version] :as definition}]
                           (swap! compiled update (dec (count @compiled)) conj [id version])
                           (compile-effect definition))))))

(defn- tick! [rt compiled t]
  (swap! compiled conj #{})
  (show/tick! rt t))

(defn- request! [rt compiled t type payload]
  (let [reply (show/submit! rt type payload)]
    (tick! rt compiled t)
    (deref reply 2000 {:status :timeout})))

(defn- instances [rt]
  (into {} (map (juxt :cue-id identity)) (vals (get-in @(:state rt) [:model :instances]))))

(defn- versions [rt n]
  (let [by-cue (instances rt)]
    (mapv #(get-in by-cue [(probe-id %) :effect-version]) (range n))))

(defn- launch-all!
  "Launch probes 0..n-1 in that order, bind `binding` to each level when given,
  and return the next free time."
  [rt compiled n binding]
  (doseq [i (range n)]
    (let [ack (request! rt compiled (* 25 (inc i)) :command {:op :cue/launch :cue-id (probe-id i)})]
      (is (= :accepted (:status ack)))
      (when binding
        (is (= :accepted (:status (request! rt compiled (* 25 (inc i)) :modulation-bind
                                            {:instance-id (:instance-id ack) :param :level :binding binding})))))))
  (* 25 (inc n)))

(defn- publish!
  "Publish every revision in one owner cycle and render that frame."
  [rt compiled definitions t]
  (let [replies (mapv #(show/submit! rt :definition {:kind :effects :definition %}) definitions)]
    (tick! rt compiled t)
    (mapv #(:status (deref % 2000 {:status :timeout})) replies)))

(defn- first-compiled
  "Per tick, the [effect-id version] pairs above version 1 that the runtime
  compiled for the first time in that tick."
  [compiled]
  (first (reduce (fn [[frames seen] frame]
                   (let [fresh (into (sorted-set) (remove seen) (filter #(< 1 (second %)) frame))]
                     [(conj frames fresh) (into seen fresh)]))
                 [[] #{}] compiled)))

(deftest revisions-beyond-the-frame-limit-take-over-on-later-frames-in-launch-order
  (let [output (atom []) compiled (atom [])
        rt (runtime 5 output compiled {:reloads-per-frame 2})]
    (try
      (let [t (launch-all! rt compiled 5 nil)
            _ (reset! compiled [])
            ;; Published in reverse order: launch order decides which go first.
            acks (publish! rt compiled (mapv #(probe % 2 {:outputs {:dimmer {:op :const :value 1.0}}}) (reverse (range 5))) t)
            after-publication (versions rt 5)
            _ (tick! rt compiled (+ t 25))
            second-frame (versions rt 5)
            _ (tick! rt compiled (+ t 50))
            third-frame (versions rt 5)
            _ (tick! rt compiled (+ t 75))]
        (is (= (repeat 5 :accepted) acks))
        (is (= [[2 2 1 1 1] [2 2 2 2 1] [2 2 2 2 2]] [after-publication second-frame third-frame]))
        (is (= [#{[:probe-0 2] [:probe-1 2]} #{[:probe-2 2] [:probe-3 2]} #{[:probe-4 2]} #{}]
               (first-compiled @compiled))
            "Each frame compiles only the revisions it attempts")
        (testing "a deferred probe renders its captured revision until it takes over"
          (let [dimmers (fn [frame] (mapv #(nth frame (head-dimmer %)) (range 5)))
                frames (mapv dimmers (take-last 4 @output))]
            (is (= [[255 255 128 128 128] [255 255 255 255 128] [255 255 255 255 255] [255 255 255 255 255]]
                   frames))))
        (is (every? (comp nil? :reload-fault) (vals (instances rt))))
        (is (nil? (get-in @(:state rt) [:status :error]))))
      (finally (show/stop! rt)))))

(def ^:private level-binding
  {:type :lfo :waveform :saw :period {:beats 1} :phase-reference :transport :from 0.6 :to 0.9})

(deftest a-deferred-revision-keeps-its-waveform-on-the-captured-definition
  ;; Probe 2's revision narrows the bound level below the waveform's range.
  ;; Deferred, it must keep sampling the captured bounds: its dimmer equals a
  ;; twin that never published, until the frame that takes the revision over.
  (let [published-output (atom []) twin-output (atom []) compiled (atom []) twin-compiled (atom [])
        published (runtime 3 published-output compiled {:reloads-per-frame 2})
        twin (runtime 3 twin-output twin-compiled {:reloads-per-frame 2})
        narrowed #(probe % 2 {:bounds [0.0 0.5]})]
    (try
      (let [t (launch-all! published compiled 3 level-binding)
            _ (launch-all! twin twin-compiled 3 level-binding)
            _ (publish! published compiled (mapv narrowed (range 3)) t)
            _ (publish! twin twin-compiled (mapv narrowed (range 2)) t)
            publication [(versions published 3) (versions twin 3)]
            _ (tick! published compiled (+ t 25))
            _ (tick! twin twin-compiled (+ t 25))
            takeover (versions published 3)
            probe-2 (fn [output] (mapv #(nth % (head-dimmer 2)) (take-last 2 @output)))]
        (is (= [[2 2 1] [2 2 1]] publication))
        (is (= [2 2 2] takeover))
        (let [[deferred-frame takeover-frame] (probe-2 published-output)
              [twin-deferred twin-next] (probe-2 twin-output)]
          (is (= twin-deferred deferred-frame) "The deferred frame samples the captured bounds, as the twin does")
          (is (< 128 twin-deferred) "The waveform lifts the captured level above its default")
          (is (not= twin-next takeover-frame) "The takeover frame samples the narrowed bounds"))
        (is (nil? (get-in @(:state published) [:status :error]))))
      (finally (show/stop! published) (show/stop! twin)))))

(deftest a-rejected-revision-within-the-limit-defers-the-frame-and-counts-once
  ;; With a reload fallback, one rejection keeps every candidate of that frame
  ;; pending without a fault (#663). The refused revision is never attempted
  ;; again, so the next frames take the remaining revisions in launch order.
  (let [output (atom []) compiled (atom [])
        rt (runtime 4 output compiled {:reloads-per-frame 2})]
    (try
      (let [t (launch-all! rt compiled 4 level-binding)
            _ (reset! compiled [])
            revisions [(probe 0 2 {:bounds [0.0 0.5]}) (rejected 1 2) (probe 2 2 {:bounds [0.0 0.5]}) (probe 3 2 {:bounds [0.0 0.5]})]
            acks (publish! rt compiled revisions t)
            frames (conj (mapv (fn [dt] (let [v (versions rt 4)] (tick! rt compiled (+ t dt)) v)) [25 50 75])
                         (versions rt 4))
            refused (get (instances rt) :probe-1)
            events (get-in @(:state rt) [:model :variables :runtime :events])]
        (is (= (repeat 4 :accepted) acks))
        (is (= [[1 1 1 1] [2 1 2 1] [2 1 2 2] [2 1 2 2]] frames))
        (is (= [1 :evaluate 2] [(:effect-version refused) (get-in refused [:reload-fault :stage])
                                (get-in refused [:reload-fault :effect-version])]))
        (is (= [:effect/reload-failed] (mapv :type events)) "One bounded event")
        (is (= [#{[:probe-0 2] [:probe-1 2]} #{[:probe-2 2]} #{[:probe-3 2]} #{}]
               (first-compiled @compiled))
            "Each frame compiles only the revisions it attempts")
        (is (nil? (get-in @(:state rt) [:status :error]))))
      (finally (show/stop! rt)))))

(deftest by-default-eight-revisions-take-over-in-one-frame
  (let [output (atom []) compiled (atom [])
        rt (runtime 9 output compiled {})]
    (try
      (let [t (launch-all! rt compiled 9 nil)]
        (publish! rt compiled (mapv #(probe % 2) (range 9)) t)
        (is (= [2 2 2 2 2 2 2 2 1] (versions rt 9)))
        (tick! rt compiled (+ t 25))
        (is (= (repeat 9 2) (versions rt 9))))
      (finally (show/stop! rt)))))

(deftest the-frame-limit-is-a-positive-integer
  (doseq [limit [0 -1 1.5 nil]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Reloads per frame"
                          (show/create-runtime {:workspace-options nil :reloads-per-frame limit})))))

(deftest a-full-compile-cache-keeps-running-definitions-compiled
  (let [calls (atom {})
        compile (@#'show/cached-compile
                 (atom {:compiler (fn [definition] (swap! calls update definition (fnil inc 0)) [:step definition])
                        :cache (atom {}) :retired (atom {})}))
        running (probe 0 1)
        others (mapv #(probe 1 (+ 2 %)) (range 900))]
    (compile running)
    (doseq [definition (take 300 others)] (compile definition) (compile running))
    (is (= 1 (get @calls running)) "A definition used every frame survives the cache filling up")
    (is (= [:step running] (compile running)))
    (doseq [definition (drop 300 others)] (compile definition))
    (compile running)
    (is (= 2 (get @calls running)) "An unused definition still leaves the cache")
    (is (every? #(= 1 %) (map @calls others)) "Every published revision is compiled once")))
