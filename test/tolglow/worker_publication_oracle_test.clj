(ns tolglow.worker-publication-oracle-test
  "Bounded worker evaluation and owned publication against the serial
  whole-frame replay oracle. Every published DMX byte must match."
  (:require [afterglow.control.model :as control]
            [afterglow.output.scheduler :as scheduler]
            [afterglow.render.frame :as render]
            [afterglow.runtime.executor :as executor]
            [afterglow.runtime.replay :as replay]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.legacy-frame-oracle.compare :as compare]
            [tolglow.legacy-frame-oracle.scenario :as scenario])
  (:import [java.util.concurrent ConcurrentLinkedQueue CountDownLatch TimeUnit]))

(def worker-counts
  "One worker, two workers, and more workers than any frame has instances."
  [1 2 12])

(defn- unsigned-universes [universes]
  (into (sorted-map) (map (fn [[id bytes]] [id (scenario/unsigned-bytes bytes)])) universes))

(defn- reversed-completion
  "Hold the first job until every other job in the same frame has completed, so
  worker completion order is the reverse of semantic order on every frame."
  [map-work]
  (fn [f jobs]
    (let [jobs (vec jobs)
          others (CountDownLatch. (max 0 (dec (count jobs))))]
      (map-work (fn [[index job]]
                  (if (zero? index)
                    (do (when-not (.await others 5 TimeUnit/SECONDS)
                          (throw (ex-info "Later jobs did not complete" {})))
                        (f job))
                    (let [result (f job)] (.countDown others) result)))
                (map-indexed vector jobs)))))

(defn- snapshot-probe
  "Wrap compiled steps transparently and record the frame map and thread seen by
  each evaluation. Step metadata is preserved."
  [compile-effect ^ConcurrentLinkedQueue seen]
  (fn [definition]
    (let [step (compile-effect definition)]
      (with-meta (fn [context]
                   (.add seen {:frame (:frame context) :thread (.getName (Thread/currentThread))})
                   (step context))
        (meta step)))))

(def ^:private private-clock-keys
  "engine/advance overlays each instance's own musical clock on the shared frame."
  [:time-ms :beat :bpm :meter :playing? :delta-ms :transport])

(defn- frozen-snapshots
  "Group evaluations by show epoch and frame number. Within each frame, every
  worker must see the same shared snapshot data and the same fixture vector
  object; only the private instance clock fields may differ."
  [^ConcurrentLinkedQueue seen]
  (let [frames (group-by (comp (juxt :epoch :number) :frame) (vec seen))]
    {:frames (count frames)
     :frozen? (every? (fn [[_ evaluations]]
                        (let [[frame & others] (map :frame evaluations)
                              shared #(apply dissoc % private-clock-keys)]
                          (every? #(and (= (shared frame) (shared %))
                                        (identical? (:fixtures frame) (:fixtures %)))
                                  others)))
                      frames)}))

(defn- publisher
  "An owned output scheduler in direct mode with an injected clock. send!
  receives fresh transport arrays, copied here for comparison."
  [clock sent]
  (scheduler/create {:now-ms #(deref clock)
                     :send! (fn [universes id]
                              (swap! sent conj {:id id :universes (unsigned-universes universes)}))}))

(defn- publishing-render
  "Render with the oracle's renderer, then hand the completed frame to the owned
  publisher and present it at the frame's physical deadline. The physical clock
  advances before the completion identity is captured: the presentation policy
  admits only strictly increasing source times."
  [owner patch clock]
  (fn [model frame contributions previous]
    (reset! clock (:time-ms frame))
    (let [token (scheduler/begin-frame! owner (:epoch model) patch {} false)
          output (replay/render-output model frame contributions previous)]
      (when-not (true? (scheduler/complete! owner token output))
        (throw (ex-info "Completed frame was not admitted" {:time-ms (:time-ms frame)})))
      (scheduler/publish! owner)
      output)))

(defn- capturing-render [frames]
  (fn [model frame contributions previous]
    (let [output (replay/render-output model frame contributions previous)]
      (swap! frames conj (unsigned-universes (:universes output)))
      output)))

(defn- worker-lanes
  "Call run with [label map-work] for each worker count, in normal and reversed
  completion order where reversal is possible. Each pool is closed afterwards."
  [run]
  (vec (for [workers worker-counts
             reversed? (if (= 1 workers) [false] [false true])]
         (with-open [^java.io.Closeable pool (executor/create {:workers workers :queue-size 4
                                                               :timeout-ms 10000})]
           (let [map-work (cond-> (executor/mapper pool) reversed? reversed-completion)]
             (run {:workers workers :reversed? reversed?} map-work))))))

;; Representative multi-universe rig. :span starts at universe 1 address 507:
;; its dimmer word, RGB and pan coarse are in universe 1, while the pan fine byte
;; and the tilt word continue in universe 2.

(def rig
  [{:id :spot :universe 1 :address 1 :position [0.6 1.5 0.0]
    :channels {:pan [1 2] :tilt [3 4] :dimmer [5 6] :red 7 :green 8 :blue 9 :white 10}
    :calibration {:pan [127 85] :tilt [0 256]}}
   {:id :span :universe 1 :address 507 :position [0.5 3.0 0.0]
    :channels {:dimmer [1 2] :red 3 :green 4 :blue 5 :pan [6 7] :tilt [8 9]}}
   {:id :wash-a :universe 2 :address 20 :position [1.0 2.0 0.0]
    :channels {:red 1 :green 2 :blue 3 :dimmer 4}}
   {:id :wash-b :universe 2 :address 30 :position [2.0 2.0 0.0]
    :channels {:red 1 :green 2 :blue 3 :dimmer 4}}
   {:id :bar :universe 3 :address 1 :position [3.0 1.0 0.0]
    :channels {:dimmer {:offset 1 :fine-offset 2} :red 3 :green 4 :blue 5}}])

(def ^:private all-ids (mapv :id rig))

(def ^:private beat-phase {:op :fract :args [{:op :beat}]})

(def ^:private x-ramp {:op :clamp :args [{:op :multiply :args [{:op :position :axis :x} 0.25]} 0.0 1.0]})

(def effects
  ;; priority, targets, outputs
  {:base {:priority 0 :targets all-ids
          :outputs [{:attribute :color :blend :replace
                     :value {:op :vec3 :args [beat-phase 0.3 x-ramp]}}]}
   :level {:priority 0 :targets all-ids
           :outputs [{:attribute :dimmer :value {:op :add :args [0.4 {:op :multiply :args [0.5 beat-phase]}]}}]}
   :aim {:priority 0 :targets [:spot]
         :outputs [{:attribute :aim :value {:op :vec3 :args [{:op :subtract :args [beat-phase 0.4]} 0.75 -4.0]}}]}
   :sweep {:priority 0 :targets [:span]
           :outputs [{:attribute :pan :value beat-phase}
                     {:attribute :tilt :value {:op :subtract :args [1.0 {:op :multiply :args [0.7 beat-phase]}]}}]}
   :raw-red {:priority -20 :targets [:wash-b :bar]
             :outputs [{:attribute :red :value (/ 49.0 255.0)}]}
   :accent {:priority 5 :targets [:span :wash-a :bar]
            :outputs [{:attribute :color :blend :max
                       :value {:op :vec3 :args [0.1 {:op :abs :args [{:op :sin :args [{:op :beat}]}]} 0.8]}}]}
   :cover-a {:priority 5 :targets [:wash-a :wash-b]
             :outputs [{:attribute :color :blend :replace :value [1.0 0.0 0.25]}]}
   :cover-b {:priority 5 :targets [:wash-b :bar]
             :outputs [{:attribute :color :blend :replace :value [0.0 1.0 0.5]}]}
   :peak {:priority 10 :targets [:span :bar :spot]
          :outputs [{:attribute :dimmer :value {:op :multiply :args [0.9 beat-phase]}}]}})

(defn- command [id epoch op payload]
  {:kind :command
   :command (merge {:command-id (pr-str id) :show-id :worker-oracle :epoch epoch :source :worker-oracle-test :op op}
                   payload)})

(defn- clock [time-ms] {:time-ms (double time-ms) :beat (/ time-ms 500.0)})

(defn- frame [time-ms] {:kind :frame :frame {:time-ms (double time-ms)} :clock (clock time-ms)})

(defn- launch [epoch cue time-ms]
  (assoc (command [:launch epoch cue time-ms] epoch :cue/launch {:cue-id cue :time-ms (double time-ms)})
         :clock (clock time-ms)))

(defn- end [epoch id instance time-ms]
  (assoc (command [:end id time-ms] epoch :instance/end {:instance-id instance :epoch epoch
                                                         :force? true :time-ms (double time-ms)})
         :clock (clock time-ms)))

(def inputs
  "Authoring commands, then launches and forced ends between frames, an
  equal-priority LTP takeover, a restart and a stale end from the old epoch.
  Instance IDs are epoch:launch-number, assigned by the real control reducer."
  (vec
   (concat
    (mapcat (fn [[id {:keys [priority targets outputs]}]]
              [(command [:definition id] 0 :registry/publish
                        {:kind :effects :definition {:id id :version 1 :targets targets :outputs outputs}})
               (command [:cue id] 0 :cue/put {:cue {:id id :effect-id id :priority priority}})])
            (sort-by key effects))
    [(frame 0)
     (launch 0 :base 0) (launch 0 :level 0) (launch 0 :aim 0) (launch 0 :raw-red 0) (launch 0 :sweep 0)
     (frame 40) (frame 80)
     (launch 0 :accent 100) (launch 0 :cover-a 100)
     (frame 120) (frame 160)
     (launch 0 :cover-b 170) (launch 0 :peak 170)
     (frame 200) (frame 240)
     (end 0 :cover-b "0:8" 250)
     (frame 280)
     (end 0 :peak "0:9" 300) (end 0 :raw-red "0:4" 300)
     (frame 320)
     (launch 0 :cover-b 330)
     (frame 360) (frame 400)
     (assoc (command [:restart 1] 0 :show/restart {:time-ms 420.0}) :clock (clock 420))
     (frame 440)
     (launch 1 :base 450) (launch 1 :accent 450) (launch 1 :peak 450)
     (end 0 :stale "0:1" 460)
     (frame 480) (frame 520) (frame 560)])))

(defn- initial-show []
  (control/new-show {:show-id :worker-oracle :rig {:fixtures rig}}))

(defn- oracle []
  (let [frames (atom [])
        recorded (replay/record (initial-show) inputs (batch/compiler-for-rig rig)
                                {:render-output (capturing-render frames)
                                 :metadata {:oracle :serial-whole-frame}})]
    (assoc recorded :frames @frames)))

(defn- byte-at [universes universe address]
  (get-in universes [universe (dec address)]))

(deftest representative-scenario-exercises-every-requested-dimension
  (let [{:keys [recording frames]} (oracle)
        frame-steps (filterv #(= :frame (:kind %)) (:steps recording))
        acks (keep #(get-in % [:expected :ack]) (:steps recording))
        at (fn [index universe addresses] (mapv #(byte-at (nth frames index) universe %) addresses))
        ;; Frame indices: 0 = 0 ms, then one frame per 40 ms up to 14 = 560 ms.
        wash-b-rgb #(at % 2 [30 31 32])
        span-dimmer #(at % 1 [507 508])]
    (is (= 15 (count frames) (count frame-steps)))
    (is (every? #(= #{1 2 3} (set (keys %))) frames) "Three universes are published every frame")
    (is (every? #(= :accepted (:status %)) (butlast acks)) "Every command except the stale end is accepted")
    (testing "coarse/fine words, including one that continues across the universe boundary"
      (is (= [112 163] (span-dimmer 1)) "16-bit dimmer in universe 1")
      (is (= [[48 78] [43 133]] [(at 1 1 [1 2]) (at 10 1 [1 2])]) "aimed 16-bit pan with movement history")
      (is (= [20 123 241 169] (into (at 1 1 [512]) (at 1 2 [1 2 3])))
          "pan coarse is universe 1 address 512; pan fine and the tilt word are in universe 2")
      (is (= [13 211] (at 5 3 [1 2])) "explicit coarse/fine offsets in universe 3")
      (is (< 10 (count (distinct (map #(byte-at % 2 1) frames)))) "the cross-universe fine byte moves"))
    (testing "HTP: the accent's componentwise maximum over the base color"
      (is (= [61 77 204] (at 3 1 [509 510 511]))))
    (testing "LTP among equal priorities, release, reveal and relaunch"
      (is (= [49 0 64] (wash-b-rgb 4)) "cover-a, with the low-priority raw red channel")
      (is (= [49 255 128] (wash-b-rgb 5)) "later cover-b takes over")
      (is (= [49 0 64] (wash-b-rgb 7)) "ending cover-b reveals cover-a")
      (is (= [255 0 64] (wash-b-rgb 8)) "ending the raw red channel reveals cover-a red")
      (is (= [0 255 128] (wash-b-rgb 9)) "relaunched cover-b is latest again"))
    (testing "priority: the high-priority dimmer launched mid-run and released mid-run"
      (is (= [13 211] (span-dimmer 5)) "peak, 30 ms after launch")
      (is (= [184 81] (span-dimmer 8)) "the base level after peak ends"))
    (testing "restart and stale epoch"
      (is (= (first frames) (nth frames 11)) "the restarted show is idle")
      (is (= :stale-epoch (:code (last acks))) "the old-epoch end is rejected")
      (is (= 3 (count (get-in (peek (:steps recording)) [:expected :instances])))))))

(deftest bounded-workers-publish-the-oracle-bytes
  (let [{:keys [recording frames output]} (oracle)
        patch (render/compile-patch rig)
        compile (batch/compiler-for-rig rig)]
    (doseq [result (worker-lanes
                    (fn [lane map-work]
                      (let [seen (ConcurrentLinkedQueue.)
                            physical (atom 0.0) sent (atom [])
                            owner (publisher physical sent)]
                        (try
                          (let [verified (replay/replay recording (snapshot-probe compile seen)
                                                        {:map-work map-work
                                                         :render-output (publishing-render owner patch physical)})]
                            (assoc lane :verified verified :sent @sent :seen (vec seen)
                                   :snapshots (frozen-snapshots seen)))
                          (finally (scheduler/close! owner))))))]
      (testing (pr-str (select-keys result [:workers :reversed?]))
        (is (= (count (:steps recording)) (get-in result [:verified :verified-steps]))
            "Every model, state, event, intent and full output digest matches the serial oracle")
        (is (= output (get-in result [:verified :output])))
        (is (= frames (mapv :universes (:sent result)))
            "Every published packet is byte-identical to the serial whole-frame render")
        (is (= {:frames 13 :frozen? true} (:snapshots result))
            "Each of the 13 frames with active effects gave all workers one frozen snapshot")
        (is (every? #(.startsWith ^String (:thread %) "afterglow-runtime-") (:seen result))
            "Effect steps ran on the bounded pool, not the caller")))))

(deftest scalar-and-batch-compilers-share-the-worker-oracle
  (let [{:keys [frames]} (oracle)
        scalar (atom [])]
    (replay/record (initial-show) inputs compiler/compile-effect {:render-output (capturing-render scalar)})
    (is (= frames @scalar))))

(deftest a-completion-from-before-a-restart-is-never-published
  (let [physical (atom 0.0) sent (atom []) owner (publisher physical sent)
        patch (render/compile-patch rig)
        idle (render/resolve-contributions rig [] {:include-values? false :compact-values? true})]
    (try
      (let [stale (scheduler/begin-frame! owner 0 patch {} false)
            current (scheduler/begin-frame! owner 1 patch {} false)]
        (is (nil? (scheduler/complete! owner stale idle)))
        (is (true? (scheduler/complete! owner current idle)))
        (scheduler/publish! owner)
        (is (= [(unsigned-universes (:universes idle))] (mapv :universes @sent))))
      (finally (scheduler/close! owner)))))

;; The #618 legacy scenario: its serial paired run produced the checked
;; receipts in tolglow.legacy-frame-oracle-test. The worker path must reproduce
;; every frame, lifecycle entry and published byte of that run.

(defn- legacy-capture []
  (scenario/read-edn (io/resource "tolglow/legacy_frame_oracle/legacy.edn")))

(deftest legacy-oracle-scenario-is-identical-under-bounded-workers
  (let [legacy (legacy-capture)
        compile (batch/compiler-for-rig compare/rig)
        serial (compare/paired-run legacy compile)
        patch (render/compile-patch compare/rig)]
    (doseq [result (worker-lanes
                    (fn [lane map-work]
                      (let [physical (atom 0.0) sent (atom []) owner (publisher physical sent)]
                        (try
                          (assoc lane
                                 :run (compare/paired-run legacy compile
                                                          {:map-work map-work
                                                           :render-output (publishing-render owner patch physical)})
                                 :sent @sent)
                          (finally (scheduler/close! owner))))))]
      (testing (pr-str (select-keys result [:workers :reversed?]))
        (is (= serial (:run result)) "Frames, contribution order, intents, events and lifecycle history")
        (is (= (mapv :universes (:frames serial)) (mapv :universes (:sent result)))
            "All 512 bytes of every published frame match the serial oracle")))))
