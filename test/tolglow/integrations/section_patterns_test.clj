(ns tolglow.integrations.section-patterns-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio.capture-test :as pcm-test]
            [tolglow.integrations.reference-position :as position]
            [tolglow.integrations.section-patterns :as patterns]
            [tolglow.integrations.track-profile :as track-profile])
  (:import [java.io ByteArrayInputStream]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem]))

(def ^:private reference {:id "sha256:recording" :version "sha256:analysis"})
(def ^:private provenance {:source-id :player :session-id "play-1" :clock-id :host})
(def ^:private stream {:source-id :main :session-id "capture-1"})
(def ^:private policy {:enabled? true :version :feel-v1 :show-seed 514
                       :patterns [:soft :pulse :bloom :sweep]})

(defn- section [id start end beat kind confidence signature]
  {:id id :start-position-ms start :end-position-ms end :start-beat beat
   :boundary-kind kind :boundary-confidence confidence
   :features (cond-> {} signature (assoc :signature signature))})

(def ^:private profile
  {:version 1 :reference reference :duration-ms 12000.0
   :sections [(section 0 0.0 4000.0 0.0 :file-start 1.0 {:bass-notes [33]})
              (assoc (section 1 4000.0 8000.0 8.0 :confirmed-change 0.9 {:bass-notes [40]}) :family-id 1)
              (section 2 8000.0 10000.0 16.0 :confirmed-change 0.2 {:bass-notes [33]})
              (section 3 10000.0 12000.0 20.0 :confirmed-change 0.9 nil)]})

(defn- observe [state now data]
  (position/advance state now (merge {:provenance provenance :occurrence (:occurrence state)
                                      :observed-at-ms now} data)))

(defn- play [state now position-ms & [details]]
  (observe state now {:type :match :match (merge {:position-ms position-ms :reference reference
                                                  :rate 1.0 :score 0.95} details)}))

(defn- lock-at [state now track-ms]
  (reduce (fn [align offset] (play align (+ now offset) (+ track-ms offset))) state [-100 -50 0]))

(defn- step [state now alignment beat boundaries & [opts]]
  (patterns/advance state
                    (merge {:now-ms now :reference-state alignment :causal-stream stream
                            :transport {:show-epoch 2 :source :manual :source-epoch 0
                                        :beat beat :playing? true :boundaries boundaries}} opts)))

(deftest repeatable-reference-patterns-use-the-authoritative-show-boundary
  (let [alignment (lock-at (position/new-state provenance) 3800 3800)
        planner (patterns/new-planner profile (assoc policy :on :bar))
        run (fn [planner]
              (let [preview (step planner 3850 alignment 7.7 #{})
                    crossed (step (:state preview) 4000 alignment 8.0 #{:beat})
                    admitted (step (:state crossed) 4100 alignment 8.2 #{:beat :bar})
                    repeated (step (:state admitted) 4100 alignment 8.2 #{:bar})]
                {:preview preview :crossed crossed :admitted admitted :repeated repeated}))
        {:keys [preview crossed admitted repeated] :as first-run} (run planner)
        [event] (:events admitted)]
    (is (= first-run (run planner)))
    (is (= [{:section-id 0 :reason :unconfirmed-boundary} {:section-id 2 :reason :low-confidence}
            {:section-id 3 :reason :missing-signature}] (:unqualified preview)))
    (is (= :reference (:mode preview)))
    (is (= 4000.0 (get-in preview [:forecast :due-at-ms])))
    (is (empty? (:events crossed)))
    (is (= [:music/section-pattern 1 :reference 8.2 4100 :feel-v1]
           ((juxt :type :version :mode :show-beat :time-ms :policy-version) event)))
    (is (= (get-in preview [:forecast :decision-seed]) (:decision-seed event)))
    (is (some #{(:pattern event)} (:patterns policy)))
    (is (empty? (:events repeated)))
    (is (not= (:decision-seed event)
              (get-in (step (patterns/new-planner profile (update policy :show-seed inc))
                            3850 alignment 7.7 #{}) [:forecast :decision-seed])))))

(deftest cancel-a-forecast-on-ambiguity-or-expiry
  (let [alignment (lock-at (position/new-state provenance) 3800 3800)
        planner (patterns/new-planner profile policy)
        predicted (step planner 3850 alignment 7.7 #{})
        ambiguous (observe alignment 4000 {:type :match
                                           :match {:position-ms 4000 :reference reference :rate 1.0 :score 0.91}
                                           :runner-up {:reference reference :score 0.87}})
        revoked (step (:state predicted) 4000 ambiguous 8.0 #{:beat})
        stale (step (:state revoked) 4001 alignment 8.0 #{:beat})
        expired (step (:state predicted) 4550 alignment 9.1 #{:beat})]
    (is (some? (:forecast predicted)))
    (is (= :ambiguous (:status (position/snapshot ambiguous 4000))))
    (doseq [result [revoked stale expired]]
      (is (= :causal (:mode result)))
      (is (nil? (:forecast result)))
      (is (empty? (:events result))))
    (testing "A known playback end exactly at the boundary excludes it"
      (let [bounded (reduce (fn [state now] (play state now now {:until-position-ms 4000}))
                            (position/new-state provenance) [3700 3750 3800])]
        (is (nil? (:forecast (step planner 3850 bounded 7.7 #{}))))))))

(deftest intentional-loop-replays-the-same-pattern-with-new-delivery-identity
  (let [align (lock-at (position/new-state provenance) 3800 3800)
        planner (patterns/new-planner profile policy)
        predicted (step planner 3850 align 7.7 #{})
        first-boundary (step (:state predicted) 4000 align 8.0 #{:beat})
        [first-event] (:events first-boundary)
        lost (observe align 4010 {:type :unavailable})
        invalidated (step (:state first-boundary) 4010 lost 8.02 #{})
        same-occurrence (lock-at lost 4150 3800)
        dedup (step (:state invalidated) 4150 same-occurrence 8.3 #{})
        looped (observe same-occurrence 4900 {:type :loop})
        canceled (step (:state dedup) 4900 looped 9.8 #{})
        relocked (lock-at looped 5001 3800)
        later-plan (step (:state canceled) 5051 relocked 10.1 #{})
        replay (step (:state later-plan) 5201 relocked 10.4 #{:beat})
        [second-event] (:events replay)]
    (is (nil? (:forecast dedup)))
    (is (nil? (:forecast canceled)))
    (is (= [0 1] (mapv #(get-in % [:fence :occurrence]) [first-event second-event])))
    (is (not= (:delivery-id first-event) (:delivery-id second-event)))
    (is (= (select-keys first-event [:decision-seed :pattern :reference :section])
           (select-keys second-event [:decision-seed :pattern :reference :section])))))

(defn- causal [sequence beat time-ms]
  {:type :audio/section-change :version 1 :confidence 0.91 :boundary-beat beat :time-ms time-ms
   :source {:type :audio-analysis :id :main} :sequence sequence :clock-source :manual :clock-epoch 0})

(deftest causal-fallback-uses-fresh-owner-clock-evidence-once
  (let [planner (patterns/new-planner nil policy)
        started (step planner 500 nil 1.0 #{})
        eligible (causal 4 2.0 5010)
        wrong-clock (assoc (causal 5 3.0 5010) :clock-epoch 1)
        historical (causal 6 0.0 5010)
        queued (step (:state started) 5010 nil 10.02 #{} {:causal-events [eligible wrong-clock historical]})
        fired (step (:state queued) 5500 nil 11.0 #{:beat})
        [event] (:events fired)
        repeated (step (:state fired) 5600 nil 11.2 #{:beat} {:causal-events [eligible]})
        expired (step (:state started) 6000 nil 12.0 #{:beat} {:causal-events [eligible]})
        overridden (step (:state queued) 5500 nil 11.0 #{:beat} {:override? true})
        resumed (step (:state overridden) 5550 nil 11.1 #{:beat} {:causal-events [eligible]})
        off (step (patterns/new-planner nil {}) 5500 nil 11.0 #{:beat} {:causal-events [eligible]})]
    (is (= :causal (:mode queued)))
    (is (nil? (:forecast queued)))
    (is (empty? (:events queued)))
    (is (= [:music/section-pattern :causal 11.0 2.0]
           ((juxt :type :mode :show-beat #(get-in % [:section :start-beat])) event)))
    (is (= stream (:stream event)))
    (is (= :stopped (:mode overridden) (:mode off)))
    (doseq [result [repeated expired overridden resumed off]]
      (is (empty? (:events result))))))

(deftest causal-decision-waits-for-its-next-owner-boundary
  (doseq [[division due-ms beat] [[:bar 8000 16.0] [:phrase 16000 32.0]]]
    (testing (name division)
      (let [planner (patterns/new-planner nil (assoc policy :on division))
            started (step planner 6000 nil 12.0 #{:beat :bar})
            queued (step (:state started) 6010 nil 12.02 #{} {:causal-events [(causal 1 12.0 6000)]})
            waiting (step (:state queued) 7000 nil 14.0 #{:beat})
            next-boundary (conj #{:beat :bar} division)
            fired (step (:state waiting) due-ms nil beat next-boundary)
            replaced (step (:state waiting) due-ms nil beat next-boundary
                           {:causal-stream (assoc stream :session-id "capture-2")})
            stopped (step (:state waiting) due-ms nil beat next-boundary {:causal-stream nil})
            overridden (step (:state waiting) due-ms nil beat next-boundary {:override? true})]
        (is (empty? (:events queued)))
        (is (empty? (:events waiting)))
        (is (= [:causal beat 12.0]
               ((juxt :mode :show-beat #(get-in % [:section :start-beat])) (first (:events fired)))))
        (doseq [result [replaced stopped overridden]]
          (is (empty? (:events result))))))))

(deftest a-local-recording-profile-drives-a-verified-reference-pattern
  (let [path (Files/createTempFile "tolglow-section-plan-" ".wav" (make-array FileAttribute 0))
        ^bytes bytes (pcm-test/pcm (pcm-test/section-pcm-values) false)
        format (AudioFormat. 8000.0 16 1 true false)]
    (try
      (with-open [input (AudioInputStream. (ByteArrayInputStream. bytes) format (quot (alength bytes) 2))]
        (AudioSystem/write input AudioFileFormat$Type/WAVE (.toFile path)))
      (let [profile (track-profile/analyze-file path {:beat-grid {:bpm 120} :beats-per-window 2})
            planner (patterns/new-planner profile policy)
            align (reduce (fn [state time-ms]
                            (play state time-ms time-ms {:reference (:reference profile)}))
                          (position/new-state provenance) [1700 1750 1800])
            preview (step planner 1800 align 3.6 #{})
            admitted (step (:state preview) 2000 align 4.0 #{:beat})
            [event] (:events admitted)]
        (is (= [2000.0] (mapv :position-ms (:events profile))))
        (is (= 2000.0 (get-in preview [:forecast :due-at-ms])))
        (is (= 1 (count (:events admitted))))
        (is (= (:reference profile) (:reference event)))
        (is (= [4.0 2000.0] ((juxt :start-beat :start-position-ms) (:section event)))))
      (finally (Files/deleteIfExists path)))))
