(ns tolglow.audio.capture-test
  (:require [clojure.test :refer :all]
            [tolglow.audio.capture :as audio]
            [tolglow.integrations.analysis :as analysis])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def config {:default? true :sample-rate 48000.0 :channels 1 :block-frames 480})
(defn close? [a b] (< (Math/abs (double (- a b))) 1.0e-9))
(defn pcm [values big-endian?]
  (byte-array (mapcat (fn [value]
                       (let [u (bit-and value 65535) low (unchecked-byte (bit-and u 255)) high (unchecked-byte (bit-shift-right u 8))]
                         (if big-endian? [high low] [low high]))) values)))

(defn memory-source [values chunk-size closed]
  (let [bytes (pcm values false) position (atom 0)]
    {:started-at-ms 0.0 :description {:name "Injected PCM"}
     :read! (fn [out offset length]
              (let [n (min length chunk-size (- (alength bytes) @position))]
                (if (zero? n) -1
                    (do (System/arraycopy bytes @position out offset n) (swap! position + n) n))))
     :close! #(swap! closed inc)}))

(deftest pcm-decoding-preserves-endianness-sign-and-stereo-average
  (doseq [big? [false true]]
    (let [bytes (pcm [-32768 -16384 0 16384 32767] big?) original (vec bytes)
          decoded (audio/decode-pcm16 bytes 0 (alength bytes) 1 big?)]
      (is (= [-1.0 -0.5 0.0 0.5 (/ 32767.0 32768.0)] (vec decoded)))
      (is (= original (vec bytes)))))
  (is (= [0.0 0.5 -1.0]
         (vec (audio/decode-pcm16 (pcm [16384 -16384 16384 16384 -32768 -32768] false) 0 12 2 false))))
  (is (= [0.5] (vec (audio/decode-pcm16 (pcm [0 16384 0] false) 2 2 1 false))))
  (is (thrown? clojure.lang.ExceptionInfo (audio/decode-pcm16 (byte-array 3) 0 3 1 false)))
  (is (thrown? clojure.lang.ExceptionInfo (audio/decode-pcm16 (byte-array 4) 0 4 nil false))))

(deftest pcm-decoding-selects-one-based-channels-at-the-physical-frame-stride
  (doseq [big? [false true]]
    (let [bytes (pcm [100 200 300 400
                      32767 1234 -32768 16384
                      -32768 3000 8192 -16384
                      500 600 700 800] big?)
          original (vec bytes)]
      (is (= [-1.0 0.25] (vec (audio/decode-pcm16 bytes 8 16 4 big? 3))))
      (is (= [0.5 -0.5] (vec (audio/decode-pcm16 bytes 8 16 4 big? 4))))
      (is (= original (vec bytes)))))
  (is (= [0.5] (vec (audio/decode-pcm16 (pcm (concat (repeat 63 -32768) [16384]) false)
                                       0 128 64 false 64))))
  (is (= [-0.5] (vec (audio/decode-pcm16 (pcm [16384 -16384] false) 0 4 2 false 2))))
  (doseq [[offset byte-count channels input-channel] [[0 8 4 nil] [0 8 4 0] [0 8 4 5] [0 8 4 2.5]
                                                       [0 8 65 64] [0 6 4 3] [Long/MAX_VALUE 8 4 3]]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (audio/decode-pcm16 (byte-array 8) offset byte-count channels false input-channel)))))

(deftest creating-and-invalid-starting-never-open-a-device
  (let [opened (atom 0) manager (audio/create-manager {:open-source! (fn [_] (swap! opened inc))})]
    (is (= :idle (:state (audio/status manager))))
    (is (zero? @opened))
    (is (= {:default? true :sample-rate 48000.0 :channels 1 :block-frames 480 :buffer-blocks 8
            :big-endian? false :analyzer-options {}} (audio/validate-config {:default? true})))
    (is (= {:channels 64 :input-channel 64}
           (select-keys (audio/validate-config (assoc config :channels 64 :input-channel 64))
                        [:channels :input-channel])))
    (doseq [bad [{} (assoc config :channels 3) (assoc config :block-frames 0)
                 (assoc config :mixer-index 0) (assoc config :sample-rate 0)
                 {:mixer-index Long/MAX_VALUE} {:default? true :input-channel 1}
                 (assoc config :channels 4 :input-channel 0) (assoc config :channels 4 :input-channel 5)
                 (assoc config :channels 4 :input-channel "3") (assoc config :channels 65 :input-channel 3)]]
      (is (thrown? clojure.lang.ExceptionInfo (audio/start! manager bad {:events! identity}))))
    (is (zero? @opened))
    (is (:closed? (audio/close! manager)))
    (is (thrown? clojure.lang.ExceptionInfo (audio/start! manager config {:events! identity})))
    (is (zero? @opened))))

(deftest partial-reads-produce-sample-counter-timestamps-and-close-on-eof
  (let [closed (atom 0) done (atom []) events (atom []) features (atom []) ticks (atom 0)
        values (repeat 1200 8192)
        manager (audio/create-manager {:open-source! (fn [_] (memory-source values 7 closed))
                                       :now-ms #(double (* 12 (swap! ticks inc)))})
        handle (audio/start! manager config {:events! #(swap! events into %)
                                             :features! #(swap! features conj %)
                                             :done! #(swap! done conj %)})
        result (deref (:result handle) 3000 ::timeout) status (audio/status manager)]
    (is (= :completed (:status result)))
    (is (= 1 @closed (count @done)))
    (is (= 1200 (get-in status [:metrics :frames]) (:sample-counter status)))
    (is (= 3 (get-in status [:metrics :blocks])))
    (is (= [5.0 15.0 15.0] (mapv :time-ms @features)))
    (is (every? #(close? 0.25 (:rms %)) @features))
    (is (= [7.0] (mapv :latency-ms @events)))
    (is (= 240 (get-in result [:analyzer-state :hop :n])))
    (is (false? (:running? status)))
    (audio/close! manager)
    (is (= 1 @closed))))

(deftest stereo-capture-mixes-before-analysis
  (let [closed (atom 0) manager (audio/create-manager {:open-source! (fn [_] (memory-source (take 960 (cycle [16384 -16384])) 11 closed))
                                                     :now-ms (constantly 12.0)})
        handle (audio/start! manager (assoc config :channels 2) {:events! identity})]
    (is (= :completed (:status (deref (:result handle) 3000 ::timeout))))
    (is (zero? (get-in (audio/status manager) [:features :rms])))
    (is (= 480 (get-in (audio/status manager) [:metrics :frames])))
    (is (= 1 @closed))
    (audio/close! manager)))

(deftest malformed-and-stalled-streams-close-and-report-errors
  (doseq [[source expected] [[{:read! (fn [out offset _] (aset-byte out offset (byte 1))
                                       (if (zero? offset) 1 -1))} :truncated-pcm]
                            [{:read! (fn [& _] 0)} :stalled-input]
                            [{:read! (fn [& _] 100000)} :invalid-read]
                            [{:read! (fn [& _] (throw (ex-info "read failed" {})))} :capture-error]]]
    (let [closed (atom 0) manager (audio/create-manager {:open-source! (fn [_] (assoc source :close! #(swap! closed inc)))
                                                       :now-ms (constantly 0.0)})
          handle (audio/start! manager config {:events! identity}) result (deref (:result handle) 3000 ::timeout)]
      (is (= :error (:status result)))
      (is (= expected (get-in result [:error :code])))
      (is (= 1 @closed))
      (audio/close! manager))))

(deftest stop-closes-a-blocked-read-once-and-releases-the-slot
  (let [entered (CountDownLatch. 1) released (CountDownLatch. 1) closed (atom 0)
        manager (audio/create-manager {:open-source! (fn [_] {:read! (fn [& _] (.countDown entered) (.await released) -1)
                                                              :close! #(do (swap! closed inc) (.countDown released))})})
        handle (audio/start! manager config {:events! identity})]
    (is (.await entered 2 TimeUnit/SECONDS))
    (is (thrown? clojure.lang.ExceptionInfo (audio/start! manager config {:events! identity})))
    (is (:stopped? (audio/stop! manager)))
    (is (= :stopped (:status (deref (:result handle) 1000 ::timeout))))
    (is (= 1 @closed))
    (is (:stopped? (audio/stop! manager)))
    (audio/close! manager)
    (is (= 1 @closed))))

(deftest stop-during-open-closes-the-late-source
  (let [opening (CountDownLatch. 1) release (promise) closed (atom 0)
        manager (audio/create-manager {:open-source! (fn [_] (.countDown opening)
                                                       (loop [] (when-not (realized? release)
                                                                  (try (Thread/sleep 1) (catch InterruptedException _)) (recur)))
                                                       {:read! (fn [& _] -1) :close! #(swap! closed inc)})})
        handle (audio/start! manager config {:events! identity})]
    (is (.await opening 2 TimeUnit/SECONDS))
    (is (false? (:stopped? (audio/stop! manager 0))))
    (deliver release true)
    (is (= :stopped (:status (deref (:result handle) 2000 ::timeout))))
    (is (= 1 @closed))
    (audio/close! manager)))

(deftest dropped-frames-reset-calibration-and-preserve-exact-timestamps
  (let [reads (atom 0) closed (atom 0) ticks (atom 0)
        manager (audio/create-manager
                  {:open-source! (fn [_] {:started-at-ms 0.0
                                         :read! (fn [out offset length]
                                                  (let [read (swap! reads inc)]
                                                    (if (> read 2) -1
                                                        (do (java.util.Arrays/fill ^bytes out (byte 0))
                                                            {:bytes length :dropped-frames (if (= 2 read) 480 0)}))))
                                         :close! #(swap! closed inc)})
                   :now-ms #(if (= 1 (swap! ticks inc)) 12.0 32.0)})
        events (atom []) handle (audio/start! manager config {:events! #(swap! events into %)})
        result (deref (:result handle) 2000 ::timeout) status (audio/status manager)]
    (is (= :completed (:status result)))
    (is (= 480 (get-in status [:metrics :dropped-input-frames])))
    (is (= 1440 (:sample-counter status)))
    (is (= [5.0 25.0] (mapv :time-ms @events)))
    (is (= [1 2] (mapv :sequence @events)))
    (is (= 0.02 (get-in status [:features :confidence])))
    (is (= 1 @closed))
    (audio/close! manager)))

(deftest possible-overflow-does-not-invent-known-loss-or-clock-alignment
  (let [reads (atom 0) closed (atom 0) clock-calls (atom 0) features (atom []) ticks (atom 0)
        manager (audio/create-manager
                  {:open-source! (fn [_] {:started-at-ms 0.0
                                         :read! (fn [_ _ length] (let [n (swap! reads inc)]
                                                                  (if (> n 2) -1 {:bytes length :possible-overflow? (= 1 n)})))
                                         :close! #(swap! closed inc)})
                   :now-ms #(* 12.0 (swap! ticks inc))})
        handle (audio/start! manager config {:events! identity :features! #(swap! features conj %)
                                             :clock! #(do (swap! clock-calls inc) {:time-ms 0 :beat 0 :bpm 120 :source :test})})]
    (is (= :completed (:status (deref (:result handle) 2000 ::timeout))))
    (is (= 1 (get-in (audio/status manager) [:metrics :possible-overflows])))
    (is (= 0 (get-in (audio/status manager) [:metrics :dropped-input-frames])))
    (is (zero? @clock-calls))
    (is (every? #(false? (get-in % [:capture :timing-valid?])) @features))
    (audio/close! manager)))

(deftest event-queue-rejection-and-subscriber-errors-are-visible
  (let [closed (atom 0) subscriptions (analysis/subscribe (analysis/new-subscriptions) :broken :all
                                                                        (fn [_] (throw (ex-info "subscriber" {}))))
        manager (audio/create-manager {:open-source! (fn [_] (memory-source (repeat 480 8192) 960 closed))
                                       :now-ms (constantly 12.0)})
        handle (audio/start! manager config {:events! (constantly false) :subscriptions subscriptions
                                             :features! (fn [_] (throw (ex-info "feature consumer" {})))
                                             :done! (fn [_] (throw (ex-info "completion consumer" {})))})]
    (is (= :completed (:status (deref (:result handle) 2000 ::timeout))))
    (is (= 1 (get-in (audio/status manager) [:metrics :dropped-events])))
    (is (= 1 (get-in (audio/status manager) [:metrics :subscriber-errors])))
    (is (= 1 (get-in (audio/status manager) [:metrics :callback-errors])))
    (is (= 1 @closed))
    (audio/close! manager)))

(deftest open-failure-and-close-failure-resolve-the-run
  (let [manager (audio/create-manager {:open-source! (fn [_] (throw (ex-info "No input" {:audio/error :missing-input})))})
        handle (audio/start! manager config {:events! identity})]
    (is (= :missing-input (get-in (deref (:result handle) 1000 ::timeout) [:error :code])))
    (is (false? (:running? (audio/status manager))))
    (audio/close! manager))
  (let [manager (audio/create-manager {:open-source! (fn [_] {:read! (fn [& _] -1)
                                                            :close! (fn [] (throw (ex-info "close failed" {})))})})
        handle (audio/start! manager config {:events! identity}) result (deref (:result handle) 1000 ::timeout)]
    (is (= :completed (:status result)))
    (is (= "close failed" (get-in result [:close-error :message])))
    (audio/close! manager)))

(deftest completion-holds-the-single-worker-slot-until-callback-returns
  (let [entered (CountDownLatch. 1) released (CountDownLatch. 1) closed (atom 0) opened (atom 0)
        manager (audio/create-manager {:open-source! (fn [_] (swap! opened inc)
                                                       {:read! (fn [& _] -1) :close! #(swap! closed inc)})})
        first-run (audio/start! manager config {:events! identity :done! (fn [_] (.countDown entered) (.await released))})]
    (is (.await entered 2 TimeUnit/SECONDS))
    (is (thrown? clojure.lang.ExceptionInfo (audio/start! manager config {:events! identity})))
    (.countDown released)
    (is (= :completed (:status (deref (:result first-run) 1000 ::timeout))))
    (let [second-run (audio/start! manager config {:events! identity})]
      (is (not= (:run-id first-run) (:run-id second-run)))
      (is (= :completed (:status (deref (:result second-run) 1000 ::timeout)))))
    (is (= 2 @closed @opened))
    (audio/close! manager)))

(deftest decoded-pcm-kicks-reach-the-clock-aware-event-boundary
  (let [values (map (fn [index]
                      (let [ms (/ (* 1000.0 index) 48000.0) local (mod (- ms 1000.0) 500.0)]
                        (if (and (>= ms 1000) (< local 160))
                          (long (* 32767 0.8 (Math/exp (/ (- local) 45.0))
                                   (Math/sin (/ (* 2 Math/PI 65 local) 1000.0)))) 0)))
                    (range 86400))
        closed (atom 0) ticks (atom 0) events (atom [])
        manager (audio/create-manager {:open-source! (fn [_] (memory-source values 512 closed))
                                       :now-ms #(+ 2.0 (* 10.0 (swap! ticks inc)))})
        handle (audio/start! manager config {:events! #(swap! events into %)
                                             :clock! (constantly {:time-ms 0.0 :beat 0 :bpm 120
                                                                  :playing? true :source :test-clock})})
        result (deref (:result handle) 5000 ::timeout)
        beats (filter #(= :audio/beat (:type %)) @events)]
    (is (= :completed (:status result)))
    (is (= [2 3] (mapv :beat beats)))
    (is (every? #(= :test-clock (:clock-source %)) beats))
    (is (every? #(true? (get-in % [:timing :capture-clock-valid?])) beats))
    (is (= 1 @closed))
    (audio/close! manager)))

(deftest a-gap-mid-block-discards-the-old-prefix-and-keeps-frame-accounting
  (let [reads (atom 0) manager (audio/create-manager
                               {:open-source! (fn [_] {:started-at-ms 0.0 :close! (fn [])
                                                      :read! (fn [_ _ length]
                                                               (case (swap! reads inc)
                                                                 1 4
                                                                 2 {:bytes 4 :dropped-frames 480}
                                                                 3 length
                                                                 -1))})
                                :now-ms (constantly 25.0)})
        handle (audio/start! manager config {:events! identity}) result (deref (:result handle) 1000 ::timeout)
        status (audio/status manager)]
    (is (= :completed (:status result)))
    (is (= 480 (get-in status [:metrics :frames]) (get-in status [:metrics :dropped-input-frames])))
    (is (= 2 (get-in status [:metrics :discarded-input-frames])))
    (is (= 962 (:sample-counter status)))
    (is (close? (+ 15.0 (/ 2000.0 48000.0)) (get-in status [:features :time-ms])))
    (audio/close! manager)))
