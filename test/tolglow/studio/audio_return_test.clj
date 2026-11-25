(ns tolglow.studio.audio-return-test
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.audio-duplex :as duplex]
            [tolglow.integrations.audio-return :as returns]
            [tolglow.show :as show]
            [tolglow.persistence :as persistence])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def fixture {:id :wash :position [0 0 0] :universe 1 :address 1 :channels {:dimmer 1}})

(def effect {:id :light :version 1 :params {:level {:type :number :bounds [0 1] :default 0.5}}
             :outputs {:dimmer {:op :param :key :level}}})

(def lane {:id "kick" :dry-cue :dry :dry-param :level :wet-cue :echo :wet-param :level
           :send-channel 1 :return-channel 2 :frequency-range [100 1200]})

(def config {:input-mixer "Live Return" :output-mixer "Live Send" :channels 2
             :sample-rate 8000 :block-frames 512 :lanes [lane]})

(defn show []
  (control/new-show {:rig {:fixtures [fixture]}
                     :registry {:effects {:light effect}
                                :cues (into {} (for [id [:dry :echo :fader :reverb]]
                                                 [id {:id id :version 1 :effect-id :light}]))}
                     :settings {:audio-returns config}}))

(defn send! [runtime op t]
  (let [receipt (show/submit! runtime op)]
    (show/tick! runtime t)
    (deref receipt 1000 :timeout)))

(defn dimmer [runtime]
  (bit-and 255 (aget ^bytes (get-in @(:state runtime) [:output :universes 1]) 0)))

(defn await-value [f]
  (let [until (+ (System/nanoTime) 3000000000)]
    (loop []
      (or (f) (when (< (System/nanoTime) until) (Thread/sleep 3) (recur))))))

(defn scale [samples gain] (double-array (map #(* gain %) samples)))

(deftest independent-lanes-validate-registry-and-channel-isolation
  (let [model (show)
        valid (update config :lanes conj (assoc lane :id "bass" :dry-cue :fader :wet-cue :reverb
                                                :send-channel 2 :return-channel 1))]
    (is (= [[0 1] [0 1]] (mapv :dry-bounds (:lanes (returns/validate-config model valid)))))
    (doseq [bad [(assoc lane :wet-cue :dry) (assoc lane :send-channel 3)
                 (assoc lane :dry-param :missing) (assoc lane :curve :broken)
                 (assoc lane :frequency-range [10 100]) (assoc lane :max-tail {:beats -1})]]
      (is (thrown? Exception (returns/validate-config model (assoc config :lanes [bad])))))
    (is (thrown? Exception (returns/validate-config model (update config :lanes conj (assoc lane :id "two")))))
    (is (thrown? Exception (returns/validate-config model (assoc valid :input-mixer "Live Send"))))
    (let [a (double-array [1.0 0.5 -1.0]) b (double-array [0.0 -0.5 0.25])
          bytes (byte-array 12)]
      (duplex/encode-channel! bytes a 2 1)
      (duplex/encode-channel! bytes b 2 2)
      (doseq [[actual expected] [[(duplex/decode-channel bytes 2 1) a] [(duplex/decode-channel bytes 2 2) b]]]
        (is (every? #(< (Math/abs (double %)) 0.00004) (map - actual expected)))))))

(deftest actual-duplex-pcm-echo-and-reverb-render-after-release
  (let [writes (LinkedBlockingQueue.) reads (LinkedBlockingQueue.) permits (LinkedBlockingQueue.)
        closed (promise)
        open! (fn [_]
                {:write! (fn [^bytes bytes offset n]
                           (if (= :close (.take permits)) -1
                             (do (.put writes (java.util.Arrays/copyOfRange bytes offset (+ offset n))) n)))
                 :read! (fn [^bytes buffer offset n]
                          (let [incoming (.take reads)]
                            (if (= :close incoming) -1
                              (do (System/arraycopy incoming 0 buffer offset n) n))))
                 :close! #(do (.offer reads :close) (.offer permits :close) (deliver closed true))})
        runtime (show/create-runtime {:model (show) :open-audio-returns! open! :workspace-options nil})
        manager (:audio-returns runtime) t (atom 0)
        model #(get @(:state runtime) :model)
        dry! (fn [] (.put permits :continue) (.poll writes 2 TimeUnit/SECONDS))
        return! (fn [samples]
                  (let [seq-before (get-in @manager [:packet :windows "kick" :sequence] 0)
                        bytes (duplex/encode-channel! (byte-array 2048) samples 2 2)]
                    (.put reads bytes)
                    (is (await-value #(when (> (get-in @manager [:packet :windows "kick" :sequence] 0)
                                               seq-before) true)))
                    (show/tick! runtime (swap! t + 25))))]
    (try
      (let [dry-id (:instance-id (send! runtime {:op :cue/launch :cue-id :dry :launch {:params {:level 0.5}}} @t))]
        (show/audio-action! runtime :return-start {})
        (is (await-value #(= :running (:status @manager))))
        (dry!)
        (let [encoded (duplex/decode-channel (dry!) 2 1)
              silent (double-array 512)]
          (is (> (reduce max encoded) 0.49))
          (return! encoded)
          (is (< 122 (dimmer runtime) 134))
          (let [id (get-in (model) [:variables :audio-return-voices "kick" :instance-id])]
            (is (string? id))
            (is (not= id dry-id)))
          (send! runtime {:op :instance/remove :instance-id dry-id} (swap! t inc))
          ;; The writer may have already generated the next block before it blocks
          ;; on the device. Flush the buffered block and the single release ramp.
          (dry!) (dry!)
          (is (every? zero? (duplex/decode-channel (dry!) 2 1)))
          (return! silent)
          (is (empty? (:instances (model))))
          (is (zero? (dimmer runtime)))
          (return! (scale encoded 0.75))
          (let [owned (get-in (model) [:variables :audio-return-voices "kick" :instance-id])
                restored (persistence/thaw (model) (persistence/freeze (model)))]
            (is (string? owned))
            (is (not= dry-id owned))
            (is (= :echo (get-in (model) [:instances owned :cue-id])))
            (is (< 89 (dimmer runtime) 100))
            (is (= config (get-in restored [:settings :audio-returns])))
            (is (empty? (:instances restored))))
          (return! (scale encoded 0.25))
          (is (< 27 (dimmer runtime) 36))
          (let [ports (get-in (model) [:routing :ports])]
            (is (> (get-in ports ["live-audio/kick/pitch-confidence" :value]) 0.95))
            (is (< 342 (get-in ports ["live-audio/kick/pitch-hz" :value]) 350))
            (is (< (get-in ports ["live-audio/kick/spectral-residual" :value]) 0.005)))
          ;; Human re-trigger of the dry cue takes precedence immediately, even
          ;; if the PCM reader still holds the older return.
          (let [replacement (:instance-id (send! runtime {:op :cue/launch :cue-id :dry
                                                           :launch {:params {:level 0.1}}} (swap! t inc)))]
            (is (= #{replacement} (set (keys (:instances (model))))))
            (is (= 0.1 (get-in (model) [:instances replacement :params :level])))
            (is (< 24 (dimmer runtime) 28)))
          (is (= :stopped (:status (show/audio-action! runtime :return-stop {}))))
          (is (deref closed 2000 false))))
      (finally (show/stop! runtime)))))

(deftest musical-deadline-and-confidence-gate-independent-of-physical-time
  (let [model (show) lane (first (:lanes (returns/validate-config model config)))
        lane (assoc lane :max-tail {:beats 1/4})
        dry {:source-id "old" :value 0.5} released (assoc dry :source-id nil)
        samples (double-array (map #(Math/sin (* 2 Math/PI 350 (/ % 8000))) (range 512)))
        analyze (fn [state d t beat pcm] (returns/analyze-return (assoc-in model [:transport :beat] beat)
                                                                lane state d pcm 8000 t))
        source (analyze nil dry 0 0 samples)
        tail (analyze (:state source) released 64 3 samples)
        held (analyze (:state tail) released 640 3 samples)
        done (analyze (:state held) released 704 13/4 samples)]
    (is (number? (get-in held [:window :value])))
    (is (= "old" (get-in done [:state :retired-id])))
    (is (nil? (get-in done [:window :value])))
    (is (nil? (get-in (analyze (:state source) dry 32 0 (double-array 512)) [:window :value])))))

(deftest abrupt-source-replacement-waits-for-previous-return-to-drain
  (let [m (show) lane (first (:lanes (returns/validate-config m config)))
        activation (fn [id level] {:id id :cue-id :dry :order 1 :phase :running :params {:level level}})
        encode (fn [id level prev drained]
                 (returns/render-dry (activation id level) lane prev drained 8000 256))
        first (encode "a" 0.4 nil nil)
        ramp (encode "b" 0.8 (:state first) nil)
        waiting (encode "b" 0.8 (:state ramp) nil)
        next (encode "b" 0.8 (:state waiting) "a")]
    (is (nil? (get-in waiting [:state :source-id])))
    (is (= "a" (get-in waiting [:state :binding])))
    (is (every? zero? (:samples waiting)))
    (is (= "b" (get-in next [:state :source-id])))
    (is (some pos? (:samples next)))))

(deftest failed-open-unplug-and-stale-session-stop-release-only-their-owner
  (let [m (show) input (promise) closed (promise)
        open! (fn [_] {:read! (fn [& _] @input -1)
                       :write! (fn [& _] @input -1)
                       :close! #(deliver closed true)})
        manager (duplex/create)]
    (try
      (duplex/start! manager m (constantly m) (constantly 0.0)
                     (fn [_] (throw (ex-info "Mixer unavailable" {}))))
      (is (await-value #(= :failed (:status (duplex/status manager)))))
      (is (= "Mixer unavailable" (:error (duplex/status manager))))
      (let [old (:session (duplex/status manager))
            current (:session (duplex/start! manager m (constantly m) (constantly 0.0) open!))]
        (is (await-value #(= :running (:status (duplex/status manager)))))
        (is (= current (:session (duplex/stop! manager old))))
        (deliver input true)
        (is (await-value #(= :failed (:status (duplex/status manager)))))
        (is (deref closed 1000 false))
        (is (nil? (duplex/frame-input! manager))))
      (finally (deliver input true) (duplex/stop! manager)))))

(deftest missed-window-lease-and-previous-epoch-are-closed-on-the-frame-boundary
  (let [m (show) cfg (returns/validate-config m config)
        sample {:session "owned" :settings config :config cfg
                :packet {:epoch (:epoch m) :show-id (:show-id m) :fresh? true :observed-at-ms 100.0
                         :windows {"kick" {:source-id "dry-old" :value 0.7 :opacity 0.5 :sequence 4
                                           :features {:rms 0.2 :pitch-confidence 1.0 :pitch-hz 220.0 :spectral-residual 0.1}}}}}
        wet (returns/advance m sample 110.0)]
    (is (= 1 (count (:instances wet))))
    (is (empty? (:instances (returns/advance wet sample 650.0))))
    (is (empty? (:instances (returns/advance wet (update-in sample [:packet :epoch] inc) 120.0))))
    (is (empty? (:instances (returns/advance wet (assoc sample :settings nil) 120.0))))))

(deftest extreme-parameter-values-remain-detectable-and-deleted-definitions-can-be-saved
  (let [m (show) lane (first (:lanes (returns/validate-config m config)))]
    (doseq [value [0.0 1.0]]
      (let [instance {:id "captured" :params {:level value}}
            first (returns/render-dry instance lane nil nil 8000 512)
            dry (returns/render-dry instance lane (:state first) nil 8000 512)
            features (:window (returns/analyze-return m lane nil (:state dry) (:samples dry) 8000 64))]
        (is (number? (:value features)))
        (is (< (Math/abs (- value (:value features))) 0.02))))
    (let [unresolved (update-in m [:registry :cues] dissoc :dry)]
      (is (= config (get-in (persistence/freeze unresolved) [:model :settings :audio-returns])))
      (is (thrown? Exception (returns/validate-config unresolved config))))))

(deftest a-stalled-capture-cannot-revive-an-overdue-release
  (let [m (show) lane (first (:lanes (returns/validate-config m config)))
        tone (double-array (map #(Math/sin (* 2 Math/PI 350 (/ % 8000))) (range 512)))
        dry {:source-id nil :binding "dead-source" :released-at-ms 50.0 :value 0.5}
        result (returns/analyze-return m lane {:source-id "dead-source"} dry tone 8000 20000.0)]
    (is (= "dead-source" (get-in result [:state :retired-id])))
    (is (nil? (get-in result [:window :value])))))
