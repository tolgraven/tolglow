(ns tolglow.visualization.cue-pads-test
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.control.pages :as pages]
            [afterglow.control.views :as views]
            [afterglow.control.devices.push-2 :as push-2]
            [afterglow.control.codecs.push-2 :as codec]
            [tolglow.demo :as demo]
            [tolglow.bench-show :as bench]
            [tolglow.bench-cue-pads :as paged]
            [tolglow.compiler :as compiler]
            [tolglow.composition.response :as response]
            [tolglow.effects.parameter-sources :as sources]
            [tolglow.modulation :as modulation]
            [tolglow.waveforms :as waveforms]
            [tolglow.effects :as fx]
            [tolglow.lighting-match :as matching]
            [tolglow.show :as show]
            [tolglow.test-support.manual-show :as manual-show]
            [afterglow.control.devices.session :as session]
            [tolglow.visualization.cue-pads :as pads]
            [tolglow.studio.query-stream :as queries]))

(def main [pads/query-id :main [0 0] 1])

(def other [pads/query-id "other" [0 0] 1])

(defn- rig []
  (-> (demo/model)
      (assoc-in [:pages :main :cells] {[0 0] :ember [0 3] :merlot})
      (assoc-in [:pages :other] {:id :other :cells {[0 0] :orchid [1 0] :sweep}})
      (assoc-in [:registry :effects :ember :outputs]
                {:color (demo/constant [1 0 0]) :dimmer {:op :beat}})
      (update-in [:registry :effects :ember] dissoc :envelope)))

(defn- at [model t]
  {:model (-> model (assoc :time-ms t :last-frame-time-ms t)
              (assoc-in [:transport :beat] (/ t 500.0)))})

(deftest mounted-regions-share-immutable-effect-keyframes
  (let [service (pads/manager) model (rig) input (at model 0) a :ipad b :push]
    (is (empty? @(:cache service)))
    (is (nil? (pads/govern! service 50 25)))
    (is (= {:ember [0.0 0.0 0.0]} (pads/present! service a input main)))
    (let [entry (:ember @(:cache service))]
      (is (= {:ember [0.125 0.0 0.0]} (pads/present! service b (at model 62.5) main)))
      (is (identical? entry (:ember @(:cache service))))
      (is (= #{:orchid} (set (keys (pads/present! service a (at model 75) other)))))
      (is (identical? entry (:ember @(:cache service))))
      (pads/release! service b)
      (is (nil? (:ember @(:cache service))))
      (is (empty? (pads/present! service a (at model 80) [pads/query-id :other [8 0] 8])))
      (is (empty? @(:cache service)))
      (is (= 0 (:clients (pads/status service))))
      (is (empty? (:instances (:model input))))
      (is (nil? (:output input))))))

(deftest sample-errors-and-opt-outs-keep-the-authored-color
  (let [service (pads/manager) model (rig)]
    (is (empty? (pads/present! service :a (at (assoc-in model [:registry :cues :ember :pad-preview] false) 0) main)))
    (is (nil? (pads/govern! service 55 25)) "A static swatch never holds back live throttling")
    (is (empty? (pads/present! service :a (at (assoc-in model [:registry :effects :ember :outputs :dimmer]
                                                         {:op :bad}) 0) main)))
    (is (= [0.125 0.0 0.0] (:ember (pads/present! service :a (at model 62.5) main))))))

(deftest preview-pressure-escalates-before-live-effects
  (let [service (pads/manager)]
    (pads/present! service :surface (at (rig) 0) main)
    (is (every? true? (repeatedly 8 #(pads/govern! service 24 25))))
    (is (= 1600 (pads/sample-period-ms service)))
    (is (false? (pads/govern! service 24 25)))
    (dotimes [_ 32] (pads/govern! service 8 25))
    (is (= 800 (pads/sample-period-ms service)))
    (pads/release! service :surface)
    (is (= {:level 0 :clients 0} (pads/status service)))))

(deftest web-and-physical-pad-readers-release-independently
  (let [service (pads/manager) source (at (rig) 75) model (:model source)
        push (pads/push-feedback service (constantly source)) handle ((:open! push))
        render! #((:project! push) handle % model)
        local {:page-id :main :origin [0 0] :ready? true}
        store (queries/create (constantly nil) (fn []) {pads/query-id
                   (fn [q] (let [lease (Object.)] {:sample! #(pads/present! service lease % q)
                                                   :close! #(pads/release! service lease)}))})
        web (get (queries/reconcile! store source #{main}) main)]
    (let [view (render! local) web-colors (:value (queries/read-result web source))
          profile (push-2/profile {:page-id :main :led-batch-size 16})
          state (merge (:initial-state profile) local {:palettes (zipmap (range 128) (repeat [0 0 0 0]))})
          messages (:midi ((:feedback profile) state view {:time-ms 75}))]
      (is (= (:ember web-colors) (get-in view [::views/pad-colors :ember])))
      (is (contains? (::views/pad-colors view) :merlot) "Hardware still shows the full page")
      (is (some #{(codec/palette-write 0 36 [10 0 0 0])} messages))
      (is (some #{[0x90 36 36]} messages)))
    (is (nil? (::views/pad-colors (render! (assoc local :target-picker {:rows []})))))
    (is (pos? (:clients (pads/status service))))
    (queries/reconcile! store source #{})
    (is (zero? (:clients (pads/status service))))
    (is (nil? (::views/pad-colors (render! (assoc local :ready? false)))))
    ((:close! push) handle)))

(deftest push-grid-is-projected-once-per-snapshot-and-viewport
  (let [service (pads/manager) source (atom (at (rig) 75))
        feedback (pads/push-feedback service #(deref source))
        lease ((:open! feedback)) local {:ready? true :page-id :main :origin [0 0]}
        render! #((:project! feedback) lease % (:model @source))
        sampled (atom 0) present! pads/present!]
    (with-redefs [pads/present! (fn [& args] (swap! sampled inc) (apply present! args))]
      (let [first-pass (render! local)]
        (is (identical? first-pass (render! local)))
        (is (= 1 @sampled))
        (is (empty? (::views/pad-colors (render! (assoc local :origin [8 0])))))
        (is (contains? (::views/pad-colors (render! (assoc local :page-id :other))) :orchid))
        (reset! source (at (rig) 100))
        (is (= [0.2 0.0 0.0] (:ember (::views/pad-colors (render! local)))))
        (is (= 4 @sampled))
        (is (nil? (::views/pad-colors (render! (assoc local :target-picker {:rows []})))))
        (is (= 0 (:clients (pads/status service))))
        (is (map? (::views/pad-colors (render! local))))
        (is (= 5 @sampled)))
      ((:close! feedback) lease)
      (is (zero? (:clients (pads/status service)))))))

(deftest push-entry-point-previews-without-starting-a-cue-or-lighting-a-fixture
  ;; All 128 palette responses belong to the same captured show frame.
  (with-redefs [show/now-ms (constantly 75)]
    (let [runtime (show/create-runtime {:model (rig) :workspace-options nil})
          input (atom nil) sent (atom [])
          capture {:open! (fn [receive!] (reset! input receive!) :capture)
                   :send! (fn [_ messages] (swap! sent into messages)) :close! (fn [_])}]
      (try
        (show/tick! runtime 0)
        (show/tick! runtime 75)
        (let [snapshot @(:state runtime)
              device (show/connect-device! runtime :push-2 {:session-id "captured-push" :midi capture
                                                           :led-batch-size 16 :schedule! nil})
              service (:cue-pad-previews runtime)]
          (doseq [index (range 128)]
            (@input (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
          (is (some #{(codec/palette-write 0 36 [10 0 0 0])} @sent))
          (is (identical? (:model snapshot) (:model @(:state runtime))))
          (is (identical? (:output snapshot) (:output @(:state runtime))))
          (is (pos? (:clients (pads/status service))))
          (is (empty? (get-in snapshot [:model :instances])))
          (let [calls (atom 0) project! pads/present!]
            (with-redefs [pads/present! (fn [& args] (swap! calls inc) (apply project! args))]
              (@input (codec/sysex 0 [0x09]))
              (is (zero? @calls) "Draining the palette ACK reuses this committed grid")))
          (session/suspend! device)
          (is (zero? (:clients (pads/status service)))))
        (finally (show/stop! runtime))))))

(deftest push-entry-point-selects-the-recent-page-without-sampling-it
  (let [model (-> (rig)
                  (assoc-in [:registry :cues :ember :swatch] "#ff0000")
                  (bench/command :recent-cue {:op :cue/launch :cue-id :ember})
                  (bench/command :end-cue {:op :instance/end :instance-id "0:1" :force? true}))]
    (manual-show/with-runtime! {:model model :workspace-options nil}
      (fn [runtime]
        (let [input (atom nil)
              sent (atom [])
              capture {:open! (fn [receive!] (reset! input receive!) :capture)
                       :send! (fn [_ messages] (swap! sent into messages)) :close! (fn [_])}
              click! (fn [cc] (@input [0xb0 cc 127]) (@input [0xb0 cc 0]))
              ack-leds! (fn [] (dotimes [_ 5] (@input (codec/sysex 0 [0x09]))))
              service (:cue-pad-previews runtime)
              note (codec/pad-note 0 7)
              device (show/connect-device! runtime :push-2 {:session-id "recent-push" :midi capture
                                                             :led-batch-size 16 :schedule! nil})
              session #(session/snapshot device)]
          (doseq [index (range 128)]
            (@input (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
          (ack-leds!)
          (is (true? (get-in (session) [:local :ready?])) (pr-str (select-keys (session) [:status :failure :errors])))
          (is (pos? (:clients (pads/status service))))
          (reset! sent [])
          (click! 51)
          (click! 22)
          (ack-leds!)
          (is (= pages/recent-page-id (get-in (session) [:local :page-id])))
          (is (empty? (:errors (session))))
          (is (zero? (:clients (pads/status service))))
          (is (some #{(codec/palette-write 0 note [64 0 0 0])} @sent))
          (@input [0x90 note 127])
          (let [id (get-in (session) [:local :selected-instance])]
            (is (= :ember (get-in @(:state runtime) [:model :instances id :cue-id]))))
          (is (empty? (:errors (session))))
          (click! 20)
          (is (= :main (get-in (session) [:local :page-id])))
          (is (pos? (:clients (pads/status service)))))))))

(deftest idle-previews-follow-the-current-meter-and-hdr-mode
  (let [service (pads/manager)
        model (-> (rig)
                  (assoc-in [:transport :meter] {:beats-per-bar 3 :bars-per-phrase 2})
                  (assoc-in [:registry :effects :ember :outputs :dimmer] {:op :bar}))
        frame (at model 750)]
    (is (= [0.5 0.0 0.0] (:ember (pads/present! service :grid frame main))))
    (let [hdr-model (assoc-in model [:registry :effects :ember :outputs :dimmer] (demo/constant 1.4))
          sample (at hdr-model 750)]
      (is (empty? (pads/present! service :grid sample main)))
      (is (= [1.0 0.0 0.0] (:ember (pads/present! service :grid (assoc sample :preview-hdr {:white-mix 0}) main)))))))

(deftest live-pad-previews-share-the-rendered-phase-reference-during-instance-pause
  (doseq [[reference result] [[:launch [0.0 0.0 0.0]] [:transport [0.5 0.0 0.0]]]]
    (let [model (-> (rig)
                    (assoc-in [:registry :effects :ember :outputs :dimmer]
                              (fx/saw {:period {:beats 2} :phase-reference reference}))
                    (bench/command :launch {:op :cue/launch :cue-id :ember}))
          id (first (keys (:instances model)))
          runtime (show/create-runtime {:model model :workspace-options nil :batch? false})]
      (try
        (show/tick! runtime 0)
        (let [receipt (show/submit! runtime {:op :instance/pause :instance-id id})]
          (show/tick! runtime 250)
          (is (= :accepted (:status @receipt))))
        (show/tick! runtime 500)
        (let [snapshot @(:state runtime)
              preview (pads/present! (:cue-pad-previews runtime) :grid snapshot main)
              emitted (get-in snapshot [:output :values :head-1 :emitted-color])]
          (is (= result emitted (:ember preview))))
        (finally (show/stop! runtime))))))

(deftest cue-targets-constrain-the-representative-head
  (let [service (pads/manager)
        model (-> (rig) (assoc-in [:registry :cues :ember :selector] {:fixture-ids [:head-4]}))
        see! (fn [m] (pads/present! service :grid (at m 75) main))]
    (is (contains? (see! model) :ember))
    (is (= :head-4 (get-in @(:cache service) [:ember :plan :rig 0 :id])))
    (is (empty? (see! (assoc-in model [:registry :cues :ember :pad-preview :fixture-id] :head-1))))))

(deftest representative-fixture-retains-the-venue-brightness-reference
  (let [dimmer {:id :weak :universe 1 :address 1 :channels {:dimmer 1} :measured-relative-brightness 0.1}
        effect {:id :ember :version 1 :scene-owner :scene :brightness-reference :global
                :params {matching/target-param {:type :number :default 0.1 :bounds [0 1]}}
                :outputs {:dimmer 1.0}}
        model (-> (rig) (assoc-in [:registry :effects :ember] effect)
                  (assoc-in [:registry :cues :ember :swatch] "#ffffff")
                  (assoc-in [:rig :fixtures] [dimmer (assoc dimmer :id :bright :address 2 :measured-relative-brightness 1.0)]))
        colors (pads/present! (pads/manager) :page (at model 50) main)]
    (is (= [1.0 1.0 1.0] (:ember colors)))))

(deftest running-previews-use-the-same-resolved-hdr-options-as-emission
  (let [hdr {:enabled? true :gain 0.5 :source-gains {:head-1 0.25}}
        model (-> (rig)
                  (assoc-in [:registry :effects :ember :outputs :dimmer] 1.0)
                  (assoc-in [:settings :hdr] hdr)
                  (bench/command :launch {:op :cue/launch :cue-id :ember}))
        runtime (show/create-runtime {:model model :workspace-options nil :batch? false})]
    (try
      (show/tick! runtime 0)
      (let [snapshot @(:state runtime)
            service (:cue-pad-previews runtime)
            color (:ember (pads/present! service :grid snapshot main))
            emitted (get-in snapshot [:output :values :head-1 :emitted-color])
            initial-window (get-in @(:cache service) [:ember :window])]
        (is (= [0.125 0.0 0.0] emitted color))
        (pads/present! service :grid (assoc-in snapshot [:preview-hdr :gain] 0.75) main)
        (is (identical? initial-window (get-in @(:cache service) [:ember :window]))
            "Continuous modulation keeps the current visualization keyframe window"))
      (finally (show/stop! runtime)))))

;; Bounded sampling on a paged show (#533). Every count below is of real
;; compiled effect steps run by the pad sampler; frame times are data.

(defn- counting-steps
  "Wrap compile-effect so each prediction a plan runs counts against its cue."
  [counts]
  (let [compile-effect compiler/compile-effect]
    (fn [& args]
      (let [step (apply compile-effect args)]
        (fn [context]
          (swap! counts update (get-in context [:instance :cue-id]) (fnil inc 0))
          (step context))))))

(defn- frame-time
  "25 ms frames with a deterministic -3..+3 ms jitter, as a scheduled show commits them."
  [i]
  (+ (* 25 i) (- (mod (* 13 i) 7) 3)))

(defn- paged-at [model i]
  (let [t (frame-time i)]
    {:model (-> model (assoc :time-ms t :last-frame-time-ms t :frame-number i)
                (assoc-in [:transport :beat] (/ t 500.0)))}))

(defn- page-cues [page] (set (map #(paged/cue-id page %) (range 64))))

(deftest a-paged-show-samples-only-shown-pages-within-a-frame-budget
  (let [model (paged/paged-model {:terms 2})
        service (pads/manager)
        counts (atom {})
        frame! (fn [i studio-page]
                 (reset! counts {})
                 (let [snapshot (paged-at model i)
                       studio (pads/observe! service :studio snapshot (paged/studio-query studio-page))
                       push (pads/present! service :push snapshot (paged/studio-query 1))]
                   {:evaluated @counts :colors (merge (:colors studio) push)
                    :spending (:frame (pads/diagnostics service))}))
        budget pads/default-samples-per-frame]
    (with-redefs [compiler/compile-effect (counting-steps counts)]
      (let [frames (mapv #(frame! % 0) (range 60))
            shown (into (page-cues 0) (page-cues 1))
            total #(reduce + 0 (vals (:evaluated %)))]
        (is (= 128 (count shown)))
        (is (every? #(<= (total %) (inc budget)) frames)
            "both readers of a frame share one prediction budget (a fresh cue costs two)")
        (is (<= budget (apply max (map total frames))) "opening two pages fills the budget")
        (is (= shown (into #{} (mapcat (comp keys :evaluated)) frames)) "only the two shown pages are sampled")
        (is (= shown (set (keys @(:cache service)))) "hidden pages have no plan and no keyframes")
        (is (= shown (set (keys (:colors (peek frames))))) "every shown cue has a colour once the budget catches up")
        (testing "in steady state each cue predicts once per keyframe window and interpolates between"
          (let [steady (subvec frames 20)
                per-cue (apply merge-with + (map :evaluated steady))]
            ;; 40 frames span 1,000 ms: eight 125 ms windows. Before #533 a
            ;; window that expired between frames cost two predictions.
            (is (= shown (set (keys per-cue))))
            (is (every? #(<= 8 % 9) (vals per-cue)) (pr-str (frequencies (vals per-cue))))
            (is (every? #(<= (total %) budget) steady))
            (is (every? #(zero? (get-in % [:spending :dropped])) steady))
            (is (some (fn [[a b]]
                        (some #(and (not (contains? (:evaluated b) %))
                                    (not= (get-in a [:colors %]) (get-in b [:colors %])))
                              shown))
                      (partition 2 1 steady))
                "a colour moves on a frame that runs no prediction for it")))
        (testing "moving the Studio grid to another page stops sampling the old one at once"
          (let [moved (mapv #(frame! % 2) (range 60 80))
                sampled (into #{} (mapcat (comp keys :evaluated)) moved)]
            (is (empty? (filter (page-cues 0) sampled)))
            (is (= (into (page-cues 1) (page-cues 2)) sampled))
            (is (empty? (filter (page-cues 0) (keys @(:cache service)))))))
        (pads/release! service :studio)
        (pads/release! service :push)
        (is (empty? @(:cache service)))
        (is (= #{} (:demand (pads/diagnostics service))))))))

(deftest chained-keyframes-cost-one-prediction-and-start-where-the-last-ended
  (let [service (pads/manager) model (paged/paged-model {:terms 2}) counts (atom {})
        cue (paged/cue-id 0 0) one [pads/query-id (paged/page-id 0) [0 0] 1]]
    (with-redefs [compiler/compile-effect (counting-steps counts)]
      (let [windows (vec (for [i (range 40)]
                           (do (pads/present! service :grid (paged-at model i) one)
                               (get-in @(:cache service) [cue :window]))))
            distinct-windows (vec (dedupe windows))]
        (is (= 8 (count distinct-windows)) "forty 25 ms frames span eight 125 ms windows")
        (is (every? #(and (:from %) (:to %)) distinct-windows))
        (is (= (+ 2 (dec (count distinct-windows))) (get @counts cue))
            "two predictions open the first window, then one per window")
        (doseq [[a b] (partition 2 1 distinct-windows)]
          (is (= (:until a) (:at b)))
          (is (identical? (:to a) (:from b))))
        (testing "a chained keyframe is the colour a fresh prediction gives at that time"
          (let [w (peek distinct-windows)
                t (:at w)
                fresh (pads/present! (pads/manager) :x {:model (-> model (assoc :time-ms t :last-frame-time-ms t)
                                                                   (assoc-in [:transport :beat] (/ t 500.0)))} one)]
            (is (< (apply max (map #(abs (- %1 %2)) (:from w) (get fresh cue))) 1e-9))))))))

(deftest an-identity-change-drops-colours-while-timing-changes-hold-them
  (let [service (pads/manager {:samples-per-frame 4})
        model (paged/paged-model {:terms 1})
        row [pads/query-id (paged/page-id 0) [0 0] 1]
        see! (fn ([m t] (pads/present! service :grid {:model (-> m (assoc :time-ms t :last-frame-time-ms t)
                                                                    (assoc-in [:transport :beat] (/ t 500.0)))} row))
                 ([m t extra] (pads/present! service :grid (merge {:model (-> m (assoc :time-ms t :last-frame-time-ms t)
                                                                               (assoc-in [:transport :beat] (/ t 500.0)))}
                                                                 extra) row)))]
    (is (= 2 (count (see! model 0))) "eight fresh cues need sixteen predictions; four fit")
    (is (= {:spent 4 :deferred 0 :dropped 6} (:frame (pads/diagnostics service))))
    (doseq [t [25 50 75]] (see! model t))
    (is (= 8 (count (see! model 100))))
    (testing "keyframes that ran out wait their turn, oldest first, holding their colour"
      (let [before @(:cache service)
            colors (see! model 210)
            refreshed (filter #(not (identical? (get before %) (get @(:cache service) %))) (keys before))]
        (is (= {:spent 4 :deferred 4 :dropped 0} (:frame (pads/diagnostics service))))
        (is (= 4 (count refreshed)))
        (is (= 8 (count colors)))
        (doseq [id (remove (set refreshed) (keys before))]
          (is (= (get-in before [id :window :to]) (get colors id))))))
    (testing "a tempo change is timing: every pad keeps a colour"
      (let [colors (see! (assoc-in model [:transport :bpm] 90) 235)]
        (is (= 8 (count colors)))
        (is (zero? (get-in (pads/diagnostics service) [:frame :dropped])))))
    (testing "an identity change (here the HDR mode) leaves pads over budget with no colour, not an old one"
      (let [colors (see! model 260 {:preview-hdr {:white-mix 0}})]
        (is (= 2 (count colors)))
        (is (= 6 (get-in (pads/diagnostics service) [:frame :dropped])))))))

(defn- run-readers
  "Drive readers [[lease page] ...] over frames of 25 ms with bpm-fn and
  playing-fn per frame; the beat advances only while playing. Returns, per
  frame, how many of each reader's 64 pads had no colour, the colours, and the
  frame's spending. After the reviewer's simulation on #891."
  [service model {:keys [frames bpm-fn playing-fn readers]}]
  (loop [i 0 beat 0.0 out []]
    (if (= i frames)
      out
      (let [t (* 25 i) bpm (bpm-fn i) playing? (playing-fn i)
            snapshot {:model (-> model (assoc :time-ms t :last-frame-time-ms t :frame-number i)
                                 (update :transport assoc :beat beat :bpm bpm :playing? playing?))}
            colors (mapv (fn [[lease page]] (pads/present! service lease snapshot (paged/studio-query page))) readers)]
        (recur (inc i) (if playing? (+ beat (* 25 (/ bpm 60000.0))) beat)
               (conj out {:i i :missing (mapv #(- 64 (count %)) colors) :colors colors
                          :frame (:frame (pads/diagnostics service)) :level (:level (pads/status service))}))))))

(defn- misses [rows] (reduce + (mapcat :missing rows)))

(deftest tempo-jitter-and-ramps-never-drop-a-pad-to-its-swatch
  (let [model (paged/paged-model {:terms 1})]
    (doseq [[label bpm-fn] [["±0.01 BPM jitter" #(+ 120.0 (* 0.01 (Math/sin %)))]
                            ["a ramp of 0.002 BPM per frame" #(+ 120.0 (* 0.002 %))]
                            ["a ramp from 70 to 180 BPM" #(+ 70.0 (* 110.0 (/ % 400.0)))]]]
      (testing label
        (let [rows (run-readers (pads/manager) model {:frames 400 :bpm-fn bpm-fn :playing-fn (constantly true)
                                                      :readers [[:studio 0]]})
              steady (subvec rows 20)]
          (is (zero? (misses steady)) (pr-str (take 5 (filter #(pos? (first (:missing %))) steady))))
          (is (zero? (reduce + (map (comp :dropped :frame) steady)))))))))

(deftest stopping-pauses-pads-at-their-colour-and-playing-resumes-without-catch-up
  (let [model (paged/paged-model {:terms 1})]
    (let [service (pads/manager)
          rows (run-readers service model {:frames 320 :bpm-fn (constantly 120.0)
                                           :playing-fn #(not (<= 200 % 259)) :readers [[:studio 0]]})
          stopped (subvec rows 200 260)]
      (do
        (is (zero? (misses (subvec rows 20))) "no pad drops to its swatch on stop or start")
        (is (apply = (map :colors stopped)) "pads hold one colour while stopped")
        (is (zero? (reduce + (map (comp :spent :frame) stopped))) "a stopped transport costs no predictions")
        (testing "the stopped colour is the colour shown when the transport stopped"
          (let [fresh (pads/present! (pads/manager {:samples-per-frame 64}) :x {:model (-> model (assoc :time-ms 5000 :last-frame-time-ms 5000)
                                                                     (update :transport assoc :beat (* 199 25 (/ 120.0 60000.0))
                                                                             :playing? false))}
                                     (paged/studio-query 0))]
            ;; The frozen colour is the interpolated one; a fresh prediction at
            ;; that beat lies within the keyframe interpolation error.
            (is (= 64 (count fresh)) "a stopped page costs one prediction per cue")
            (is (every? (fn [[id c]] (< (apply max (map #(abs (- %1 %2)) c (get fresh id))) 0.05))
                        (first (:colors (first stopped)))))))
        (testing "playing again resumes from the stopped beat and refreshes within the budget"
          (is (every? #(<= (:spent (:frame %)) pads/default-samples-per-frame) (subvec rows 260))))))))

(deftest two-readers-at-180-bpm-both-keep-every-pad
  (let [service (pads/manager)
        rows (run-readers service (paged/paged-model {:terms 1})
                          {:frames 400 :bpm-fn (constantly 180.0) :playing-fn (constantly true)
                           :readers [[:studio 0] [:push 1]]})
        steady (subvec rows 200)]
    (is (= [0 0] (apply mapv + (map :missing steady))) "neither the first nor the second reader misses pads")
    ;; Cues admitted together expire together; now and then two such groups
    ;; meet the same frame and one holds its colour for that frame.
    (is (<= (count (filter (comp pos? :deferred :frame) steady)) 2))
    (is (= 1 (:level (pads/status service))) "sustained deferrals slowed keyframes one level")
    (testing "at 120 BPM the same pages fit the budget, so keyframes stay at the base rate"
      (let [service (pads/manager)
            rows (run-readers service (paged/paged-model {:terms 1})
                              {:frames 400 :bpm-fn (constantly 120.0) :playing-fn (constantly true)
                               :readers [[:studio 0] [:push 1]]})]
        (is (zero? (misses (subvec rows 20))))
        (is (zero? (:level (pads/status service))))))))

(defn- playing-snapshot [model i beat bpm]
  (let [t (* 25 i)]
    {:model (-> model (assoc :time-ms t :last-frame-time-ms t :frame-number i)
                (update :transport assoc :beat beat :bpm bpm :playing? true))}))

(deftest a-reader-on-the-previous-frame-shares-its-budget-and-never-moves-keyframes-back
  ;; Studio reads frame i while the Push still reads frame i-1, as when the
  ;; Push takes (snapshot!) on its own schedule. After the delta review of #891.
  (let [model (paged/paged-model {:terms 1})]
    (doseq [bpm [120.0 180.0]]
      (testing (str bpm " BPM")
        (let [service (pads/manager) frames 400
              beats (vec (reductions + 0.0 (repeat frames (* 25 (/ bpm 60000.0)))))
              predictions #(:predictions (:totals (pads/diagnostics service)))
              starts #(into {} (keep (fn [[id e]] (some->> (:window e) :at (vector id)))) @(:cache service))
              backwards (fn [before after] (count (filter (fn [[id a]] (< a (get before id a))) after)))
              rows (loop [i 1 out []]
                     (if (= i frames)
                       out
                       (let [before (predictions) start (starts)
                             studio (pads/present! service :studio (playing-snapshot model i (beats i) bpm) (paged/studio-query 0))
                             after-studio (starts)
                             push (pads/present! service :push (playing-snapshot model (dec i) (beats (dec i)) bpm)
                                                 (paged/studio-query 1))]
                         (recur (inc i)
                                (conj out {:spent (- (predictions) before)
                                           :missing (- 128 (count studio) (count push))
                                           :backwards (+ (backwards start after-studio) (backwards after-studio (starts)))})))))
              steady (subvec rows 200)]
          (is (every? #(<= (:spent %) pads/default-samples-per-frame) rows)
              "both readers spend at most one budget per committed frame")
          (is (zero? (reduce + (map :backwards rows))) "no keyframe window moves back to the older frame")
          (is (zero? (reduce + (map :missing steady))) "the lagging reader keeps every pad")
          (is (= (if (= bpm 180.0) 1 0) (:level (pads/status service)))
              "the governor sees the same pressure as readers in step"))))))

(deftest the-pad-level-relaxes-when-the-demand-fits-again
  ;; Two full pages need 29.9 predictions per frame at 140 BPM (fits 32) and
  ;; 38.4 at 180 BPM (does not). Before, relief needed 0.9 x 32 = 28.8, so the
  ;; level stayed raised from 140 to 155 BPM.
  (let [bpm-at #(cond (< % 600) 140.0 (< % 1200) 180.0 :else 140.0)
        rows (run-readers (pads/manager) (paged/paged-model {:terms 1})
                          {:frames 1800 :bpm-fn bpm-at :playing-fn (constantly true)
                           :readers [[:studio 0] [:push 1]]})
        levels (mapv :level rows)]
    (is (zero? (misses (subvec rows 20))) "no pad drops to its swatch through either tempo change")
    (is (every? zero? (subvec levels 200 600)) "140 BPM settles at the base rate")
    (is (every? #(= 1 %) (subvec levels 800 1200)) "180 BPM settles one level slower")
    (is (every? zero? (subvec levels 1300)) "back at 140 BPM the level relaxes and stays relaxed")
    ;; The calm frames at 180 BPM count, so it can relax on the first 140 BPM frame.
    (is (= 1 (count (filter (fn [[a b]] (not= a b)) (partition 2 1 (subvec levels 1199)))))
        "it relaxes once, without oscillating")))

(deftest pausing-or-releasing-many-cues-at-once-holds-their-colours
  ;; Pause and release are timing, like tempo: the paused colour is frozen
  ;; where it was and a release fades from the colour shown. 32 changed cues
  ;; need 64 predictions, twice the budget, so half must hold for a frame.
  (let [cues (map #(paged/cue-id 0 %) (range 32))
        base (reduce #(assoc-in %1 [:registry :effects %2 :envelope] {:release {:ms 4000}})
                     (paged/paged-model {:terms 1}) cues)
        running (reduce (fn [m cue] (bench/command m cue {:op :cue/launch :cue-id cue})) base cues)
        ids (keys (:instances running))
        at (fn [model i] (playing-snapshot model i (* i 25 (/ 120.0 60000.0)) 120.0))
        timing #(select-keys % [:phase :release-at-ms :system])]
    (is (= 32 (count ids)))
    (doseq [[label op] [["paused" #(hash-map :op :instance/pause :instance-id %)]
                        ["released" #(hash-map :op :instance/end :instance-id %)]]]
      (testing (str "32 visible cues " label " in one frame")
        (let [service (pads/manager)
              changed (reduce (fn [m id] (bench/command m (str label id) (assoc (op id) :time-ms 2000))) running ids)
              before (mapv #(pads/present! service :grid (at running %) (paged/studio-query 0)) (range 80))
              first-changed (pads/present! service :grid (at changed 80) (paged/studio-query 0))
              spending (:frame (pads/diagnostics service))
              after (mapv #(pads/present! service :grid (at changed %) (paged/studio-query 0)) (range 81 100))]
          (is (every? #(not= (timing (get-in running [:instances %])) (timing (get-in changed [:instances %]))) ids)
              "every instance's pause or release state changed")
          (is (= 64 (count (peek before))))
          (is (= 64 (count first-changed)) "no pad falls back to its swatch on the frame they change")
          (is (every? #(= 64 (count %)) after) "or after it")
          (is (zero? (:dropped spending)))
          (is (pos? (:deferred spending)) "the cues over budget held their colour")
          (is (every? (fn [id] (some? (get-in @(:cache service) [id :window :from]))) cues)
              "and every changed cue has been refreshed since"))))))

(deftest stop-and-resume-hold-millisecond-timed-pads-without-catch-up
  ;; A millisecond saw: the instance clock offset grows under clock/hold
  ;; while the transport is stopped, so no millisecond progress is lost.
  (let [model (-> (rig)
                  (assoc-in [:registry :effects :ember :outputs :dimmer] (fx/saw {:period {:ms 2000}}))
                  (bench/command :launch {:op :cue/launch :cue-id :ember}))
        runtime (show/create-runtime {:model model :workspace-options nil :batch? false})
        service (:cue-pad-previews runtime)
        frame! (fn [t & [command]]
                 (let [reply (when command (show/submit! runtime :command command))]
                   (show/tick! runtime t)
                   (when reply (is (= :accepted (:status (deref reply 1000 nil)))))
                   (let [snapshot @(:state runtime)]
                     {:t t :pad (:ember (pads/present! service :grid snapshot main))
                      :emitted (get-in snapshot [:output :values :head-1 :emitted-color])
                      :offset (get-in (val (first (get-in snapshot [:model :instances]))) [:system :clock-offset-ms])})))
        close? (fn [a b tolerance] (and a b (every? #(< (abs %) tolerance) (map - a b))))]
    (try
      (let [running (mapv frame! (range 0 1000 25))
            stop (frame! 1000 {:op :transport/set :transport {:playing? false}})
            stopped (mapv frame! (range 1025 2000 25))
            play (frame! 2000 {:op :transport/set :transport {:playing? true}})
            resumed (mapv frame! (range 2025 2500 25))
            at-stop (:pad (peek running))]
        (is (close? at-stop (:emitted (peek running)) 1e-9) "a linear saw interpolates exactly")
        (is (pos? (first at-stop)) "the saw is lit when the transport stops")
        (testing "while stopped the pad shows the colour it had at the stop"
          (is (every? #(= (:pad stop) (:pad %)) stopped))
          (is (close? (:pad stop) at-stop 0.02) "within one frame of the saw")
          (is (every? #(close? (:emitted %) (:emitted stop) 1e-9) stopped) "the light holds too")
          (is (< 900 (:offset (peek stopped))) "the instance clock offset grew under clock/hold"))
        (testing "after play the pad resumes from that colour, not where the saw would have run on to"
          ;; 1,000 ms unheld would have moved the saw by half a cycle.
          (is (close? (:pad play) (:pad stop) 0.03))
          (is (close? (:pad (first resumed)) (:pad stop) 0.04))
          (is (every? (fn [{:keys [pad emitted]}] (close? pad emitted 0.03)) resumed)
              "and keeps following the light it previews")))
      (finally (show/stop! runtime)))))

(deftest lighting-output-never-waits-for-the-pad-level
  (let [model (bench/command (paged/paged-model {:terms 1}) :launch {:op :cue/launch :cue-id (paged/cue-id 0 0)})
        model (assoc-in model [:transport :bpm] 180.0)
        start #(show/create-runtime {:model model :workspace-options nil :batch? false})
        observed (start) alone (start)
        service (:cue-pad-previews observed)
        leases [:a :b :c :d]]
    (try
      (let [frames (vec (for [i (range 120)]
                          (let [t (* 25 i)]
                            (show/tick! observed t)
                            (show/tick! alone t)
                            (let [snapshot @(:state observed)]
                              (doseq [[lease page] (map vector leases (range))]
                                (pads/present! service lease snapshot (paged/studio-query page)))
                              {:observed (:output snapshot) :alone (:output @(:state alone))
                               :frame (get-in snapshot [:model :frame-number])
                               :alone-frame (get-in @(:state alone) [:model :frame-number])}))))]
        (is (<= 2 (:level (pads/status service))) "four full pages at 180 BPM raise the pad level")
        (is (every? #(= (:frame %) (:alone-frame %)) frames) "every frame commits")
        (is (every? #(= (get-in % [:observed :values]) (get-in % [:alone :values])) frames)
            "lighting output is identical with or without pad readers"))
      (testing "the pad level never reaches the live scheduler's own level or its defer gate"
        ;; A manager no show ticks, so wall-clock frame cost plays no part.
        (let [pads-only (pads/manager)]
          (run-readers pads-only model {:frames 120 :bpm-fn (constantly 180.0) :playing-fn (constantly true)
                                        :readers (map vector leases (range))})
          (is (<= 2 (:level (pads/status pads-only))))
          (is (zero? (:level @(:governor pads-only))) "the owner's frame-cost level stays where frame cost put it")
          (is (false? (pads/govern! pads-only 5 25)) "a cheap frame is never deferred, whatever the pad level")
          (doseq [lease leases] (pads/release! pads-only lease))))
      (finally
        (doseq [lease leases] (pads/release! service lease))
        (show/stop! observed) (show/stop! alone)))))

(deftest pad-prediction-reads-the-frame-sample-and-never-samples-stateful-sources
  (let [model (paged/paged-model {:terms 2})
        runtime (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 10000})
        service (:cue-pad-previews runtime)
        calls (atom {})
        count! (fn [k f] (fn [& args] (swap! calls update k (fnil inc 0)) (apply f args)))
        compile-response response/compile-response]
    (try
      (show/tick! runtime 0)
      (let [launched (show/submit! runtime {:op :cue/launch :cue-id (paged/cue-id 0 0)})]
        (show/tick! runtime 25)
        (is (= :accepted (:status (deref launched 1000 nil)))))
      (with-redefs [response/compile-response (fn [spec]
                                               (swap! calls update :response-compiles (fnil inc 0))
                                               (compile-response spec))
                    sources/sample (count! :source-samples sources/sample)
                    modulation/advance (count! :controller-advances modulation/advance)]
        (doseq [i (range 2 14)]
          (show/tick! runtime (* 25 i))
          (reset! calls {})
          (let [snapshot @(:state runtime)
                instance (val (first (get-in snapshot [:model :instances])))]
            ;; The Studio stream reads the instance first, then both pad readers.
            (waveforms/effective-params snapshot instance)
            (pads/observe! service :studio snapshot (paged/studio-query 0))
            (pads/present! service :push snapshot (paged/studio-query 1))
            (is (<= (get @calls :response-compiles 0) 1) "the response chain resolves at most once for the frame")
            (is (nil? (:source-samples @calls)) "no waveform source is sampled again")
            (is (nil? (:controller-advances @calls)) "no controller advances"))))
      (finally
        (pads/release! service :studio) (pads/release! service :push)
        (show/stop! runtime)))))

(deftest pad-faults-say-what-failed-instead-of-showing-a-colour
  (let [service (pads/manager) model (rig) broken? (atom true) counts (atom {})
        compile-effect compiler/compile-effect
        observe (fn [m t] (pads/observe! service :grid (at m t) main))]
    (testing "a plan that does not compile"
      (let [result (observe (assoc-in model [:registry :effects :ember :outputs :dimmer] {:op :bad}) 0)]
        (is (nil? (get-in result [:colors :ember])))
        (is (= {:stage :plan :message "Invalid effect expression"} (get-in result [:faults :ember])))))
    (testing "a prediction that throws retries once per keyframe interval, then recovers"
      (with-redefs [compiler/compile-effect
                    (fn [& args]
                      (let [step (apply compile-effect args)]
                        (fn [context]
                          (swap! counts update :ember (fnil inc 0))
                          (if @broken? (throw (ex-info "sampler exploded" {})) (step context)))))]
        (let [m (assoc-in model [:registry :effects :ember :outputs :color] (demo/constant [0 1 0]))
              results (mapv #(observe m %) [0 25 50 75 100])]
          (is (every? #(= {:stage :sample :message "sampler exploded"} (get-in % [:faults :ember])) results))
          (is (every? #(nil? (get-in % [:colors :ember])) results))
          (is (= 1 (:ember @counts)) "a faulted cue does not retry every frame")
          (is (= {:stage :sample :message "sampler exploded"} (get-in (pads/diagnostics service) [:faults :ember])))
          (reset! broken? false)
          (let [recovered (observe m 130)]
            (is (some? (get-in recovered [:colors :ember])))
            (is (nil? (:faults recovered)))))))
    (testing "a retry that misses the budget keeps its fault and waits behind healthy cues"
      (let [tight (pads/manager {:samples-per-frame 1})
            ember-only [pads/query-id :main [0 0] 1]
            both [pads/query-id :main [0 0] 4]]
        (reset! broken? true)
        (with-redefs [compiler/compile-effect
                      (fn [& args]
                        (let [step (apply compile-effect args)]
                          (fn [context]
                            (if (and @broken? (= :ember (get-in context [:instance :cue-id])))
                              (throw (ex-info "sampler exploded" {}))
                              (step context)))))]
          (is (= :sample (get-in (pads/observe! tight :grid (at model 0) ember-only) [:faults :ember :stage])))
          (let [result (pads/observe! tight :grid (at model 130) both)]
            (is (= {:spent 2 :deferred 1 :dropped 0} (:frame (pads/diagnostics tight)))
                "the newly shown cue goes first; the due retry waits")
            (is (some? (get-in result [:colors :merlot])))
            (is (= :sample (get-in result [:faults :ember :stage])) "the notice does not flicker off"))
          (testing "repeated failures back off"
            (let [counted (atom 0) service (pads/manager)]
              (with-redefs [compiler/compile-effect
                            (fn [& args]
                              (let [step (apply compile-effect args)]
                                (fn [context] (swap! counted inc) (throw (ex-info "always" {})) (step context))))]
                (doseq [t (range 0 1000 25)] (pads/observe! service :grid (at model t) ember-only))
                ;; Retries at 0, 125, 375 and 875 ms: 125, 250 and 500 ms apart.
                (is (= 4 @counted))))))))
    (testing "an instance whose parameters cannot resolve"
      (let [running (bench/command model :launch {:op :cue/launch :cue-id :ember})]
        (with-redefs [waveforms/effective-params (fn [_ _] (throw (IllegalStateException. "response chain failed")))]
          (is (= {:stage :parameters :message "response chain failed"}
                 (get-in (pads/observe! (pads/manager) :grid (at running 0) main) [:faults :ember]))))))
    (testing "a refused lease"
      (let [crowded (pads/manager)]
        (doseq [lease (range 64)] (pads/present! crowded lease (at model 0) main))
        (is (= {:colors {} :fault {:stage :lease :message "Visible cue preview limit reached"}}
               (pads/observe! crowded :one-too-many (at model 0) main)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"limit reached"
                              (pads/present! crowded :one-too-many (at model 0) main)))))))

(deftest studio-reads-colours-and-faults-through-its-query-surface
  (let [service (pads/manager)
        model (assoc-in (rig) [:registry :effects :merlot :outputs :dimmer] {:op :bad})
        source (at model 0)
        store (queries/create (constantly nil) (fn []) {pads/query-id (pads/studio-surface service)})
        query [pads/query-id :main [0 0] 4]
        lease (get (queries/reconcile! store source #{query}) query)
        {:keys [value]} (queries/read-result lease source)]
    (is (= [0.0 0.0 0.0] (get-in value [:colors :ember])))
    (is (= {:merlot {:stage :plan :message "Invalid effect expression"}} (:faults value)))
    (queries/reconcile! store source #{})
    (is (zero? (:clients (pads/status service))))
    (is (empty? @(:cache service)))))
