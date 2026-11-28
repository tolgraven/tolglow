(ns tolglow.video-editing-test
  "Studio video editing (#672, #674): the rectangle and numeric placement
  geometry Studio and the REPL share, explicit-target strips for fixture
  groups, and undo/redo of video settings edits through the show queue,
  checked in captured DMX for the labelled grid and across save/reload."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit]
            [tolglow.video.edit-history :as history]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.mapping :as mapping]
            [tolglow.video.source :as source])
  (:import [java.io ByteArrayInputStream]))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(defn- points-close? [a b] (and (= (count a) (count b)) (every? true? (map #(every? true? (map close? %1 %2)) a b))))

;; Shared geometry -------------------------------------------------------------

(deftest a-rectangle-becomes-a-strip-along-its-longer-side-in-the-drag-direction
  (testing "wider than tall on a 16:9 frame: a horizontal strip, height from the rectangle"
    (let [{:keys [points height]} (mapping/rect-strip [0.1 0.2] [0.7 0.3] 16/9)]
      (is (points-close? [[0.1 0.25] [0.7 0.25]] points))
      (is (close? 0.1 height))))
  (testing "dragged right to left, the first pixel is on the right"
    (is (points-close? [[0.7 0.25] [0.1 0.25]] (:points (mapping/rect-strip [0.7 0.2] [0.1 0.3] 16/9)))))
  (testing "taller than wide in pixels: a vertical strip whose height is the width in frame heights"
    (let [{:keys [points height]} (mapping/rect-strip [0.4 0.9] [0.5 0.1] 16/9)]
      (is (points-close? [[0.45 0.9] [0.45 0.1]] points) "bottom-to-top drag starts at the bottom")
      (is (close? (* 0.1 16/9) height))))
  (testing "corners outside the frame are clamped"
    (is (points-close? [[0.0 0.5] [1.0 0.5]] (:points (mapping/rect-strip [-0.2 0.4] [1.3 0.6] 1.0))))))

(deftest numeric-placement-round-trips-and-moves-a-strip-predictably
  (let [aspect 16/9
        strip {:id :a :layout :tube-a :points [[0.1 0.5] [0.9 0.5]] :height 0.04}
        placement (mapping/strip-placement strip aspect)]
    (is (points-close? [[0.5 0.5]] [(:center placement)]))
    (is (close? 0.8 (:length placement)))
    (is (close? 0.0 (:angle placement)))
    (is (close? 0.04 (:height placement)))
    (testing "setting each value changes only that value"
      (let [moved (mapping/place-strip strip {:center [0.4 0.25]} aspect)]
        (is (points-close? [[0.0 0.25] [0.8 0.25]] (:points moved))))
      (let [turned (mapping/place-strip strip {:angle 90} aspect)
            p (mapping/strip-placement turned aspect)]
        (is (close? 90 (:angle p)))
        (is (close? 0.8 (:length p)) "length stays in frame widths")
        (is (close? 0.5 (first (:center p))))
        (is (close? 0.5 (first (first (:points turned)))) "vertical about the centre"))
      (let [shorter (mapping/place-strip strip {:length 0.4 :height 0.2} aspect)]
        (is (points-close? [[0.3 0.5] [0.7 0.5]] (:points shorter)))
        (is (close? 0.2 (:height shorter)))))
    (testing "a placement read back from a turned polyline places it again unchanged"
      (let [poly {:id :p :targets [:x] :points [[0.2 0.5] [0.5 0.6] [0.8 0.5]] :height 0.1}
            turned (mapping/place-strip poly {:angle 30 :center [0.6 0.4]} aspect)
            again (mapping/place-strip turned (mapping/strip-placement turned aspect) aspect)]
        (is (points-close? (:points turned) (:points again)))
        (is (close? 30 (:angle (mapping/strip-placement turned aspect))))))
    (testing "a zero-length strip is redrawn as a line"
      (let [dot (assoc strip :points [[0.5 0.5] [0.5 0.5]])]
        (is (points-close? [[0.25 0.5] [0.75 0.5]] (:points (mapping/place-strip dot {:length 0.5} aspect))))))))

;; History (pure) ----------------------------------------------------------------

(def base-settings
  {:sources {:grid {:kind :test-pattern}}
   :mappings {:wall {:id :wall :source :grid :strips [{:id :a :layout :tube-a :points [[0 0.125] [1 0.125]] :height 0.05}]}}})

(defn- model-with [settings] {:settings {:video settings}})

(deftest edits-record-one-unit-and-undo-redo-restore-it
  (let [m0 (model-with base-settings)
        m1 (history/apply-edit m0 {:op :put-strip :mapping-id :wall
                                   :strip {:id :a :layout :tube-a :points [[0 0.375] [1 0.375]] :height 0.05}})
        m2 (history/apply-edit m1 {:op :put-strip :mapping-id :wall
                                   :strip {:id :g :targets [:p1 :p2] :points [[0 0.5] [1 0.5]]}})
        m3 (history/apply-edit m2 {:op :put-surface :surface {:id :s :source :grid :shape :plane :position [0 2 0] :size-m [4 2]}})]
    (is (= {:undo 3 :redo 0 :undo-label "surface s" :redo-label nil :undo-id 3 :redo-id nil :shaders {}} (history/summary m3)))
    (let [u1 (history/step m3 :undo)
          u2 (history/step u1 :undo)
          u3 (history/step u2 :undo)]
      (is (nil? (get-in u1 [:settings :video :surfaces])) "the surface edit is undone and leaves no empty map")
      (is (= (get-in m1 [:settings :video]) (get-in u2 [:settings :video])))
      (is (= base-settings (get-in u3 [:settings :video])))
      (is (= {:undo 0 :redo 3} (select-keys (history/summary u3) [:undo :redo])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Nothing to undo" (history/step u3 :undo)))
      (testing "redo replays in order, and a new edit drops the redo list"
        (let [r1 (history/step u3 :redo)
              r2 (history/step r1 :redo)]
          (is (= (get-in m1 [:settings :video]) (get-in r1 [:settings :video])))
          (is (= (get-in m2 [:settings :video]) (get-in r2 [:settings :video])))
          (let [branched (history/apply-edit r2 {:op :delete-strip :mapping-id :wall :strip-id :g})]
            (is (= 0 (:redo (history/summary branched))))
            (is (= 3 (:undo (history/summary branched))))))))
    (testing "an edit that changes nothing is not recorded"
      (is (= 0 (:undo (history/summary (history/apply-edit m0 {:op :put-source :id :grid :source {:kind :test-pattern}}))))))
    (testing "the history keeps the newest entries"
      (let [m (reduce (fn [m i] (history/apply-edit m {:op :put-strip :mapping-id :wall
                                                       :strip {:id :a :layout :tube-a :points [[0 (/ i 100.0)] [1 0.5]]}}))
                      m0 (range 1 41))]
        (is (= history/max-entries (:undo (history/summary m))))))
    (testing "a restore the settings refuse throws; nothing is returned to replace the model"
      (let [broken (assoc-in m1 [:extensions history/state-key :undo 0 :before :value 0 :points] [[0 0]])]
        (is (thrown? clojure.lang.ExceptionInfo (history/step broken :undo)))))))

(defn- code-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))

(def surf {:id :s :source :grid :shape :plane :position [0 2 0] :size-m [4 2]})

(deftest undo-names-the-step-it-saw-and-never-reverts-another-writer
  (let [m1 (history/apply-edit (model-with base-settings)
                               {:op :put-strip :mapping-id :wall :strip {:id :a :layout :tube-a :points [[0 0.5] [1 0.5]] :height 0.05}})
        seen (:undo-id (history/summary m1))]
    (testing "Studio showed Undo strip a; the REPL then added a surface: the undo is refused, not applied to the surface"
      (let [m2 (history/apply-edit m1 {:op :put-surface :surface surf})]
        (is (= :video/history-moved (code-of #(history/step m2 :undo seen))))
        (is (re-find #"surface s" (try (history/step m2 :undo seen) (catch clojure.lang.ExceptionInfo e (ex-message e))))
            "the refusal names the step that is now newest")
        (is (some? (get-in (history/step m2 :undo (:undo-id (history/summary m2))) [:settings :video :mappings :wall]))
            "naming the newest step undoes it")))
    (testing "a unit changed outside the history is never overwritten by undo or redo"
      (let [m (history/apply-edit (model-with base-settings) {:op :put-surface :surface surf})
            outside (assoc-in m [:settings :video :surfaces :s :position] [5 5 5])]
        (is (= :video/history-conflict (code-of #(history/step outside :undo))))
        (let [u (history/step m :undo)
              outside (assoc-in u [:settings :video :surfaces] {:s (assoc surf :position [5 5 5])})]
          (is (= :video/history-conflict (code-of #(history/step outside :redo)))))))
    (testing "an empty list refuses with its code"
      (is (= :video/nothing-to-redo (code-of #(history/step m1 :redo)))))))

(deftest coalescing-never-reaches-behind-an-undo-and-a-turn-back-leaves-no-step
  (let [put #(history/apply-edit %1 {:op :put-strip :mapping-id :wall :coalesce %3
                                     :strip {:id :a :layout :tube-a :points [[0 %2] [1 %2]] :height 0.05}})
        back (-> (model-with base-settings) (put 0.3 "t") (put 0.125 "t"))]
    (is (= 0 (:undo (history/summary back))) "turned back to where it began")
    (is (= base-settings (get-in back [:settings :video])))
    (let [m (-> (model-with base-settings) (put 0.3 "t1") (put 0.4 "t2"))
          c (put (history/step m :undo) 0.6 "t1")]
      (is (= 2 (:undo (history/summary c))) "a stale token after an undo starts a new step")
      (is (= 0.3 (get-in (history/step c :undo) [:settings :video :mappings :wall :strips 0 :points 0 1]))))))

(deftest a-room-patch-changes-only-the-pose
  (let [m (history/apply-edit (model-with base-settings) {:op :put-surface :surface surf})
        resized (history/apply-edit m {:op :put-surface :surface (assoc surf :size-m [8 4])})
        patched (history/apply-edit resized {:op :patch-surface :id :s :changes {:position [1 2 0] :rotation-deg [0 30 0]}})]
    (is (= (assoc surf :size-m [8 4] :position [1 2 0] :rotation-deg [0 30 0]) (get-in patched [:settings :video :surfaces :s]))
        "a size another writer set while the drag ran survives")
    (is (= "surface s" (:undo-label (history/summary patched))))
    (is (= [8 4] (get-in (history/step patched :undo) [:settings :video :surfaces :s :size-m])))
    (is (thrown? clojure.lang.ExceptionInfo (history/apply-edit m {:op :patch-surface :id :none :changes {:position [0 0 0]}})))
    (is (thrown? clojure.lang.ExceptionInfo (history/apply-edit m {:op :patch-surface :id :s :changes {:id :t}})))))

(deftest saved-histories-are-checked
  (let [m (-> (model-with base-settings) (history/apply-edit {:op :put-surface :surface surf}))
        h (history/history m)]
    (is (history/valid-saved-state? h))
    (is (history/valid-saved-state? (history/history (history/record-shader m {:effect-id :fx :op :edit :from 1} 2))))
    (is (not (history/valid-saved-state? (assoc-in h [:undo 0 :path] [:settings :rig]))))
    (is (not (history/valid-saved-state? (assoc h :undo (vec (repeat 33 (first (:undo h))))))))
    (is (not (history/valid-saved-state? (assoc-in h [:undo 0 :before] {:present? "yes"}))))))

(deftest the-edits-of-one-knob-turn-are-one-undo-step
  (let [put #(history/apply-edit %1 {:op :put-strip :mapping-id :wall :coalesce %2
                                     :strip {:id :a :layout :tube-a :points [[0 %3] [1 %3]] :height 0.05}})
        m (-> (model-with base-settings) (put "turn-1" 0.2) (put "turn-1" 0.3) (put "turn-1" 0.4) (put "turn-2" 0.5))]
    (is (= 2 (:undo (history/summary m))))
    (is (= 0.4 (get-in (history/step m :undo) [:settings :video :mappings :wall :strips 0 :points 0 1])))
    (testing "a step keeps its id while the turn goes on, so an Undo clicked mid-turn still names it"
      (let [one (put (model-with base-settings) "turn-1" 0.2)
            more (put one "turn-1" 0.3)]
        (is (= (:undo-id (history/summary one)) (:undo-id (history/summary more))))
        (is (= 0.125 (get-in (history/step more :undo (:undo-id (history/summary one)))
                             [:settings :video :mappings :wall :strips 0 :points 0 1])))))
    (is (= base-settings (get-in (-> m (history/step :undo) (history/step :undo)) [:settings :video]))
        "undoing the first turn returns to before its first edit")))

;; Through a show runtime ------------------------------------------------------

(def tubes
  [{:id :tube-a :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 8 :points [[0 0 0] [2 0 0]]}
   {:id :tube-b :shape :path :color-layout :rgb :universe 1 :address 101 :pixels 8 :points [[0 1 0] [2 1 0]]}])

(defn- start []
  (let [runtime (show/create-runtime {:model (-> (demo/model)
                                                 (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts tubes}))
                                                 (assoc-in [:settings :video]
                                                           (assoc-in base-settings [:sources :grid] {:kind :test-pattern :width 640 :height 360})))
                                      :workspace-options nil})]
    (video-hub/reconcile! (:video runtime) {:grid {:kind :test-pattern :width 640 :height 360}})
    runtime))

(defn- accepted! [reply] (is (= :accepted (:status @reply)) (pr-str @reply)) @reply)

(defn- dmx [runtime address n]
  (let [^bytes u (get-in @(:state runtime) [:output :universes 1])
        base (+ (dec address) (* 3 n))]
    (mapv #(bit-and 0xff (aget u (+ base %))) (range 3))))

(defn- expected [column row]
  (mapv (fn [b] (let [c (/ b 255.0)]
                  (* 255.0 (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4)))))
        (source/grid-color column row)))

(defn- near? [e a] (every? #(<= (Math/abs (double %)) 1.01) (map - e a)))

(defn- edit! [runtime op t]
  (let [reply (show/video-action! runtime :edit op)]
    (show/tick! runtime t)
    (accepted! reply)))

(deftest placement-targets-and-undo-reach-captured-dmx-and-survive-save
  (let [runtime (start)]
    (try
      (let [replies [(show/submit! runtime :definition {:kind :effects :definition {:id :wall :version 1 :outputs {:color {:op :video :mapping :wall}}}})
                     (show/submit! runtime :definition {:kind :cues :definition {:id :wall :effect-id :wall :priority 10}})]]
        (show/tick! runtime 0)
        (run! accepted! replies))
      (let [reply (show/submit! runtime {:op :cue/launch :cue-id :wall})]
        (show/tick! runtime 25)
        (accepted! reply))
      (show/tick! runtime 30)
      (is (every? #(near? (expected % 0) (dmx runtime 1 %)) (range 8)) "strip a samples row 0")
      (testing "a numeric placement moves the strip to row 2"
        (let [strip (get-in @(:state runtime) [:model :settings :video :mappings :wall :strips 0])]
          (edit! runtime {:op :put-strip :mapping-id :wall :strip (mapping/place-strip strip {:center [0.5 0.625]} 16/9)} 50)
          (show/tick! runtime 75)
          (is (every? #(near? (expected % 2) (dmx runtime 1 %)) (range 8)))))
      (testing "an explicit-target strip writes a fixture group in the order given"
        (edit! runtime {:op :put-strip :mapping-id :wall
                        :strip {:id :group :targets [:tube-b-head-3 :tube-b-head-1 :tube-b-head-2]
                                :points [[0.0625 0.875] [0.8125 0.875]] :height 0.0 :filter :nearest}} 100)
        (show/tick! runtime 125)
        (is (near? (expected 1 3) (dmx runtime 101 2)) "head 3 samples the first footprint")
        (is (near? (expected 3 3) (dmx runtime 101 0)))
        (is (near? (expected 5 3) (dmx runtime 101 1)))
        (is (= [0 0 0] (dmx runtime 101 3)) "heads outside the group stay unlit"))
      (testing "undo removes the group, then puts the strip back on row 0; redo moves it again"
        (edit! runtime {:op :undo} 150)
        (show/tick! runtime 175)
        (is (= [0 0 0] (dmx runtime 101 2)))
        (edit! runtime {:op :undo} 200)
        (show/tick! runtime 225)
        (is (every? #(near? (expected % 0) (dmx runtime 1 %)) (range 8)))
        (edit! runtime {:op :redo} 250)
        (show/tick! runtime 275)
        (is (every? #(near? (expected % 2) (dmx runtime 1 %)) (range 8)))
        (is (= {:undo 1 :redo 1 :undo-label "strip a" :redo-label "strip group"}
               (select-keys (get-in (show/video-action! runtime :status {}) [:history]) [:undo :redo :undo-label :redo-label]))))
      (testing "with nothing left, undo is refused with its code and changes nothing"
        (edit! runtime {:op :undo} 300)
        (let [reply (show/video-action! runtime :edit {:op :undo})]
          (show/tick! runtime 325)
          (is (= :rejected (:status @reply)))
          (is (= :video/nothing-to-undo (get-in @reply [:details :code])))))
      (testing "a full save keeps the settings and their history, and undo works after reload"
        (let [saved (persistence/read-string (show/export-show-source runtime))
              thawed (persistence/thaw (demo/model) saved)]
          (is (= (get-in @(:state runtime) [:model :settings :video]) (get-in thawed [:settings :video])))
          (is (= (history/summary (:model @(:state runtime))) (history/summary thawed)))
          (is (points-close? [[0 0.625] [1 0.625]]
                             (get-in (history/step thawed :redo (get-in (history/summary thawed) [:redo-id]))
                                     [:settings :video :mappings :wall :strips 0 :points]))
              "after reload, redo moves strip a back to row 2")
          (is (thrown? clojure.lang.ExceptionInfo
                       (persistence/thaw (demo/model) (assoc-in saved [:model :extensions history/state-key :undo] [{:id "x"}])))
              "a malformed saved history is refused on load")))
      (testing "the generic settings message cannot write video settings around the history"
        (let [reply (show/submit! runtime :settings {:scope :video :values {:surfaces {}}})]
          (show/tick! runtime 400)
          (is (= :rejected (:status @reply)))
          (is (= :video/use-video-mapping (get-in @reply [:details :code])))))
      (testing "a refused restore leaves the show's settings and history as they were"
        (let [before (:model @(:state runtime))
              _ (swap! (:state runtime) assoc-in [:model :settings :video :mappings :wall :strips 0 :height] 0.5)
              moved (:model @(:state runtime))
              reply (show/video-action! runtime :edit {:op :redo})]
          (show/tick! runtime 425)
          (is (= :rejected (:status @reply)))
          (is (= :video/history-conflict (get-in @reply [:details :code])))
          (is (= (get-in moved [:settings :video]) (get-in @(:state runtime) [:model :settings :video])))
          (is (= (history/history moved) (history/history (:model @(:state runtime)))))
          (swap! (:state runtime) assoc :model before)))
      (finally (show/stop! runtime)))))

(defn- while-ticking [runtime f]
  (let [operation (future (f))
        deadline (+ (System/nanoTime) 6000000000)]
    (try (loop [t 0]
           (cond (realized? operation) @operation
                 (> (System/nanoTime) deadline) (throw (ex-info "Owner command did not settle" {}))
                 :else (do (show/tick! runtime t) (Thread/sleep 2) (recur (+ t 5)))))
         (finally (future-cancel operation)))))

(deftest studio-undoes-a-surface-move-through-the-same-route
  (let [runtime (start)
        handler (studio/handler runtime)
        headers {"host" "localhost" "content-type" "application/transit+json" "accept" "application/transit+json"}
        post! (fn [body] (while-ticking runtime #(handler {:request-method :post :uri "/api/video-mapping" :headers headers
                                                           :body (ByteArrayInputStream. (.getBytes (transit/encode body) "UTF-8"))})))
        surface {:id :sheet :source :grid :shape :plane :position [1 0 0] :size-m [4 2] :contact {:distance-m 0.25}}]
    (try
      (show/tick! runtime 0)
      (is (= 200 (:status (post! {:op :put-surface :surface surface}))))
      (let [count-at #(get-in (show/video-action! runtime :status {}) [:surfaces :authored :sheet :count])
            touching (count-at)]
        (is (= 16 touching) "both tubes lie in the sheet")
        (is (= 200 (:status (post! {:op :put-surface :surface (assoc surface :position [1 0 1])}))) "a Room drag commits once")
        (is (= [1 0 1] (get-in @(:state runtime) [:model :settings :video :surfaces :sheet :position])))
        (is (= 0 (count-at)) "a metre in front of the tubes, nothing is in contact")
        (testing "the undo route checks the session and the step Studio showed"
          (let [{:keys [epoch]} (:model @(:state runtime))
                runtime-id (:id runtime)
                seen (get-in (show/video-action! runtime :status {}) [:history :undo-id])]
            (is (= 422 (:status (post! {:op :undo :expected-entry seen :expected-epoch (dec epoch) :runtime-id runtime-id})))
                "a previous show session is refused")
            (is (= 422 (:status (post! {:op :undo :expected-entry (inc seen) :expected-epoch epoch :runtime-id runtime-id})))
                "a step other than the newest is refused")
            (is (= [1 0 1] (get-in @(:state runtime) [:model :settings :video :surfaces :sheet :position])))
            (is (= 200 (:status (post! {:op :undo :expected-entry seen :expected-epoch epoch :runtime-id runtime-id}))))))
        (is (= [1 0 0] (get-in @(:state runtime) [:model :settings :video :surfaces :sheet :position])))
        (is (= touching (count-at))))
      (finally (show/stop! runtime)))))
