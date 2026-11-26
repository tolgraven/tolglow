(ns tolglow.studio.timeline-model-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [tolglow.studio.timeline-model :as model]))

(def window {:bars 8 :past-beats 32.0 :future-beats 32.0 :from-beat 60.0 :to-beat 140.0})

(defn- close? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest zoom-steps-stay-inside-the-server-window-range
  (is (= 4 (model/zoom 8 :in)))
  (is (= 16 (model/zoom 8 :out)))
  (is (= 1 (model/zoom 1 :in)))
  (is (= 32 (model/zoom 32 :out)))
  (is (= 4 (model/zoom 7 :in)) "an unknown size falls back to the default step"))

(deftest now-is-at-the-centre-and-beats-map-linearly-to-pixels
  (let [scale (model/scale 960 window)]
    (is (= {:width 960 :ppb 15 :centre 480} scale))
    (is (= 600 (model/x scale 60.0 100.0)))
    (testing "the layer translation puts the playhead beat under the centre line"
      (is (= 480 (+ (model/x scale 60.0 100.0) (model/offset scale 60.0 100.0))))
      (is (= 495 (+ (model/x scale 60.0 101.0) (model/offset scale 60.0 100.0))) "one beat ahead is 15 px right of now"))
    (testing "an uneven window keeps now at its share of the width"
      (is (= {:width 900 :ppb 10 :centre 300} (model/scale 900 {:past-beats 30 :future-beats 60}))))
    (is (nil? (model/scale 0 window)))
    (is (nil? (model/scale 960 nil)))))

(deftest layer-origin-is-stable-until-pixels-would-lose-precision
  (let [scale (model/scale 960 window)]
    (is (= 60.0 (model/origin nil window scale)))
    (is (= 60.0 (model/origin 60.0 (assoc window :from-beat 64.0) scale)) "scrolling does not move positioned elements")
    (is (= 60.0 (model/origin 60.0 (assoc window :from-beat 13000.0) scale)))
    (is (= 14000.0 (model/origin 60.0 (assoc window :from-beat 14000.0) scale)) "200,000 px away, rebase")))

(defn- sample [beat time-ms & {:as extra}]
  (merge {:beat beat :time-ms time-ms :playing? true :epoch 1} extra))

(deftest playhead-interpolates-between-committed-samples-and-never-runs-ahead
  (let [first-state (model/accept nil (sample 10.0 1000) 5000)]
    (testing "the first sample is shown as committed"
      (is (= 10.0 (model/position first-state 5000) (model/position first-state 9000))))
    (let [second-state (model/accept first-state (sample 10.05 1025) 5030)]
      (testing "the next sample is approached in a straight line over one sample period"
        (is (= 10.0 (model/position second-state 5030)))
        (is (close? 10.02 (model/position second-state 5040)))
        (is (close? 10.05 (model/position second-state 5055))))
      (testing "with no newer sample the playhead holds the committed beat; it does not extrapolate"
        (is (= 10.05 (model/position second-state 6000))))
      (testing "an early arrival continues from the displayed beat, without a jump"
        (let [third-state (model/accept second-state (sample 10.1 1050) 5045)]
          (is (close? 10.03 (model/position third-state 5045)))
          (is (close? 10.1 (model/position third-state 5070)))
          (is (apply <= (map #(model/position third-state %) (range 5045 5080 5))))))
      (testing "a repeated sample changes nothing"
        (is (= second-state (model/accept second-state (sample 10.05 1025) 5050))))
      (testing "a seek, a loop, a pause, a new show epoch and a long gap all snap"
        (doseq [[label next-sample] {"loop" (sample 2.0 1050)
                                     "seek" (sample 90.0 1050)
                                     "pause" (sample 10.06 1050 :playing? false)
                                     "epoch" (sample 10.1 1050 :epoch 2)
                                     "gap" (sample 11.0 1500)}]
          (let [state (model/accept second-state next-sample 5060)]
            (is (= (:beat next-sample) (model/position state 5060) (model/position state 5061)) label))))
      (testing "resuming from a pause does not sweep across the stopped interval"
        (let [paused (model/accept second-state (sample 10.06 1050 :playing? false) 5060)
              resumed (model/accept paused (sample 10.11 1075) 5085)]
          (is (= 10.11 (model/position resumed 5085))))))
    (is (nil? (model/position nil 0)))
    (is (nil? (model/accept nil {:time-ms 5} 0)))))

(deftest grid-density-follows-pixel-spacing
  (let [lines [{:beat 60.0 :kind :bar :bar 15} {:beat 61.0 :kind :beat} {:beat 62.0 :kind :beat}
               {:beat 63.0 :kind :beat} {:beat 64.0 :kind :phrase :bar 16 :phrase 4}]
        meter {:beats-per-bar 4 :bars-per-phrase 4}
        marks (fn [ppb] (model/grid-marks lines {:ppb ppb} meter))]
    (testing "wide enough: every beat, labelled bars and phrases"
      (is (= [[:bar true] [:beat nil] [:beat nil] [:beat nil] [:phrase true]] (mapv (juxt :kind :label?) (marks 15))))
      (is (true? (:phrase-label? (last (marks 15))))))
    (testing "zoomed out: beats go first, then bar labels, then bars; phrases always stay"
      (is (= [:bar :phrase] (mapv :kind (marks 6))))
      (is (= [false true] (mapv :label? (marks 6))))
      (is (= [:phrase] (mapv :kind (marks 1.5))))
      (is (false? (:phrase-label? (first (marks 1.5))))))))

(deftest events-are-touch-targets-centred-on-their-beat
  (let [scale (model/scale 960 window)
        box (model/event-box scale 60.0 {:event-id "drop" :key "pending/drop/0" :beat 100.0 :title "Drop"})]
    (is (= 600 (:centre box)))
    (is (= 578 (:left box)))
    (is (= 44 (- (+ (:left box) model/target) (:left box))))
    (is (close? (+ 622 8 (* 6.2 4)) (:right box)) "room for the caption follows the icon")
    (is (= 110 (model/caption-width (apply str (repeat 80 "x")))) "long captions are capped")
    (is (= 0 (model/caption-width nil)))))

(deftest ramps-span-start-to-end-and-are-never-smaller-than-a-touch-target
  (let [scale (model/scale 960 window)
        ramp {:event-id "swell" :key "swell/x/96" :start-beat 96.0 :end-beat 112.0 :state :running}]
    (is (= [540 780 240] ((juxt :left :right :span) (model/ramp-box scale 60.0 ramp))))
    (testing "a quarter-beat ramp is still a 44 px target, and its drawn span stays true"
      (is (= [540 584 3.75] ((juxt :left :right :span) (model/ramp-box scale 60.0 (assoc ramp :end-beat 96.25))))))
    (testing "a stopped ramp ends where it was cancelled"
      (is (= [540 660 120] ((juxt :left :right :span)
                            (model/ramp-box scale 60.0 (assoc ramp :state :cancelled :stopped-beat 104.0))))))))

(defn- box [id left right & {:as extra}]
  (merge {:event-id id :key id :left left :right right} extra))

(deftest overlapping-items-stack-into-lines-and-keep-them
  (let [{:keys [items lines assignment]} (model/stack [(box "a" 0 100) (box "b" 50 150) (box "c" 120 220) (box "d" 300 400)])]
    (is (= {"a" 0 "b" 1 "c" 0 "d" 0} (into {} (map (juxt :event-id :line)) items)))
    (is (= 2 lines))
    (testing "when a leaves the window, b stays on its line instead of jumping up"
      (let [later (model/stack [(box "b" 50 150) (box "c" 120 220) (box "e" 60 90)] assignment 2)]
        (is (= {"b" 1 "c" 0 "e" 0} (into {} (map (juxt :event-id :line)) (:items later))))
        (is (= 2 (:lines later)))))
    (testing "an event keeps its line when it fires, although its key changes"
      (let [fired (model/stack [(box "b" 50 150 :key "fired/b/12.5")] assignment 2)]
        (is (= 1 (:line (first (:items fired)))))))
    (testing "a scheduled ramp keeps its line when it starts and gains a destination"
      (let [planned (model/stack [(box "a" 0 100) (box "swell" 50 400)])
            started (model/stack [(box "swell" 50 400 :destination {:target :master})] (:assignment planned) 2)]
        (is (= 1 (:line (first (:items started))))))))
  (testing "an item that moved onto an occupied line gives it up; the one already there stays"
    (let [{:keys [items]} (model/stack [(box "stays" 100 200) (box "moved" 150 250)] {"stays" 0 "moved" 0} 2)]
      (is (= {"stays" 0 "moved" 1} (into {} (map (juxt :event-id :line)) items)))))
  (testing "a new item takes the lowest line free of retained neighbours on both sides"
    (let [{:keys [items]} (model/stack [(box "old" 200 300) (box "new" 150 250)] {"old" 0} 2)]
      (is (= {"old" 0 "new" 1} (into {} (map (juxt :event-id :line)) items)))))
  (testing "when every line is taken, an event drops its caption and shortens a neighbour's rather than overlap"
    (let [event (fn [id left] (box id left (+ left 44 100) :kind :event :caption 100))
          full (map #(event (str "e" %) (* 10 %)) (range model/max-lines))
          {:keys [items lines]} (model/stack (conj (vec full) (event "late" 110)))
          by-id (into {} (map (juxt :event-id identity)) items)
          late (by-id "late")
          neighbour (some #(when (and (not= late %) (= (:line late) (:line %))) %) items)]
      (is (= model/max-lines lines))
      (is (= {:caption 0 :compact? true :right 154} (select-keys late [:caption :compact? :right])))
      (is (= {:event-id "e0" :right 108 :caption 64} (select-keys neighbour [:event-id :right :caption]))
          "the first line's caption now ends before the late event")
      (is (not-any? (fn [[a b]] (and (= (:line a) (:line b)) (< (:left a) (:right b)) (< (:left b) (:right a))))
                    (for [a items b items :when (not= a b)] [a b]))
          "nothing overlaps")
      (is (every? #(= 100 (:caption %)) (remove #(#{"late" "e0"} (:event-id %)) items)) "other captions are untouched")))
  (testing "a ramp that finds no free line sits over a caption too"
    (let [event (fn [id left] (box id left (+ left 44 100) :kind :event :caption 100))
          {:keys [items]} (model/stack (conj (mapv #(event (str "e" %) (* 10 %)) (range model/max-lines))
                                             (box "ramp" 120 164 :kind :ramp)))
          ramp (some #(when (= "ramp" (:event-id %)) %) items)]
      (is (= [0 120 164] ((juxt :line :left :right) ramp)))
      (is (= 74 (:caption (some #(when (= "e0" (:event-id %)) %) items))))))
  (testing "a pile-up is capped at the last line rather than growing without bound"
    (let [{:keys [lines items]} (model/stack (map #(box (str "e" %) 0 100) (range 12)))]
      (is (= model/max-lines lines))
      (is (= 7 (count (filter #(= (dec model/max-lines) (:line %)) items))))))
  (is (= {:items [] :lines 0 :assignment {}} (model/stack []))))

(deftest ramp-curves-are-drawn-from-the-projected-easing-samples
  (let [shape [0 0.125 0.5 0.875 1]]
    (is (= "0,1.0000 1,0.8750 2,0.5000 3,0.1250 4,0.0000" (model/curve-points shape false)))
    (is (= "0,0.0000 1,0.1250 2,0.5000 3,0.8750 4,1.0000" (model/curve-points shape true))
        "a falling ramp is the same curve mirrored")
    (is (str/includes? (model/curve-points [0 1.4 1] false) "1,-0.1000") "spring overshoot is kept, clipped to the box")
    (is (nil? (model/curve-points nil false)))
    (is (nil? (model/curve-points [0 nil 1] false))))
  (is (true? (model/falling? {:from 0.8 :to 0.2})))
  (is (false? (model/falling? {:from 0.2 :to 0.8})))
  (is (false? (model/falling? {:from [1 0 0] :to [0 1 0]})) "colours have no direction; the curve rises"))

(deftest sections-and-audio-columns-map-to-screen-pixels
  (let [scale (model/scale 960 window)]
    (testing "a closed section spans its beats; an open one runs to the window edge for the past layer to clip"
      (is (= {:left 60 :width 240} (model/section-box scale 60.0 {:start-beat 64.0 :end-beat 80.0} window)))
      (is (= {:left 300 :width 900} (model/section-box scale 60.0 {:start-beat 80.0 :open? true} window))))
    (testing "an audio column is placed relative to the playhead, with its measured extrema"
      (is (= [450 15 21 40] (model/column-rect [98.0 99.0 -0.5 0.5 0.2 0.1 0.1 0.1] 100.0 scale 82))
          "half scale is half the lane, centred on its middle line")
      (is (= [465 15 1 80] (model/column-rect [99.0 100.0 -1.0 1.0 0.5 0 0 0] 100.0 scale 82)) "full scale fills the lane")
      (is (= [472.5 7.5 41 1] (model/column-rect [98.0 99.0 0.0 0.0 0 0 0 0] 98.5 scale 82))
          "audio is never drawn right of now, and silence is one pixel high")
      (is (nil? (model/column-rect [100.5 101.0 -0.5 0.5 0 0 0 0] 100.0 scale 82)) "a column ahead of the playhead waits")
      (is (nil? (model/column-rect [10.0 11.0 -0.5 0.5 0 0 0 0] 100.0 scale 82)) "left of the lane"))))

(deftest details-read-back-the-projection-in-the-schedulers-coordinates
  (let [pending {:kind :event :event-id "drop" :title "Drop" :state :pending :op :cue/launch :category :cue
                 :position {:bar 64 :beat 2.0 :phrase 16} :due-beat 258.0 :select {:ids [:wash] :pick :first}
                 :source {:adapter :agent :session-id "run-7"} :cancellable? true}
        ramp {:kind :ramp :event-id "swell" :title "Swell" :state :running :op :param/ramp :category :ramp
              :start-position {:bar 64 :beat 0.0} :end-position {:bar 72 :beat 0.0} :unit :beats
              :destination {:target :param :instance-id "1:3" :param :level} :from 0.2 :to 0.8
              :over {:bars 8} :curve :ease-in-out :source {:adapter :agent :session-id "run-7"} :cancellable? true}
        stopped (assoc ramp :state :cancelled :stopped-beat 270.5 :reason :human-override :cancellable? false
                       :by {:op :gesture/begin :source {:adapter :push-3}})]
    (is (= [["Action" "cue/launch"] ["Due" "bar 64 · beat 2"] ["Target" "wash (first)"] ["Posted by" "agent · run-7"]]
           (model/details pending)))
    (is (= [["Action" "param/ramp"] ["Span" "bar 64 · beat 0 → bar 72 · beat 0"] ["Target" "1:3 / level"]
            ["Value" "0.2 → 0.8"] ["Over" "8 bars · ease-in-out"] ["Posted by" "agent · run-7"]]
           (model/details ramp)))
    (is (= [["Stopped" "beat 270.5"] ["Reason" "human-override by push-3"]]
           (filterv #(#{"Stopped" "Reason"} (first %)) (model/details stopped))))
    (is (= ["At" "bar 0 · beat 3.5"]
           (second (model/details (assoc pending :state :fired :position {:bar 0 :beat 3.5} :at-beat 3.5 :due-beat 3.5)))))
    (is (some #{["Was due" "beat 3"]}
              (model/details (assoc pending :state :fired :position {:bar 0 :beat 3.25} :at-beat 3.25 :due-beat 3.0))))
    (is (some #{["Message" "No current target matches"]}
              (model/details (assoc pending :state :rejected :message "No current target matches"))))
    (let [timed (model/details (assoc ramp :unit :ms :remaining-ms 1500 :start-position {:bar 0 :beat 1.0}
                                      :end-position {:bar 1 :beat 1.0}))]
      (is (some #{["Span" "bar 0 · beat 1 → bar 1 · beat 1 (milliseconds, at this tempo)"]} timed))
      (is (some #{["Ends in" "1.5 s"]} timed) "a millisecond ramp ends on the owner's clock, not on a beat"))
    (testing "accessible names say what, which state and where"
      (is (= "Drop, cue, scheduled, bar 64 · beat 2" (model/item-label pending)))
      (is (= "Swell, ramp, running, bar 64 · beat 0" (model/item-label ramp))))
    (testing "only cancellable items offer an action, named for what it does"
      (is (= "Cancel event" (model/action-label pending)))
      (is (= "Stop ramp" (model/action-label ramp)))
      (is (= "Cancel event" (model/action-label (assoc ramp :state :pending))))
      (is (nil? (model/action-label stopped)))
      (is (nil? (model/action-label (assoc pending :cancellable? false)))))))
