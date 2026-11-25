(ns tolglow.studio.pad-breath-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.pad-breath :as breath]
            [tolglow.studio.views :as views]))

(defn- close? [a b] (< (js/Math.abs (- a b)) 1e-9))

(defn- near? [a b] (< (js/Math.abs (- a b)) 1e-6))

(deftest interpolates-the-projected-transport-between-samples
  (let [anchor (breath/next-anchor nil {:beat 8 :bpm 120 :playing? true :time-ms 1000} 0 5000)]
    (is (close? 0 (breath/breath-phase anchor 5000)))
    (is (close? 0.25 (breath/breath-phase anchor 5125)) "120 bpm advances a quarter beat in 125 ms")
    (is (close? 0.5 (breath/breath-phase anchor 5250)))
    (testing "a sample that has not changed keeps its anchor, so re-renders do not drift the clock"
      (is (identical? anchor (breath/next-anchor anchor {:beat 8 :bpm 120 :playing? true :time-ms 1000} 0 5400))))
    (testing "a sample without a tempo does not invent one"
      (let [untimed (breath/next-anchor nil {:beat 8.5 :playing? true :time-ms 1000} 0 5000)]
        (is (nil? (:bpm untimed)))
        (is (close? 0.5 (breath/breath-phase untimed 9000)))
        (is (nil? (breath/start-time untimed)))))
    (testing "a silent stream holds rather than inventing progress"
      (is (= (breath/beat-at anchor 60000) (breath/beat-at anchor (+ 5000 breath/max-extrapolation-ms)))))
    (testing "a paused transport holds its phase"
      (let [paused (breath/next-anchor nil {:beat 8.3 :bpm 120 :playing? false :time-ms 1000} 0 5000)]
        (is (close? 0.3 (breath/breath-phase paused 9000)))))))

(deftest a-tempo-change-follows-the-new-sample
  (let [fast (breath/next-anchor nil {:beat 4 :bpm 120 :playing? true :time-ms 0} 0 0)
        ;; The server moved to 60 bpm. Its next sample, 200 ms later, says beat 4.3.
        slow (breath/next-anchor fast {:beat 4.3 :bpm 60 :playing? true :time-ms 200} 0 200)]
    (is (not (identical? fast slow)))
    (is (close? 0.3 (breath/breath-phase slow 200)) "the projected beat wins over the old extrapolation (0.4)")
    (is (close? 0.55 (breath/breath-phase slow 450)) "60 bpm advances a quarter beat in 250 ms, not half")
    (is (close? 0.05 (breath/breath-phase slow 950)) "the phase wraps at the next downbeat")))

(deftest a-seek-re-anchors-at-once
  (let [before (breath/next-anchor nil {:beat 12.9 :bpm 120 :playing? true :time-ms 0} 3 0)
        ;; A seek bumps the transport generation. The beat may even go backwards.
        after (breath/next-anchor before {:beat 2.25 :bpm 120 :playing? true :time-ms 40} 4 40)]
    (is (close? 0.98 (breath/breath-phase before 40)))
    (is (= 4 (:generation after)))
    (is (close? 0.25 (breath/breath-phase after 40)))
    (is (close? 0.5 (breath/breath-phase after 165)))
    (testing "a seek to the same beat at the same frame time still counts as a new sample"
      (let [same (breath/next-anchor after {:beat 2.25 :bpm 120 :playing? true :time-ms 40} 5 900)]
        (is (not (identical? after same)))
        (is (close? 0.25 (breath/breath-phase same 900)))))))

(deftest animations-start-on-the-downbeat
  (let [anchor (breath/next-anchor nil {:beat 8.25 :bpm 120 :playing? true :time-ms 1000} 0 5000)]
    (is (close? 500 (breath/beat-duration-ms anchor)))
    (is (close? 4875 (breath/start-time anchor)) "a quarter beat at 120 bpm started 125 ms earlier")
    (testing "progress from that start equals the beat phase, for whole and half-beat animations"
      (doseq [now [5000 5100 5333 6020]]
        (let [elapsed (- now (breath/start-time anchor))]
          (is (close? (breath/breath-phase anchor now) (/ (mod elapsed 500) 500)))
          (is (close? (mod (* 2 (breath/breath-phase anchor now)) 1) (/ (mod elapsed 250) 250))))))
    (testing "after a tempo change and a seek the start follows the new sample"
      (let [slow (breath/next-anchor anchor {:beat 9.5 :bpm 60 :playing? true :time-ms 1200} 0 5200)
            seek (breath/next-anchor slow {:beat 2.1 :bpm 60 :playing? true :time-ms 1300} 1 5300)]
        (is (close? 4700 (breath/start-time slow)))
        (is (close? 5200 (breath/start-time seek)))))))

(deftest start-times-a-whole-beat-apart-share-a-phase
  (is (close? 0 (breath/phase-offset-ms 5375 4875 500)))
  (is (close? 0 (breath/phase-offset-ms 4875 6875 500)))
  (is (close? 25 (breath/phase-offset-ms 5400 4875 500)))
  (is (close? -25 (breath/phase-offset-ms 4850 5375 500)))
  (is (close? -250 (breath/phase-offset-ms 5125 4875 500)) "half a beat away is the far side"))

(deftest release-blink-keyframes-keep-afterglow-phase
  (let [css (.readFileSync (js/require "fs") "resources/studio/console.css" "utf8")]
    (is (close? 0.4 breath/release-bright-fraction))
    (is (str/includes? css "@keyframes pad-release-blink { 0%, 40% { opacity: .3; }")
        "bright through Afterglow's first 40% of the beat")
    (is (str/includes? css "pad-queued-flash calc(var(--beat-duration) / 2)") "queued pads blink twice per beat")
    (doseq [name breath/animation-names]
      (is (str/includes? css (str "@keyframes " name " ")) name))))

;; A host that records what the clock does at its boundary.
(defn- fake-host [state]
  {:now #(:now @state)
   :grid #(:grid @state)
   :hidden? #(:hidden? @state)
   :reduced? #(:reduced? @state)
   :set-timeout (fn [f ms] (let [id (inc (:last-timer @state 0))]
                             (swap! state #(-> % (assoc :last-timer id) (assoc-in [:timers id] {:f f :ms ms})))
                             id))
   :clear-timeout (fn [id] (swap! state update :timers dissoc id))
   :animations (fn [_] (swap! state update :walks inc) (:animations @state))
   :write! (fn [el mode duration] (swap! state update :writes conj [el mode duration]))
   :watch (fn [callbacks] (swap! state assoc :watching callbacks)
            #(swap! state assoc :watching nil :unwatched? true))})

(defn- fire-timers! [state]
  (let [timers (vals (:timers @state))]
    (swap! state assoc :timers {})
    (doseq [{:keys [f]} timers] (f))))

(def ^:private playing {:beat 8.25 :bpm 120 :playing? true :time-ms 1000})

(deftest clock-driver-lifecycle
  (let [animation #js {:animationName "pad-breath" :startTime nil}
        state (atom {:now 5000 :grid :grid :animations [animation] :writes [] :timers {} :walks 0})
        ctl (breath/controller (fake-host state))]
    (breath/attach! ctl)
    (is (fn? (:changed (:watching @state))) "listens for visibility and reduced-motion changes")
    (testing "a playing transport with a running pad starts live and aligned"
      (breath/sample! ctl playing 0 true)
      (is (= [[:grid :live 500]] (:writes @state)))
      (is (close? 4875 (.-startTime animation)))
      (is (= 1 (count (:timers @state))) "one stale-stream timer, no per-pad timers")
      (is (close? 2000 (:ms (first (vals (:timers @state)))))))
    (testing "an unchanged sample or one within tolerance writes nothing"
      (swap! state assoc :now 5100)
      (breath/sample! ctl playing 0 true)
      (swap! state assoc :now 5120)
      (breath/sample! ctl (assoc playing :beat 8.491 :time-ms 1120) 0 true)
      (is (= 1 (count (:writes @state))))
      (is (close? 4875 (.-startTime animation)))
      (is (= 1 (count (:timers @state)))))
    (testing "jitter under the tolerance leaves animations alone"
      (swap! state assoc :now 5140)
      (breath/sample! ctl (assoc playing :beat 8.55 :time-ms 1140) 0 true)
      (is (close? 4875 (.-startTime animation)) "10 ms off the transport, below the 20 ms tolerance"))
    (testing "re-renders with an unchanged sample do not walk the animations or reset the timer"
      (let [walks (:walks @state) timers (:timers @state)]
        (dotimes [_ 3] (breath/sample! ctl (assoc playing :beat 8.55 :time-ms 1140) 0 true))
        (is (= walks (:walks @state)))
        (is (= timers (:timers @state)))))
    (testing "tempo jitter below 0.1 ms of beat length does not rewrite the duration"
      (let [writes (count (:writes @state))]
        (swap! state assoc :now 5160)
        (breath/sample! ctl (assoc playing :beat 8.57 :bpm 120.0001 :time-ms 1160) 0 true)
        (is (= 500 (breath/beat-duration-ms {:bpm 120.0001})))
        (is (= writes (count (:writes @state))))))
    (testing "a new pad animation is aligned when it starts"
      (let [late #js {:animationName "pad-release-blink" :startTime 9999}]
        (swap! state update :animations conj late)
        ((:animation-started (:watching @state)))
        (is (close? 4875 (.-startTime late)) "aligned to the latest sample")))
    (testing "a stalled stream goes still once and schedules nothing more"
      (swap! state assoc :now 7200)
      (fire-timers! state)
      (is (= [:grid :still nil] (peek (:writes @state))))
      (is (empty? (:timers @state)))
      (testing "and the next sample restarts it"
        (breath/sample! ctl (assoc playing :beat 12.75 :time-ms 3200) 0 true)
        (is (= :live (second (peek (:writes @state)))))
        (is (= 1 (count (:timers @state))))))
    (testing "a paused transport is still with no timer"
      (breath/sample! ctl (assoc playing :playing? false :time-ms 3300) 0 true)
      (is (= [:grid :still nil] (peek (:writes @state))))
      (is (empty? (:timers @state))))
    (testing "a missing tempo is still"
      (breath/sample! ctl (dissoc playing :bpm) 0 true)
      (is (= :still (:mode @ctl)))
      (is (empty? (:timers @state))))
    (testing "a hidden page is still, and resumes when shown"
      (breath/sample! ctl (assoc playing :time-ms 3400) 0 true)
      (is (= :live (:mode @ctl)))
      (swap! state assoc :hidden? true)
      ((:changed (:watching @state)))
      (is (= :still (:mode @ctl)))
      (is (empty? (:timers @state)))
      (swap! state assoc :hidden? false)
      ((:changed (:watching @state)))
      (is (= :live (:mode @ctl))))
    (testing "switching on reduced motion stops the clock"
      (let [writes (count (:writes @state))]
        (swap! state assoc :reduced? true)
        ((:changed (:watching @state)))
        (is (= :still (:mode @ctl)))
        (is (= (inc writes) (count (:writes @state))))
        (is (empty? (:timers @state)))
        (breath/sample! ctl (assoc playing :time-ms 3500) 0 true)
        (is (= (inc writes) (count (:writes @state))) "no writes while motion is reduced")))
    (testing "nothing to animate is still"
      (swap! state assoc :reduced? false)
      (breath/sample! ctl (assoc playing :time-ms 3600) 0 false)
      (is (= :still (:mode @ctl))))
    (testing "unmounting stops listening and clears the timer"
      (breath/sample! ctl (assoc playing :time-ms 3700) 0 true)
      (is (seq (:timers @state)))
      (breath/detach! ctl)
      (is (:unwatched? @state))
      (is (empty? (:timers @state))))))

(deftest the-stale-timer-decides-when-the-frame-clock-reads-short
  ;; The document timeline advances once per display frame. A 2000 ms timeout
  ;; armed inside a frame runs when headless Chrome 155 reports 1999.92 ms
  ;; elapsed (120 frames of 16.666 ms), so a second clock comparison says live.
  (let [animation #js {:animationName "pad-breath" :startTime nil}
        state (atom {:now 5000 :grid :grid :animations [animation] :writes [] :timers {} :walks 0})
        ctl (breath/controller (fake-host state))]
    (breath/attach! ctl)
    (breath/sample! ctl playing 0 true)
    (is (= :live (:mode @ctl)))
    (swap! state assoc :now 6999.92)
    (fire-timers! state)
    (is (= :still (:mode @ctl)) "a silent stream goes still when its timer fires")
    (is (= [:grid :still nil] (peek (:writes @state))))
    (is (empty? (:timers @state)))
    (testing "the same stale sample stays still through re-renders and visibility changes"
      (breath/sample! ctl playing 0 true)
      ((:changed (:watching @state)))
      (is (= :still (:mode @ctl)))
      (is (empty? (:timers @state))))
    (testing "the next sample restarts the pads and their timer"
      (swap! state assoc :now 7100)
      (breath/sample! ctl (assoc playing :beat 12.45 :time-ms 3100) 0 true)
      (is (= :live (:mode @ctl)))
      (is (= 1 (count (:timers @state)))))))

(defn- counting-animation
  "A fake animation that counts writes to its start time."
  [name]
  (let [animation #js {:animationName name :held nil :sets 0}]
    (js/Object.defineProperty animation "startTime"
                              #js {:get (fn [] (.-held animation))
                                   :set (fn [value] (set! (.-held animation) value)
                                          (set! (.-sets animation) (inc (.-sets animation))))})
    animation))

(deftest steady-samples-do-not-rewrite-start-times
  (let [animation (counting-animation "pad-breath")
        flash (counting-animation "pad-queued-flash")
        state (atom {:now 5000 :grid :grid :animations [animation flash] :writes [] :timers {} :walks 0})
        ctl (breath/controller (fake-host state))
        ;; `delay` is how stale the projected beat is when it reaches the clock.
        sample! (fn [now delay]
                  (swap! state assoc :now now)
                  (breath/sample! ctl {:beat (+ 8.25 (/ (- now 5000 delay) 500)) :bpm 120 :playing? true
                                       :time-ms (- now 4000 delay)}
                                  0 true))]
    (breath/attach! ctl)
    (sample! 5000 0)
    (is (near? 4875 (.-startTime animation)))
    (is (= [1 1] [(.-sets animation) (.-sets flash)]))
    (testing "a later beat with the same phase is the same alignment"
      (doseq [now [5500 6000 6533.3 7250]] (sample! now 0))
      (is (= [1 1] [(.-sets animation) (.-sets flash)]) "one write, not one per beat")
      (is (near? 4875 (.-startTime animation))))))

(deftest the-least-delayed-sample-sets-the-phase
  ;; Stream and frame latency only ever delay a sample. Measured on loopback,
  ;; consecutive samples differ by 25 to 33 ms, more than the tolerance.
  (let [animation (counting-animation "pad-breath")
        state (atom {:now 5000 :grid :grid :animations [animation] :writes [] :timers {} :walks 0})
        ctl (breath/controller (fake-host state))
        sample! (fn [now delay]
                  (swap! state assoc :now now)
                  (breath/sample! ctl {:beat (+ 8.25 (/ (- now 5000 delay) 500)) :bpm 120 :playing? true
                                       :time-ms (- now 4000 delay)}
                                  0 true))
        phase-start #(mod (.-startTime animation) 500)]
    (breath/attach! ctl)
    (sample! 5000 30)
    (is (near? 405 (phase-start)) "the first sample is all the clock has: 30 ms after the true downbeat at 375")
    (testing "a slightly fresher sample within the tolerance changes nothing"
      (sample! 5033.3 25)
      (is (= 1 (.-sets animation))))
    (testing "a sample that arrived sooner is closer to the transport and is followed at once"
      (sample! 5066.6 5)
      (is (near? 380 (phase-start)))
      (is (= 2 (.-sets animation))))
    (testing "samples delayed beyond the tolerance do not nudge the pads"
      (sample! 5100 38)
      (sample! 5133.3 30)
      (sample! 5166.6 12)
      (is (near? 380 (phase-start)))
      (is (= 2 (.-sets animation))))
    (testing "when every sample for a second is later, the transport itself moved"
      (doseq [now (range 5200 6040 33.3)] (sample! now 35))
      (is (= 2 (.-sets animation)) "the fresher sample still counts within its window")
      (doseq [now (range 6040 6200 33.3)] (sample! now 35))
      (is (near? 410 (phase-start)))
      (is (= 3 (.-sets animation)))
      (is (<= (count (:recent (:lock @ctl))) 31) "the estimate keeps one window of samples"))
    (testing "a jump far beyond receive jitter is a transport change and is followed at once"
      (sample! 6233.3 235)
      (is (near? 110 (phase-start)))
      (is (= 4 (.-sets animation))))
    (testing "a tempo change or a seek starts a fresh estimate"
      (swap! state assoc :now 6300)
      (breath/sample! ctl {:beat 20.5 :bpm 100 :playing? true :time-ms 2300} 0 true)
      (is (near? 6000 (.-startTime animation)) "half a 600 ms beat before now")
      (breath/sample! ctl {:beat 3.25 :bpm 100 :playing? true :time-ms 2300} 1 true)
      (is (near? 6150 (.-startTime animation)))
      (is (= 6 (.-sets animation))))))

(def ^:private cues
  [{:id :ember :name "Ember"} {:id :haze :name "Haze"} {:id :strobe :name "Strobe"}
   {:id :wash :name "Wash"} {:id :gate :name "Gate"} {:id :idle :name "Idle"}])

(defn- grid-db []
  {:connection {:connected? true} :ui {:view :perform}
   :show {}
   :server-projection
   {:view {:cues cues
           :instances [{:id "1" :cue-id :ember :phase :running}
                       {:id "2" :cue-id :haze :phase :releasing}
                       {:id "3" :cue-id :strobe :phase :running}
                       {:id "4" :cue-id :gate :phase "running" :launch-mode "held"}]
           :pending-cue-launches [{:command-id "q1" :cue-id :strobe :due-beat 8}
                                  {:command-id "q2" :cue-id :wash :due-beat 8}]}}})

(def ^:private page
  {:id :main :cells (mapv (fn [x cue] {:x x :y 0 :cue-id (:id cue)}) (range) cues)})

(defn- pad [db x]
  (let [html (renderToStaticMarkup (r/as-element [@#'views/cue-cell db page x 0 nil]))
        classes (set (str/split (second (re-find #"class=\"([^\"]*)\"" html)) #"\s+"))
        label (second (re-find #"aria-label=\"([^\"]*)\"" html))]
    {:classes classes :label label}))

(deftest cue-pads-distinguish-running-queued-held-and-releasing
  (let [db (grid-db)
        [ember haze strobe wash gate idle] (map #(pad db %) (range 6))]
    (is (every? (:classes ember) ["cue-cell" "active" "running"]))
    (is (not-any? (:classes ember) ["queued" "releasing" "held"]))
    (is (str/includes? (:label ember) "row 1, running"))
    (is (every? (:classes haze) ["active" "releasing"]))
    (is (not ((:classes haze) "running")) "a releasing pad blinks instead of breathing")
    (is (every? (:classes strobe) ["active" "running" "queued"]) "running and queued at once")
    (is (str/includes? (:label strobe) "running, queued"))
    (is (every? (:classes wash) ["queued"]))
    (is (not-any? (:classes wash) ["active" "running"]))
    (is (every? (:classes gate) ["active" "running" "held"]))
    (is (str/includes? (:label gate) "row 1, held"))
    (is (= #{"cue-cell"} (:classes idle)))
    (testing "the shared clock runs only while this page has something to animate"
      (is (true? (@#'views/pad-breath-wanted? db page)))
      (is (false? (@#'views/pad-breath-wanted? (assoc-in db [:server-projection :view]
                                                            {:cues cues :instances [] :pending-cue-launches []})
                                                page))))))

(deftest pad-state-accepts-wire-strings-and-keywords
  (is (= #{:active :running} (breath/pad-state [{:phase "running"}] false)))
  (is (= #{:active :releasing :queued} (breath/pad-state [{:phase :releasing}] true)))
  (is (breath/animated? #{:queued}))
  (is (not (breath/animated? #{}))))
