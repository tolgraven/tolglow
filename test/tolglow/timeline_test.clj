(ns tolglow.timeline-test
  "The musical timeline projection (#646): scheduled and fired events, ramps,
  sections, grid, captured audio and meters, read through the real owner tick,
  capture worker and Studio state stream."
  (:require [afterglow.control.model :as control]
            [afterglow.render.easing :as easing]
            [clojure.test :refer [deftest is testing]]
            [ring.adapter.jetty :as jetty]
            [tolglow.audio.capture :as capture]
            [tolglow.audio.capture-test :as pcm-test]
            [tolglow.connections :as connections]
            [tolglow.integrations.reference-position :as position]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.timeline :as timeline]
            [tolglow.transit :as transit])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def ^:private effect
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- live-model []
  (control/new-show
   {:show-id :timeline-test :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:wash effect}
               :cues {:wash {:id :wash :effect-id :wash :version 1}}}
    :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(def ^:private agent-source {:adapter :agent :session-id "agent-run"})

(def ^:private studio-source {:adapter :studio :session-id "tab"})

(defn- runtime [] (show/create-runtime {:model (live-model) :workspace-options nil}))

(defn- at! [rt time payload]
  (let [reply (show/submit! rt :command payload)]
    (show/tick! rt time)
    (deref reply 1000 ::missing)))

(defn- by-id [entries id] (first (filter #(= id (:event-id %)) entries)))

(defn- close? [a b] (< (Math/abs (- (double a) (double b))) 1.0e-6))

;; Window and grid ------------------------------------------------------------------

(deftest positions-are-zero-based-transport-coordinates
  (is (= {:bar 64 :beat 2.0 :phrase 16} (timeline/position {:beats-per-bar 4 :bars-per-phrase 4} 258.0)))
  (is (= {:bar 3 :beat 0.25 :phrase 1} (timeline/position {:beats-per-bar 7/2 :bars-per-phrase 2} 10.75)))
  (is (= {:bar -1 :beat 3.5 :phrase -1} (timeline/position {:beats-per-bar 4 :bars-per-phrase 4} -0.5)))
  (is (nil? (timeline/position {:beats-per-bar 4 :bars-per-phrase 4} nil))))

(deftest window-options-are-validated
  (is (= {:bars 8} (timeline/options nil)))
  (is (= {:bars 2} (timeline/options {:bars 2})))
  (doseq [bad [{:bars 0} {:bars 33} {:bars 2.5} {:beats 4} [:bars 8]]]
    (is (thrown? clojure.lang.ExceptionInfo (timeline/options bad)) (pr-str bad))))

(deftest grid-lists-beats-bars-and-phrases-from-the-transport-meter
  (let [view (timeline/view {:model (assoc-in (live-model) [:transport :beat] 18.5)} {:bars 2})
        lines (get-in view [:grid :lines])
        at (fn [beat] (first (filter #(= beat (:beat %)) lines)))]
    (testing "the window is the visible span plus one bar, snapped to bars"
      (is (= {:bars 2 :past-beats 8.0 :future-beats 8.0 :from-beat 4.0 :to-beat 32.0} (:window view)))
      (is (= {:beats-per-bar 4 :bars-per-phrase 4} (:meter view)))
      (is (= {:bar 4 :beat 2.5 :phrase 1} (:position view)) "now, in the scheduler's zero-based coordinates"))
    (testing "every quarter note is listed once, with the strongest boundary kind"
      (is (= (map double (range 4 33)) (map :beat lines)))
      (is (= {:beat 16.0 :kind :phrase :bar 4 :phrase 1} (at 16.0)))
      (is (= {:beat 32.0 :kind :phrase :bar 8 :phrase 2} (at 32.0)))
      (is (= {:beat 20.0 :kind :bar :bar 5} (at 20.0)))
      (is (= {:beat 21.0 :kind :beat} (at 21.0))))
    (testing "bar positions are the zero-based coordinates the scheduler accepts in :at"
      (is (= 20.0 (* 4.0 (:bar (at 20.0)))))))
  (testing "a 7/8 bar is three and a half quarter notes, and phrases follow the bars"
    (let [model (-> (live-model)
                    (assoc-in [:transport :meter] {:beats-per-bar 7/2 :bars-per-phrase 2})
                    (assoc-in [:transport :beat] 8.0))
          lines (get-in (timeline/view {:model model} {:bars 1}) [:grid :lines])]
      (is (= [[0.0 :phrase] [1.0 :beat] [2.0 :beat] [3.0 :beat] [3.5 :bar] [4.0 :beat] [5.0 :beat] [6.0 :beat]
              [7.0 :phrase] [8.0 :beat] [9.0 :beat] [10.0 :beat] [10.5 :bar] [11.0 :beat] [12.0 :beat]
              [13.0 :beat] [14.0 :phrase] [15.0 :beat] [16.0 :beat] [17.0 :beat] [17.5 :bar]]
             (mapv (juxt :beat :kind) lines)))))
  (testing "wide windows keep bars and phrases only"
    (let [lines (get-in (timeline/view {:model (live-model)} {:bars 32}) [:grid :lines])]
      (is (every? #{:bar :phrase} (map :kind lines)))
      (is (= 68 (count lines)) "bars -33 to 34 around beat zero")
      (is (= 17 (count (filter #(= :phrase (:kind %)) lines)))))))

;; Events and ramps through the real owner ---------------------------------------------

(deftest scheduled-fired-cancelled-and-rejected-events-keep-their-beats
  (let [rt (runtime)]
    (try
      (let [posted (at! rt 0 {:op :schedule/post :source agent-source
                              :events [{:id "soon" :label "Wash in" :op :cue/launch :cue-id :wash :at {:beat 2}}
                                       {:id "later" :label "Drop" :op :cue/launch :cue-id :wash :at {:bar 3}}
                                       {:id "gone" :op :master/set :value 0.5 :in {:bars 2}}
                                       {:id "nothing" :op :instance/end :select {:ids ["missing"]} :at {:beat 3}}
                                       {:id "far" :op :cue/launch :cue-id :wash :at {:bar 40}}]})]
        (is (= :rejected (:status posted)) "an agent cannot schedule the master")
        (is (= :accepted (:status (at! rt 0 {:op :schedule/post :source studio-source
                                             :events [{:id "soon" :label "Wash in" :op :cue/launch :cue-id :wash :at {:beat 2}}
                                                      {:id "later" :label "Drop" :op :cue/launch :cue-id :wash :at {:bar 3}}
                                                      {:id "gone" :op :master/set :value 0.5 :in {:bars 2}}
                                                      {:id "nothing" :op :instance/end :select {:ids ["missing"]} :at {:beat 3}}
                                                      {:id "far" :op :cue/launch :cue-id :wash :at {:bar 40}}]}))))
        (testing "before anything fires, everything in the window is pending at its due beat"
          (let [{:keys [events beat]} (show/timeline rt {:bars 4})]
            (is (= 0.0 beat))
            (is (= [["soon" 2.0] ["nothing" 3.0] ["gone" 8.0] ["later" 12.0]] (mapv (juxt :event-id :beat) events))
                "bar 40 lies outside the four-bar window")
            (is (every? #(= :pending (:state %)) events))
            (is (every? :cancellable? events))
            (is (= {:category :cue :op :cue/launch :label "Wash in" :title "Wash in" :origin :scheduled
                    :source studio-source :due-beat 2.0}
                   (select-keys (by-id events "soon") [:category :op :label :title :origin :source :due-beat])))
            (is (= :master (:category (by-id events "gone"))))))
        (doseq [t [500 1000 1125 1500 1625]] (show/tick! rt t))
        (testing "the timeline cancel is the REPL's validated owner command"
          (let [receipt (deref (show/cancel-events! rt {:event-ids ["gone"]}) 10 ::queued)]
            (is (= ::queued receipt) "the owner applies it on its next cycle")
            (show/tick! rt 1750)))
        (let [{:keys [events beat]} (show/timeline rt {:bars 4})
              fired (by-id events "soon")
              rejected (by-id events "nothing")
              cancelled (by-id events "gone")]
          (is (= 3.5 beat))
          (testing "a fired event sits at the beat of the frame that fired it"
            (is (= :fired (:state fired)))
            (is (= 2.0 (:due-beat fired) (:at-beat fired) (:beat fired)))
            (is (false? (:cancellable? fired))))
          (testing "a rejected event keeps its reason"
            (is (= :rejected (:state rejected)))
            (is (= 3.0 (:beat rejected)) "the frame at 1500 ms reached beat 3")
            (is (string? (:message rejected))))
          (testing "a cancelled event leaves the future and is recorded where it was cancelled"
            (is (= :cancelled (:state cancelled)))
            (is (= :operator-cancel (:reason cancelled)))
            (is (= 3.5 (:beat cancelled)))
            (is (not-any? #(and (= "gone" (:event-id %)) (= :pending (:state %))) events)))
          (is (= [:pending 12.0 {:bar 3 :beat 0.0 :phrase 0}] ((juxt :state :beat :position) (by-id events "later")))
              "the position reads back as the :at {:bar 3} it was posted with")
          (is (apply <= (map :beat events)) "events are in beat order")))
      (finally (show/stop! rt)))))

(deftest ramps-are-bars-from-start-to-end-with-the-owners-easing
  (let [rt (runtime)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))]
        (is (= :accepted (:status (at! rt 250 {:op :param/ramp :event-id "swell" :label "Swell" :instance-id id :param :level
                                               :to 1.0 :over {:beats 8} :curve :ease-in-out :source agent-source}))))
        (is (= :accepted (:status (at! rt 500 {:op :param/ramp :event-id "next" :instance-id id :param :level
                                               :to 0.25 :over {:bars 2} :curve {:type :power :exponent 3}
                                               :start {:at {:bar 4}} :source agent-source}))))
        (testing "a running ramp spans the beats the owner started and will end it at"
          (let [{:keys [ramps events beat]} (show/timeline rt {:bars 8})
                swell (by-id ramps "swell")]
            (is (= 1.0 beat))
            (is (= {:state :running :start-beat 0.5 :end-beat 8.5 :unit :beats :curve :ease-in-out
                    :from 0.0 :to 1.0 :label "Swell" :cancellable? true :category :ramp}
                   (select-keys swell [:state :start-beat :end-beat :unit :curve :from :to :label :cancellable? :category])))
            (is (= {:target :param :instance-id id :param :level} (:destination swell)))
            (is (= [{:bar 0 :beat 0.5 :phrase 0} {:bar 2 :beat 0.5 :phrase 0}]
                   ((juxt :start-position :end-position) swell)))
            (is (= agent-source (select-keys (:source swell) [:adapter :session-id])))
            (testing "the drawn curve is the owner's easing function, sampled as data"
              (is (= 17 (count (:shape swell))))
              (is (every? true? (map-indexed (fn [index value]
                                               (close? value (/ (Math/round (* 1.0e4 (easing/sample :ease-in-out (/ index 16.0)))) 1.0e4)))
                                             (:shape swell)))))
            (testing "now sits inside the bar at the owner's progress"
              (is (close? (/ (- beat (:start-beat swell)) (- (:end-beat swell) (:start-beat swell))) 1/16)))
            (testing "the fired ramp event is the bar itself, not a second marker"
              (is (nil? (by-id events "swell"))))))
        (testing "a scheduled ramp is a pending bar with its planned span and curve"
          (let [{:keys [ramps events]} (show/timeline rt {:bars 8})
                planned (by-id ramps "next")]
            (is (= {:state :pending :start-beat 16.0 :end-beat 24.0 :to 0.25 :cancellable? true}
                   (select-keys planned [:state :start-beat :end-beat :to :cancellable? :from])))
            (is (close? 0.125 (nth (:shape planned) 8)) "a cubic at half progress")
            (is (nil? (by-id events "next")))))
        (testing "a tempo change leaves musical positions alone"
          (let [before (show/timeline rt {:bars 8})]
            (is (= :accepted (:status (at! rt 1000 {:op :transport/set :transport {:bpm 60.0}
                                                    :source {:adapter :repl :session-id "r"}}))))
            (show/tick! rt 2000)
            (let [after (show/timeline rt {:bars 8})]
              (is (= 60.0 (:bpm after)))
              (is (= 3.0 (:beat after)) "2 beats by 1000 ms at 120 BPM, then one more beat in the next second")
              (is (= (mapv (juxt :event-id :start-beat :end-beat) (:ramps before))
                     (mapv (juxt :event-id :start-beat :end-beat) (:ramps after))))
              (is (= (:grid before) (:grid after)) "the same bars and phrases are still in the window"))))
        (testing "a finished ramp stays as a bar in the past with its outcome"
          (doseq [t (range 3000 9000 500)] (show/tick! rt t))
          (let [{:keys [ramps beat]} (show/timeline rt {:bars 8})
                done (by-id ramps "swell")]
            (is (= 9.5 beat))
            (is (= {:state :completed :start-beat 0.5 :end-beat 8.5 :curve :ease-in-out :from 0.0 :to 1.0
                    :cancellable? false}
                   (select-keys done [:state :start-beat :end-beat :curve :from :to :cancellable?])))
            (is (= 17 (count (:shape done))))))
        (testing "stopping a ramp from the timeline records where it stopped"
          (is (= :accepted (:status (at! rt 9000 {:op :param/ramp :event-id "fade" :instance-id id :param :level
                                                  :to 0.0 :over {:beats 8} :source agent-source}))))
          (show/tick! rt 10000)
          (is (= :accepted (:status (at! rt 11000 {:op :schedule/cancel :event-ids ["fade"] :source studio-source}))))
          (let [{:keys [ramps beat]} (show/timeline rt {:bars 8})
                stopped (by-id ramps "fade")]
            (is (= 12.0 beat))
            (is (= {:state :cancelled :start-beat 10.0 :end-beat 18.0 :stopped-beat 12.0 :reason :operator-cancel}
                   (select-keys stopped [:state :start-beat :end-beat :stopped-beat :reason]))))))
      (finally (show/stop! rt)))))

(deftest millisecond-ramps-are-placed-on-the-beat-axis-at-the-current-tempo
  (let [rt (runtime)]
    (try
      (let [id (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent}}))]
        (is (= :accepted (:status (at! rt 500 {:op :param/ramp :event-id "ms" :instance-id id :param :level
                                               :to 1.0 :over {:ms 2000} :source agent-source}))))
        (show/tick! rt 1000)
        (let [{:keys [ramps beat]} (show/timeline rt)
              ramp (by-id ramps "ms")]
          (is (= 2.0 beat))
          (is (= :ms (:unit ramp)))
          (is (close? 1.0 (:start-beat ramp)) "500 ms ago is one beat ago at 120 BPM")
          (is (close? 5.0 (:end-beat ramp)) "1500 ms remain: three beats")
          (is (= 1500.0 (:remaining-ms ramp)) "the owner's millisecond clock decides when it ends"))
        (at! rt 1000 {:op :transport/set :transport {:bpm 60.0} :source {:adapter :repl :session-id "r"}})
        (let [ramp (by-id (:ramps (show/timeline rt)) "ms")]
          (is (close? 1.5 (:start-beat ramp)) "the same 500 ms is half a beat at 60 BPM")
          (is (close? 3.5 (:end-beat ramp)))
          (is (close? 0.25 (/ (- 2.0 (:start-beat ramp)) (- (:end-beat ramp) (:start-beat ramp))))
              "now still sits at the ramp's millisecond progress")))
      (finally (show/stop! rt)))))

(deftest subscriber-steps-are-listed-but-not-cancellable-from-the-timeline
  (let [rt (runtime)]
    (try
      (let [reply (show/register-event-subscriber!
                   rt {:id :beats :match {:type :clock/bar}
                       :actions [{:op :cue/launch :select {:ids [:wash] :pick :first} :delay {:beats 2}}]})]
        (show/tick! rt 0)
        (is (= :accepted (:status (deref reply 1000 ::timeout)))))
      (doseq [t [1000 2000 2125]] (show/tick! rt t))
      (let [step (first (filter #(= :subscriber (:origin %)) (:events (show/timeline rt))))]
        (is (some? step) "the pending step of the subscriber sequence is on the timeline")
        (is (= [:pending 6.0 false :beats] ((juxt :state :beat :cancellable? :subscriber-id) step))))
      (finally (show/stop! rt)))))

;; Sections -----------------------------------------------------------------------

(defn- section-change [boundary confirmed & {:as extra}]
  (merge {:type :audio/section-change :version 1 :boundary-beat boundary :confirmed-at-beat confirmed
          :confidence 0.9 :evidence {:drivers [:bass-pattern]} :clock-source :manual :clock-epoch 0
          :time-ms (* 500.0 confirmed) :sequence (long confirmed)} extra))

(defn- at-beat [model beat]
  (-> model (assoc :time-ms (* 500.0 beat))
      (update :transport assoc :beat (double beat) :time-ms (* 500.0 beat) :source :manual :source-epoch 0)))

(deftest causal-sections-run-between-confirmed-boundaries
  (let [model (-> (live-model) (at-beat 40.0)
                  (timeline/remember-sections [(section-change 8.0 16.0)
                                               {:type :audio/kick :time-ms 100.0}
                                               (section-change 24.0 32.0 :evidence {:drivers [:bass-pattern :groove-accents]}
                                                               :confidence 0.7)])
                  (timeline/remember-sections [(section-change 36.0 44.0 :clock-epoch 9)
                                               (section-change 48.0 56.0)]))
        items (get-in (timeline/view {:model model} {:bars 8}) [:sections :items])]
    (is (= 4 (count (get-in model [:variables :audio :sections]))) "only section changes are remembered")
    (is (= [{:key "causal/8.0" :source :causal :state :confirmed :start-beat 8.0 :end-beat 24.0
             :label "Bass change" :drivers [:bass-pattern] :confidence 0.9 :confirmed-at-beat 16.0
             :start-position {:bar 2 :beat 0.0 :phrase 0}}
            {:key "causal/24.0" :source :causal :state :confirmed :start-beat 24.0 :open? true
             :label "Bass + Groove change" :drivers [:bass-pattern :groove-accents] :confidence 0.7
             :confirmed-at-beat 32.0 :start-position {:bar 6 :beat 0.0 :phrase 1}}]
           items)
        "another clock epoch and a boundary ahead of the transport are not history on this clock")
    (testing "history is bounded"
      (let [many (reduce #(timeline/remember-sections %1 [(section-change (* 4.0 %2) (+ 8.0 (* 4.0 %2)))])
                         (live-model) (range 50))]
        (is (= 32 (count (get-in many [:variables :audio :sections]))))
        (is (= 196.0 (:boundary-beat (peek (get-in many [:variables :audio :sections])))))))))

(def ^:private profile
  {:reference {:id "sha256:track" :version "sha256:analysis"}
   :sections [{:id 1 :family-id 0 :start-position-ms 8000.0 :end-position-ms 24000.0 :start-beat 16.0 :boundary-confidence 0.8}
              {:id 2 :family-id 1 :start-position-ms 24000.0 :end-position-ms 44000.0 :start-beat 48.0 :boundary-confidence 0.6}
              {:id 3 :family-id 0 :start-position-ms 44000.0 :end-position-ms 90000.0 :start-beat 88.0 :boundary-confidence 0.9}]})

(defn- lock [rate observations]
  (let [provenance {:source-id :deck :session-id "s1" :clock-id :host}
        match (fn [state observed position]
                (position/advance state observed
                                  {:type :match :provenance provenance :occurrence 0 :observed-at-ms observed
                                   :match {:reference (:reference profile) :position-ms position :rate rate :score 0.95}}))]
    (reduce (fn [state [observed position]] (match state observed position))
            (position/new-state provenance) observations)))

(deftest reference-sections-are-predicted-through-the-lock-and-vanish-when-it-breaks
  (let [model (at-beat (live-model) 40.0)
        ;; The recording is 20 s in when the show is at beat 40 (120 BPM: 2 beats a second).
        locked (lock 1.0 [[19800.0 19800.0] [19900.0 19900.0] [20000.0 20000.0]])
        view (fn [state now] (timeline/view {:model model :reference {:profile profile :lock (position/snapshot state now)}}
                                            {:bars 8}))
        {:keys [items match]} (:sections (view locked 20000.0))]
    (is (= :locked (:status match)))
    (is (= (:reference profile) (:reference match)))
    (is (= [["reference/1" :matched 16.0 48.0 "A" 0.8]
            ["reference/2" :predicted 48.0 88.0 "B" 0.6]]
           (mapv (juxt :key :state :start-beat :end-beat :label :confidence) items))
        "section 3 starts at beat 88, beyond the eight-bar window and its margin")
    (testing "half speed stretches the forecast: the same boundary is twice as far away"
      (let [slow (lock 0.5 [[19800.0 19900.0] [19900.0 19950.0] [20000.0 20000.0]])
            items (get-in (view slow 20000.0) [:sections :items])]
        (is (= 0.5 (:rate (position/snapshot slow 20000.0))))
        (is (= [56.0 :predicted] ((juxt :start-beat :state) (second items))))))
    (testing "an expired lock removes every reference section and says why"
      (let [{:keys [items match]} (:sections (view locked 21000.0))]
        (is (empty? items))
        (is (= [:unavailable :expired] ((juxt :status :reason) match)))))
    (testing "a lock on another recording does not place this profile"
      (let [other (assoc-in profile [:reference :id] "sha256:other")
            sections (:sections (timeline/view {:model model :reference {:profile other :lock (position/snapshot locked 20000.0)}}))]
        (is (empty? (:items sections)))))
    (is (nil? (get-in (timeline/view {:model model}) [:sections :match])) "no reference attached")))

;; Audio ----------------------------------------------------------------------------

(defn- bins [from to width]
  (mapv (fn [start] {:min -0.5 :max 0.5 :rms 0.25 :bands {:low 0.2 :mid 0.1 :high 0.05}
                     :start-time-ms (double start) :end-time-ms (double (+ start width)) :samples 480})
        (range from to width)))

(deftest audio-columns-follow-the-captured-transport-across-a-tempo-change
  ;; 120 BPM until 2000 ms (beat 4), then 60 BPM. The capture worker stamped a
  ;; marker on every beat from the transport it was given at the time.
  (let [markers (concat (for [beat (range 0 5)] {:time-ms (* 500.0 beat) :beat (double beat)})
                        (for [beat (range 5 7)] {:time-ms (+ 2000.0 (* 1000.0 (- beat 4))) :beat (double beat)}))
        waveform {:bins (bins 0 4500 100) :beat-markers (vec markers) :timing-valid? true :available? true
                  :source-id "run" :generation 1}
        model (-> (live-model) (assoc :time-ms 4500.0)
                  (update :transport assoc :beat 6.5 :bpm 60.0 :time-ms 4500.0))
        {:keys [columns column-ms legend] :as audio} (:audio (timeline/view {:model model :waveform waveform} {:bars 2}))
        column (fn [start-ms] (get columns (long (/ start-ms 100))))]
    (is (:available? audio))
    (is (= 100.0 column-ms))
    (is (= [:start-beat :end-beat :min :max :rms :low :mid :high] legend))
    (is (= 45 (count columns)))
    (testing "100 ms is a fifth of a beat at 120 BPM and a tenth at 60 BPM"
      (is (= [0.0 0.2 -0.5 0.5 0.25 0.2 0.1 0.05] (column 0)))
      (is (= [3.8 4.0] (subvec (column 1900) 0 2)))
      (is (= [4.0 4.1] (subvec (column 2000) 0 2)))
      (is (= [5.9 6.0] (subvec (column 3900) 0 2))))
    (testing "audio after the last marker reaches the committed transport beat and stops at now"
      (is (= [6.4 6.5] (subvec (column 4400) 0 2))))
    (testing "an unchanged column keeps its exact value on the next frame, so a delta carries only new audio"
      (let [later (-> model (assoc :time-ms 4600.0) (update :transport assoc :beat 6.6 :time-ms 4600.0))
            next-columns (get-in (timeline/view {:model later :waveform (update waveform :bins conj (first (bins 4500 4600 100)))}
                                                {:bars 2})
                                 [:audio :columns])]
        (is (= columns (dissoc next-columns 45)))
        (is (= [6.5 6.6] (subvec (get next-columns 45) 0 2)))))
    (testing "only the window and its margin are carried"
      (let [narrow (get-in (timeline/view {:model model :waveform waveform} {:bars 1}) [:audio :columns])]
        (is (= [0.0 0.2] (subvec (get narrow 0) 0 2)) "bar 1 minus one bar of margin starts at beat 0")
        (is (= 45 (count narrow)))))))

(deftest audio-is-not-placed-without-a-musical-position
  (let [waveform {:bins (bins 0 1000 100) :beat-markers [] :timing-valid? true :available? true}
        paused (-> (live-model) (assoc :time-ms 1000.0)
                   (update :transport assoc :beat 2.0 :time-ms 1000.0 :playing? false))]
    (is (empty? (get-in (timeline/view {:model paused :waveform waveform}) [:audio :columns]))
        "a stopped transport gives captured audio no beat")
    (is (= 10 (count (get-in (timeline/view {:model (assoc-in paused [:transport :playing?] true) :waveform waveform})
                             [:audio :columns])))
        "a running transport places audio from its tempo until the first marker arrives")
    (is (= {:available? true :columns {} :timing-valid? false}
           (select-keys (:audio (timeline/view {:model (assoc-in paused [:transport :playing?] true)
                                                :waveform (assoc waveform :timing-valid? false)}))
                        [:available? :columns :timing-valid?]))
        "captured audio without valid capture timing is reported, not placed")
    (is (= {:available? false :columns {} :timing-valid? false}
           (select-keys (:audio (timeline/view {:model paused})) [:available? :columns :timing-valid?])))
    (is (= {:available? false :reason :no-reference-waveform}
           (get-in (timeline/view {:model paused :waveform waveform}) [:audio :reference]))
        "the matched-recording look-ahead is a named seam on this base"))
  (testing "a loop back keeps only audio captured since the transport restarted"
    (let [markers [{:time-ms 0.0 :beat 62.0} {:time-ms 500.0 :beat 63.0} {:time-ms 1000.0 :beat 0.0} {:time-ms 1500.0 :beat 1.0}]
          model (-> (live-model) (assoc :time-ms 1750.0) (update :transport assoc :beat 1.5 :time-ms 1750.0))
          columns (get-in (timeline/view {:model model :waveform {:bins (bins 0 1700 100) :beat-markers markers
                                                                   :timing-valid? true :available? true}})
                          [:audio :columns])]
      (is (= [1.0 1.2] (subvec (get columns 15) 0 2)))
      (is (= [-0.2 0.0] (subvec (get columns 9) 0 2)) "older audio continues backwards from the new position")
      (is (every? #(<= (second %) 1.5) (vals columns))))))

(deftest real-capture-fills-the-history-and-the-section-lane
  (let [closed (atom 0)
        model (-> (live-model) (assoc :time-ms 5000.0) (update :transport assoc :time-ms 5000.0 :beat 10.0))
        rt (show/create-runtime
            {:model model :workspace-options nil
             :audio-options {:open-source! (fn [_] (pcm-test/memory-source (pcm-test/section-pcm-values) 719 closed))}})]
    (try
      (show/tick! rt 5000)
      (is (false? (get-in (show/timeline rt) [:audio :available?])) "nothing captured yet")
      (let [handle (connections/start-audio! (:connections rt)
                                             {:default? true :sample-rate 8000.0 :block-frames 640
                                              :analyzer-options {:sections {:beats-per-window 2}}}
                                             (show/audio-callbacks rt))]
        (is (= :completed (:status (deref (:result handle) 5000 ::timeout)))))
      (doseq [t [5001 5002 5020 5050]] (show/tick! rt t))
      (let [manager (connections/audio (:connections rt))
            history (capture/history-waveform manager)
            {:keys [audio sections meters beat]} (show/timeline rt)]
        (testing "the capture worker keeps a longer, coarser history beside the public status"
          (is (= 60.0 (- (:end-time-ms (first (:bins history))) (:start-time-ms (first (:bins history))))))
          (is (= 5000.0 (:end-time-ms history)) "all five seconds of the injected PCM")
          (is (= 250 (count (:bins (capture/waveform manager)))) "the 20 ms projection is unchanged")
          (is (not (contains? (capture/status manager) :history))))
        (testing "captured columns are placed by the beats the transport reported during capture"
          (is (:available? audio))
          (is (= 60.0 (:column-ms audio)))
          (is (seq (:columns audio)))
          (is (every? (fn [[from to low high rms]] (and (< from to) (<= to beat) (<= -1.0 low high 1.0) (<= 0.0 rms 1.0)))
                      (vals (:columns audio)))))
        (testing "the analyzer's confirmed change is a section on the lane"
          (is (= [{:source :causal :state :confirmed :start-beat 4.0 :open? true :label "Bass change" :confirmed-at-beat 8.0}]
                 (mapv #(select-keys % [:source :state :start-beat :open? :label :confirmed-at-beat]) (:items sections)))))
        (testing "meters read the analyzer's own features"
          (is (= [:rms :energy-now :audio/energy :brightness :activity] (mapv :id meters)))
          (is (every? :available? (take 3 meters)))
          (is (every? #(<= 0.0 (:value %) 1.0) (take 3 meters)))))
      (finally (show/stop! rt)))))

;; Meters and the feel slot -----------------------------------------------------------

(deftest meters-report-measured-levels-and-never-a-stale-reading
  (let [features {:time-ms 900.0 :rms 0.25 :peak 0.6 :energy-dbfs -12.04 :relative-energy 0.4 :energy-class :typical
                  :breakdown? false}
        model (-> (live-model) (assoc :time-ms 1000.0)
                  (assoc-in [:variables :audio :features] features)
                  (assoc-in [:variables :audio/energy] 0.3)
                  (assoc-in [:variables :lighting/autopilot]
                            {:enabled? true :targets {:brightness 0.5 :activity 0.4}
                             :statistics {:brightness {:instant 0.62 :mean 0.48} :activity {:instant 0.2 :mean 0.35}}}))
        output {:lighting-budget {:global {:used-watts 120.04 :limit-watts 400.0}}
                :lighting-activity {:scene {:score 0.9 :brightness 0.9}}}
        by-meter (fn [view] (into {} (map (juxt :id identity)) (:meters view)))
        live (by-meter (timeline/view {:model model :output output}))]
    (is (= {:id :rms :label "RMS" :available? true :value 0.25 :peak 0.6 :dbfs -12.0} (:rms live)))
    (is (= {:id :energy-now :label "Now" :available? true :value 0.2 :relative 0.4 :class :typical :breakdown? false}
           (:energy-now live))
        "energy against the rolling baseline: 1.0 is typical and maps to half scale")
    (is (= {:id :audio/energy :label "Energy" :available? true :value 0.3} (:audio/energy live)))
    (is (= {:id :brightness :label "Light" :available? true :value 0.62 :mean 0.48 :target 0.5
            :used-watts 120.0 :limit-watts 400.0}
           (:brightness live)))
    (is (= {:id :activity :label "Activity" :available? true :value 0.2 :mean 0.35 :target 0.4} (:activity live)))
    (testing "without the conductor the measured scene is shown and no target is drawn"
      (let [manual (by-meter (timeline/view {:model (update model :variables dissoc :lighting/autopilot) :output output}))]
        (is (= {:id :activity :label "Activity" :available? true :value 0.9} (:activity manual)))
        (is (= 0.9 (get-in manual [:brightness :value])))
        (is (not (contains? (:brightness manual) :target)))))
    (testing "features older than two seconds are reported unavailable, not as a level"
      (let [stale (by-meter (timeline/view {:model (assoc model :time-ms 3000.0) :output {}}))]
        (is (= [false false false] (mapv #(get-in stale [% :available?]) [:rms :energy-now :audio/energy])))
        (is (not-any? #(contains? (get stale %) :value) [:rms :energy-now :audio/energy]))
        (is (true? (get-in stale [:brightness :available?])) "the conductor's own statistics remain")))
    (testing "an empty show has every meter present and unavailable"
      (is (= [[:rms false] [:energy-now false] [:audio/energy false] [:brightness false] [:activity false]]
             (mapv (juxt :id :available?) (:meters (timeline/view {:model (live-model)}))))))))

(deftest feel-slot-is-filled-from-data
  (let [feel {:id :squircle :label "Squircle" :icon-svg "<svg/>" :palette ["#f0a" "#0af"] :geometry {:private true}}]
    (is (nil? (:feel (timeline/view {:model (live-model)}))))
    (is (= (dissoc feel :geometry) (:feel (timeline/view {:model (live-model) :feel feel}))))
    (let [rt (runtime)]
      (try
        (is (= :accepted (:status (at! rt 0 {:op :variable/set :variable :music/personality :value feel
                                             :source {:adapter :repl :session-id "r"}}))))
        (is (= "Squircle" (get-in (show/timeline rt) [:feel :label])))
        (finally (show/stop! rt))))))

;; Studio state stream -------------------------------------------------------------------

(defn- connect [server session]
  (let [port (.getLocalPort (first (.getConnectors server)))
        messages (LinkedBlockingQueue.)
        buffer (StringBuilder.)
        uri (URI. (str "ws://127.0.0.1:" port "/api/state/stream?session=" session))
        socket (.get (.buildAsync (.newWebSocketBuilder (HttpClient/newHttpClient)) uri
                                  (reify WebSocket$Listener
                                    (onOpen [_ ws] (.request ws 1))
                                    (onText [_ ws chunk last?]
                                      (.append buffer ^CharSequence chunk)
                                      (when last?
                                        (.offer messages (transit/decode (.toString buffer)))
                                        (.setLength buffer 0))
                                      (.request ws 1)
                                      nil))) 5 TimeUnit/SECONDS)]
    {:socket socket :messages messages}))

(defn- receive [client]
  (or (.poll ^LinkedBlockingQueue (:messages client) 3 TimeUnit/SECONDS)
      (throw (ex-info "Expected a pushed state message" {}))))

(defn- send-control [client data]
  (.get (.sendText ^WebSocket (:socket client) (transit/encode data) true) 3 TimeUnit/SECONDS))

(defn- apply-patch [base {:keys [kind view changes]}]
  (if (= kind :full) view
      (reduce (fn [data [op path value]]
                (case op
                  :set (assoc-in data path value)
                  :remove (update-in data (vec (butlast path)) dissoc (peek path))))
              base changes)))

(defn- next-view [client base]
  (let [message (receive client)
        view (apply-patch base message)]
    (send-control client {:op :ack :token (:token message)})
    view))

(deftest studio-leases-the-same-projection-and-sees-a-cancel
  (let [rt (runtime)
        server (jetty/run-jetty (studio/handler rt) {:host "127.0.0.1" :port 0 :join? false})
        client (connect server "timeline&view=queries")
        query [timeline/query-id {:bars 4}]
        value (fn [view] (get-in view [:subscriptions query :value]))]
    (try
      (is (= :accepted (:status (at! rt 0 {:op :schedule/post :source agent-source
                                           :events [{:id "drop" :label "Drop" :op :cue/launch :cue-id :wash :at {:bar 2}}]}))))
      (let [base (next-view client nil)]
        (send-control client {:op :queries :queries [query]})
        (let [leased (next-view client base)]
          (is (= (show/timeline rt {:bars 4}) (value leased))
              "the socket carries exactly what the REPL reads, with its keyword and numeric types")
          (is (= [["drop" :pending 8.0 true]] (mapv (juxt :event-id :state :beat :cancellable?) (:events (value leased)))))
          (testing "a cancel sent as the shared command reaches the leased view as a change"
            (is (= :accepted (:status (at! rt 250 {:op :schedule/cancel :event-ids ["drop"] :source studio-source}))))
            (let [after (loop [view leased remaining 5]
                          (let [view (next-view client view)]
                            (if (or (zero? remaining) (= :cancelled (:state (first (:events (value view))))))
                              view (recur view (dec remaining)))))]
              (is (= [["drop" :cancelled 0.5 false]]
                     (mapv (juxt :event-id :state :beat :cancellable?) (:events (value after)))))
              (is (= (show/timeline rt {:bars 4}) (value after)))))
          (testing "an invalid window is an unavailable query, not a guess"
            (send-control client {:op :queries :queries [[timeline/query-id {:bars 400}]]})
            (let [view (next-view client nil)]
              (is (= {:error :unavailable} (get-in view [:subscriptions [timeline/query-id {:bars 400}]])))))))
      (finally (.abort ^WebSocket (:socket client)) (show/stop! rt) (.stop server)))))
