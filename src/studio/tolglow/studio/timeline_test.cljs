(ns tolglow.studio.timeline-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.events-test :refer [base first-command transition]]
            [tolglow.studio.timeline :as timeline]
            [tolglow.studio.timeline-model :as model]))

(def view
  "A projection as (show/timeline runtime {:bars 2}) returns it."
  {:version 1 :beat 20.0 :bpm 120.0 :playing? true :time-ms 10000.0 :epoch 7
   :position {:bar 5 :beat 0.0 :phrase 1}
   :meter {:beats-per-bar 4 :bars-per-phrase 4}
   :window {:bars 2 :past-beats 8.0 :future-beats 8.0 :from-beat 8.0 :to-beat 36.0}
   :grid {:lines [{:beat 16.0 :kind :phrase :bar 4 :phrase 1} {:beat 17.0 :kind :beat}
                  {:beat 20.0 :kind :bar :bar 5} {:beat 24.0 :kind :bar :bar 6}]}
   :events [{:key "fired/wash/18" :event-id "wash" :state :fired :beat 18.0 :due-beat 18.0 :at-beat 18.0
             :op :cue/launch :category :cue :label "Wash in" :title "Wash in" :cancellable? false
             :position {:bar 4 :beat 2.0 :phrase 1} :source {:adapter :agent :session-id "run-7"}}
            {:key "pending/drop/0" :event-id "drop" :state :pending :beat 24.0 :due-beat 24.0
             :op :cue/launch :category :cue :label "Drop" :title "Drop" :origin :scheduled :cancellable? true
             :position {:bar 6 :beat 0.0 :phrase 1} :source {:adapter :agent :session-id "run-7"}}
            {:key "pending/dark/0" :event-id "dark" :state :pending :beat 24.0 :due-beat 24.0
             :op :master/set :category :master :title "master set" :value 0.0 :origin :scheduled :cancellable? true
             :position {:bar 6 :beat 0.0 :phrase 1} :source {:adapter :studio :session-id "tab"}}
            {:key "pending/subscriber/kicks/3/1" :event-id "subscriber/kicks/3" :state :pending :beat 26.0
             :op :cue/launch :category :cue :title "cue launch" :origin :subscriber :cancellable? false
             :position {:bar 6 :beat 2.0 :phrase 1}}]
   :ramps [{:key "swell/dest/16.0" :event-id "swell" :state :running :category :ramp :op :param/ramp
            :label "Swell" :title "Swell" :start-beat 16.0 :end-beat 24.0 :unit :beats :curve :ease-in-out
            :shape [0.0 0.125 0.5 0.875 1.0] :from 0.2 :to 0.8 :over {:bars 2} :cancellable? true
            :destination {:target :param :instance-id "1:3" :param :level}
            :start-position {:bar 4 :beat 0.0 :phrase 1} :end-position {:bar 6 :beat 0.0 :phrase 1}}]
   :sections {:items [{:key "causal/12.0" :source :causal :state :confirmed :start-beat 12.0 :open? true
                       :label "Bass change" :confidence 0.9}
                      {:key "reference/2" :source :reference :state :predicted :start-beat 28.0 :end-beat 44.0
                       :label "B" :confidence 0.6}]
              :match {:status :locked :reference {:id "sha256:track"}}}
   :audio {:available? true :timing-valid? true :columns {166 [19.8 19.9 -0.5 0.5 0.25 0.2 0.1 0.05]}}
   :meters [{:id :rms :label "RMS" :available? true :value 0.25 :peak 0.6 :dbfs -12.0}
            {:id :energy-now :label "Now" :available? true :value 0.2 :relative 0.4 :class :typical :breakdown? true}
            {:id :audio/energy :label "Energy" :available? false}
            {:id :brightness :label "Light" :available? true :value 0.62 :mean 0.48 :target 0.5
             :used-watts 120.0 :limit-watts 400.0}
            {:id :activity :label "Activity" :available? true :value 0.2 :mean 0.35 :target 0.4}]
   :feel nil})

(defn- store-with [projection]
  (let [store (timeline/new-store)]
    (reset! (:width store) 800)
    (timeline/install! store projection 1000)
    store))

(defn- html [component] (renderToStaticMarkup (r/as-element component)))

(defn- element
  "The opening tag whose attributes contain `marker`."
  [markup marker]
  (some #(when (str/includes? % marker) %) (re-seq #"<[a-z]+[^>]*>" markup)))

(deftest settings-default-to-eight-bars-with-every-lane-open
  (is (= {:open? true :bars 8 :collapsed #{}} (timeline/settings {})))
  (is (= 4 (:bars (timeline/settings {:ui {:timeline {:bars 4}}}))))
  (testing "selecting the selected item again closes its details"
    (let [chosen (timeline/toggle-selection {} "drop")]
      (is (= "drop" (get-in chosen [:ui :timeline :selected])))
      (is (nil? (get-in (timeline/toggle-selection chosen "drop") [:ui :timeline :selected])))
      (is (= "swell" (get-in (timeline/toggle-selection chosen "swell") [:ui :timeline :selected]))))))

(deftest installing-a-frame-only-touches-the-lanes-that-changed
  (let [store (store-with view)
        cell (fn [key] @(get-in store [:parts key]))
        events (cell :events)]
    (is (= 8.0 @(:origin store)) "the layer origin is the first window edge seen")
    (is (= {:bar 5 :phrase 1 :beat 0 :bpm 120.0 :playing? true} (cell :clock)))
    (is (= {:available? true :timing-valid? true :empty? false} (cell :audio-status)))
    (timeline/install! store (assoc view :beat 20.05 :time-ms 10025.0 :position {:bar 5 :beat 0.05 :phrase 1}) 1030)
    (is (identical? events (cell :events)) "an advancing beat does not re-render the event lane")
    (is (= 0 (:beat (cell :clock))) "the readout changes once a beat")
    (testing "the playhead got both committed samples and joins them"
      (is (= 20.0 (model/position @(:playhead store) 1030)))
      (is (= 20.05 (model/position @(:playhead store) 1055))))
    (timeline/install! store nil 1100)
    (is (identical? events (cell :events)) "a window still loading keeps the previous lanes")))

(deftest events-are-icons-by-category-and-ramps-are-bars-with-their-curve
  (let [store (store-with view)
        markup (html [timeline/events-lane store "drop" identity])
        tag (partial element markup)]
    (testing "one button per event, coloured and named by category and state"
      (is (= 4 (count (re-seq #"class=\"timeline-event" markup))))
      (is (str/includes? (tag "data-event-id=\"wash\"") "data-state=\"fired\""))
      (is (str/includes? (tag "data-event-id=\"wash\"") "aria-label=\"Wash in, cue, fired, bar 4 · beat 2\""))
      (is (str/includes? (tag "data-event-id=\"dark\"") "data-category=\"master\""))
      (is (str/includes? (tag "data-event-id=\"drop\"") "aria-pressed=\"true\""))
      (is (str/includes? (tag "data-event-id=\"dark\"") "aria-pressed=\"false\"")))
    (testing "an event sits at its beat: 50 px a beat, measured from the layer origin at beat 8"
      (is (str/includes? (tag "data-event-id=\"wash\"") "left:478.00px") "beat 18 is 500 px in, minus half a target")
      (is (str/includes? (tag "data-event-id=\"drop\"") "left:778.00px")))
    (testing "two events on the same beat and the ramp ending there take separate lines"
      (is (= #{"top:0.00px" "top:48.00px" "top:96.00px"}
             (set (map #(re-find #"top:[0-9.]+px" (tag (str "data-event-id=\"" % "\""))) ["swell" "drop" "dark"]))))
      (is (str/includes? markup "height:144.00px")))
    (testing "the ramp is a bar from start to end with the projected easing inside"
      (let [ramp (tag "class=\"timeline-ramp")]
        (is (str/includes? ramp "left:400.00px"))
        (is (str/includes? ramp "width:400.00px"))
        (is (str/includes? ramp "data-state=\"running\"")))
      (is (str/includes? markup "points=\"0,1.0000 1,0.8750 2,0.5000 3,0.1250 4,0.0000\"")))
    (testing "the past half holds the same bar filled, so the part left of now is its progress"
      (is (str/includes? (tag "class=\"timeline-past\"") "width:400.00px") "clipped at the centre line")
      (is (str/includes? markup "class=\"timeline-ramp-fill\""))
      (is (str/includes? markup "<polygon points=\"0,1.15 0,1.0000")))
    (testing "every event action is at least a 44 px target"
      (is (every? #(<= 44 (js/parseFloat %)) (map second (re-seq #"<button[^>]*width:([0-9.]+)px" markup)))))
    (is (str/includes? (html [timeline/events-lane (store-with (assoc view :events [] :ramps [])) nil identity])
                       "Nothing scheduled in this window"))))

(deftest selected-events-show-details-and-cancel-through-the-shared-command
  (let [store (store-with view)
        scale (model/scale 800 (:window view))
        drop-item (model/event-box scale 8.0 (second (:events view)))
        ramp-item (model/ramp-box scale 8.0 (first (:ramps view)))
        sent (atom nil)
        click! (fn [hiccup]
                 (let [button (some #(when (and (vector? %) (= :button.button.timeline-cancel (first %))) %)
                                    (tree-seq #(and (sequential? %) (not (map? %))) seq hiccup))]
                   (with-redefs [rf/dispatch #(reset! sent %)] ((:on-click (second button))))
                   @sent))]
    (testing "a scheduled event can be cancelled; the command is the one the REPL submits"
      (let [markup (html [timeline/detail-panel drop-item true])]
        (is (str/includes? markup "Cancel event"))
        (is (str/includes? markup "aria-label=\"Cancel event Drop\""))
        (is (str/includes? markup "bar 6 · beat 0"))
        (is (str/includes? markup "agent · run-7")))
      (is (= [:studio/command "schedule/cancel" {:event-ids ["drop"]}] (click! (timeline/detail-panel drop-item true))))
      (let [command (first-command (transition (assoc-in base [:show :runtime-id] "host") @sent))]
        (is (= "schedule/cancel" (:op command)))
        (is (= {:event-ids ["drop"]} (:fields command)))
        (is (= 7 (:epoch command)) "sent against the show epoch on screen")))
    (testing "a running ramp is stopped by its event ID"
      (is (str/includes? (html [timeline/detail-panel ramp-item true]) "Stop ramp"))
      (is (= [:studio/command "schedule/cancel" {:event-ids ["swell"]}] (click! (timeline/detail-panel ramp-item true)))))
    (testing "offline, the action is visible and disabled"
      (is (re-find #"<button[^>]*disabled[^>]*>Cancel event" (html [timeline/detail-panel drop-item false]))))
    (testing "fired events and subscriber steps offer no cancel"
      (doseq [event [(first (:events view)) (nth (:events view) 3)]]
        (is (not (str/includes? (html [timeline/detail-panel (model/event-box scale 8.0 event) true]) "timeline-cancel")))))
    (is (some? store))))

(deftest sections-split-into-confirmed-past-and-predicted-future
  (let [markup (html [timeline/sections-lane (store-with view)])
        [past future] (str/split markup #"class=\"timeline-future\"")]
    (is (str/includes? past "data-source=\"causal\""))
    (is (str/includes? past "Bass change"))
    (is (str/includes? past "90%"))
    (is (not (str/includes? past "data-source=\"reference\"")))
    (is (str/includes? future "data-state=\"predicted\""))
    (is (str/includes? future "data-shift=\"400\"") "the future half starts at the centre line")
    (is (str/includes? future "left:1000.00px") "beat 28 on the shared 50 px a beat scale")
    (is (str/includes? future "Matched recording"))
    (testing "a broken match says so and shows no forecast"
      (let [lost (html [timeline/sections-lane
                        (store-with (assoc view :sections {:items [] :match {:status :unavailable :reason :expired}}))])]
        (is (str/includes? lost "No match · expired"))
        (is (not (str/includes? lost "data-state=\"predicted\"")))))
    (is (str/includes? (html [timeline/sections-lane (store-with (assoc view :sections {:items [] :match nil}))])
                       "No section analysis"))))

(deftest grid-ruler-uses-the-projected-lines-and-emphasises-phrases
  (let [markup (html [timeline/grid-lane (store-with view)])]
    (is (= 4 (count (re-seq #"class=\"timeline-tick\"" markup))))
    (is (re-find #"<div data-kind=\"phrase\" style=\"left:400.00px\"[^>]*><span>4</span><small>phrase 1</small>" markup))
    (is (re-find #"<div data-kind=\"bar\" style=\"left:600.00px\"[^>]*><span>5</span></div>" markup))
    (is (re-find #"<div data-kind=\"beat\" style=\"left:450.00px\"[^>]*></div>" markup) "a beat tick carries no label")))

(defn- tick-screen-x
  "Where the grid tick at content `left` is painted: its left inside the layer
  plus the translation that layer was rendered with."
  [markup left]
  (let [layer (element markup "class=\"timeline-layer")
        [_ shift] (re-find #"translate3d\((-?[0-9.]+)px" (str layer))]
    (is (some? shift) "the layer is rendered with its transform")
    (+ left (js/parseFloat shift))))

(deftest a-zoom-renders-new-positions-with-their-own-transform
  ;; #801: the frame loop used to apply the new transform one tick after the
  ;; render that moved the children, so zooming out drew upcoming events behind now.
  (let [store (store-with view)
        before (html [timeline/grid-lane store])]
    (is (re-find #"data-kind=\"bar\" style=\"left:600.00px\"" before) "beat 20 is 12 beats from origin 8 at 50 px a beat")
    (is (= 400 (tick-screen-x before 600)) "the playhead's bar line is under the centre line")
    (timeline/install! store (assoc view :window {:bars 4 :past-beats 16.0 :future-beats 16.0 :from-beat 0.0 :to-beat 40.0})
                       1500)
    (is (= 8.0 @(:origin store)) "the zoom keeps the layer origin")
    (let [after (html [timeline/grid-lane store])]
      (is (re-find #"data-kind=\"bar\" style=\"left:300.00px\"" after) "25 px a beat after zooming out")
      (is (= 400 (tick-screen-x after 300)) "the same render moves the layer, so the bar line stays at now"))
    (testing "every layer of the event lane agrees with its children"
      (let [lane (html [timeline/events-lane store nil identity])]
        (is (= #{"translate3d(100.00px,0,0)"} (set (map second (re-seq #"transform:(translate3d\([^)]*\))" lane)))))
        (is (str/includes? (element lane "data-event-id=\"drop\"") "left:378.00px")
            "the pending event at beat 24 is drawn 16 beats from the origin, so 4 beats right of now")))))

(deftest meters-show-measured-levels-and-mark-missing-readings
  (let [markup (html [timeline/meters-panel (store-with view)])
        tag (partial element markup)]
    (is (= 5 (count (re-seq #"role=\"meter\"" markup))))
    (is (str/includes? (tag "data-meter=\"rms\"") "aria-valuenow=\"0.25\""))
    (is (str/includes? (tag "data-meter=\"rms\"") "aria-valuetext=\"25%, -12 dBFS\""))
    (is (str/includes? (tag "data-meter=\"energy-now\"") "0.4 × recent level, breakdown"))
    (is (str/includes? (tag "data-meter=\"brightness\"") "62%, 120 of 400 W"))
    (testing "an absent reading is not drawn as a level"
      (is (str/includes? (tag "data-meter=\"audio/energy\"") "unavailable"))
      (is (str/includes? (tag "data-meter=\"audio/energy\"") "aria-valuetext=\"No reading\""))
      (is (str/includes? markup "<output>—</output>")))
    (testing "budget targets are marked where the conductor has them"
      (is (= 2 (count (re-seq #"timeline-meter-mark target" markup))))
      (is (str/includes? markup "bottom:50%")))
    (testing "the feel slot is empty until a personality is published, then drawn from its data"
      (is (str/includes? markup "aria-label=\"Track feel: none\""))
      (let [filled (html [timeline/feel-slot {:id :squircle :label "Squircle" :icon-svg "<svg xmlns='http://www.w3.org/2000/svg'/>"
                                              :palette ["#ff00aa" "#00aaff" 7]}])]
        (is (str/includes? filled "aria-label=\"Track feel: Squircle\""))
        (is (str/includes? filled "src=\"data:image/svg+xml;charset=utf-8,%3Csvg"))
        (is (= 2 (count (re-seq #"<i style=\"background:" filled))) "only colour strings become swatches")))))

(deftest a-stale-sample-says-the-timeline-is-behind
  ;; The server marks a query :stale while its sampler is behind (#906).
  (let [store (timeline/new-store)
        markup #(renderToStaticMarkup (r/as-element [timeline/status-line store]))]
    (reset! (:status store) :stale)
    (reset! (:stale store) "The timeline is still being sampled; showing frame 40.")
    (is (str/includes? (markup) "The timeline is still being sampled; showing frame 40."))
    (reset! (:status store) :ready)
    (is (= "" (markup)))))
